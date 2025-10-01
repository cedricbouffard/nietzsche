
#' Module résultats — UI
#'
#' Affiche le graphique principal (ECharts) de puissance vs nombre de répétitions.
#' Respecte les libellés, styles et comportements du fichier original.
#'
#' @param id identifiant du module
#' @keywords internal
mod_results_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    full_screen = TRUE,

    # En-tête identique
    bslib::card_header(
      class = "bg-primary text-white",
      htmltools::tags$strong(
        shiny::icon("chart-area"),
        " Puissance statistique vs Nombre de répétitions"
      )
    ),

    # Graphique interactif
    bslib::card_body(
      shinycssloaders::withSpinner(
        echarts4r::echarts4rOutput(ns("main_plot"), height = "520px"),
        type = 4
      )
    )
  )
}


#' Module résultats — server
#'
#' Calcule la puissance (différence) ou la puissance TOST (équivalence)
#' sur une grille de n répétitions et trace les courbes.
#'
#' @param id identifiant du module
#' @param params liste réactive retournée par `mod_params_server()`
#' @keywords internal
mod_results_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ================== Simulation principale ==================
    resultats_sim <- shiny::reactive({
      mu        <- params$mu()
      sd_val    <- params$sd_eff()
      alpha     <- params$alpha()
      n_sims    <- params$n_sims()
      alt       <- params$alternative()
      equal_var <- params$equal_var()
      reps      <- 2:30
      mode_hyp  <- params$hyp_mode()
      if (identical(mode_hyp, "diff")) {
        # --- Comparaison de traitements ---
        df_trt <- params$traitements()
        if (nrow(df_trt) == 0) return(NULL)

        effets <- df_trt |>
          dplyr::mutate(
            effet_num = dplyr::if_else(.data$mode == "add", .data$val, .data$val / 100)
          ) |>
          dplyr::select(nom, mode, effet_num)

        purrr::pmap_dfr(
          list(effet = effets$effet_num, nom = effets$nom, mode = effets$mode),
          function(effet, nom, mode) {
            tibble::tibble(
              x = reps,
              y = vapply(
                reps,
                function(nr) simuler_puissance(
                  n_rep = nr,
                  n_sims = n_sims,
                  mu = mu,
                  sd = sd_val,
                  effet = effet,
                  alpha = alpha,
                  alternative = alt,
                  equal_var = equal_var,
                  effect_mode = mode
                ),
                numeric(1)
              ),
              traitement_nom  = nom,
              traitement_mode = mode,
              effet_num       = effet,
              sd              = sd_val,
              p_value         = alpha,
              hyp             = "diff"
            )
          }
        )

      } else {
        # --- Test d'équivalence (TOST) ---
        delta_abs <- if (identical(params$equiv_mode(), "mult")) {
          (params$equiv_val() / 100) * mu
        } else {
          params$equiv_val()
        }

        purrr::map_dfr(reps, function(nr) {
          tibble::tibble(
            x = nr,
            y = simuler_puissance_tost(
              n_rep = nr,
              n_sims = n_sims,
              mu = mu,
              sd = sd_val,
              alpha = alpha,
              delta_abs = delta_abs,
              equal_var = equal_var
            ),
            traitement_nom  = "Équivalence",
            traitement_mode = "add",
            effet_num       = 0,
            sd              = sd_val,
            p_value         = alpha,
            hyp             = "equiv"
          )
        })
      }
    })

    # ================== Graphique principal ==================
    output$main_plot <- echarts4r::renderEcharts4r({
      data <- resultats_sim()
      mu        <- params$mu()
      sd_val    <- params$sd_eff()
      alpha     <- params$alpha()
      n_sims    <- params$n_sims()
      alt       <- params$alternative()
      equal_var <- params$equal_var()
      reps      <- 2:30
      mode_hyp  <- params$hyp_mode()

      # Sous-titre
      delta_abs = if (identical(params$equiv_mode(), "mult")) {
        paste0("δ = ±", .format_with_sign(params$equiv_val(), 2), " %")
      } else {
        paste0("δ = ±", .format_with_sign(params$equiv_val(), 3), " t/ha")
      }
      extra_equiv <- if (identical(mode_hyp, "equiv")) paste0(" — ", delta_abs) else ""
      subtext_str <- paste0(
        "Alpha : ", fmt_num(unique(alpha)[1]), " — ",

          paste0("Écart-type = ", fmt_num(sd_val), " t/ha"),

        extra_equiv
      )
      # Graphique "placeholder" si aucun traitement (libellés et axe X centré)
      if (is.null(data) || nrow(data) == 0) {
        ref <- data.frame(x = c(2, 15), y = c(0, 0))
        return(
          ref |>
            echarts4r::e_charts(x) |>
            echarts4r::e_x_axis(
              name = "Nombre de répétitions",
              nameLocation = "middle",
              nameGap = 28
            ) |>
            echarts4r::e_y_axis(
              min = 0, max = 1,
              axisLabel = list(
                formatter = htmlwidgets::JS("v => (v*100).toFixed(0) + ' %'")
              )
            ) |>
            echarts4r::e_mark_line(data = list(yAxis = 0.80), symbol = "none",
                                   lineStyle = list(type = "dashed"), silent = TRUE) |>
            echarts4r::e_mark_line(data = list(yAxis = 0.95), symbol = "none",
                                   lineStyle = list(type = "dashed"), silent = TRUE) |>
            echarts4r::e_title("Ajoute au moins un traitement pour voir la simulation") |>
            echarts4r::e_tooltip(trigger = "axis") |>
            echarts4r::e_legend(bottom = 0) |>
            echarts4r::e_grid(left = "8%", right = "4%", top = "14%", bottom = "16%")
        )
      }

      mu <- params$mu()

      # Étiquettes identiques
      chart_data <- data |>
        dplyr::mutate(
          traitement = dplyr::case_when(
            .data$hyp == "diff" & .data$traitement_mode == "add"  ~
              paste0(.data$traitement_nom, " (", .format_with_sign(.data$effet_num, 3), " t/ha)"),
            .data$hyp == "diff" & .data$traitement_mode == "mult" ~
              paste0(.data$traitement_nom, " (", .format_with_sign(.data$effet_num * 100, 2), " %)"),
            .data$hyp == "equiv" ~
              paste0(.data$traitement_nom, " (TOST)")
          )
        ) |>
        dplyr::arrange(traitement, x)

      chart_data |>
        dplyr::group_by(traitement) |>
        echarts4r::e_charts(x) |>
        echarts4r::e_line(y, smooth = TRUE, symbol = "none") |>
        echarts4r::e_x_axis(
          name = "Nombre de répétitions",
          nameLocation = "middle",
          nameGap = 28
        ) |>
        echarts4r::e_y_axis(
          min = 0, max = 1,
          name = "Puissance",
          nameLocation = "middle",
          nameGap = 42,
          axisLabel = list(
            formatter = htmlwidgets::JS("v => (v*100).toFixed(0) + ' %'")
          )
        ) |>

        echarts4r::e_title(
          text    = paste0("Puissance (rendement moyen ", fmt_num(mu), " t/ha)"),
          subtext = subtext_str
        ) |>
        echarts4r::e_mark_line(
          data = list(yAxis = 0.80),
          symbol = "none",
          lineStyle = list(opacity = 0.9, width = 1, color = "#222", type = "dashed"),
          title = "80 %", title_position = "end",
          silent = TRUE
        ) |>
        echarts4r::e_mark_line(
          data = list(yAxis = 0.95),
          symbol = "none",
          lineStyle = list(opacity = 0.9, width = 1, color = "#222", type = "dashed"),
          title = "95 %", title_position = "end",
          silent = TRUE
        ) |>
        echarts4r::e_tooltip(
          trigger = "axis",
          formatter = htmlwidgets::JS(
            "function (params) {
  if (!params || !params.length) return '';
  const reps = params[0].axisValue;
  let html = '<div><b>Nb de répétitions: ' + reps + '</b></div>';
  params.forEach(p => {
    if (p.seriesName === '_ref_') return;
    const y = Array.isArray(p.value) ? p.value[1] : p.value;
    if (y == null || isNaN(y)) return;
    const pct = (Number(y) * 100).toFixed(1) + ' %';
    html += '<div>' + p.marker + ' ' + p.seriesName + ': <b>' + pct + '</b></div>';
  });
  return html;
}"
          )
        ) |>
        echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_grid(left = "8%", right = "4%", top = "14%", bottom = "16%") |>
        echarts4r::e_animation(duration = 300)
    })
  })
}
