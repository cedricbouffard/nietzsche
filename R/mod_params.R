
#' Module paramètres — UI
#' @param id identifiant du module
#' @keywords internal
mod_params_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::accordion(
    open = c("essai_trt"),

    # --- Bloc 1 : Paramètres de l'essai et des traitements
    bslib::accordion_panel(
      title = htmltools::tagList(shiny::icon("seedling"), " Essai & traitements"),
      value = "essai_trt",

      shiny::numericInput(
        ns("mean_yield"),
        htmltools::tags$span(shiny::icon("seedling"), " Rendement moyen (t/ha)"),
        value = 10, min = 0, step = 0.1
      ),
      htmltools::tags$hr(),

      # --- Sous-bloc : Mode différence (ajout de traitements)
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'diff'", ns("hyp_mode")),
        htmltools::div(
          class = "d-grid gap-2",
          shiny::actionButton(
            ns("open_trt_modal"), "Ajouter un traitement",
            icon = shiny::icon("plus"), class = "btn-success"
          )
        ),
        htmltools::tags$hr(),
        shiny::uiOutput(ns("trt_remove_ui")),
        htmltools::tags$small(
          style = "color:#6c757d; display:block; margin-top:.25rem;",
          "En mode Différence : ajoutez un ou plusieurs traitements (valeurs négatives permises)."
        )
      ),

      # --- Sous-bloc : Mode équivalence (δ)
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'equiv'", ns("hyp_mode")),
        htmltools::tags$hr(),
        htmltools::tags$strong("Équivalence"),
        shiny::radioButtons(
          ns("equiv_mode_sb"), "Unité de δ",
          choices = c("t/ha" = "add", "%" = "mult"),
          selected = "mult", inline = TRUE
        ),
        shiny::numericInput(
          ns("equiv_value_sb"), "Valeur de δ",
          value = 10, min = 0, step = 0.01
        ),
        htmltools::tags$small(
          style = "color:#6c757d;",
          "δ est la marge d’écart maximale que vous jugez négligeable entre traitements ; tant que l’effet estimé reste dans ±δ, on considère les traitements équivalents.
Par exemple, si vous fixez δ = 0,1 t/ha (100 kg/ha), alors même si l’essai montre une baisse de 100 kg/ha, vous concluez que le rendement est équivalent (pratiquement le même)."
        )
      ),

      # Sélecteur caché pour le mode d'hypothèse (peut être changé via le modal d'accueil)
      htmltools::tags$div(
        style = "display:none;",
        shiny::radioButtons(ns("hyp_mode"), label = NULL,
                            choices = c("diff" = "diff", "equiv" = "equiv"),
                            selected = "diff")
      )
    ),

    # --- Bloc 2 : Options statistiques
    bslib::accordion_panel(
      title = htmltools::tagList(shiny::icon("balance-scale"), " Options statistiques"),
      value = "stats",

      shiny::radioButtons(
        ns("var_mode"), "Mesure de variabilité",
        choices  = c("Écart-type (t/ha)" = "sd", "Coefficient de variation (%)" = "cv"),
        selected = "cv", inline = TRUE
      ),
      shiny::uiOutput(ns("var_value_ui")),
      htmltools::tags$hr(),

      shiny::radioButtons(
        ns("equal_var"), "Hypothèse sur les variances",
        choices  = c(
          "Welch (variances inégales, recommandé)" = "welch",
          "Pooled (variances égales)"             = "pooled"
        ),
        selected = "welch"
      ),

      shiny::radioButtons(
        ns("alternative"), "Type d'hypothèse",
        choices  = c("Bilatéral" = "two.sided", "Unilatéral (gain)" = "greater", "Unilatéral (perte)" = "less"),
        selected = "two.sided", inline = TRUE
      ),

      shiny::numericInput(
        ns("p_value"), "Alpha", value = 0.05, min = 0, max = 1, step = 0.005
      )
    ),

    # --- Bloc 3 : Options générales
    bslib::accordion_panel(
      title = htmltools::tagList(shiny::icon("gear"), " Options générales"),
      value = "general",

      shiny::sliderInput(
        ns("n_sims"), "Nombre d'itérations par scénario",
        min = 100, max = 3000, value = 1000, step = 100
      ),
      htmltools::tags$small(
        style="color:#6c757d;",
        "Plus de simulations = courbes plus lisses, mais calcul plus long."
      )
    )
  )
}

#' Module paramètres — Server
#' @inheritParams mod_params_ui
#' @return une `reactive` list contenant les paramètres normalisés et traitements
#' @keywords internal
mod_params_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Modal d'accueil : choix de l'objectif (Différence vs Équivalence) ---
    shiny::onFlushed(function() {
      shiny::showModal(
        shiny::modalDialog(
          easyClose = FALSE, footer = NULL, size = "m",
          title = htmltools::tagList(shiny::icon("question-circle"),
                                     " Choisir l'hypothèse à tester"),
          htmltools::tags$div(
            class = "d-flex justify-content-center gap-3",
            style = "margin-top: .5rem;",
            shiny::actionButton(
              ns("choose_diff"),
              label = htmltools::HTML(
                '<div style="width:200px;height:140px;display:flex;flex-direction:column;align-items:center;justify-content:center;">
               <i class="fa fa-balance-scale fa-2x"></i>
               <div style="margin-top:10px;font-weight:700;">Différence</div>
               <div style="font-size:12px;opacity:.7;">Test t</div>
             </div>'
              ),
              class = "btn btn-outline-primary", style = "padding:0;border-radius:16px;"
            ),
            shiny::actionButton(
              ns("choose_equiv"),
              label = htmltools::HTML(
                '<div style="width:200px;height:140px;display:flex;flex-direction:column;align-items:center;justify-content:center;">
               <i class="fa fa-equals fa-2x"></i>
               <div style="margin-top:10px;font-weight:700;">Pas de différence</div>
               <div style="font-size:12px;opacity:.7;">Équivalence TOST</div>
             </div>'
              ),
              class = "btn btn-outline-success", style = "padding:0;border-radius:16px;"
            )
          ),
          htmltools::tags$p(
            class = "text-muted", style = "margin-top:12px;text-align:center;",
            "Que voulez-vous conclure, le traitement A augmente le rendement ou le traitement A ne diminue pas le rendement."
          )
        )
      )
    }, once = TRUE)

    shiny::observeEvent(input$choose_diff, {
      shiny::updateRadioButtons(session, "hyp_mode", selected = "diff")
      shiny::removeModal()
    })
    shiny::observeEvent(input$choose_equiv, {
      shiny::updateRadioButtons(session, "hyp_mode", selected = "equiv")
      shiny::removeModal()
    })

    # --- UI dynamique pour variabilité (sd ou cv)
    output$var_value_ui <- shiny::renderUI({
      mode <- input$var_mode %||% "cv"
      if (mode == "sd") {
        shiny::numericInput(ns("sd_val"), "Écart-type (t/ha)", value = 1, min = 0, step = 0.1)
      } else {
        shiny::numericInput(ns("cv_val"), "Coefficient de variation (%)", value = 10, min = 0, step = 0.1)
      }
    })

    # --- Conversion sd <-> cv
    sd_eff <- shiny::reactive({
      mu <- input$mean_yield %||% 0
      mode <- input$var_mode %||% "cv"
      if (mode == "sd") (input$sd_val %||% 1) else ((input$cv_val %||% 10)/100) * mu
    })

    # --- Table des traitements (nom, valeur, mode)
    traitements <- shiny::reactiveVal(
      tibble::tibble(nom = character(0), val = numeric(0), mode = character(0))
    )

    # --- Modal ajout traitement
    shiny::observeEvent(input$open_trt_modal, {
      shiny::showModal(shiny::modalDialog(
        title = htmltools::tagList(shiny::icon("flask"), " Ajouter un traitement"),
        size = "m", easyClose = TRUE,
        footer = htmltools::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(ns("save_trt"), "Ajouter", class = "btn-primary")
        ),
        shiny::textInput(ns("modal_trt_name"), "Nom du traitement", placeholder = "Ex.: Dose réduite d'azote"),
        shiny::radioButtons(ns("modal_effect_mode"), "Type d'effet",
                            choices = c("Additif (t/ha)" = "add", "Multiplicatif (%)" = "mult"),
                            selected = "mult", inline = TRUE
        ),
        shiny::uiOutput(ns("modal_trt_value_ui"))
      ))
    })

    # UI valeur traitement
    output$modal_trt_value_ui <- shiny::renderUI({
      if (identical(input$modal_effect_mode, "add")) {
        shiny::textInput(ns("modal_trt_value_txt"), "Gain ou perte attendu (t/ha)", placeholder = "Exemple: +0.1 pour +0.10 t/ha", value = "")
      } else {
        shiny::textInput(ns("modal_trt_value_txt"), "Gain ou perte attendu (%)", placeholder = "Exemple: +10 pour +10 %", value = "")
      }
    })

    # Normalisation de l'entrée valeur
    shiny::observeEvent(input$modal_trt_value_txt, {
      s <- input$modal_trt_value_txt
      if (!is.null(s) && nzchar(s)) {
        v <- .parse_signed_numeric(s)
        if (!is.na(v)) {
          digs <- if (identical(input$modal_effect_mode, "add")) 3L else 2L
          norm <- .format_with_sign(v, digits = digs)
          if (!identical(norm, s)) shiny::updateTextInput(session, "modal_trt_value_txt", value = norm)
        }
      }
    }, ignoreInit = TRUE)

    # Sauvegarde traitement
    shiny::observeEvent(input$save_trt, {
      nm <- base::trimws(input$modal_trt_name %||% "")
      md <- input$modal_effect_mode
      vl <- .parse_signed_numeric(input$modal_trt_value_txt)

      if (nzchar(nm) && md %in% c("add","mult") && is.finite(vl)) {
        df <- traitements()
        if (nm %in% df$nom) {
          df$val[df$nom == nm] <- vl
          df$mode[df$nom == nm] <- md
        } else {
          df <- dplyr::bind_rows(df, tibble::tibble(nom = nm, val = vl, mode = md))
        }
        traitements(df)
        shiny::removeModal()
        shiny::showNotification("Traitement ajouté.", type = "message")
      } else {
        shiny::showNotification("Entrée invalide (nom vide ou valeur non numérique).", type = "warning")
      }
    })

    # UI suppression traitement
    output$trt_remove_ui <- shiny::renderUI({
      df <- traitements()
      shiny::tagList(
        if (nrow(df) > 0) shiny::div(style = "margin-bottom:.5rem;", shiny::tableOutput(ns("trt_table"))),
        shiny::fluidRow(
          shiny::column(8, shiny::selectInput(ns("trt_remove"), "Supprimer un traitement",
                                              choices = if (nrow(df) > 0) stats::setNames(df$nom, df$nom) else character(0),
                                              selected = character(0)
          )),
          shiny::column(4, shiny::br(), shiny::actionButton(ns("rm_trt"), "Supprimer", icon = shiny::icon("trash"), class = "btn-outline-danger"))
        )
      )
    })

    # Table traitements affichée (corrigée : dplyr::if_else qualifié)
    output$trt_table <- shiny::renderTable({
      df <- traitements()
      if (nrow(df) == 0) return(NULL)
      df |>
        dplyr::mutate(
          `Type d'effet` = dplyr::if_else(.data$mode == "add", "Additif (t/ha)", "Multiplicatif (%)"),
          `Valeur` = dplyr::if_else(
            .data$mode == "add",
            paste0(.format_with_sign(.data$val, digits = 3), " t/ha"),
            paste0(.format_with_sign(.data$val, digits = 2), " %")
          )
        ) |>
        dplyr::select(nom, `Type d'effet`, `Valeur`)
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

    # Suppression traitement
    shiny::observeEvent(input$rm_trt, {
      nm <- input$trt_remove
      if (!is.null(nm) && nzchar(nm)) {
        df <- traitements()
        df <- dplyr::filter(df, .data$nom != nm)
        traitements(df)
      }
    })

    # Sorties réactives
    list(
      hyp_mode   = shiny::reactive(input$hyp_mode %||% "diff"),
      mu         = shiny::reactive(input$mean_yield %||% 0),
      sd_eff     = sd_eff,
      n_sims     = shiny::reactive(input$n_sims %||% 1000),
      alpha      = shiny::reactive(input$p_value %||% 0.05),
      alternative= shiny::reactive(input$alternative %||% "two.sided"),
      equal_var  = shiny::reactive(identical((input$equal_var %||% "welch"), "pooled")),
      traitements= traitements,
      equiv_mode = shiny::reactive(input$equiv_mode_sb %||% "add"),
      equiv_val  = shiny::reactive(as.numeric(input$equiv_value_sb %||% 0.1))
    )
  })
}
