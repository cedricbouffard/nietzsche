
#' UI de l'application {nietzsche}
#' @keywords internal
app_ui <- function() {
  bslib::page_sidebar(
    theme = bslib::bs_theme(
      bootswatch = "flatly",
      primary = "#2C3E50",
      base_font = bslib::font_google("Roboto")
    ),
    title = "Nietzsche 1.0 - Trouver le bon nombre de répétitions pour optimiser la puissance d'un essai à la ferme",

    # Contrôle global (caché) pour compat rester proche du fichier initial


    # Styles issus du fichier initial
    htmltools::tags$style(htmltools::HTML("
  .accordion .accordion-item {
    border: 1px solid #e5e7eb;
    border-radius: 12px;
    overflow: hidden;
    margin-bottom: 10px;
    box-shadow: 0 2px 8px rgba(17,24,39,0.04);
  }
  .accordion-button {
    background: #f8fafc;
    font-weight: 600;
    color: #1f2937;
  }
  .accordion-body {
    background: #ffffff;
    padding-top: 0.75rem;
  }
  .form-label {
    font-weight: 600;
    color: #334155;
  }
  .btn-success, .btn-primary {
    border-radius: 9999px;
  }
  .form-control, .form-select {
    border-radius: 10px;
  }
    ")),

    sidebar = bslib::sidebar(
      width = 380,

      htmltools::tags$div(
        style = "margin-bottom: 1rem; padding-bottom: .5rem; border-bottom: 2px solid #e0e0e0;",
        htmltools::tags$h5(
          style = "color: #2C3E50; font-weight: 600; margin: 0;",
          shiny::icon("sliders"), " Paramètres"
        )
      ),

      # Module paramètres (remplace le bloc UI monolithique)
      mod_params_ui("params")
    ),

    htmltools::tags$div(
      style = "padding: 1rem;",

      htmltools::tags$h3(style = "color:#2C3E50; margin-bottom: 1rem;",
                         shiny::icon("chart-bar"), " Résultats"),
      mod_results_ui("results"),

      # Bloc d'explications repris du fichier initial
      bslib::card(
        bslib::card_header(
          class = "bg-light",
          htmltools::tags$strong(shiny::icon("chart-line"), " Comment lire le graphique ?")
        ),
        bslib::card_body(
          htmltools::tags$ul(
            htmltools::tags$li(htmltools::tags$strong("Axe X :"), " nombre de répétitions par traitement."),
            htmltools::tags$li(htmltools::tags$strong("Axe Y :"), " puissance statistique (0–1)."),
            htmltools::tags$li(htmltools::tags$strong("Objectif :"), " viser une puissance \u2265 0,80."),
            htmltools::tags$li(htmltools::tags$strong("Lignes pointillées :"), " repères à 80 % et 95 %."),
            htmltools::tags$li(
              htmltools::tags$strong("Courbes :"),
              " une courbe par traitement en mode ",
              htmltools::tags$em("Différence"),
              " ; une seule courbe ",
              htmltools::tags$em("TOST"),
              " en mode ",
              htmltools::tags$em("Équivalence")
            )
          )

        )
      ),

      # --- Carte : Puissance, \u03B1 et \u03B4 : en clair ---
      bslib::card(
        bslib::card_header(
          class = "bg-light",
          htmltools::tags$strong(shiny::icon("book-open"), " Puissance, \u03B1 et \u03B4")
        ),
        htmltools::div(
          class = "alert alert-warning",
          htmltools::tags$strong("Attention : "),
          "un p-value > \u03B1 ne prouve pas que les traitements sont ",
          htmltools::tags$em("identiques"),
          ". Avec une puissance faible, le risque de ",
          htmltools::tags$strong("non-détection (erreur de type II)"),
          " est élevé."
        ),
        bslib::card_body(
          htmltools::tags$p(
            htmltools::tags$strong("Puissance"),
            " = probabilité que le test conclue correctement (détection d’une différence réelle, ",
            "ou conclusion d’équivalence selon la marge choisie). Cible habituelle : ",
            htmltools::tags$strong("80 %"),
            "."
          ),
          htmltools::tags$p(
            htmltools::tags$strong("\u03B1 (alpha)"),
            " = seuil du test (souvent 0,05). ", htmltools::tags$br(),
            htmltools::tags$strong("\u03B4 (delta)"),
            " = ",
            htmltools::tags$em("marge d’équivalence"),
            " : plus petite différence jugée négligeable (en t/ha ou en %). ",
            "Ne pas confondre le ",
            htmltools::tags$em("seuil statistique"),
            " (\u03B1) et la ",
            htmltools::tags$em("tolérance pratique"),
            " (\u03B4)."
          ),
          htmltools::tags$ul(
            "Ce qui affecte la puissance",
            htmltools::tags$li(htmltools::tags$strong("Taille d’effet visée"), "(un effet plus important \u2192 plus de puissance)."),
            htmltools::tags$li(htmltools::tags$strong("Variabilité"), " (Écart-type ou Coefficient de variation plus faible \u2192 plus de puissance)."),
            htmltools::tags$li(htmltools::tags$strong("Nombre de répétitions"), " (plus \u2192 plus de puissance)."),
            htmltools::tags$li(htmltools::tags$strong("\u03B1"), " (moins strict \u2192 plus de puissance)."),
            htmltools::tags$li(htmltools::tags$strong("Tests"), "(les tests unilatéraux  \u2192 plus de puissance)."),

            htmltools::tags$li(htmltools::tags$strong("\u03B4 Tolérance pratique"), " (conclure qu'une plus grande différence de moyenne sont équivalente \u2192 plus de puissance).")
          ),
          htmltools::tags$hr(),
          htmltools::tags$p(
            htmltools::tags$strong("Plans en blocs : "),
            "le simulateur suppose des répétitions indépendantes. ",
            "Un blocage pertinent peut ",
            htmltools::tags$em("augmenter"),
            " la puissance (variance résiduelle réduite), ",
            "mais s’il n’explique pas grand-chose ou consomme des degrés de liberté, ",
            "l’effet peut être ",
            htmltools::tags$em("nul voire négatif"),
            "."
          )

        )
      )
    )
  )
}
