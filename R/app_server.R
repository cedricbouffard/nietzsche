
#' Serveur de l'application {nietzsche}
#' @keywords internal
app_server <- function(input, output, session) {
  params <- mod_params_server("params")
  mod_results_server("results", params)
}
