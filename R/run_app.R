#' Lancer l'application Shiny nietzsche
#' @export
#' @examples
#' if (interactive()) run_app()
run_app <- function() {
  shiny::shinyApp(ui = app_ui(), server = app_server)
}
