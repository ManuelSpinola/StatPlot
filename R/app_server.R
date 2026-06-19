#' Application Server
#'
#' @param input,output,session Internal parameters for Shiny.
#' @noRd
app_server <- function(input, output, session) {

  # Data module
  app_data <- mod_upload_server("upload")

  mod_acerca_de_server("acerca_de")

  # Módulos futuros:
  # mod_grafico_server("grafico", data = app_data)
  # mod_patchwork_server("patchwork")

  session$onSessionEnded(function() {})
}
