#' Application Server
#'
#' @param input,output,session Internal parameters for Shiny.
#' @noRd
app_server <- function(input, output, session) {

  # Data module
  app_data <- mod_upload_server("upload")

  # Gráfico — devuelve lista reactiva de gráficos guardados
  graficos_guardados <- mod_grafico_server("grafico", data = app_data)

  # Composición con patchwork
  mod_patchwork_server("patchwork", graficos = graficos_guardados)

  mod_acerca_de_server("acerca_de")

  session$onSessionEnded(function() {})
}
