#' Application Server
#'
#' @param input,output,session Internal parameters for Shiny.
#' @noRd
app_server <- function(input, output, session) {

  # Data module
  app_data <- mod_upload_server("upload")

  # Gráfico — devuelve lista reactiva de gráficos guardados
  graficos_guardados <- mod_ggplot2_server("ggplot2", data = app_data)

  # Tidyplot
  graficos_tidyplots <- mod_tidyplots_server("tidyplots", data = app_data)

  # Combinar gráficos de ggplot2 y tidyplots para patchwork
  todos_graficos <- reactive({
    c(graficos_guardados(), graficos_tidyplots())
  })

  # Gráficos avanzados
  mod_avanzado_server("avanzado", data = app_data)

  # Composición con patchwork
  mod_patchwork_server("patchwork", graficos = todos_graficos)

  mod_acerca_de_server("acerca_de")

  session$onSessionEnded(function() {})
}
