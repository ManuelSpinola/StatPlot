#' Application UI
#'
#' @return A Shiny UI object.
#' @noRd
app_ui <- function() {

  golem::add_resource_path(
    "www",
    system.file("app/www", package = "StatPlot")
  )

  bslib::page_navbar(
    title = div(
      style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
      img(src = "www/hexsticker_StatPlot.png", height = "38px"),
      span("StatPlot", style = "font-weight: 600;")
    ),
    theme = tema_app,
    lang  = "es",

    bslib::nav_panel(
      title = "Datos",
      icon  = bsicons::bs_icon("upload"),
      mod_upload_ui("upload")
    ),

    bslib::nav_panel(
      title = "Gráfico",
      icon  = bsicons::bs_icon("bar-chart"),
      mod_grafico_ui("grafico")
    ),

    bslib::nav_panel(
      title = "Composición",
      icon  = bsicons::bs_icon("grid"),
      mod_patchwork_ui("patchwork")
    ),

    bslib::nav_spacer(),

    bslib::nav_panel(
      title = "Acerca de",
      icon  = bsicons::bs_icon("info-circle"),
      mod_acerca_de_ui("acerca_de")
    ),

    bslib::nav_item(
      tags$span(class = "text-white-50 small", "StatPlot v0.1")
    )
  )
}
