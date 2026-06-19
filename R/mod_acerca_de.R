# ============================================================
# mod_acerca_de.R — Información sobre StatPlot
# StatPlot · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

mod_acerca_de_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "py-4 px-3",
      style = "max-width: 780px; margin: 0 auto;",

      h4(
        bs_icon("info-circle", class = "me-2"),
        "Acerca de StatPlot",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(class = "text-muted mb-4",
        "StatPlot es el m\u00f3dulo de visualizaci\u00f3n de datos de ",
        "StatSuite, desarrollado en el ICOMVIS de la Universidad Nacional, ",
        "Costa Rica. Permite crear, personalizar y combinar gr\u00e1ficos ",
        "basados en ", strong("ggplot2"), " de forma interactiva, sin necesidad ",
        "de conocimientos de programaci\u00f3n."
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header(bs_icon("collection", class = "me-1"),
                      "StatSuite \u2014 Ecosistema completo"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("StatDesign"),  " \u2014 Dise\u00f1o de estudios y muestreo"),
              tags$li(strong("StatFlow"),    " \u2014 Primeros an\u00e1lisis y visualizaci\u00f3n"),
              tags$li(strong("StatGeo"),     " \u2014 An\u00e1lisis espacial y mapas"),
              tags$li(strong("StatMonitor"), " \u2014 Monitoreo poblacional"),
              tags$li(strong("StatModels"),  " \u2014 Modelos estad\u00edsticos"),
              tags$li(strong("StatH3sdm"),   " \u2014 SDM con grillas H3"),
              tags$li(strong("StatComm"),    " \u2014 An\u00e1lisis multivariado"),
              tags$li(strong("StatML"),      " \u2014 Machine learning"),
              tags$li(strong("StatPlot"),    " \u2014 Visualizaci\u00f3n \u2190 aqu\u00ed")
            )
          )
        ),

        card(
          card_header(bs_icon("box-seam", class = "me-1"),
                      "Ecosistema R utilizado"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("ggplot2"),
                      " \u2014 Sistema de visualizaci\u00f3n basado en la gram\u00e1tica de gr\u00e1ficos"),
              tags$li(strong("patchwork"),
                      " \u2014 Composici\u00f3n de m\u00faltiples gr\u00e1ficos"),
              tags$li(strong("plotly"),
                      " \u2014 Gr\u00e1ficos interactivos"),
              tags$li(strong("ggrepel"),
                      " \u2014 Etiquetas sin solapamiento"),
              tags$li(strong("scales"),
                      " \u2014 Escalas y formatos"),
              tags$li(strong("dplyr"),
                      " \u2014 Manipulaci\u00f3n de datos")
            )
          )
        )
      ),

      card(
        class = "mt-3",
        card_header(bs_icon("database", class = "me-1"),
                    "Datos de ejemplo incluidos"),
        card_body(
          tags$ul(
            class = "small",
            tags$li(strong("Penguins"), " \u2014 Pingu\u00eanos de Palmer (palmerpenguins)"),
            tags$li(strong("Gapminder"), " \u2014 Indicadores socioecon\u00f3micos globales (gapminder)"),
            tags$li(strong("Meuse"), " \u2014 Contaminaci\u00f3n de suelos, r\u00edo Meuse (sp)"),
            tags$li(strong("Birthwt"), " \u2014 Peso al nacer y factores de riesgo (MASS)"),
            tags$li(strong("Coronavirus"), " \u2014 Casos COVID-19 globales (coronavirus)"),
            tags$li(strong("\u00c1caros (mites)"), " \u2014 Comunidad de \u00e1caros oribátidos, variables mixtas (vegan)")
          )
        )
      ),

      card(
        class = "mt-3",
        card_header(bs_icon("code-slash", class = "me-1"),
                    "Desarrollo"),
        card_body(
          p(class = "small mb-2",
            bs_icon("person-fill", class = "me-1"),
            strong("Autor:"), " Manuel Sp\u00ednola \u2014 ICOMVIS, ",
            "Universidad Nacional, Costa Rica."),
          p(class = "small mb-2",
            bs_icon("robot", class = "me-1"),
            strong("Asistencia en desarrollo:"), " StatPlot fue desarrollado ",
            "con asistencia de ", strong("Claude (Anthropic)"),
            " para la estructura de m\u00f3dulos, interfaz de usuario y contenido did\u00e1ctico."),
          p(class = "small mb-0",
            bs_icon("building", class = "me-1"),
            strong("Instituci\u00f3n:"), " Instituto Internacional en ",
            "Conservaci\u00f3n y Manejo de Vida Silvestre (ICOMVIS), ",
            "Universidad Nacional de Costa Rica.")
        )
      ),

      div(
        class = "alert alert-info small mt-3 mb-0",
        bs_icon("envelope", class = "me-1"),
        "Contacto: ",
        tags$a(href = "mailto:manuel.spinola@una.ac.cr",
               "manuel.spinola@una.ac.cr")
      )
    )
  )
}

mod_acerca_de_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # sin lógica reactiva
  })
}
