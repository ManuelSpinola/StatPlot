# ============================================================
# mod_acerca_de.R — Información sobre StatPlot
# StatPlot · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

mod_acerca_de_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "py-4 px-3",
      style = "max-width: 860px; margin: 0 auto;",

      h4(
        bs_icon("info-circle", class = "me-2"),
        "Acerca de StatPlot",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(class = "text-muted mb-4",
        "StatPlot es el módulo de visualización de datos de StatSuite, ",
        "desarrollado en el ICOMVIS de la Universidad Nacional, Costa Rica. ",
        "Permite crear, personalizar y combinar gráficos con ",
        strong("ggplot2"), " y ", strong("tidyplots"),
        " de forma interactiva, sin necesidad de conocimientos de programación. ",
        "Todos los gráficos generan código R reproducible."
      ),

      layout_columns(
        col_widths = c(6, 6),

        # ── StatSuite ──
        card(
          card_header(bs_icon("collection", class = "me-1"),
                      "StatSuite — Ecosistema completo"),
          card_body(
            tags$p(class = "small fw-bold mb-1", "General"),
            tags$ul(
              class = "small mb-2",
              tags$li(strong("StatDesign"),  " — Diseño de estudios y muestreo"),
              tags$li(strong("StatPlot"),    " — Visualización ← aquí"),
              tags$li(strong("StatFlow"),    " — Primeros análisis y visualización"),
              tags$li(strong("StatModels"),  " — Modelos estadísticos"),
              tags$li(strong("StatML"),      " — Machine learning"),
              tags$li(strong("StatGeo"),     " — Análisis espacial y mapas")
            ),
            tags$p(class = "small fw-bold mb-1", "Ecología aplicada"),
            tags$ul(
              class = "small mb-0",
              tags$li(strong("StatComm"),    " — Análisis multivariado de comunidades"),
              tags$li(strong("StatMonitor"), " — Monitoreo poblacional"),
              tags$li(strong("StatOccu"),    " — Modelos de ocupación"),
              tags$li(strong("StatH3sdm"),   " — SDM con grillas H3")
            )
          )
        ),

        # ── Ecosistema R ──
        card(
          card_header(bs_icon("box-seam", class = "me-1"),
                      "Ecosistema R utilizado"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("ggplot2"),
                      " — Sistema de visualización basado en la gramática de gráficos"),
              tags$li(strong("tidyplots"),
                      " — Visualización moderna con sintaxis por pipe"),
              tags$li(strong("patchwork"),
                      " — Composición de múltiples gráficos"),
              tags$li(strong("ggrepel"),
                      " — Etiquetas sin solapamiento"),
              tags$li(strong("colourpicker"),
                      " — Selector de colores interactivo"),
              tags$li(strong("scales"),
                      " — Escalas y formatos"),
              tags$li(strong("dplyr"), " / ", strong("tidyr"),
                      " — Manipulación de datos")
            )
          )
        )
      ),

      # ── Datos de ejemplo ──
      card(
        class = "mt-3",
        card_header(bs_icon("database", class = "me-1"),
                    "Datos de ejemplo incluidos"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            tagList(
              tags$p(class = "small fw-bold mb-1", "Clásicos"),
              tags$ul(
                class = "small mb-0",
                tags$li(strong("Penguins"),
                        " — Pingüinos de Palmer (palmerpenguins)"),
                tags$li(strong("Gapminder"),
                        " — Indicadores socioeconómicos globales (gapminder)"),
                tags$li(strong("Meuse"),
                        " — Contaminación de suelos, río Meuse (sp)"),
                tags$li(strong("Birthwt"),
                        " — Peso al nacer y factores de riesgo (MASS)"),
                tags$li(strong("Mites"),
                        " — Comunidad de ácaros oribátidos (vegan)")
              )
            ),
            tagList(
              tags$p(class = "small fw-bold mb-1", "tidyplots"),
              tags$ul(
                class = "small mb-0",
                tags$li(strong("Animales"),
                        " — Morfología comparada de 60 especies"),
                tags$li(strong("Clima"),
                        " — Temperaturas históricas mensuales desde 1891"),
                tags$li(strong("Energía"),
                        " — Consumo energético por fuente (2002–2022)"),
                tags$li(strong("Gastos"),
                        " — Registro de gastos personales por categoría")
              )
            )
          )
        )
      ),

      # ── Cómo citar ──
      card(
        class = "mt-3",
        card_header(bs_icon("quote", class = "me-1"), "Cómo citar"),
        card_body(
          p(class = "small mb-2",
            "Si usás StatPlot en tu investigación o docencia, por favor citá:"),
          div(
            class = "p-3 rounded small",
            style = paste0("background:", colores$fondo,
                           "; border-left: 4px solid ", colores$primario, ";",
                           " font-family: monospace;"),
            "Spínola, M. (", format(Sys.Date(), "%Y"), "). ",
            em("StatPlot: Visualización interactiva de datos con ggplot2 y tidyplots"),
            " (StatSuite v0.1). Instituto Internacional en Conservación y Manejo ",
            "de Vida Silvestre (ICOMVIS), Universidad Nacional, Costa Rica. ",
            "Disponible en: https://statplot.una.ac.cr"
          )
        )
      ),

      # ── Desarrollo ──
      card(
        class = "mt-3",
        card_header(bs_icon("code-slash", class = "me-1"), "Desarrollo"),
        card_body(
          p(class = "small mb-2",
            bs_icon("person-fill", class = "me-1"),
            strong("Autor:"), " Manuel Spínola — ICOMVIS, ",
            "Universidad Nacional, Costa Rica."),
          p(class = "small mb-2",
            bs_icon("robot", class = "me-1"),
            strong("Asistencia en desarrollo:"), " StatPlot fue desarrollado ",
            "con asistencia de ", strong("Claude (Anthropic)"),
            " para la estructura de módulos, interfaz de usuario y contenido didáctico."),
          p(class = "small mb-0",
            bs_icon("building", class = "me-1"),
            strong("Institución:"), " Instituto Internacional en ",
            "Conservación y Manejo de Vida Silvestre (ICOMVIS), ",
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
