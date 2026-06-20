# =============================================================================
# mod_patchwork.R — Composición de gráficos con patchwork para StatPlot
# StatPlot · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Exporta:
#   mod_patchwork_ui(id)
#   mod_patchwork_server(id, graficos)
# =============================================================================

# ── UI ────────────────────────────────────────────────────────────────────────
mod_patchwork_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "px-1 pt-2 pb-2",
      h4(style = paste0("color:", colores$primario, "; font-weight:700; margin-bottom:4px;"),
         bs_icon("grid", class = "me-2"), "Composición"),
      p(class = "text-muted small mb-0",
        "Combiná los gráficos guardados en una composición. ",
        "Usá patchwork para controlar el layout.")
    ),

    layout_columns(
      col_widths = c(3, 9),

      # ── PANEL IZQUIERDO ───────────────────────────────────────────────────
      div(

        card(
          card_header(bs_icon("collection", class = "me-1"), "Gráficos guardados"),
          card_body(
            uiOutput(ns("lista_graficos")),
            uiOutput(ns("sin_graficos_msg"))
          )
        ),

        card(
          card_header(bs_icon("layout-wtf", class = "me-1"), "Layout"),
          card_body(
            radioButtons(ns("tipo_layout"), "Disposición",
                         choices = c(
                           "Lado a lado"     = "horizontal",
                           "Apilados"        = "vertical",
                           "Grid automático" = "auto",
                           "Personalizado"   = "custom"
                         ),
                         selected = "auto"),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'custom'", ns("tipo_layout")),
              textInput(ns("layout_design"),
                        "Diseño (notación patchwork)",
                        placeholder = "Ej: 'AB\\nCC'",
                        value = "")
            ),
            tags$hr(),
            numericInput(ns("ncol"), "Columnas (grid auto)", value = 2, min = 1, max = 6),
            sliderInput(ns("alto"), "Alto total (pulgadas)", min = 4, max = 20, value = 8, step = 1),
            sliderInput(ns("ancho"), "Ancho total (pulgadas)", min = 4, max = 20, value = 12, step = 1)
          )
        ),

        card(
          card_header(bs_icon("fonts", class = "me-1"), "Anotaciones"),
          card_body(
            textInput(ns("titulo_global"),    "Título global",    placeholder = ""),
            textInput(ns("subtitulo_global"), "Subtítulo global", placeholder = ""),
            textInput(ns("caption_global"),   "Caption",          placeholder = ""),
            tags$hr(),
            checkboxInput(ns("tags_letras"), "Agregar etiquetas (A, B, C...)", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s']", ns("tags_letras")),
              selectInput(ns("tag_prefix"), "Tipo de etiqueta",
                          choices = c("A, B, C" = "A",
                                      "a, b, c" = "a",
                                      "1, 2, 3" = "1",
                                      "i, ii, iii" = "i"),
                          selected = "A")
            )
          )
        )
      ),

      # ── PANEL DERECHO ─────────────────────────────────────────────────────
      div(
        navset_card_tab(

          nav_panel(
            title = tagList(bs_icon("image", class = "me-1"), "Composición"),
            card_body(
              uiOutput(ns("composicion_msg")),
              plotOutput(ns("composicion"), height = "560px"),
              tags$hr(),
              div(
                class = "d-flex gap-2 flex-wrap",
                downloadButton(ns("dl_png"), "PNG",
                               icon = bs_icon("download"),
                               class = "btn-sm btn-outline-primary"),
                downloadButton(ns("dl_pdf"), "PDF",
                               icon = bs_icon("download"),
                               class = "btn-sm btn-outline-primary"),
                downloadButton(ns("dl_svg"), "SVG",
                               icon = bs_icon("download"),
                               class = "btn-sm btn-outline-primary")
              )
            )
          ),

          nav_panel(
            title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
            card_body(
              p(class = "text-muted small mb-2",
                "Código reproducible para generar esta composición."),
              verbatimTextOutput(ns("codigo_r")),
              downloadButton(ns("dl_script"), "Descargar .R",
                             icon = bs_icon("download"),
                             class = "btn-sm btn-outline-primary mt-2")
            )
          )
        )
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────
mod_patchwork_server <- function(id, graficos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Lista de gráficos disponibles ─────────────────────────────────────────
    output$lista_graficos <- renderUI({
      lista <- graficos()
      if (length(lista) == 0) return(NULL)

      tagList(
        p(class = "small text-muted mb-2",
          "Seleccioná los gráficos a incluir y el orden:"),
        checkboxGroupInput(
          ns("seleccion"),
          label    = NULL,
          choices  = names(lista),
          selected = names(lista)
        ),
        tags$hr(),
        actionButton(ns("limpiar"), "Limpiar lista",
                     icon  = bs_icon("trash"),
                     class = "btn-sm btn-outline-danger w-100")
      )
    })

    output$sin_graficos_msg <- renderUI({
      lista <- graficos()
      if (length(lista) > 0) return(NULL)
      div(
        class = "alert alert-info small py-2 px-3",
        bs_icon("info-circle", class = "me-1"),
        "Guardá gráficos desde la pestaña ",
        tags$b("Gráfico"), " para componerlos aquí."
      )
    })

    # ── Composición reactiva ──────────────────────────────────────────────────
    composicion_reactiva <- reactive({
      lista    <- graficos()
      sel      <- input$seleccion %||% character(0)
      req(length(lista) > 0, length(sel) > 0)

      # Filtrar y ordenar según selección
      plots <- lista[sel[sel %in% names(lista)]]
      req(length(plots) > 0)

      # Construir composición
      comp <- switch(input$tipo_layout %||% "auto",
        "horizontal" = patchwork::wrap_plots(plots, nrow = 1),
        "vertical"   = patchwork::wrap_plots(plots, ncol = 1),
        "auto"       = patchwork::wrap_plots(plots, ncol = input$ncol %||% 2),
        "custom"     = {
          design <- input$layout_design %||% ""
          if (nzchar(design))
            patchwork::wrap_plots(plots, design = design)
          else
            patchwork::wrap_plots(plots, ncol = input$ncol %||% 2)
        },
        patchwork::wrap_plots(plots, ncol = input$ncol %||% 2)
      )

      # Etiquetas A, B, C...
      if (isTRUE(input$tags_letras)) {
        comp <- comp +
          patchwork::plot_annotation(tag_levels = input$tag_prefix %||% "A")
      }

      # Anotaciones globales
      titulo    <- input$titulo_global    %||% ""
      subtitulo <- input$subtitulo_global %||% ""
      caption   <- input$caption_global   %||% ""

      if (nzchar(titulo) || nzchar(subtitulo) || nzchar(caption)) {
        comp <- comp +
          patchwork::plot_annotation(
            title    = if (nzchar(titulo))    titulo    else NULL,
            subtitle = if (nzchar(subtitulo)) subtitulo else NULL,
            caption  = if (nzchar(caption))   caption   else NULL,
            theme    = ggplot2::theme(
              plot.title    = ggplot2::element_text(face = "bold", size = 14,
                                                     color = colores$primario),
              plot.subtitle = ggplot2::element_text(size = 11, color = colores$texto),
              plot.caption  = ggplot2::element_text(size = 9,  color = colores$texto)
            )
          )
      }

      comp
    })

    # ── Mensaje si no hay gráficos seleccionados ──────────────────────────────
    output$composicion_msg <- renderUI({
      lista <- graficos()
      sel   <- input$seleccion %||% character(0)
      if (length(lista) == 0 || length(sel) == 0)
        div(class = "alert alert-info small",
            bs_icon("info-circle", class = "me-1"),
            "Seleccioná al menos un gráfico para ver la composición.")
      else NULL
    })

    # ── Render composición ────────────────────────────────────────────────────
    output$composicion <- renderPlot({
      composicion_reactiva()
    }, res = 96)

    # ── Descargas ─────────────────────────────────────────────────────────────
    descargar_comp <- function(formato) {
      downloadHandler(
        filename = function() paste0("composicion_", format(Sys.Date(), "%Y%m%d"),
                                     ".", formato),
        content  = function(file) {
          ggplot2::ggsave(file,
                          plot   = composicion_reactiva(),
                          device = formato,
                          width  = input$ancho %||% 12,
                          height = input$alto  %||% 8,
                          dpi    = 300,
                          units  = "in")
        }
      )
    }

    output$dl_png <- descargar_comp("png")
    output$dl_pdf <- descargar_comp("pdf")
    output$dl_svg <- descargar_comp("svg")

    # ── Código R reproducible ─────────────────────────────────────────────────
    codigo_reactivo <- reactive({
      sel <- input$seleccion %||% character(0)
      req(length(sel) > 0)

      layout_str <- switch(input$tipo_layout %||% "auto",
        "horizontal" = "wrap_plots(lista_plots, nrow = 1)",
        "vertical"   = "wrap_plots(lista_plots, ncol = 1)",
        "auto"       = paste0("wrap_plots(lista_plots, ncol = ", input$ncol %||% 2, ")"),
        "custom"     = {
          d <- input$layout_design %||% ""
          if (nzchar(d))
            paste0("wrap_plots(lista_plots, design = '", d, "')")
          else
            paste0("wrap_plots(lista_plots, ncol = ", input$ncol %||% 2, ")")
        }
      )

      tags_str <- if (isTRUE(input$tags_letras))
        paste0(" +\n  plot_annotation(tag_levels = '", input$tag_prefix %||% "A", "')")
      else ""

      titulo    <- input$titulo_global    %||% ""
      subtitulo <- input$subtitulo_global %||% ""
      caption   <- input$caption_global   %||% ""
      annot_str <- if (nzchar(titulo) || nzchar(subtitulo) || nzchar(caption))
        paste0(" +\n  plot_annotation(\n",
               if (nzchar(titulo))    paste0("    title    = '", titulo,    "',\n") else "",
               if (nzchar(subtitulo)) paste0("    subtitle = '", subtitulo, "',\n") else "",
               if (nzchar(caption))   paste0("    caption  = '", caption,   "'\n")  else "",
               "  )")
      else ""

      paste0(
        encabezado_script("Composición con patchwork"),
        "library(ggplot2)\n",
        "library(patchwork)\n\n",
        "# Asumiendo que p1, p2, ... son tus objetos ggplot\n",
        "# Generados desde la pestaña Gráfico\n\n",
        "lista_plots <- list(\n",
        paste0("  ", sel, collapse = ",\n"), "\n",
        ")\n\n",
        "comp <- ", layout_str, tags_str, annot_str, "\n\n",
        "# Guardar\n",
        "ggsave('composicion.png', plot = comp,\n",
        "       width = ", input$ancho %||% 12, ", height = ", input$alto %||% 8,
        ", dpi = 300, units = 'in')\n"
      )
    })

    output$codigo_r <- renderText({ codigo_reactivo() })

    output$dl_script <- downloadHandler(
      filename = function() paste0("composicion_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(codigo_reactivo(), file)
    )

  })
}
