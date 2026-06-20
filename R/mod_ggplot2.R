# =============================================================================
# mod_ggplot2.R — Construcción de gráficos ggplot2 para StatPlot
# StatPlot · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Exporta:
#   mod_ggplot2_ui(id)
#   mod_ggplot2_server(id, data)
# =============================================================================

# ── Helpers internos ──────────────────────────────────────────────────────────

# Geometrías disponibles según tipo de variables
geoms_disponibles <- function(tipo_x, tipo_y = NULL) {
  num <- "Numérica continua"
  dis <- "Numérica discreta"
  cat <- "Categórica"
  tmp <- "Temporal"
  es_num <- function(t) t %in% c(num, dis)

  if (is.null(tipo_y)) {
    if (es_num(tipo_x))
      return(c(
        "Ninguna (quitar capa base)" = "none",
        "Histograma"    = "histogram",
        "Densidad"      = "density",
        "Boxplot"       = "boxplot",
        "Violín"        = "violin"
      ))
    if (tipo_x == cat)
      return(c(
        "Ninguna (quitar capa base)" = "none",
        "Barras"        = "bar",
        "Torta"         = "pie"
      ))
  } else {
    if (es_num(tipo_x) && es_num(tipo_y))
      return(c(
        "Ninguna (quitar capa base)" = "none",
        "Dispersión"         = "point",
        "Líneas"             = "line",
        "Dispersión + smooth" = "point_smooth",
        "Área"               = "area"
      ))
    if ((tipo_x == cat || tipo_x == dis) && es_num(tipo_y))
      return(c(
        "Ninguna (quitar capa base)" = "none",
        "Puntos por grupo"   = "point_group",
        "Boxplot por grupo"  = "boxplot_group",
        "Violín por grupo"   = "violin_group",
        "Barras (media)"     = "bar_mean"
      ))
    if (es_num(tipo_x) && (tipo_y == cat || tipo_y == dis))
      return(c(
        "Ninguna (quitar capa base)" = "none",
        "Puntos por grupo"   = "point_group",
        "Boxplot por grupo"  = "boxplot_group",
        "Violín por grupo"   = "violin_group"
      ))
    if (tipo_x == tmp && es_num(tipo_y))
      return(c(
        "Ninguna (quitar capa base)" = "none",
        "Líneas"             = "line",
        "Dispersión"         = "point",
        "Área"               = "area"
      ))
  }
  c("Ninguna (quitar capa base)" = "none", "Dispersión" = "point")
}

# Temas ggplot2 disponibles
temas_gg <- c(
  "Minimal"    = "minimal",
  "Classic"    = "classic",
  "Light"      = "light",
  "Gray"       = "gray",
  "BW"         = "bw",
  "Void"       = "void"
)

# ── UI ────────────────────────────────────────────────────────────────────────
mod_ggplot2_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "px-1 pt-2 pb-2",
      h4(style = paste0("color:", colores$primario, "; font-weight:700; margin-bottom:4px;"),
         bs_icon("bar-chart", class = "me-2"), "Gráfico"),
      p(class = "text-muted small mb-0",
        "Construí tu gráfico seleccionando variables, geometría y capas. ",
        "El código R se genera automáticamente.")
    ),

    layout_columns(
      col_widths = c(3, 9),

      # ══════════════════════════════════════════════════════════════════════
      # PANEL IZQUIERDO — Variables, Facetas, Geometría
      # ══════════════════════════════════════════════════════════════════════
      div(

        card(
          card_header(bs_icon("table", class = "me-1"), "Variables"),
          card_body(
            uiOutput(ns("var_selectores_ui"))
          )
        ),

        card(
          card_header(bs_icon("grid", class = "me-1"), "Facetas (paneles)"),
          card_body(
            uiOutput(ns("var_facet_ui"))
          )
        ),

        card(
          card_header(bs_icon("layers", class = "me-1"), "Geometría"),
          card_body(
            uiOutput(ns("geom_ui")),
            tags$hr(),
            checkboxGroupInput(
              ns("capas"),
              label = "Capas adicionales",
              choices = c(
                "Boxplot"         = "boxplot_layer",
                "Violín"          = "violin_layer",
                "Smooth (lm)"     = "smooth_lm",
                "Smooth (loess)"  = "smooth_loess",
                "Media (punto)"   = "mean_point",
                "Línea H (media)" = "hline_mean",
                "Línea V (media)" = "vline_mean",
                "Etiquetas (text)"   = "text_label",
                "Etiquetas (label)"  = "label_label",
                "Etiquetas (repel)"  = "repel_label"
              )
            ),
            uiOutput(ns("var_etiqueta_ui"))
          )
        )
      ),

      # ══════════════════════════════════════════════════════════════════════
      # PANEL DERECHO — gráfico + código + etiquetas + estética + color
      # ══════════════════════════════════════════════════════════════════════
      div(
        navset_card_tab(

          nav_panel(
            title = tagList(bs_icon("image", class = "me-1"), "Gráfico"),
            card_body(
              uiOutput(ns("grafico_msg")),
              plotOutput(ns("grafico"), height = "680px", width = "100%"),
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
              ),
              tags$hr(),
              # ── Etiquetas + Estética debajo del gráfico ──
              layout_columns(
                col_widths = c(6, 6),
                fill = FALSE,

                card(
                  card_header(bs_icon("fonts", class = "me-1"), "Etiquetas"),
                  card_body(
                    textInput(ns("nombre_grafico"), "Nombre del gráfico",
                              placeholder = "para composición con patchwork"),
                    textInput(ns("lbl_titulo"),    "Título",    placeholder = ""),
                    textInput(ns("lbl_subtitulo"), "Subtítulo", placeholder = ""),
                    textInput(ns("lbl_x"),         "Eje X",     placeholder = "automático"),
                    textInput(ns("lbl_y"),         "Eje Y",     placeholder = "automático"),
                    textInput(ns("lbl_color"),     "Leyenda",   placeholder = "automático"),
                    tags$hr(),
                    actionButton(ns("guardar_grafico"), "Guardar gráfico",
                                 icon = bs_icon("bookmark-check"),
                                 class = "btn-primary btn-sm w-100"),
                    uiOutput(ns("graficos_guardados_msg"))
                  )
                ),

                div(
                  card(
                    card_header(bs_icon("palette", class = "me-1"), "Estética"),
                    card_body(
                      selectInput(ns("tema"), "Tema", choices = temas_gg, selected = "light"),
                      sliderInput(ns("alpha"), "Transparencia", min = 0.1, max = 1,   value = 0.8, step = 0.1),
                      sliderInput(ns("size"),  "Tamaño",        min = 0.5, max = 5,   value = 2,   step = 0.5),
                      sliderInput(ns("bins"),  "Bins (histograma)", min = 5, max = 100, value = 30, step = 5),
                      checkboxInput(ns("eje_y_cero"), "Eje Y desde cero", value = FALSE)
                    )
                  ),
                  card(
                    card_header(bs_icon("palette2", class = "me-1"), "Color"),
                    card_body(
                      uiOutput(ns("color_fijo_ui"))
                    )
                  )
                )
              )
            )
          ),

          nav_panel(
            title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
            card_body(
              p(class = "text-muted small mb-2",
                "Código reproducible que genera este gráfico."),
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
mod_ggplot2_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Clasificar variable (reutiliza helper de mod_upload) ─────────────────
    clasificar <- function(x) {
      if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("Temporal")
      if (is.factor(x) || is.character(x) || is.logical(x)) return("Categórica")
      if (is.numeric(x)) {
        if (length(unique(x[!is.na(x)])) <= 10 &&
            all(x == as.integer(x), na.rm = TRUE))
          return("Numérica discreta")
        return("Numérica continua")
      }
      "Otra"
    }

    # Los selectores de variables son dinámicos (renderUI) — se resetean solos

    # ── Selectores de variables — todos en renderUI para poder volver a ninguna ─
    output$var_selectores_ui <- renderUI({
      df <- data()
      req(df)
      nms_all <- c("— ninguna —" = "", names(df))
      tagList(
        selectizeInput(ns("var_x"), "Variable X",
                       choices = names(df), selected = names(df)[1]),
        selectizeInput(ns("var_y"), "Variable Y",
                       choices = nms_all, selected = "",
                       options = list(placeholder = "— ninguna —",
                                      allowEmptyOption = TRUE)),
        selectizeInput(ns("var_color"), "Color / grupo",
                       choices = nms_all, selected = "",
                       options = list(placeholder = "— ninguna —",
                                      allowEmptyOption = TRUE,
                                      dropdownParent = "body"))
      )
    })

    # ── Facet selectors ───────────────────────────────────────────────────────
    output$var_facet_ui <- renderUI({
      df <- data()
      req(df)
      nms_fac <- c("— ninguna —" = "",
                   names(df)[sapply(df, function(x) is.factor(x) || is.character(x))])
      tagList(
        tags$p(class = "small text-muted mb-2",
               "Una variable divide en columnas; dos variables crean una cuadrícula."),
        selectInput(ns("var_facet"),     "Columnas", choices = nms_fac, selected = ""),
        uiOutput(ns("var_facet_col_ui"))
      )
    })

    # ── Selector de variable de etiqueta ─────────────────────────────────────
    output$var_etiqueta_ui <- renderUI({
      capas <- input$capas %||% character(0)
      if (!any(c("text_label", "label_label", "repel_label") %in% capas)) return(NULL)
      df <- data()
      req(df)
      nms_all <- c("— ninguna —" = "", names(df))
      selectizeInput(ns("var_etiqueta"), "Variable de etiqueta",
                     choices  = nms_all, selected = "",
                     options  = list(placeholder = "— ninguna —",
                                     allowEmptyOption = TRUE,
                                     dropdownParent = "body"))
    })

    output$var_facet_col_ui <- renderUI({
      df <- data()
      req(df)
      nms_fac <- c("— ninguna —" = "",
                   names(df)[sapply(df, function(x) is.factor(x) || is.character(x))])
      selectizeInput(ns("var_facet_col"), "Filas", choices = nms_fac, selected = "",
                     options = list(placeholder = "— ninguna —",
                                    allowEmptyOption = TRUE,
                                    dropdownParent = "body"))
    })

    # ── Color — paleta por variable o color fijo ─────────────────────────────
    output$color_fijo_ui <- renderUI({
      tagList(
        # Paleta cuando hay variable de color
        conditionalPanel(
          condition = sprintf("input['%s'] !== ''", ns("var_color")),
          tags$p(class = "small fw-bold mb-1", "Paleta de color"),
          selectInput(ns("paleta_var"), label = NULL,
                      choices = c(
                        "Tableau (accesible)" = "tableau",
                        "Viridis (D)"         = "viridis",
                        "Magma (A)"           = "magma",
                        "Inferno (B)"         = "inferno",
                        "Plasma (C)"          = "plasma",
                        "Cividis (E)"         = "cividis",
                        "Rocket (F)"          = "rocket",
                        "Mako (G)"            = "mako",
                        "Turbo (H)"           = "turbo"
                      ),
                      selected = "tableau")
        ),
        # Color fijo cuando no hay variable de color
        conditionalPanel(
          condition = sprintf("input['%s'] === ''", ns("var_color")),

          radioButtons(ns("tipo_color_fijo"), label = NULL,
                       choices = c("Paleta Tableau" = "tableau",
                                   "Color libre"    = "libre"),
                       selected = "tableau", inline = TRUE),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'tableau'", ns("tipo_color_fijo")),
            selectizeInput(ns("color_tableau"), label = NULL,
                          choices = c(
                            "Azul oscuro"    = "#1170AA",
                            "Naranja"        = "#FC7D0B",
                            "Gris medio"     = "#A3ACB9",
                            "Naranja oscuro" = "#C85200",
                            "Azul cielo"     = "#7BC8ED",
                            "Azul claro"     = "#5FA2CE",
                            "Amarillo"       = "#F1CE63",
                            "Marrón"         = "#9F8B75"
                          ),
                          selected = "#1170AA",
                          options = list(dropdownParent = "body"))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'libre'", ns("tipo_color_fijo")),
            if (requireNamespace("colourpicker", quietly = TRUE))
              colourpicker::colourInput(ns("color_libre"), label = NULL,
                                        value = "#1170AA")
            else
              textInput(ns("color_libre"), label = "Color (hex)",
                        value = "#1170AA", placeholder = "#1170AA")
          )
        )
      )
    })

    # ── Selector de geometría según variables elegidas ───────────────────────
    output$geom_ui <- renderUI({
      df <- data()
      req(df, input$var_x)
      tipo_x <- clasificar(df[[input$var_x]])
      tipo_y <- if (nzchar(input$var_y %||% ""))
                  clasificar(df[[input$var_y]]) else NULL
      geoms  <- geoms_disponibles(tipo_x, tipo_y)
      selectInput(ns("geom"), "Geometría", choices = geoms)
    })

    # ── Construir gráfico ────────────────────────────────────────────────────
    grafico_reactivo <- reactive({
      df    <- data()
      req(df, input$var_x, input$geom)

      var_x     <- input$var_x
      var_y     <- if (nzchar(input$var_y     %||% "")) input$var_y     else NULL
      var_color <- if (nzchar(input$var_color %||% "")) input$var_color else NULL
      geom      <- input$geom
      alpha     <- input$alpha %||% 0.8
      size      <- input$size  %||% 2
      bins      <- input$bins  %||% 30
      tema      <- input$tema  %||% "light"
      capas     <- input$capas %||% character(0)

      # ── Etiquetas ──
      lbl_x    <- if (nzchar(input$lbl_x        %||% "")) input$lbl_x        else var_x
      lbl_y    <- if (nzchar(input$lbl_y        %||% "")) input$lbl_y        else var_y %||% ""
      lbl_col  <- if (nzchar(input$lbl_color    %||% "")) input$lbl_color    else var_color %||% ""
      lbl_tit  <- input$lbl_titulo    %||% ""
      lbl_sub  <- input$lbl_subtitulo %||% ""

      # ── Color fijo cuando no hay var_color ──
      cf <- if (is.null(var_color)) {
        v <- if (isTRUE(input$tipo_color_fijo == "libre"))
          input$color_libre %||% colores$primario
        else
          input$color_tableau %||% colores$primario
        if (is.null(v) || !nzchar(v)) colores$primario else v
      } else NULL

      # ── Aesthetics base ──
      aes_base <- if (!is.null(var_color)) {
        ggplot2::aes(x = .data[[var_x]], color = .data[[var_color]],
                     fill = .data[[var_color]])
      } else {
        ggplot2::aes(x = .data[[var_x]])
      }
      if (!is.null(var_y))
        aes_base <- ggplot2::aes(x = .data[[var_x]], y = .data[[var_y]],
                                  color = if (!is.null(var_color)) .data[[var_color]] else NULL,
                                  fill  = if (!is.null(var_color)) .data[[var_color]] else NULL)

      p <- ggplot2::ggplot(df, aes_base)

      # ── Validaciones ──
      if (geom != "none") {
        if (geom %in% c("bar", "pie") && !is.null(var_y))
          validate(need(FALSE, "Barras y torta requieren solo variable X. Quitá la variable Y."))
        if (geom %in% c("point", "line", "point_smooth", "area",
                        "boxplot_group", "violin_group", "bar_mean", "point_group") &&
            is.null(var_y))
          validate(need(FALSE, "Este gráfico requiere variable Y."))
      }

      # Helper para pasar color/fill solo cuando cf no es NULL
      fill_cf  <- if (!is.null(cf)) list(fill  = cf) else list()
      color_cf <- if (!is.null(cf)) list(color = cf) else list()

      p <- switch(geom,
        "none"          = p,
        "histogram"     = p + do.call(ggplot2::geom_histogram,
                                c(list(bins = bins, alpha = alpha, color = "white"), fill_cf)),
        "density"       = p + do.call(ggplot2::geom_density,
                                c(list(alpha = alpha), fill_cf, color_cf)),
        "boxplot"       = p + do.call(ggplot2::geom_boxplot,
                                c(list(alpha = alpha, width = 0.5), fill_cf)),
        "violin"        = p + do.call(ggplot2::geom_violin,
                                c(list(alpha = alpha), fill_cf)),
        "point"         = p + do.call(ggplot2::geom_point,
                                c(list(alpha = alpha, size = size), color_cf)),
        "line"          = p + do.call(ggplot2::geom_line,
                                c(list(alpha = alpha, linewidth = size / 3), color_cf)),
        "point_smooth"  = p + do.call(ggplot2::geom_point,
                                c(list(alpha = alpha, size = size), color_cf)),
        "area"          = p + do.call(ggplot2::geom_area,
                                c(list(alpha = alpha), fill_cf, color_cf)),
        "bar"           = p + do.call(ggplot2::geom_bar,
                                c(list(alpha = alpha), fill_cf)),
        "pie"           = {
                            ggplot2::ggplot(df, ggplot2::aes(x = "", fill = .data[[var_x]])) +
                              ggplot2::geom_bar(width = 1, alpha = alpha, color = "white") +
                              ggplot2::coord_polar(theta = "y") +
                              scale_fill_tableau_cb() +
                              ggplot2::theme_void() +
                              ggplot2::theme(
                                legend.position = "right",
                                plot.title    = ggplot2::element_text(face = "bold", size = 13,
                                                                       color = colores$primario),
                                plot.subtitle = ggplot2::element_text(size = 10,
                                                                       color = colores$texto)
                              ) +
                              ggplot2::labs(
                                title    = if (nzchar(lbl_tit)) lbl_tit else NULL,
                                subtitle = if (nzchar(lbl_sub)) lbl_sub else NULL,
                                fill     = if (nzchar(lbl_col)) lbl_col else var_x
                              )
                          },
        "boxplot_group" = p + do.call(ggplot2::geom_boxplot,
                                c(list(alpha = alpha, width = 0.5), fill_cf)),
        "violin_group"  = p + do.call(ggplot2::geom_violin,
                                c(list(alpha = alpha), fill_cf)),
        "bar_mean"      = p + do.call(ggplot2::stat_summary,
                                c(list(fun = mean, geom = "bar", alpha = alpha), fill_cf)) +
                              ggplot2::stat_summary(fun.data = ggplot2::mean_se,
                                                    geom = "errorbar", width = 0.2),
        "point_group"   = p + do.call(ggplot2::geom_jitter,
                                c(list(alpha = alpha, size = size, width = 0.2), color_cf)),
        p + do.call(ggplot2::geom_point,
              c(list(alpha = alpha, size = size), color_cf))
      )

      # ── Capas adicionales ──
      # ── Capas de etiqueta ──
      var_etiqueta <- if (nzchar(input$var_etiqueta %||% "")) input$var_etiqueta else NULL
      if (!is.null(var_etiqueta)) {
        if ("text_label" %in% capas)
          p <- p + ggplot2::geom_text(
            ggplot2::aes(label = .data[[var_etiqueta]]),
            size = 3, vjust = -0.5
          )
        if ("label_label" %in% capas)
          p <- p + ggplot2::geom_label(
            ggplot2::aes(label = .data[[var_etiqueta]]),
            size = 3, vjust = -0.5
          )
        if ("repel_label" %in% capas) {
          if (requireNamespace("ggrepel", quietly = TRUE))
            p <- p + ggrepel::geom_text_repel(
              ggplot2::aes(label = .data[[var_etiqueta]]),
              size = 3, max.overlaps = 20
            )
          else
            p <- p + ggplot2::geom_text(
              ggplot2::aes(label = .data[[var_etiqueta]]),
              size = 3, vjust = -0.5
            )
        }
      }

      if ("boxplot_layer" %in% capas) {
        if (!is.null(var_x) && is.numeric(df[[var_x]]))
          p <- p + ggplot2::geom_boxplot(ggplot2::aes(group = .data[[var_x]]),
                                          alpha = 0.3, width = 0.5, outlier.shape = NA)
        else
          p <- p + ggplot2::geom_boxplot(alpha = 0.3, width = 0.5, outlier.shape = NA)
      }
      if ("violin_layer" %in% capas) {
        if (!is.null(var_x) && is.numeric(df[[var_x]]))
          p <- p + ggplot2::geom_violin(ggplot2::aes(group = .data[[var_x]]), alpha = 0.2)
        else
          p <- p + ggplot2::geom_violin(alpha = 0.2)
      }
      if ("smooth_lm" %in% capas)
        p <- p + ggplot2::geom_smooth(method = "lm",    formula = y ~ x,
                                       se = TRUE, color = colores$peligro)
      if ("smooth_loess" %in% capas)
        p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                                       se = TRUE, color = colores$peligro)

      if ("mean_point" %in% capas)
        p <- p + ggplot2::stat_summary(fun = mean, geom = "point",
                                        shape = 18, size = 4,
                                        color = colores$peligro)
      if ("hline_mean" %in% capas && !is.null(var_y))
        p <- p + ggplot2::geom_hline(yintercept = mean(df[[var_y]], na.rm = TRUE),
                                      linetype = "dashed", color = colores$texto)
      if ("vline_mean" %in% capas)
        p <- p + ggplot2::geom_vline(xintercept = mean(df[[var_x]], na.rm = TRUE),
                                      linetype = "dashed", color = colores$texto)

      # ── Escalas de color ──
      if (!is.null(var_color)) {
        paleta <- input$paleta_var %||% "tableau"
        es_num <- is.numeric(df[[var_color]])
        if (es_num) {
          # Variable numérica — siempre usar escala continua
          opcion <- switch(paleta %||% "viridis",
            "viridis" = "D", "magma" = "A", "inferno" = "B", "plasma" = "C",
            "cividis" = "E", "rocket" = "F", "mako" = "G", "turbo" = "H", "D")
          p <- p + ggplot2::scale_fill_viridis_c(option = opcion) +
                   ggplot2::scale_color_viridis_c(option = opcion)
        } else {
          # Variable discreta — tableau o viridis_d
          if (paleta == "tableau") {
            p <- p + scale_fill_tableau_cb() + scale_color_tableau_cb()
          } else {
            opcion <- switch(paleta,
              "viridis" = "D", "magma" = "A", "inferno" = "B", "plasma" = "C",
              "cividis" = "E", "rocket" = "F", "mako" = "G", "turbo" = "H", "D")
            p <- p + ggplot2::scale_fill_viridis_d(option = opcion) +
                     ggplot2::scale_color_viridis_d(option = opcion)
          }
        }
      } else {
        # Color fijo
        cf <- if (isTRUE(input$tipo_color_fijo == "libre"))
          input$color_libre %||% colores$primario
        else
          input$color_tableau %||% colores$primario
        cf <- if (is.null(cf) || !nzchar(cf)) colores$primario else cf
        # color ya aplicado directamente en el geom via cf
      }

      # ── Eje Y desde cero ──
      if (isTRUE(input$eje_y_cero) && !is.null(var_y))
        p <- p + ggplot2::scale_y_continuous(limits = c(0, NA))

      # ── Facets — automático según cuántas variables se eligieron ──
      facet_col  <- if (nzchar(input$var_facet     %||% "")) input$var_facet     else NULL
      facet_fila <- if (nzchar(input$var_facet_col %||% "")) input$var_facet_col else NULL
      if (!is.null(facet_col) && !is.null(facet_fila)) {
        # Dos variables → facet_grid
        p <- p + ggplot2::facet_grid(
          stats::as.formula(paste(facet_fila, "~", facet_col))
        )
      } else if (!is.null(facet_col)) {
        # Una variable → facet_wrap
        p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_col]]))
      }

      # ── Etiquetas ──
      labs_list <- list(
        title    = if (nzchar(lbl_tit)) lbl_tit else NULL,
        subtitle = if (nzchar(lbl_sub)) lbl_sub else NULL,
        x        = lbl_x,
        y        = if (nzchar(lbl_y))  lbl_y   else NULL,
        color    = if (nzchar(lbl_col)) lbl_col else NULL,
        fill     = if (nzchar(lbl_col)) lbl_col else NULL
      )
      p <- p + do.call(ggplot2::labs, Filter(Negate(is.null), labs_list))

      # ── Tema ──
      p <- p + switch(tema,
        "minimal" = ggplot2::theme_minimal(),
        "classic" = ggplot2::theme_classic(),
        "light"   = ggplot2::theme_light(),
        "gray"    = ggplot2::theme_gray(),
        "bw"      = ggplot2::theme_bw(),
        "void"    = ggplot2::theme_void(),
        ggplot2::theme_light()
      )

      p + ggplot2::theme(
        plot.title       = ggplot2::element_text(face = "bold", size = 16,
                                                  color = colores$primario),
        plot.subtitle    = ggplot2::element_text(size = 13, color = colores$texto),
        axis.title       = ggplot2::element_text(size = 14, color = colores$texto),
        axis.text        = ggplot2::element_text(size = 12, color = colores$texto),
        legend.text      = ggplot2::element_text(size = 12),
        legend.title     = ggplot2::element_text(size = 13),
        strip.background = ggplot2::element_rect(fill = colores$primario, color = NA),
        strip.text       = ggplot2::element_text(color = "white", face = "bold", size = 12)
      )
    })

    # ── Mensaje si no hay datos ──────────────────────────────────────────────
    output$grafico_msg <- renderUI({
      df <- data()
      if (is.null(df))
        div(class = "alert alert-info small",
            bs_icon("info-circle", class = "me-1"),
            "Cargá un dataset en la pestaña Datos para comenzar.")
      else NULL
    })

    # ── Render gráfico ───────────────────────────────────────────────────────
    output$grafico <- renderPlot({
      req(data(), input$var_x, input$geom)
      grafico_reactivo()
    }, res = 96)

    # ── Descargas ────────────────────────────────────────────────────────────
    descargar_grafico <- function(formato, ancho = 8, alto = 6) {
      downloadHandler(
        filename = function() paste0("grafico_", format(Sys.Date(), "%Y%m%d"),
                                     ".", formato),
        content  = function(file) {
          ggplot2::ggsave(file, plot = grafico_reactivo(),
                          device = formato, width = ancho, height = alto,
                          dpi = 300, units = "in")
        }
      )
    }

    output$dl_png <- descargar_grafico("png")
    output$dl_pdf <- descargar_grafico("pdf")
    output$dl_svg <- descargar_grafico("svg")

    # ── Código R reproducible ────────────────────────────────────────────────
    codigo_reactivo <- reactive({
      req(input$var_x, input$geom)

      var_x     <- input$var_x
      var_y     <- if (nzchar(input$var_y     %||% "")) input$var_y     else NULL
      var_color <- if (nzchar(input$var_color %||% "")) input$var_color else NULL
      geom      <- input$geom
      alpha     <- input$alpha %||% 0.8
      size      <- input$size  %||% 2
      bins      <- input$bins  %||% 30
      tema      <- input$tema  %||% "light"
      capas     <- input$capas %||% character(0)
      lbl_tit   <- input$lbl_titulo    %||% ""
      lbl_sub   <- input$lbl_subtitulo %||% ""
      lbl_x     <- if (nzchar(input$lbl_x     %||% "")) input$lbl_x     else var_x
      lbl_y     <- if (nzchar(input$lbl_y     %||% "")) input$lbl_y     else var_y %||% ""
      lbl_col   <- if (nzchar(input$lbl_color %||% "")) input$lbl_color else var_color %||% ""

      aes_str <- if (!is.null(var_y) && !is.null(var_color))
        paste0("aes(x = ", var_x, ", y = ", var_y, ", color = ", var_color,
               ", fill = ", var_color, ")")
      else if (!is.null(var_y))
        paste0("aes(x = ", var_x, ", y = ", var_y, ")")
      else if (!is.null(var_color))
        paste0("aes(x = ", var_x, ", color = ", var_color, ", fill = ", var_color, ")")
      else
        paste0("aes(x = ", var_x, ")")

      geom_str <- switch(geom,
        "histogram"     = paste0("geom_histogram(bins = ", bins, ", alpha = ", alpha, ", color = 'white')"),
        "density"       = paste0("geom_density(alpha = ", alpha, ")"),
        "boxplot"       = paste0("geom_boxplot(alpha = ", alpha, ", width = 0.5)"),
        "violin"        = paste0("geom_violin(alpha = ", alpha, ")"),
        "point"         = paste0("geom_point(alpha = ", alpha, ", size = ", size, ")"),
        "line"          = paste0("geom_line(alpha = ", alpha, ")"),
        "point_smooth"  = paste0("geom_point(alpha = ", alpha, ", size = ", size, ")"),
        "area"          = paste0("geom_area(alpha = ", alpha, ")"),
        "bar"           = paste0("geom_bar(alpha = ", alpha, ")"),
        "pie"           = paste0("geom_bar(alpha = ", alpha, ") +\n  coord_polar(theta = 'y')"),
        "boxplot_group" = paste0("geom_boxplot(alpha = ", alpha, ", width = 0.5)"),
        "violin_group"  = paste0("geom_violin(alpha = ", alpha, ")"),
        "bar_mean"      = paste0("stat_summary(fun = mean, geom = 'bar', alpha = ", alpha, ") +\n",
                                  "  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.2)"),
        "point_group"   = paste0("geom_jitter(alpha = ", alpha, ", size = ", size, ", width = 0.2)"),
        paste0("geom_point(alpha = ", alpha, ")")
      )

      capas_str <- paste(sapply(capas, function(c) switch(c,
        "smooth_lm"    = "  geom_smooth(method = 'lm', se = TRUE) +",
        "smooth_loess" = "  geom_smooth(method = 'loess', se = TRUE) +",
        "jitter"       = "  geom_jitter(alpha = 0.4, size = 1.5, width = 0.2) +",
        "mean_point"   = "  stat_summary(fun = mean, geom = 'point', shape = 18, size = 4) +",
        "hline_mean"   = paste0("  geom_hline(yintercept = mean(datos$", var_y,
                                 ", na.rm = TRUE), linetype = 'dashed') +"),
        "vline_mean"   = paste0("  geom_vline(xintercept = mean(datos$", var_x,
                                 ", na.rm = TRUE), linetype = 'dashed') +"),
        ""
      )), collapse = "\n")

      facet_col_c  <- if (nzchar(input$var_facet     %||% "")) input$var_facet     else NULL
      facet_fila_c <- if (nzchar(input$var_facet_col %||% "")) input$var_facet_col else NULL
      facet_str <- if (!is.null(facet_col_c) && !is.null(facet_fila_c))
        paste0("  facet_grid(", facet_fila_c, " ~ ", facet_col_c, ") +\n")
      else if (!is.null(facet_col_c))
        paste0("  facet_wrap(~", facet_col_c, ") +\n")
      else ""

      color_fijo <- if (isTRUE(input$tipo_color_fijo == "libre"))
        input$color_libre %||% colores$primario
      else
        input$color_tableau %||% colores$primario
      df_c <- data()
      color_str <- if (!is.null(var_color)) {
        if (!is.null(df_c) && is.numeric(df_c[[var_color]]))
          "  scale_fill_gradient(low = '#5FA2CE', high = '#1170AA') +\n  scale_color_gradient(low = '#5FA2CE', high = '#1170AA') +\n"
        else
          "  scale_fill_manual(values = c('#1170AA','#FC7D0B','#A3ACB9','#57606C','#C85200')) +\n  scale_color_manual(values = c('#1170AA','#FC7D0B','#A3ACB9','#57606C','#C85200')) +\n"
      } else
        paste0("  scale_fill_manual(values = '", color_fijo, "') +\n",
               "  scale_color_manual(values = '", color_fijo, "') +\n")

      labs_args <- paste(
        Filter(nzchar, c(
          if (nzchar(lbl_tit)) paste0("title = '", lbl_tit, "'"),
          if (nzchar(lbl_sub)) paste0("subtitle = '", lbl_sub, "'"),
          paste0("x = '", lbl_x, "'"),
          if (nzchar(lbl_y))  paste0("y = '", lbl_y, "'"),
          if (nzchar(lbl_col)) paste0("color = '", lbl_col, "', fill = '", lbl_col, "'")
        )),
        collapse = ", "
      )

      tema_str <- switch(tema,
        "minimal" = "theme_minimal()",
        "classic" = "theme_classic()",
        "light"   = "theme_light()",
        "gray"    = "theme_gray()",
        "bw"      = "theme_bw()",
        "void"    = "theme_void()",
        "theme_light()"
      )

      paste0(
        encabezado_script("Gráfico"),
        "library(ggplot2)\n\n",
        "# Cargá tus datos\n",
        "datos <- read.csv('tu_archivo.csv')\n\n",
        "# Gráfico\n",
        "ggplot(datos, ", aes_str, ") +\n",
        "  ", geom_str, " +\n",
        if (nzchar(capas_str)) paste0(capas_str, "\n") else "",
        color_str,
        facet_str,
        "  labs(", labs_args, ") +\n",
        "  ", tema_str, " +\n",
        "  theme(\n",
        "    plot.title    = element_text(face = 'bold', size = 13),\n",
        "    plot.subtitle = element_text(size = 10),\n",
        "    axis.title    = element_text(size = 10)\n",
        "  )\n"
      )
    })

    output$codigo_r <- renderText({ codigo_reactivo() })

    output$dl_script <- downloadHandler(
      filename = function() paste0("grafico_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(codigo_reactivo(), file)
    )

    # ── Lista de gráficos guardados para mod_patchwork ────────────────────────
    graficos_guardados <- reactiveVal(list())

    observeEvent(input$guardar_grafico, {
      req(grafico_reactivo())
      nombre <- if (nzchar(input$nombre_grafico %||% ""))
        input$nombre_grafico
      else
        paste0("Gráfico ", length(graficos_guardados()) + 1)

      lista_actual <- graficos_guardados()
      lista_actual[[nombre]] <- grafico_reactivo()
      graficos_guardados(lista_actual)
    })

    output$graficos_guardados_msg <- renderUI({
      lista <- graficos_guardados()
      if (length(lista) == 0) return(NULL)
      div(
        class = "mt-2",
        lapply(seq_along(lista), function(i) {
          nm <- names(lista)[i]
          div(
            class = "d-flex align-items-center justify-content-between mt-1",
            tags$span(class = "small",
                      bs_icon("check-circle-fill", style = paste0("color:", colores$exito),
                              class = "me-1"),
                      nm),
            actionButton(ns(paste0("eliminar_", i)), label = NULL,
                         icon = bs_icon("x"),
                         class = "btn-sm btn-outline-danger py-0 px-1")
          )
        })
      )
    })

    # Retornar gráficos guardados para mod_patchwork
    return(graficos_guardados)

  })
}
