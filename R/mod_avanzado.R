# =============================================================================
# mod_avanzado.R — Gráficos avanzados para StatPlot
# StatPlot · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Sub-tabs: Heatmap, Alluvial, Treemap, Correlaciones
# =============================================================================

# ── UI ────────────────────────────────────────────────────────────────────────
mod_avanzado_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "px-1 pt-2 pb-2",
      h4(style = paste0("color:", colores$primario, "; font-weight:700; margin-bottom:4px;"),
         bs_icon("graph-up", class = "me-2"), "Gráficos avanzados"),
      p(class = "text-muted small mb-0",
        "Visualizaciones especializadas que requieren estructuras de datos específicas. ",
        "El módulo prepara los datos automáticamente.")
    ),

    navset_card_tab(

      # ══════════════════════════════════════════════════════════════════════
      # HEATMAP
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("grid-3x3", class = "me-1"), "Heatmap"),
        card_body(
          layout_columns(
            col_widths = c(3, 9),

            div(
              card(
                card_header(bs_icon("table", class = "me-1"), "Variables"),
                card_body(
                  radioButtons(ns("hm_formato"), "Formato de datos",
                               choices = c("Largo (x, y, valor)" = "largo",
                                           "Ancho (matriz)"       = "ancho"),
                               selected = "largo", inline = TRUE),
                  tags$hr(),
                  uiOutput(ns("hm_vars_ui"))
                )
              ),
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Opciones"),
                card_body(
                  selectInput(ns("hm_escala"), "Escala",
                              choices = c(
                                "Sin escala"        = "none",
                                "Z-score por fila"  = "row",
                                "Z-score por col."  = "column",
                                "Min-max por fila"  = "minmax_row",
                                "Min-max por col."  = "minmax_col"
                              ), selected = "none"),
                  selectInput(ns("hm_paleta"), "Paleta",
                              choices = list(
                                "Divergentes" = c(
                                  "Blue-Red"   = "blue2red",
                                  "Blue-Brown" = "blue2brown",
                                  "BuRd"       = "BuRd",
                                  "Spectral"   = "spectral",
                                  "Icefire"    = "icefire"
                                ),
                                "Continuas (viridis)" = c(
                                  "Viridis"  = "viridis",
                                  "Magma"    = "magma",
                                  "Inferno"  = "inferno",
                                  "Plasma"   = "plasma",
                                  "Cividis"  = "cividis",
                                  "Rocket"   = "rocket",
                                  "Mako"     = "mako",
                                  "Turbo"    = "turbo"
                                )
                              ), selected = "blue2red"),
                  checkboxInput(ns("hm_rotate"),  "Rotar etiquetas X", value = TRUE),
                  checkboxInput(ns("hm_labels"),  "Mostrar valores",   value = FALSE),
                  conditionalPanel(
                    condition = sprintf("input['%s']", ns("hm_labels")),
                    sliderInput(ns("hm_label_size"), "Tamaño texto valores",
                                min = 6, max = 16, value = 9, step = 1)
                  ),
                  tags$hr(),
                  radioButtons(ns("hm_num_format"), "Formato numérico",
                               choices = c("Español (1 234,5)" = "es",
                                           "Inglés (1,234.5)"  = "en"),
                               selected = "es", inline = TRUE),
                  textInput(ns("hm_titulo"),    "Título",    placeholder = ""),
                  textInput(ns("hm_lbl_x"),     "Eje X",     placeholder = "automático"),
                  textInput(ns("hm_lbl_y"),     "Eje Y",     placeholder = "automático"),
                  textInput(ns("hm_lbl_color"), "Leyenda",   placeholder = "automático")
                )
              )
            ),

            div(
              navset_card_tab(
                nav_panel(
                  title = tagList(bs_icon("image", class = "me-1"), "Gráfico"),
                  card_body(
                    uiOutput(ns("hm_msg")),
                    plotOutput(ns("hm_plot"), height = "680px", width = "100%"),
                    tags$hr(),
                    div(class = "d-flex gap-2",
                        downloadButton(ns("hm_png"), "PNG", icon = bs_icon("download"),
                                       class = "btn-sm btn-outline-primary"),
                        downloadButton(ns("hm_pdf"), "PDF", icon = bs_icon("download"),
                                       class = "btn-sm btn-outline-primary"),
                        downloadButton(ns("hm_svg"), "SVG", icon = bs_icon("download"),
                                       class = "btn-sm btn-outline-primary"))
                  )
                ),
                nav_panel(
                  title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
                  card_body(
                    verbatimTextOutput(ns("hm_codigo")),
                    downloadButton(ns("hm_script"), "Descargar .R",
                                   icon = bs_icon("download"),
                                   class = "btn-sm btn-outline-primary mt-2")
                  )
                )
              )
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════════════════════════
      # ALLUVIAL
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("bezier2", class = "me-1"), "Alluvial"),
        card_body(
          layout_columns(
            col_widths = c(3, 9),

            div(
              card(
                card_header(bs_icon("table", class = "me-1"), "Variables"),
                card_body(
                  p(class = "text-muted small mb-2",
                    "Seleccioná las variables categóricas que forman las etapas del diagrama. ",
                    "El orden determina el flujo de izquierda a derecha."),
                  uiOutput(ns("al_vars_ui"))
                )
              ),
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Opciones"),
                card_body(
                  selectInput(ns("al_paleta"), "Paleta",
                              choices = c(
                                "Tableau"  = "tableau",
                                "Viridis"  = "viridis",
                                "Friendly" = "friendly"
                              ), selected = "tableau"),
                  sliderInput(ns("al_alpha"), "Transparencia flujos",
                              min = 0.1, max = 1, value = 0.5, step = 0.1),
                  checkboxInput(ns("al_etiquetas"), "Mostrar etiquetas", value = TRUE),
                  textInput(ns("al_titulo"),  "Título", placeholder = ""),
                  textInput(ns("al_lbl_x"),  "Eje X",  placeholder = "automático"),
                  textInput(ns("al_lbl_y"),  "Eje Y",  placeholder = "automático")
                )
              )
            ),

            div(
              navset_card_tab(
                nav_panel(
                  title = tagList(bs_icon("image", class = "me-1"), "Gráfico"),
                  card_body(
                    uiOutput(ns("al_msg")),
                    plotOutput(ns("al_plot"), height = "680px", width = "100%"),
                    tags$hr(),
                    div(class = "d-flex gap-2",
                        downloadButton(ns("al_png"), "PNG", icon = bs_icon("download"),
                                       class = "btn-sm btn-outline-primary"),
                        downloadButton(ns("al_pdf"), "PDF", icon = bs_icon("download"),
                                       class = "btn-sm btn-outline-primary"),
                        downloadButton(ns("al_svg"), "SVG", icon = bs_icon("download"),
                                       class = "btn-sm btn-outline-primary"))
                  )
                ),
                nav_panel(
                  title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
                  card_body(
                    verbatimTextOutput(ns("al_codigo")),
                    downloadButton(ns("al_script"), "Descargar .R",
                                   icon = bs_icon("download"),
                                   class = "btn-sm btn-outline-primary mt-2")
                  )
                )
              )
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════════════════════════
      # TREEMAP
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("diagram-3", class = "me-1"), "Treemap"),
        card_body(
          layout_columns(
            col_widths = c(3, 9),

            div(
              card(
                card_header(bs_icon("table", class = "me-1"), "Variables"),
                card_body(
                  p(class = "text-muted small mb-2",
                    "Seleccioná la variable de área (numérica) y la variable de etiqueta (categórica)."),
                  uiOutput(ns("tm_var_area_ui")),
                  uiOutput(ns("tm_var_label_ui")),
                  uiOutput(ns("tm_var_fill_ui"))
                )
              ),
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Opciones"),
                card_body(
                  sliderInput(ns("tm_size_label"), "Tamaño etiqueta",
                              min = 6, max = 20, value = 10, step = 1),
                  textInput(ns("tm_titulo"),  "Título",  placeholder = ""),
                  textInput(ns("tm_lbl_fill"), "Leyenda", placeholder = "automático")
                )
              )
            ),

            div(
              navset_card_tab(
                nav_panel(
                  title = tagList(bs_icon("image", class = "me-1"), "Gráfico"),
                  card_body(
                    uiOutput(ns("tm_msg")),
                    plotOutput(ns("tm_plot"), height = "680px", width = "100%"),
                    tags$hr(),
                    div(class = "d-flex gap-2",
                        downloadButton(ns("tm_png"), "PNG", icon = bs_icon("download"),
                                       class = "btn-sm btn-outline-primary"),
                        downloadButton(ns("tm_pdf"), "PDF", icon = bs_icon("download"),
                                       class = "btn-sm btn-outline-primary"),
                        downloadButton(ns("tm_svg"), "SVG", icon = bs_icon("download"),
                                       class = "btn-sm btn-outline-primary"))
                  )
                ),
                nav_panel(
                  title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
                  card_body(
                    verbatimTextOutput(ns("tm_codigo")),
                    downloadButton(ns("tm_script"), "Descargar .R",
                                   icon = bs_icon("download"),
                                   class = "btn-sm btn-outline-primary mt-2")
                  )
                )
              )
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════════════════════════
      # CORRELACIONES
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("arrow-left-right", class = "me-1"), "Correlaciones"),
        card_body(
          layout_columns(
            col_widths = c(3, 9),

            div(
              card(
                card_header(bs_icon("table", class = "me-1"), "Variables"),
                card_body(
                  p(class = "text-muted small mb-2",
                    "Seleccioná las variables numéricas a incluir en la matriz de correlaciones."),
                  uiOutput(ns("co_vars_ui"))
                )
              ),
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Opciones"),
                card_body(
                  selectInput(ns("co_metodo"), "Método",
                              choices = c("Pearson"  = "pearson",
                                          "Spearman" = "spearman",
                                          "Kendall"  = "kendall"),
                              selected = "pearson"),
                  selectInput(ns("co_tipo"), "Tipo de gráfico",
                              choices = c("Completo"      = "full",
                                          "Inferior"      = "lower",
                                          "Superior"      = "upper"),
                              selected = "lower"),
                  selectInput(ns("co_forma"), "Forma",
                              choices = c("Círculo"  = "circle",
                                          "Cuadrado" = "square"),
                              selected = "circle"),
                  checkboxInput(ns("co_numeros"), "Mostrar coeficientes", value = FALSE),
                  checkboxInput(ns("co_pvalor"), "Mostrar p-valores", value = TRUE),
                  textInput(ns("co_titulo"), "Título", placeholder = ""),
                  tags$hr(),
                  radioButtons(ns("co_num_format"), "Formato numérico",
                               choices = c("Español (1 234,5)" = "es",
                                           "Inglés (1,234.5)"  = "en"),
                               selected = "es", inline = TRUE)
                )
              )
            ),

            div(
              navset_card_tab(
                nav_panel(
                  title = tagList(bs_icon("image", class = "me-1"), "Gráfico"),
                  card_body(
                    uiOutput(ns("co_msg")),
                    plotOutput(ns("co_plot"), height = "680px", width = "100%"),
                    tags$hr(),
                    div(class = "d-flex gap-2",
                        downloadButton(ns("co_png"), "PNG", icon = bs_icon("download"),
                                       class = "btn-sm btn-outline-primary"),
                        downloadButton(ns("co_pdf"), "PDF", icon = bs_icon("download"),
                                       class = "btn-sm btn-outline-primary"),
                        downloadButton(ns("co_svg"), "SVG", icon = bs_icon("download"),
                                       class = "btn-sm btn-outline-primary"))
                  )
                ),
                nav_panel(
                  title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
                  card_body(
                    verbatimTextOutput(ns("co_codigo")),
                    downloadButton(ns("co_script"), "Descargar .R",
                                   icon = bs_icon("download"),
                                   class = "btn-sm btn-outline-primary mt-2")
                  )
                )
              )
            )
          )
        )
      )

    ) # /navset_card_tab
  )
}


# ── Server ────────────────────────────────────────────────────────────────────
mod_avanzado_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Helpers ───────────────────────────────────────────────────────────────
    vars_num <- reactive({
      df <- data(); req(df)
      names(df)[sapply(df, is.numeric)]
    })

    vars_cat <- reactive({
      df <- data(); req(df)
      names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    })

    paleta_tableau <- c("#1170AA","#FC7D0B","#A3ACB9","#57606C","#C85200",
                        "#7BC8ED","#5FA2CE","#F1CE63","#9F8B75")

    # ══════════════════════════════════════════════════════════════════════════
    # HEATMAP
    # ══════════════════════════════════════════════════════════════════════════

    # ── Selectores dinámicos según formato ──────────────────────────────────
    output$hm_vars_ui <- renderUI({
      df  <- data(); req(df)
      fmt <- input$hm_formato %||% "largo"
      if (fmt == "largo") {
        nms_all <- c("— ninguna —" = "", names(df))
        tagList(
          p(class = "text-muted small mb-2",
            "Formato largo: cada fila es una observación con x, y y valor."),
          selectizeInput(ns("hm_var_x"), "Variable X (columnas)",
                         choices = names(df), selected = names(df)[1]),
          selectizeInput(ns("hm_var_y"), "Variable Y (filas)",
                         choices = names(df), selected = names(df)[min(2, length(names(df)))]),
          selectizeInput(ns("hm_var_color"), "Variable de valor (color)",
                         choices  = c("— ninguna —" = "", vars_num()),
                         selected = if (length(vars_num()) > 0) vars_num()[1] else "",
                         options  = list(placeholder = "— ninguna —",
                                         allowEmptyOption = TRUE,
                                         dropdownParent = "body"))
        )
      } else {
        tagList(
          p(class = "text-muted small mb-2",
            "Formato ancho: seleccioná la variable de filas y las columnas numéricas."),
          selectizeInput(ns("hm_var_fila"), "Variable de filas (eje Y)",
                         choices  = c("— ninguna —" = "", vars_cat()),
                         selected = if (length(vars_cat()) > 0) vars_cat()[1] else "",
                         options  = list(placeholder = "— ninguna —",
                                         allowEmptyOption = TRUE,
                                         dropdownParent = "body")),
          checkboxGroupInput(ns("hm_vars_cols"), "Variables numéricas (columnas)",
                             choices  = vars_num(),
                             selected = vars_num()[seq_len(min(5, length(vars_num())))])
        )
      }
    })

    hm_data <- reactive({
      df  <- data(); req(df)
      fmt <- input$hm_formato %||% "largo"

      if (fmt == "largo") {
        req(input$hm_var_x, input$hm_var_y, nzchar(input$hm_var_color %||% ""))
        escala_tp <- input$hm_escala %||% "none"
        if (escala_tp %in% c("minmax_row", "minmax_col")) escala_tp <- "none"
        list(df = df, escala_tp = escala_tp,
             var_x = input$hm_var_x, var_y = input$hm_var_y,
             var_color = input$hm_var_color, formato = "largo")
      } else {
        var_fila <- input$hm_var_fila
        cols     <- input$hm_vars_cols
        req(nzchar(var_fila %||% ""), length(cols) >= 2)

        df_sel <- df[, c(var_fila, cols), drop = FALSE]
        escala <- input$hm_escala %||% "none"

        if (escala == "minmax_row") {
          df_sel[cols] <- t(apply(df_sel[cols], 1, function(x) {
            rng <- range(x, na.rm = TRUE)
            if (diff(rng) == 0) rep(0, length(x)) else (x - rng[1]) / diff(rng)
          }))
          escala_tp <- "none"
        } else if (escala == "minmax_col") {
          df_sel[cols] <- apply(df_sel[cols], 2, function(x) {
            rng <- range(x, na.rm = TRUE)
            if (diff(rng) == 0) rep(0, length(x)) else (x - rng[1]) / diff(rng)
          })
          escala_tp <- "none"
        } else {
          escala_tp <- escala
        }

        df_long <- tidyr::pivot_longer(df_sel, cols = dplyr::all_of(cols),
                                        names_to = "variable", values_to = "valor")
        list(df = df_long, escala_tp = escala_tp,
             var_x = "variable", var_y = var_fila,
             var_color = "valor", formato = "ancho")
      }
    })

    hm_plot_reactivo <- reactive({
      d      <- hm_data()
      paleta <- input$hm_paleta %||% "blue2red"
      rotate <- isTRUE(input$hm_rotate)
      labels <- isTRUE(input$hm_labels)
      titulo <- input$hm_titulo %||% ""

      pal_fn <- switch(paleta,
        "blue2red"   = tidyplots::colors_diverging_blue2red,
        "blue2brown" = tidyplots::colors_diverging_blue2brown,
        "BuRd"       = tidyplots::colors_diverging_BuRd,
        "spectral"   = tidyplots::colors_diverging_spectral,
        "icefire"    = tidyplots::colors_diverging_icefire,
        "viridis"    = tidyplots::colors_continuous_viridis,
        "magma"      = tidyplots::colors_continuous_magma,
        "inferno"    = tidyplots::colors_continuous_inferno,
        "plasma"     = tidyplots::colors_continuous_plasma,
        "cividis"    = tidyplots::colors_continuous_cividis,
        "rocket"     = tidyplots::colors_continuous_rocket,
        "mako"       = tidyplots::colors_continuous_mako,
        "turbo"      = tidyplots::colors_continuous_turbo,
        tidyplots::colors_diverging_blue2red
      )

      tp <- d$df |>
        tidyplots::tidyplot(x     = .data[[d$var_x]],
                             y     = .data[[d$var_y]],
                             color = .data[[d$var_color]]) |>
        tidyplots::add_heatmap(scale         = d$escala_tp,
                                rotate_labels = if (rotate) 90 else 0) |>
        tidyplots::adjust_colors(pal_fn) |>
        tidyplots::adjust_size(width = NA, height = NA) |>
        tidyplots::adjust_font(fontsize = 12)

      if (labels) {
        label_size <- input$hm_label_size %||% 9
        num_fmt    <- input$hm_num_format %||% "es"
        fmt_val <- function(x) {
          if (num_fmt == "es")
            formatC(x, format = "f", digits = 1, big.mark = " ", decimal.mark = ",")
          else
            formatC(x, format = "f", digits = 1, big.mark = ",", decimal.mark = ".")
        }
        tp <- tp |> tidyplots::add(
          ggplot2::geom_label(
            ggplot2::aes(label = fmt_val(.data[[d$var_color]])),
            size       = label_size / ggplot2::.pt,
            color      = "black",
            fill       = "white",
            alpha      = 0.8,
            label.size = 0,
            hjust      = 0.5,
            vjust      = 0.5
          )
        )
      }

      # Etiquetas de ejes
      lbl_x     <- if (nzchar(input$hm_lbl_x     %||% "")) input$hm_lbl_x     else NULL
      lbl_y     <- if (nzchar(input$hm_lbl_y     %||% "")) input$hm_lbl_y     else NULL
      lbl_color <- if (nzchar(input$hm_lbl_color %||% "")) input$hm_lbl_color else NULL
      if (!is.null(lbl_x))     tp <- tp |> tidyplots::adjust_x_axis_title(lbl_x)
      if (!is.null(lbl_y))     tp <- tp |> tidyplots::adjust_y_axis_title(lbl_y)
      if (!is.null(lbl_color)) tp <- tp |> tidyplots::adjust_legend_title(lbl_color)

      if (nzchar(titulo)) tp <- tp |> tidyplots::add_title(titulo)
      tp
    })

    output$hm_msg <- renderUI({
      df <- data()
      if (is.null(df))
        div(class = "alert alert-info small",
            bs_icon("info-circle", class = "me-1"),
            "Cargá un dataset en la pestaña Datos.")
      else NULL
    })

    output$hm_plot <- renderPlot({
      req(data(), hm_data())
      hm_plot_reactivo()
    }, res = 96)

    output$hm_png <- downloadHandler(
      filename = function() paste0("heatmap_", format(Sys.Date(), "%Y%m%d"), ".png"),
      content  = function(file) tidyplots::save_plot(hm_plot_reactivo(), filename = file)
    )
    output$hm_pdf <- downloadHandler(
      filename = function() paste0("heatmap_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
      content  = function(file) tidyplots::save_plot(hm_plot_reactivo(), filename = file)
    )
    output$hm_svg <- downloadHandler(
      filename = function() paste0("heatmap_", format(Sys.Date(), "%Y%m%d"), ".svg"),
      content  = function(file) tidyplots::save_plot(hm_plot_reactivo(), filename = file)
    )

    output$hm_codigo <- renderText({
      req(input$hm_var_fila, input$hm_vars_cols)
      cols_str <- paste0('c("', paste(input$hm_vars_cols, collapse = '", "'), '")')
      paste0(
        encabezado_script("Heatmap"),
        "library(tidyplots)\nlibrary(tidyr)\n\n",
        "datos <- read.csv('tu_archivo.csv')\n\n",
        "datos |>\n",
        "  select(", input$hm_var_fila, ", ", cols_str, ") |>\n",
        "  pivot_longer(cols = ", cols_str, ",\n",
        "               names_to = 'variable', values_to = 'valor') |>\n",
        "  tidyplot(x = variable, y = ", input$hm_var_fila, ", color = valor) |>\n",
        "  add_heatmap(scale = '", input$hm_escala %||% "none", "') |>\n",
        "  adjust_colors(colors_diverging_blue2red)\n"
      )
    })

    output$hm_script <- downloadHandler(
      filename = function() paste0("heatmap_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(output$hm_codigo(), file)
    )

    # ══════════════════════════════════════════════════════════════════════════
    # ALLUVIAL
    # ══════════════════════════════════════════════════════════════════════════

    output$al_vars_ui <- renderUI({
      checkboxGroupInput(ns("al_vars"), "Variables categóricas (etapas)",
                         choices  = vars_cat(),
                         selected = vars_cat()[seq_len(min(3, length(vars_cat())))])
    })

    al_plot_reactivo <- reactive({
      df   <- data()
      vars <- input$al_vars
      req(df, length(vars) >= 2)

      alpha   <- input$al_alpha   %||% 0.5
      titulo  <- input$al_titulo  %||% ""
      etiq    <- isTRUE(input$al_etiquetas)
      paleta  <- input$al_paleta  %||% "tableau"

      df_sel <- df[, vars, drop = FALSE] |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.factor))

      # Usar as.data.frame(table()) para formato correcto
      df_long <- ggalluvial::to_lodes_form(df_sel, axes = seq_along(vars))

      p <- ggplot2::ggplot(df_long,
             ggplot2::aes(x = x, y = 1, stratum = stratum,
                          alluvium = alluvium, fill = stratum)) +
           ggalluvial::geom_flow(alpha = alpha, color = "white", linewidth = 0.3) +
           ggalluvial::geom_stratum(alpha = 0.85, color = "white", linewidth = 0.3) +
           ggplot2::scale_x_discrete(limits = vars) +
           ggplot2::theme_minimal() +
           ggplot2::theme(
             legend.position  = "none",
             axis.title       = ggplot2::element_text(size = 12),
             axis.text        = ggplot2::element_text(size = 11),
             plot.title       = ggplot2::element_text(face = "bold", size = 14,
                                                       color = colores$primario)
           ) +
           ggplot2::labs(
             x     = if (nzchar(input$al_lbl_x %||% "")) input$al_lbl_x else NULL,
             y     = if (nzchar(input$al_lbl_y %||% "")) input$al_lbl_y else "Frecuencia",
             title = if (nzchar(titulo)) titulo else NULL)

      if (etiq)
        p <- p + ggplot2::geom_text(stat = ggalluvial::StatStratum,
                                     ggplot2::aes(label = ggplot2::after_stat(stratum)),
                                     size = 3.5, color = "white", fontface = "bold")

      if (paleta == "tableau")
        p <- p + scale_fill_tableau_cb()
      else if (paleta == "viridis")
        p <- p + ggplot2::scale_fill_viridis_d(option = "D")
      else
        p <- p + ggplot2::scale_fill_manual(
          values = tidyplots::colors_discrete_friendly)

      p
    })

    output$al_msg <- renderUI({
      df <- data()
      if (is.null(df))
        div(class = "alert alert-info small",
            bs_icon("info-circle", class = "me-1"),
            "Cargá un dataset en la pestaña Datos.")
      else NULL
    })

    output$al_plot <- renderPlot({
      req(data(), input$al_vars, length(input$al_vars) >= 2)
      al_plot_reactivo()
    }, res = 96)

    output$al_png <- downloadHandler(
      filename = function() paste0("alluvial_", format(Sys.Date(), "%Y%m%d"), ".png"),
      content  = function(file)
        ggplot2::ggsave(file, al_plot_reactivo(), width = 10, height = 7, dpi = 300)
    )
    output$al_pdf <- downloadHandler(
      filename = function() paste0("alluvial_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
      content  = function(file)
        ggplot2::ggsave(file, al_plot_reactivo(), width = 10, height = 7)
    )
    output$al_svg <- downloadHandler(
      filename = function() paste0("alluvial_", format(Sys.Date(), "%Y%m%d"), ".svg"),
      content  = function(file)
        ggplot2::ggsave(file, al_plot_reactivo(), width = 10, height = 7)
    )

    output$al_codigo <- renderText({
      req(input$al_vars)
      vars_str <- paste0('c("', paste(input$al_vars, collapse = '", "'), '")')
      paste0(
        encabezado_script("Alluvial"),
        "library(ggplot2)\nlibrary(ggalluvial)\n\n",
        "datos <- read.csv('tu_archivo.csv')\n\n",
        "df_sel <- datos[, ", vars_str, "] |>\n",
        "  dplyr::mutate(dplyr::across(everything(), as.factor))\n\n",
        "df_long <- to_lodes_form(df_sel, axes = 1:", length(input$al_vars), ")\n\n",
        "ggplot(df_long, aes_flow(alluvium = alluvium)) +\n",
        "  aes(x = x, stratum = stratum, fill = stratum) +\n",
        "  geom_flow(alpha = ", input$al_alpha %||% 0.5, ", color = 'white') +\n",
        "  geom_stratum(alpha = 0.85, color = 'white') +\n",
        "  geom_text(stat = StatStratum, aes(label = stratum),\n",
        "            size = 3.5, color = 'white', fontface = 'bold') +\n",
        "  theme_minimal() +\n",
        "  labs(x = NULL, y = 'Frecuencia')\n"
      )
    })

    output$al_script <- downloadHandler(
      filename = function() paste0("alluvial_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(output$al_codigo(), file)
    )

    # ══════════════════════════════════════════════════════════════════════════
    # TREEMAP
    # ══════════════════════════════════════════════════════════════════════════

    output$tm_var_area_ui <- renderUI({
      selectizeInput(ns("tm_var_area"), "Variable de área (numérica)",
                     choices = vars_num(), selected = vars_num()[1])
    })

    output$tm_var_label_ui <- renderUI({
      selectizeInput(ns("tm_var_label"), "Etiqueta (categórica)",
                     choices = vars_cat(), selected = vars_cat()[1])
    })

    output$tm_var_fill_ui <- renderUI({
      all_vars <- c("— misma que etiqueta —" = "", vars_cat(), vars_num())
      selectizeInput(ns("tm_var_fill"), "Color de relleno",
                     choices  = all_vars, selected = "",
                     options  = list(placeholder = "— misma que etiqueta —",
                                     allowEmptyOption = TRUE))
    })

    tm_plot_reactivo <- reactive({
      df        <- data()
      var_area  <- input$tm_var_area
      var_label <- input$tm_var_label
      var_fill  <- if (nzchar(input$tm_var_fill %||% "")) input$tm_var_fill else var_label
      req(df, var_area, var_label)

      size_label <- input$tm_size_label %||% 10
      titulo     <- input$tm_titulo %||% ""

      p <- ggplot2::ggplot(df,
             ggplot2::aes(area  = .data[[var_area]],
                          fill  = .data[[var_fill]],
                          label = .data[[var_label]])) +
           treemapify::geom_treemap() +
           treemapify::geom_treemap_text(
             color    = "white",
             place    = "centre",
             size     = size_label,
             fontface = "bold",
             reflow   = TRUE
           ) +
           scale_fill_tableau_cb() +
           ggplot2::theme(
             legend.position = "bottom",
             plot.title      = ggplot2::element_text(face = "bold", size = 14,
                                                      color = colores$primario)
           )

      lbl_fill <- if (nzchar(input$tm_lbl_fill %||% "")) input$tm_lbl_fill else NULL
      labs_list <- list(
        title = if (nzchar(titulo)) titulo else NULL,
        fill  = lbl_fill
      )
      p <- p + do.call(ggplot2::labs, Filter(Negate(is.null), labs_list))
      p
    })

    output$tm_msg <- renderUI({
      df <- data()
      if (is.null(df))
        div(class = "alert alert-info small",
            bs_icon("info-circle", class = "me-1"),
            "Cargá un dataset en la pestaña Datos.")
      else NULL
    })

    output$tm_plot <- renderPlot({
      req(data(), input$tm_var_area, input$tm_var_label)
      tm_plot_reactivo()
    }, res = 96)

    output$tm_png <- downloadHandler(
      filename = function() paste0("treemap_", format(Sys.Date(), "%Y%m%d"), ".png"),
      content  = function(file)
        ggplot2::ggsave(file, tm_plot_reactivo(), width = 10, height = 7, dpi = 300)
    )
    output$tm_pdf <- downloadHandler(
      filename = function() paste0("treemap_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
      content  = function(file)
        ggplot2::ggsave(file, tm_plot_reactivo(), width = 10, height = 7)
    )
    output$tm_svg <- downloadHandler(
      filename = function() paste0("treemap_", format(Sys.Date(), "%Y%m%d"), ".svg"),
      content  = function(file)
        ggplot2::ggsave(file, tm_plot_reactivo(), width = 10, height = 7)
    )

    output$tm_codigo <- renderText({
      req(input$tm_var_area, input$tm_var_label)
      paste0(
        encabezado_script("Treemap"),
        "library(ggplot2)\nlibrary(treemapify)\n\n",
        "datos <- read.csv('tu_archivo.csv')\n\n",
        "ggplot(datos, aes(area = ", input$tm_var_area,
        ", fill = ", input$tm_var_label,
        ", label = ", input$tm_var_label, ")) +\n",
        "  geom_treemap() +\n",
        "  geom_treemap_text(color = 'white', place = 'centre',\n",
        "                    size = ", input$tm_size_label %||% 10,
        ", fontface = 'bold', reflow = TRUE)\n"
      )
    })

    output$tm_script <- downloadHandler(
      filename = function() paste0("treemap_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(output$tm_codigo(), file)
    )

    # ══════════════════════════════════════════════════════════════════════════
    # CORRELACIONES
    # ══════════════════════════════════════════════════════════════════════════

    output$co_vars_ui <- renderUI({
      checkboxGroupInput(ns("co_vars"), "Variables numéricas",
                         choices  = vars_num(),
                         selected = vars_num()[seq_len(min(6, length(vars_num())))])
    })

    co_plot_reactivo <- reactive({
      df   <- data()
      vars <- input$co_vars
      req(df, length(vars) >= 2)

      metodo     <- input$co_metodo     %||% "pearson"
      tipo       <- input$co_tipo       %||% "lower"
      forma      <- input$co_forma      %||% "circle"
      pval       <- isTRUE(input$co_pvalor)
      titulo     <- input$co_titulo     %||% ""
      num_format <- input$co_num_format %||% "es"

      dec_mark <- if (num_format == "es") "," else "."
      big_mark <- if (num_format == "es") "\u00a0" else ","   # espacio fino / coma

      mat <- cor(df[, vars, drop = FALSE], method = metodo, use = "complete.obs")

      numeros <- isTRUE(input$co_numeros)

      # Formateador para la leyenda (fill continuo)
      fmt_leyenda <- function(x) {
        formatC(x, digits = 2, format = "f",
                decimal.mark = dec_mark, big.mark = big_mark)
      }

      # Si se muestran coeficientes, crear versión formateada de mat para lab_col
      # ggcorrplot usa lab_col internamente; el truco es redondear la matriz
      # y dejar que ggcorrplot la formatee, luego sobreescribir la escala fill.
      p <- ggcorrplot::ggcorrplot(
        mat,
        method  = forma,
        type    = tipo,
        lab     = numeros,
        digits  = 2,
        p.mat   = if (pval) ggcorrplot::cor_pmat(df[, vars, drop = FALSE],
                                                   method = metodo) else NULL,
        colors  = c(colores$peligro, "white", colores$primario),
        ggtheme = ggplot2::theme_minimal()
      ) +
      ggplot2::scale_fill_gradient2(
        low    = colores$peligro,
        mid    = "white",
        high   = colores$primario,
        limits = c(-1, 1),
        labels = fmt_leyenda
      ) +
      ggplot2::theme(
        axis.text        = ggplot2::element_text(size = 11),
        plot.title       = ggplot2::element_text(face = "bold", size = 14,
                                                  color = colores$primario),
        legend.title     = ggplot2::element_text(size = 11)
      )

      # Si se muestran coeficientes sobre el gráfico, reemplazar las etiquetas
      # de texto que ggcorrplot genera (siempre con punto) por el formato correcto
      if (numeros && num_format == "es") {
        p$layers <- lapply(p$layers, function(lyr) {
          if (inherits(lyr$geom, "GeomText")) {
            lyr$data$label <- gsub("\\.", ",", lyr$data$label)
          }
          lyr
        })
      }

      if (nzchar(titulo)) p <- p + ggplot2::labs(title = titulo)
      p
    })

    output$co_msg <- renderUI({
      df <- data()
      if (is.null(df))
        div(class = "alert alert-info small",
            bs_icon("info-circle", class = "me-1"),
            "Cargá un dataset en la pestaña Datos.")
      else NULL
    })

    output$co_plot <- renderPlot({
      req(data(), input$co_vars, length(input$co_vars) >= 2)
      co_plot_reactivo()
    }, res = 96)

    output$co_png <- downloadHandler(
      filename = function() paste0("correlaciones_", format(Sys.Date(), "%Y%m%d"), ".png"),
      content  = function(file)
        ggplot2::ggsave(file, co_plot_reactivo(), width = 8, height = 8, dpi = 300)
    )
    output$co_pdf <- downloadHandler(
      filename = function() paste0("correlaciones_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
      content  = function(file)
        ggplot2::ggsave(file, co_plot_reactivo(), width = 8, height = 8)
    )
    output$co_svg <- downloadHandler(
      filename = function() paste0("correlaciones_", format(Sys.Date(), "%Y%m%d"), ".svg"),
      content  = function(file)
        ggplot2::ggsave(file, co_plot_reactivo(), width = 8, height = 8)
    )

    output$co_codigo <- renderText({
      req(input$co_vars)
      vars_str <- paste0('c("', paste(input$co_vars, collapse = '", "'), '")')
      paste0(
        encabezado_script("Correlaciones"),
        "library(ggplot2)\nlibrary(ggcorrplot)\n\n",
        "datos <- read.csv('tu_archivo.csv')\n\n",
        "mat <- cor(datos[, ", vars_str, "],\n",
        "           method = '", input$co_metodo %||% "pearson", "',\n",
        "           use = 'complete.obs')\n\n",
        "ggcorrplot(mat,\n",
        "           method = '", input$co_forma %||% "circle", "',\n",
        "           type   = '", input$co_tipo   %||% "lower",  "',\n",
        "           lab    = ", input$co_forma == "number", ")\n"
      )
    })

    output$co_script <- downloadHandler(
      filename = function() paste0("correlaciones_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(output$co_codigo(), file)
    )

  })
}
