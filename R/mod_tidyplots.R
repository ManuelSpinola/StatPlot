# =============================================================================
# mod_tidyplots.R — Visualización con tidyplots para StatPlot
# StatPlot · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Exporta:
#   mod_tidyplots_ui(id)
#   mod_tidyplots_server(id, data)
# =============================================================================

# ── Tipos de gráfico disponibles ──────────────────────────────────────────────
tipos_grafico <- list(
  " " = c(
    "Ninguna (quitar capa base)" = "none"
  ),
  "Resumen estadístico" = c(
    "Barras (media)"   = "mean_bar",
    "Línea (media)"    = "mean_line",
    "Punto (media)"    = "mean_dot",
    "Punto (mediana)"  = "median_dot",
    "Línea (mediana)"  = "median_line"
  ),
  "Distribución" = c(
    "Boxplot"          = "boxplot",
    "Violín"           = "violin",
    "Beeswarm"         = "beeswarm"
  ),
  "Proporción" = c(
    "Barras apiladas"  = "barstack",
    "Área apilada"     = "areastack",
    "Torta"            = "pie",
    "Dona"             = "donut"
  ),
  "Datos crudos" = c(
    "Puntos"           = "points",
    "Puntos beeswarm"  = "points_beeswarm"
  ),
  "Especial" = c(
    "Heatmap"          = "heatmap",
    "Histograma"       = "histogram"
  )
)

# ── Paletas disponibles ───────────────────────────────────────────────────────
paletas_tidyplot <- c(
  "Friendly (accesible)" = "friendly",
  "Seaside"              = "seaside",
  "Apple"                = "apple",
  "Viridis"              = "viridis",
  "Plasma"               = "plasma",
  "Mako"                 = "mako",
  "Blue-Brown"           = "blue2brown",
  "Blue-Red"             = "blue2red",
  "Spectral"             = "spectral"
)

# ── UI ────────────────────────────────────────────────────────────────────────
mod_tidyplots_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "px-1 pt-2 pb-2",
      h4(style = paste0("color:", colores$primario, "; font-weight:700; margin-bottom:4px;"),
         bs_icon("palette", class = "me-2"), "Tidyplot"),
      p(class = "text-muted small mb-0",
        "Gráficos con tidyplots — sintaxis moderna por pipe. ",
        "Ideal para heatmaps, beeswarm, tortas y composiciones estadísticas.")
    ),

    layout_columns(
      col_widths = c(3, 9),

      # ── PANEL IZQUIERDO ───────────────────────────────────────────────────
      div(

        # ── Variables ──────────────────────────────────────────────────────
        card(
          card_header(bs_icon("table", class = "me-1"), "Variables"),
          card_body(
            uiOutput(ns("var_selectores_ui"))
          )
        ),

        # ── Tipo de gráfico ────────────────────────────────────────────────
        card(
          card_header(bs_icon("bar-chart", class = "me-1"), "Tipo de gráfico"),
          card_body(
            selectInput(ns("tipo"), "Gráfico",
                        choices  = tipos_grafico,
                        selected = "mean_bar"),
            tags$hr(),
            # Capas adicionales
            checkboxGroupInput(
              ns("capas"),
              label   = "Capas adicionales",
              choices = c(
                "Barra de error (SEM)"  = "sem_errorbar",
                "Barra de error (SD)"   = "sd_errorbar",
                "IC 95%"                = "ci95_errorbar",
                "Puntos de datos"       = "data_points",
                "Beeswarm"              = "data_beeswarm",
                "Smooth"                = "curvefit",
                "Comparación (p-valor)" = "test_pvalue",
                "Comparación (*)"       = "test_asterisks"
              )
            )
          )
        ),

        # ── Estética ───────────────────────────────────────────────────────
        card(
          card_header(bs_icon("palette", class = "me-1"), "Estética"),
          card_body(
            selectInput(ns("paleta"), "Paleta de color",
                        choices  = paletas_tidyplot,
                        selected = "friendly"),
            sliderInput(ns("alpha"), "Transparencia",
                        min = 0.1, max = 1, value = 0.8, step = 0.1),
            checkboxInput(ns("invertir_paleta"), "Invertir paleta", value = FALSE),
            tags$hr(),
            textInput(ns("lbl_titulo"),    "Título",    placeholder = ""),
            textInput(ns("lbl_caption"),   "Caption",   placeholder = ""),
            textInput(ns("lbl_x"),         "Eje X",     placeholder = "automático"),
            textInput(ns("lbl_y"),         "Eje Y",     placeholder = "automático"),
            tags$hr(),
            textInput(ns("nombre_grafico"), "Nombre del gráfico",
                      placeholder = "para composición con patchwork"),
            actionButton(ns("guardar_grafico"), "Guardar gráfico",
                         icon  = bs_icon("bookmark-check"),
                         class = "btn-primary btn-sm w-100"),
            uiOutput(ns("graficos_guardados_msg"))
          )
        )
      ),

      # ── PANEL DERECHO ─────────────────────────────────────────────────────
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
                               icon  = bs_icon("download"),
                               class = "btn-sm btn-outline-primary"),
                downloadButton(ns("dl_pdf"), "PDF",
                               icon  = bs_icon("download"),
                               class = "btn-sm btn-outline-primary"),
                downloadButton(ns("dl_svg"), "SVG",
                               icon  = bs_icon("download"),
                               class = "btn-sm btn-outline-primary")
              )
            )
          ),

          nav_panel(
            title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
            card_body(
              p(class = "text-muted small mb-2",
                "Código reproducible con sintaxis tidyplots."),
              verbatimTextOutput(ns("codigo_r")),
              downloadButton(ns("dl_script"), "Descargar .R",
                             icon  = bs_icon("download"),
                             class = "btn-sm btn-outline-primary mt-2")
            )
          )
        )
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────
mod_tidyplots_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Selectores de variables ───────────────────────────────────────────────
    output$var_selectores_ui <- renderUI({
      df <- data()
      req(df)
      nms_all <- c("— ninguna —" = "", names(df))
      tagList(
        selectizeInput(ns("var_x"), "Variable X",
                       choices = names(df), selected = names(df)[1]),
        selectizeInput(ns("var_y"), "Variable Y",
                       choices  = nms_all, selected = "",
                       options  = list(placeholder  = "— ninguna —",
                                       allowEmptyOption = TRUE,
                                       dropdownParent   = "body")),
        selectizeInput(ns("var_color"), "Color",
                       choices  = nms_all, selected = "",
                       options  = list(placeholder  = "— ninguna —",
                                       allowEmptyOption = TRUE,
                                       dropdownParent   = "body"))
      )
    })

    # ── Paleta de colores ─────────────────────────────────────────────────────
    paleta_fn <- reactive({
      pal     <- input$paleta %||% "friendly"
      inv     <- isTRUE(input$invertir_paleta)
      discretas   <- c("friendly", "seaside", "apple")
      divergentes <- c("blue2brown", "blue2red", "spectral")
      continuas   <- c("viridis", "plasma", "mako")

      if (pal %in% discretas) {
        fn <- switch(pal,
          "friendly" = tidyplots::colors_discrete_friendly,
          "seaside"  = tidyplots::colors_discrete_seaside,
          "apple"    = tidyplots::colors_discrete_apple
        )
        if (inv) rev(fn) else fn
      } else if (pal %in% continuas) {
        fn <- switch(pal,
          "viridis" = tidyplots::colors_continuous_viridis,
          "plasma"  = tidyplots::colors_continuous_plasma,
          "mako"    = tidyplots::colors_continuous_mako
        )
        if (inv) rev(fn) else fn
      } else {
        fn <- switch(pal,
          "blue2brown" = tidyplots::colors_diverging_blue2brown,
          "blue2red"   = tidyplots::colors_diverging_blue2red,
          "spectral"   = tidyplots::colors_diverging_spectral
        )
        if (inv) rev(fn) else fn
      }
    })

    # ── Construir gráfico ─────────────────────────────────────────────────────
    grafico_reactivo <- reactive({
      df    <- data()
      req(df, input$var_x, input$tipo)

      var_x     <- input$var_x
      var_y     <- if (nzchar(input$var_y     %||% "")) input$var_y     else NULL
      var_color <- if (nzchar(input$var_color %||% "")) input$var_color else NULL
      tipo      <- input$tipo
      capas     <- input$capas %||% character(0)
      alpha     <- input$alpha %||% 0.8
      lbl_tit   <- input$lbl_titulo  %||% ""
      lbl_cap   <- input$lbl_caption %||% ""
      lbl_x     <- if (nzchar(input$lbl_x %||% "")) input$lbl_x else NULL
      lbl_y     <- if (nzchar(input$lbl_y %||% "")) input$lbl_y else NULL

      # ── Validar que Y esté presente cuando es necesario ──
      tipos_requieren_y <- c("mean_bar", "mean_line", "mean_dot", "median_dot",
                             "median_line", "boxplot", "violin", "beeswarm",
                             "barstack", "areastack", "points", "points_beeswarm",
                             "heatmap")
      if (tipo != "none" && tipo %in% tipos_requieren_y && is.null(var_y))
        validate(need(FALSE,
          "Este tipo de gráfico requiere una Variable Y. Seleccioná una variable Y."))

      # ── Construir tidyplot base ──
      tp <- tryCatch({
        if (!is.null(var_y) && !is.null(var_color)) {
          tidyplots::tidyplot(df,
                              x     = !!rlang::sym(var_x),
                              y     = !!rlang::sym(var_y),
                              color = !!rlang::sym(var_color))
        } else if (!is.null(var_y)) {
          tidyplots::tidyplot(df,
                              x = !!rlang::sym(var_x),
                              y = !!rlang::sym(var_y))
        } else if (!is.null(var_color)) {
          tidyplots::tidyplot(df,
                              x     = !!rlang::sym(var_x),
                              color = !!rlang::sym(var_color))
        } else {
          tidyplots::tidyplot(df, x = !!rlang::sym(var_x))
        }
      }, error = function(e) NULL)

      validate(need(!is.null(tp), "No se pudo crear el gráfico con estas variables."))

      # ── Geometría base ──
      tp <- switch(tipo,
        "none"          = tp,
        "mean_bar"      = tp |> tidyplots::add_mean_bar(alpha = alpha),
        "mean_line"     = tp |> tidyplots::add_mean_line(alpha = alpha),
        "mean_dot"      = tp |> tidyplots::add_mean_dot(size = 3),
        "median_dot"    = tp |> tidyplots::add_median_dot(size = 3),
        "median_line"   = tp |> tidyplots::add_median_line(alpha = alpha),
        "boxplot"       = tp |> tidyplots::add_boxplot(alpha = alpha),
        "violin"        = tp |> tidyplots::add_violin(alpha = alpha),
        "beeswarm"      = tp |> tidyplots::add_data_points_beeswarm(alpha = alpha),
        "barstack"      = tp |> tidyplots::add_barstack_absolute(),
        "areastack"     = tp |> tidyplots::add_areastack_absolute(),
        "pie"           = tp |> tidyplots::add_pie(),
        "donut"         = tp |> tidyplots::add_donut(),
        "points"        = tp |> tidyplots::add_data_points(alpha = alpha),
        "points_beeswarm" = tp |> tidyplots::add_data_points_beeswarm(alpha = alpha),
        "heatmap"       = tp |> tidyplots::add_heatmap(),
        "histogram"     = tp |> tidyplots::add_histogram(),
        tp |> tidyplots::add_mean_bar(alpha = alpha)
      )

      # ── Capas adicionales ──
      if ("sem_errorbar"   %in% capas) tp <- tp |> tidyplots::add_sem_errorbar()
      if ("sd_errorbar"    %in% capas) tp <- tp |> tidyplots::add_sd_errorbar()
      if ("ci95_errorbar"  %in% capas) tp <- tp |> tidyplots::add_ci95_errorbar()
      if ("data_points"    %in% capas) tp <- tp |> tidyplots::add_data_points(alpha = 0.5)
      if ("data_beeswarm"  %in% capas) tp <- tp |> tidyplots::add_data_points_beeswarm(alpha = 0.5)
      if ("curvefit"       %in% capas) tp <- tp |> tidyplots::add_curvefit()
      if ("test_pvalue"    %in% capas) tp <- tp |> tidyplots::add_test_pvalue()
      if ("test_asterisks" %in% capas) tp <- tp |> tidyplots::add_test_asterisks()

      # ── Paleta ──
      tp <- tp |> tidyplots::adjust_colors(paleta_fn())

      # ── Tamaño y fuente ──
      tp <- tp |>
        tidyplots::adjust_size(width = NA, height = NA) |>
        tidyplots::adjust_font(fontsize = 12)

      # ── Etiquetas ──
      if (nzchar(lbl_tit)) tp <- tp |> tidyplots::add_title(lbl_tit)
      if (nzchar(lbl_cap)) tp <- tp |> tidyplots::add_caption(lbl_cap)
      if (!is.null(lbl_x)) tp <- tp |> tidyplots::adjust_x_axis_title(lbl_x)
      if (!is.null(lbl_y)) tp <- tp |> tidyplots::adjust_y_axis_title(lbl_y)

      tp
    })

    # ── Mensaje si no hay datos ───────────────────────────────────────────────
    output$grafico_msg <- renderUI({
      df <- data()
      if (is.null(df))
        div(class = "alert alert-info small",
            bs_icon("info-circle", class = "me-1"),
            "Cargá un dataset en la pestaña Datos para comenzar.")
      else NULL
    })

    # ── Render gráfico ────────────────────────────────────────────────────────
    output$grafico <- renderPlot({
      req(data(), input$var_x, input$tipo)
      grafico_reactivo()
    }, res = 96)

    # ── Descargas ─────────────────────────────────────────────────────────────
    descargar <- function(formato, ancho = 8, alto = 6) {
      downloadHandler(
        filename = function() paste0("tidyplot_", format(Sys.Date(), "%Y%m%d"), ".", formato),
        content  = function(file) {
          p <- grafico_reactivo()
          tidyplots::save_plot(p, filename = file,
                               width = ancho, height = alto)
        }
      )
    }

    output$dl_png <- descargar("png")
    output$dl_pdf <- descargar("pdf")
    output$dl_svg <- descargar("svg")

    # ── Código R reproducible ─────────────────────────────────────────────────
    codigo_reactivo <- reactive({
      req(input$var_x, input$tipo)

      var_x     <- input$var_x
      var_y     <- if (nzchar(input$var_y     %||% "")) input$var_y     else NULL
      var_color <- if (nzchar(input$var_color %||% "")) input$var_color else NULL
      tipo      <- input$tipo
      capas     <- input$capas %||% character(0)
      alpha     <- input$alpha %||% 0.8
      pal       <- input$paleta %||% "friendly"
      lbl_tit   <- input$lbl_titulo  %||% ""
      lbl_cap   <- input$lbl_caption %||% ""
      lbl_x     <- if (nzchar(input$lbl_x %||% "")) input$lbl_x else NULL
      lbl_y     <- if (nzchar(input$lbl_y %||% "")) input$lbl_y else NULL

      aes_str <- paste0("x = ", var_x,
                        if (!is.null(var_y))     paste0(", y = ", var_y)     else "",
                        if (!is.null(var_color)) paste0(", color = ", var_color) else "")

      geom_str <- switch(tipo,
        "mean_bar"        = paste0("add_mean_bar(alpha = ", alpha, ")"),
        "mean_line"       = paste0("add_mean_line(alpha = ", alpha, ")"),
        "mean_dot"        = "add_mean_dot(size = 3)",
        "median_dot"      = "add_median_dot(size = 3)",
        "median_line"     = paste0("add_median_line(alpha = ", alpha, ")"),
        "boxplot"         = paste0("add_boxplot(alpha = ", alpha, ")"),
        "violin"          = paste0("add_violin(alpha = ", alpha, ")"),
        "beeswarm"        = paste0("add_data_points_beeswarm(alpha = ", alpha, ")"),
        "barstack"        = "add_barstack_absolute()",
        "areastack"       = "add_areastack_absolute()",
        "pie"             = "add_pie()",
        "donut"           = "add_donut()",
        "points"          = paste0("add_data_points(alpha = ", alpha, ")"),
        "points_beeswarm" = paste0("add_data_points_beeswarm(alpha = ", alpha, ")"),
        "heatmap"         = "add_heatmap()",
        "histogram"       = "add_histogram()",
        paste0("add_mean_bar(alpha = ", alpha, ")")
      )

      capas_str <- paste(Filter(nzchar, sapply(capas, function(c) switch(c,
        "sem_errorbar"   = "|>\n  add_sem_errorbar()",
        "sd_errorbar"    = "|>\n  add_sd_errorbar()",
        "ci95_errorbar"  = "|>\n  add_ci95_errorbar()",
        "data_points"    = "|>\n  add_data_points(alpha = 0.5)",
        "data_beeswarm"  = "|>\n  add_data_points_beeswarm(alpha = 0.5)",
        "curvefit"       = "|>\n  add_curvefit()",
        "test_pvalue"    = "|>\n  add_test_pvalue()",
        "test_asterisks" = "|>\n  add_test_asterisks()",
        ""
      ))), collapse = "\n")

      pal_fn <- switch(pal,
        "friendly"   = "colors_discrete_friendly",
        "seaside"    = "colors_discrete_seaside",
        "apple"      = "colors_discrete_apple",
        "viridis"    = "colors_continuous_viridis",
        "plasma"     = "colors_continuous_plasma",
        "mako"       = "colors_continuous_mako",
        "blue2brown" = "colors_diverging_blue2brown",
        "blue2red"   = "colors_diverging_blue2red",
        "spectral"   = "colors_diverging_spectral",
        "colors_discrete_friendly"
      )

      etiquetas_str <- paste(Filter(nzchar, c(
        if (nzchar(lbl_tit)) paste0("|>\n  add_title('", lbl_tit, "')"),
        if (nzchar(lbl_cap)) paste0("|>\n  add_caption('", lbl_cap, "')"),
        if (!is.null(lbl_x)) paste0("|>\n  adjust_x_axis_title('", lbl_x, "')"),
        if (!is.null(lbl_y)) paste0("|>\n  adjust_y_axis_title('", lbl_y, "')")
      )), collapse = "\n")

      paste0(
        encabezado_script("Tidyplot"),
        "library(tidyplots)\n\n",
        "# Cargá tus datos\n",
        "datos <- read.csv('tu_archivo.csv')\n\n",
        "# Gráfico\n",
        "datos |>\n",
        "  tidyplot(", aes_str, ") |>\n",
        "  ", geom_str,
        if (nzchar(capas_str)) paste0(" ", capas_str) else "",
        " |>\n",
        "  adjust_colors(", pal_fn, ")",
        if (nzchar(etiquetas_str)) paste0(" ", etiquetas_str) else "",
        "\n"
      )
    })

    output$codigo_r <- renderText({ codigo_reactivo() })

    output$dl_script <- downloadHandler(
      filename = function() paste0("tidyplot_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(codigo_reactivo(), file)
    )

    # ── Lista de gráficos guardados para mod_patchwork ────────────────────────
    graficos_guardados <- reactiveVal(list())

    observeEvent(input$guardar_grafico, {
      req(grafico_reactivo())
      nombre <- if (nzchar(input$nombre_grafico %||% ""))
        input$nombre_grafico
      else
        paste0("Tidyplot ", length(graficos_guardados()) + 1)

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
