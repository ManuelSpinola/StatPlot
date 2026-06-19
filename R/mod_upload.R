# =============================================================================
# mod_upload.R — Carga y exploración de datos para StatPlot
# StatPlot · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Exporta:
#   mod_upload_ui(id)
#   mod_upload_server(id) → reactive data.frame
# =============================================================================

# ── Helpers internos ──────────────────────────────────────────────────────────

# Clasificar tipo de variable
clasificar_variable <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("Temporal")
  if (is.factor(x) || is.character(x) || is.logical(x)) return("Categórica")
  if (is.numeric(x)) {
    if (length(unique(x[!is.na(x)])) <= 10 && all(x == as.integer(x), na.rm = TRUE))
      return("Numérica discreta")
    return("Numérica continua")
  }
  return("Otra")
}

# Color por tipo
color_tipo <- function(tipo) {
  switch(tipo,
    "Numérica continua"  = colores$primario,
    "Numérica discreta"  = colores$secundario,
    "Categórica"         = colores$acento,
    "Temporal"           = "#5FA2CE",
    colores$texto
  )
}

# Leer archivo CSV o Excel
leer_archivo <- function(path, ext) {
  tryCatch({
    if (ext == "csv") {
      read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      as.data.frame(readxl::read_excel(path))
    }
  }, error = function(e) NULL)
}

# ── UI ────────────────────────────────────────────────────────────────────────
mod_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "px-1 pt-2 pb-2",
      h4(style = paste0("color:", colores$primario, "; font-weight:700; margin-bottom:4px;"),
         bs_icon("database", class = "me-2"),
         "Datos"),
      p(class = "text-muted small mb-0",
        "Cargá un dataset de ejemplo o subí tus propios datos. ",
        "Explorá las variables para decidir qué gráfico crear.")
    ),

    navset_card_tab(

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 1: Datos
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("folder2-open", class = "me-1"), "Cargar datos"),
        card_body(

          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,

            # ── Panel izquierdo: selector ──────────────────────────────
            div(
              p(class = "text-muted small mb-3",
                "Elegí un dataset de ejemplo o subí tu propio archivo CSV o Excel."),

              radioButtons(
                ns("fuente"),
                label = "Fuente de datos",
                choices = c(
                  "Pingüinos de Palmer"        = "penguins",
                  "Gapminder"                  = "gapminder",
                  "Contaminación río Meuse"    = "meuse",
                  "Peso al nacer"              = "birthwt",
                  "Ácaros oribátidos (mites)"  = "mites",
                  "Subir mi archivo"           = "subir"
                ),
                selected = "penguins"
              ),

              conditionalPanel(
                condition = sprintf("input['%s'] == 'subir'", ns("fuente")),
                fileInput(
                  ns("archivo"),
                  label       = NULL,
                  accept      = c(".csv", ".xlsx", ".xls"),
                  placeholder = "Seleccionar archivo...",
                  buttonLabel = "Buscar"
                )
              ),

              uiOutput(ns("info_dataset"))
            ),

            # ── Panel derecho: vista previa ────────────────────────────
            div(
              uiOutput(ns("badges_variables")),
              tags$hr(),
              DT::DTOutput(ns("tabla_preview"))
            )
          )
        )
      ), # /PESTAÑA 1

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 2: Explorar variables
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("search", class = "me-1"), "Explorar variables"),
        card_body(

          layout_columns(
            col_widths = c(3, 9),
            fill = FALSE,

            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Variable"),
                card_body(
                  uiOutput(ns("sel_variable")),
                  uiOutput(ns("badge_tipo")),
                  tags$hr(),
                  bslib::accordion(
                    open = FALSE,
                    bslib::accordion_panel(
                      title = tagList(bs_icon("book", class = "me-1"),
                                      "Tipos de variables"),
                      tags$ul(class = "small mb-0",
                        tags$li(
                          tags$span(class = "badge me-1",
                                    style = paste0("background:", colores$primario),
                                    "Numérica continua"),
                          " — valores en un rango continuo. Ej: peso, temperatura"
                        ),
                        tags$li(
                          tags$span(class = "badge me-1",
                                    style = paste0("background:", colores$secundario),
                                    "Numérica discreta"),
                          " — enteros contables. Ej: número de hijos, conteos"
                        ),
                        tags$li(
                          tags$span(class = "badge me-1",
                                    style = paste0("background:", colores$acento),
                                    "Categórica"),
                          " — grupos o etiquetas. Ej: especie, sexo, país"
                        ),
                        tags$li(
                          tags$span(class = "badge me-1",
                                    style = "background:#5FA2CE",
                                    "Temporal"),
                          " — fechas o tiempos. Ej: año, fecha de muestreo"
                        )
                      )
                    )
                  )
                )
              )
            ),

            div(
              uiOutput(ns("resumen_variable")),
              tags$hr(),
              uiOutput(ns("sugerencia_graficos"))
            )
          )
        )
      ) # /PESTAÑA 2

    ) # /navset_card_tab
  )
}


# ── Server ────────────────────────────────────────────────────────────────────
mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Ruta a datos de ejemplo ──────────────────────────────────────────────
    data_path <- app_sys("app/data")

    # ── Datos activos ────────────────────────────────────────────────────────
    datos <- reactive({
      if (input$fuente == "subir") {
        req(input$archivo)
        ext <- tolower(tools::file_ext(input$archivo$name))
        df  <- leer_archivo(input$archivo$datapath, ext)
        validate(need(!is.null(df),
                      "No se pudo leer el archivo. Verificá que sea CSV o Excel."))
        df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
      } else {
        path <- file.path(data_path, paste0(input$fuente, ".rds"))
        obj  <- readRDS(path)
        obj$data
      }
    })

    # ── Info del dataset ─────────────────────────────────────────────────────
    output$info_dataset <- renderUI({
      if (input$fuente == "subir") return(NULL)
      path <- file.path(data_path, paste0(input$fuente, ".rds"))
      obj  <- readRDS(path)
      m    <- obj$meta
      div(
        class = "mt-3 p-3 rounded",
        style = paste0("background:", colores$fondo,
                       "; border-left: 4px solid ", colores$primario, ";"),
        tags$p(class = "fw-bold mb-1",
               style = paste0("color:", colores$primario), m$name),
        tags$p(class = "small text-muted mb-2", m$description),
        tags$p(class = "small mb-0",
               tags$b("Filas: "), m$n_rows, " · ",
               tags$b("Columnas: "), m$n_cols, " · ",
               tags$b("Fuente: "), m$source),
        if (!is.null(m$referencia))
          tags$p(class = "small text-muted fst-italic mb-0", m$referencia)
      )
    })

    # ── Badges de variables ──────────────────────────────────────────────────
    output$badges_variables <- renderUI({
      df    <- datos()
      req(df)
      tipos <- sapply(df, clasificar_variable)
      div(
        class = "d-flex flex-wrap gap-2 mb-3",
        lapply(seq_along(tipos), function(i) {
          tags$span(
            class = "badge",
            style = paste0("background:", color_tipo(tipos[i]),
                           "; font-size: 0.78rem;"),
            paste0(names(tipos)[i], " (", tipos[i], ")")
          )
        })
      )
    })

    # ── Tabla preview ────────────────────────────────────────────────────────
    output$tabla_preview <- DT::renderDT({
      DT::datatable(
        datos(),
        options = list(
          pageLength = 8,
          scrollX    = TRUE,
          language   = list(
            search     = "Buscar:",
            lengthMenu = "Mostrar _MENU_ filas",
            info       = "Mostrando _START_ a _END_ de _TOTAL_ registros",
            paginate   = list(previous = "Anterior", `next` = "Siguiente")
          )
        ),
        rownames = FALSE,
        class    = "table table-sm table-hover"
      )
    })

    # ── Selector de variable ─────────────────────────────────────────────────
    output$sel_variable <- renderUI({
      df <- datos()
      req(df)
      selectInput(
        ns("variable"),
        label    = "Variable a explorar",
        choices  = names(df),
        selected = names(df)[1]
      )
    })

    # ── Badge tipo de variable ───────────────────────────────────────────────
    tipo_actual <- reactive({
      df <- datos()
      req(df, input$variable)
      clasificar_variable(df[[input$variable]])
    })

    output$badge_tipo <- renderUI({
      tipo <- tipo_actual()
      div(
        class = "mt-2",
        tags$span(
          class = "badge fs-6",
          style = paste0("background:", color_tipo(tipo)),
          paste("Tipo:", tipo)
        )
      )
    })

    # ── Resumen de variable ──────────────────────────────────────────────────
    output$resumen_variable <- renderUI({
      df   <- datos()
      req(df, input$variable)
      tipo <- tipo_actual()
      x    <- df[[input$variable]]

      if (grepl("Numérica", tipo)) {
        tagList(
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            card(
              card_header(bs_icon("bullseye", class = "me-1"),
                          "Tendencia central"),
              card_body(
                renderTable({
                  data.frame(
                    Estadístico = c("Media", "Mediana"),
                    Valor = c(
                      round(mean(x, na.rm = TRUE), 3),
                      round(median(x, na.rm = TRUE), 3)
                    )
                  )
                }, striped = TRUE, hover = TRUE, bordered = TRUE)
              )
            ),
            card(
              card_header(bs_icon("arrows-expand", class = "me-1"),
                          "Dispersión"),
              card_body(
                renderTable({
                  data.frame(
                    Estadístico = c("Desv. estándar", "Mín.", "Máx.", "NAs"),
                    Valor = c(
                      round(sd(x, na.rm = TRUE), 3),
                      round(min(x, na.rm = TRUE), 3),
                      round(max(x, na.rm = TRUE), 3),
                      sum(is.na(x))
                    )
                  )
                }, striped = TRUE, hover = TRUE, bordered = TRUE)
              )
            )
          )
        )
      } else if (tipo == "Categórica") {
        card(
          card_header(bs_icon("bar-chart", class = "me-1"),
                      "Frecuencias"),
          card_body(
            renderTable({
              as.data.frame(table(x)) |>
                dplyr::rename(Categoría = x, Frecuencia = Freq) |>
                dplyr::mutate(
                  Porcentaje = paste0(round(Frecuencia / sum(Frecuencia) * 100, 1), "%")
                ) |>
                dplyr::arrange(dplyr::desc(Frecuencia))
            }, striped = TRUE, hover = TRUE, bordered = TRUE)
          )
        )
      } else if (tipo == "Temporal") {
        card(
          card_header(bs_icon("calendar", class = "me-1"),
                      "Rango temporal"),
          card_body(
            renderTable({
              data.frame(
                Estadístico = c("Fecha mínima", "Fecha máxima", "NAs"),
                Valor = c(
                  as.character(min(x, na.rm = TRUE)),
                  as.character(max(x, na.rm = TRUE)),
                  as.character(sum(is.na(x)))
                )
              )
            }, striped = TRUE, hover = TRUE, bordered = TRUE)
          )
        )
      }
    })

    # ── Sugerencia de gráficos ───────────────────────────────────────────────
    output$sugerencia_graficos <- renderUI({
      df   <- datos()
      req(df, input$variable)
      tipo <- tipo_actual()

      sugerencias <- switch(tipo,
        "Numérica continua" = list(
          list(icono = "bar-chart",    nombre = "Histograma",
               desc  = "Distribución de frecuencias"),
          list(icono = "activity",     nombre = "Densidad",
               desc  = "Estimación de la densidad"),
          list(icono = "box-seam",     nombre = "Boxplot",
               desc  = "Distribución y outliers"),
          list(icono = "music-note-beamed", nombre = "Violín",
               desc  = "Distribución por grupos")
        ),
        "Numérica discreta" = list(
          list(icono = "bar-chart",    nombre = "Barras",
               desc  = "Frecuencia de cada valor"),
          list(icono = "box-seam",     nombre = "Boxplot",
               desc  = "Distribución y outliers")
        ),
        "Categórica" = list(
          list(icono = "bar-chart-fill", nombre = "Barras",
               desc  = "Frecuencia de categorías"),
          list(icono = "pie-chart",      nombre = "Torta",
               desc  = "Proporción de categorías"),
          list(icono = "grid-1x2",       nombre = "Treemap",
               desc  = "Proporciones jerárquicas")
        ),
        "Temporal" = list(
          list(icono = "graph-up",     nombre = "Líneas",
               desc  = "Tendencia a lo largo del tiempo"),
          list(icono = "bar-chart",    nombre = "Barras temporales",
               desc  = "Valores por período")
        )
      )

      if (is.null(sugerencias)) return(NULL)

      card(
        card_header(bs_icon("lightbulb", class = "me-1"),
                    paste("Gráficos sugeridos para", tipo)),
        card_body(
          div(
            class = "d-flex flex-wrap gap-2",
            lapply(sugerencias, function(s) {
              div(
                class = "p-2 rounded text-center",
                style = paste0("background:", colores$fondo,
                               "; border: 1px solid ", colores$borde,
                               "; min-width: 110px;"),
                bs_icon(s$icono, class = "d-block mx-auto mb-1",
                        style = paste0("color:", colores$primario,
                                       "; font-size: 1.4rem;")),
                tags$p(class = "small fw-bold mb-0", s$nombre),
                tags$p(class = "small text-muted mb-0", s$desc)
              )
            })
          )
        )
      )
    })

    # ── Retornar datos ───────────────────────────────────────────────────────
    return(datos)

  })
}
