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
    navset_card_tab(

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 1: Datos de ejemplo
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("database", class = "me-1"), "Datos de ejemplo"),
        fillable = FALSE,
        card_body(

          div(
            class = "px-1 pb-2",
            h4(style = paste0("color:", colores$primario, "; font-weight:700; margin-bottom:4px;"),
               bs_icon("database", class = "me-2"),
               "Datos"),
            p(class = "text-muted small mb-0",
              "Cargá un dataset de ejemplo o subí tus propios datos. ",
              "Explorá las variables para decidir qué gráfico crear.")
          ),

          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,

            # ── Panel izquierdo: selector ──────────────────────────────
            div(
              radioButtons(
                ns("fuente"),
                label = NULL,
                choices = c(
                  "Pingüinos de Palmer"        = "penguins",
                  "Gapminder"                  = "gapminder",
                  "Contaminación río Meuse"    = "meuse",
                  "Peso al nacer"              = "birthwt",
                  "Ácaros oribátidos (mites)"  = "mites",
                  "Animales"                   = "animals",
                  "Clima"                      = "climate",
                  "Dinosaurios"                = "dinosaurs",
                  "Energía"                    = "energy",
                  "Gastos"                     = "spendings"
                ),
                selected = "penguins"
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
      # PESTAÑA 2: Mis datos
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("upload", class = "me-1"), "Mis datos"),
        fillable = FALSE,
        card_body(

          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,

            # ── Panel izquierdo: subir archivo ──────────────────────────
            div(
              p(class = "text-muted small mb-2",
                "Podés cargar archivos en formato ",
                strong("CSV"), " o ", strong("Excel (.xlsx, .xls)"), "."),
              fileInput(
                ns("archivo"),
                label       = NULL,
                accept      = c(".csv", ".xlsx", ".xls"),
                placeholder = "Seleccionar archivo...",
                buttonLabel = "Buscar"
              ),
              uiOutput(ns("resumen_datos_propio"))
            ),

            # ── Panel derecho: vista previa de mis datos ────────────────
            div(
              uiOutput(ns("cards_datos_propio")),
              tags$hr(),
              DT::DTOutput(ns("tabla_preview_propio"))
            )
          )
        )
      ), # /PESTAÑA 2

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 3: Variables
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("table", class = "me-1"), "Variables"),
        fillable = FALSE,
        card_body(

          p(class = "text-muted small mb-3",
            bs_icon("info-circle", class = "me-1"),
            "Revisá el tipo detectado para cada variable y corregilo si es necesario. ",
            "Variables mal tipificadas pueden causar errores al graficar. ",
            "Podés también ", strong("excluir"), " variables que no necesitás."),

          uiOutput(ns("tabla_tipos")),
          uiOutput(ns("tipos_aplicados_msg")),

          tags$hr(),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,
            radioButtons(
              ns("manejo_na"),
              label    = tagList(bs_icon("exclamation-diamond", class = "me-1"),
                                 "Valores perdidos (NA)"),
              choices  = c(
                "Conservar"              = "conservar",
                "Eliminar filas con NA"  = "eliminar"
              ),
              selected = "conservar"
            ),
            uiOutput(ns("na_info"))
          ),

          tags$hr(),

          layout_columns(
            col_widths = c(3, 9),
            fill = FALSE,

            div(
              card(
                fill = FALSE,
                card_header(bs_icon("book", class = "me-1"),
                            "Tipos de variables"),
                card_body(
                  tags$ul(class = "small mb-0",
                    tags$li(
                      tags$span(class = "badge me-1",
                                style = paste0("background:", colores$primario),
                                "Numérica"),
                      " — valores continuos o discretos. Ej: peso, temperatura, conteos"
                    ),
                    tags$li(
                      tags$span(class = "badge me-1",
                                style = paste0("background:", colores$acento),
                                "Factor"),
                      " — grupos o etiquetas. Ej: especie, sexo, país, año categórico"
                    ),
                    tags$li(
                      tags$span(class = "badge me-1",
                                style = "background:#5FA2CE",
                                "Fecha"),
                      " — fechas o tiempos. Ej: fecha de muestreo"
                    ),
                    tags$li(
                      tags$span(class = "badge me-1",
                                style = paste0("background:", colores$texto),
                                "Excluir"),
                      " — variable no se usará en los gráficos"
                    )
                  )
                )
              )
            ),

            div(
              uiOutput(ns("sel_variable")),
              uiOutput(ns("resumen_variable")),
              tags$hr(),
              uiOutput(ns("sugerencia_graficos"))
            )
          )
        )
      ) # /PESTAÑA 3

    ) # /navset_card_tab
  )
}


# ── Server ────────────────────────────────────────────────────────────────────
mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Ruta a datos de ejemplo ──────────────────────────────────────────────
    data_path <- app_sys("app/data")

    # ── Objeto .rds del dataset activo (evita doble lectura) ────────────────
    obj_dataset <- reactive({
      req(nzchar(input$fuente %||% ""))
      path <- file.path(data_path, paste0(input$fuente, ".rds"))
      validate(need(file.exists(path),
                    paste0("No se encontró el archivo: ", basename(path))))
      readRDS(path)
    })

    # ── Datos de ejemplo (solo la fuente seleccionada) ───────────────────────
    datos_ejemplo <- reactive({
      req(nzchar(input$fuente %||% ""))
      obj_dataset()$data |>
        dplyr::mutate(dplyr::across(where(is.character), as.factor))
    })

    # ── Datos propios (solo el archivo subido) ───────────────────────────────
    datos_propio <- reactive({
      req(input$archivo)
      ext <- tolower(tools::file_ext(input$archivo$name))
      df  <- leer_archivo(input$archivo$datapath, ext)
      validate(need(!is.null(df),
                    "No se pudo leer el archivo. Verificá que sea CSV o Excel."))
      df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
    })

    # ── Datos activos (unificados: prioriza archivo propio) ──────────────────
    datos <- reactive({
      req(nzchar(input$fuente %||% "") || !is.null(input$archivo))
      if (!is.null(input$archivo)) {
        datos_propio()
      } else {
        datos_ejemplo()
      }
    })

    # ── Vista previa de datos propios ────────────────────────────────────────
    output$resumen_datos_propio <- renderUI({
      req(datos_propio())
      d <- datos_propio()
      div(class = "small text-muted mt-2",
          bs_icon("check-circle-fill",
                  style = paste0("color:", colores$exito), class = "me-1"),
          paste0(nrow(d), " filas \u00b7 ", ncol(d), " columnas"))
    })

    output$cards_datos_propio <- renderUI({
      req(datos_propio())
      d    <- datos_propio()
      tipos <- sapply(d, clasificar_variable)
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

    output$tabla_preview_propio <- DT::renderDT({
      req(datos_propio())
      DT::datatable(
        datos_propio(),
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

    # ── Info del dataset ─────────────────────────────────────────────────────
    output$info_dataset <- renderUI({
      req(nzchar(input$fuente %||% ""))
      if (!is.null(input$archivo)) return(NULL)
      m <- obj_dataset()$meta
      div(
        class = "mt-3 p-3 rounded",
        style = paste0("background:", colores$fondo,
                       "; border-left: 4px solid ", colores$primario, ";"),
        tags$p(class = "fw-bold mb-1",
               style = paste0("color:", colores$primario), m$titulo),
        tags$p(class = "small text-muted mb-1", m$desc),
        tags$p(class = "small mb-1",
               tags$b("Fuente: "), m$fuente),
        tags$p(class = "small mb-0",
               tags$b("Variables: "), m$vars)
      )
    })

    # ── Badges de variables ──────────────────────────────────────────────────
    output$badges_variables <- renderUI({
      df    <- datos_final()
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
        datos_final(),
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
      df <- datos_conv()
      req(df)
      selectInput(
        ns("variable"),
        label    = "Variable a explorar",
        choices  = names(df),
        selected = names(df)[1]
      )
    })

    # ── Tipos definidos por el usuario (StatModels pattern) ─────────────────
    tipos_usuario <- reactiveVal(NULL)

    # Reset limpio al cambiar dataset
    observeEvent(input$fuente, {
      tipos_usuario(NULL)
    })

    # Observar cambios en los selectores de tipo por variable
    observe({
      df <- datos()
      req(df)
      tu <- lapply(names(df), function(nm) {
        val <- input[[paste0("tipo_", nm)]]
        if (!is.null(val)) val else NULL
      })
      names(tu) <- names(df)
      tu <- tu[!sapply(tu, is.null)]
      if (length(tu) > 0) tipos_usuario(tu)
    })

    # ── Tabla de tipos (patrón StatModels) ──────────────────────────────────
    output$tabla_tipos <- renderUI({
      df <- datos()
      req(df)
      tu <- tipos_usuario()

      filas <- lapply(names(df), function(nm) {
        col    <- df[[nm]]
        actual <- if (is.factor(col) || is.character(col)) "factor"
                  else if (inherits(col, c("Date","POSIXct","POSIXlt"))) "fecha"
                  else "numeric"
        icono  <- if (actual == "factor")
          bs_icon("tag-fill", style = paste0("color:", colores$acento))
        else if (actual == "fecha")
          bs_icon("calendar", style = "color:#5FA2CE")
        else
          bs_icon("123", style = paste0("color:", colores$primario))

        sel <- if (!is.null(tu) && !is.null(tu[[nm]])) tu[[nm]] else actual

        tags$tr(
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  div(class = "d-flex align-items-center gap-2", icono, strong(nm))),
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  tags$span(class = "badge",
                            style = paste0("background:",
                              if (actual == "factor") colores$acento
                              else if (actual == "fecha") "#5FA2CE"
                              else colores$primario,
                              "; font-size:0.75rem;"),
                            if (actual == "factor") "Factor"
                            else if (actual == "fecha") "Fecha"
                            else "Numérico")),
          tags$td(style = "padding:5px 8px;",
                  selectInput(
                    inputId  = ns(paste0("tipo_", nm)),
                    label    = NULL,
                    choices  = c("Numérico" = "numeric",
                                 "Factor (categórico)" = "factor",
                                 "Fecha" = "fecha",
                                 "Excluir" = "excluir"),
                    selected = sel, width = "190px")),
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  if (!is.null(tu) && !is.null(tu[[nm]]) && tu[[nm]] != actual)
                    tags$span(class = "badge",
                              style = paste0("background:", colores$exito),
                              "Modificado")
                  else
                    tags$span(class = "text-muted small", "Sin cambios"))
        )
      })

      tagList(
        tags$table(
          class = "table table-sm table-hover small mb-0",
          tags$thead(
            style = paste0("background:", colores$primario,
                           " !important; color:#fff !important;"),
            tags$tr(
              tags$th(style = "padding:7px 8px;", "Variable"),
              tags$th(style = "padding:7px 8px;", "Tipo detectado"),
              tags$th(style = "padding:7px 8px;", "Tipo a usar"),
              tags$th(style = "padding:7px 8px;", "Estado")
            )
          ),
          tags$tbody(filas)
        )
      )
    })

    output$tipos_aplicados_msg <- renderUI({
      tu <- tipos_usuario()
      if (is.null(tu)) return(NULL)
      df <- datos()
      req(df)
      n_cambios <- sum(sapply(names(tu), function(nm) {
        if (!nm %in% names(df)) return(FALSE)
        col    <- df[[nm]]
        actual <- if (is.factor(col) || is.character(col)) "factor"
                  else if (inherits(col, c("Date","POSIXct","POSIXlt"))) "fecha"
                  else "numeric"
        !is.null(tu[[nm]]) && tu[[nm]] != actual && tu[[nm]] != "excluir"
      }))
      n_excl <- sum(sapply(tu, function(t) !is.null(t) && t == "excluir"))
      if (n_cambios == 0 && n_excl == 0) return(NULL)
      div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
          bs_icon("check-circle", class = "me-1",
                  style = paste0("color:", colores$exito)),
          if (n_cambios > 0) paste0(n_cambios, " variable(s) convertida(s). "),
          if (n_excl > 0) paste0(n_excl, " variable(s) excluida(s). "),
          "Los gráficos usarán estos tipos.")
    })

    # ── Datos con tipos aplicados ────────────────────────────────────────────
    datos_conv <- reactive({
      df <- datos()
      tu <- tipos_usuario()
      req(df)
      for (nm in names(df)) {
        tipo_dest <- if (!is.null(tu) && !is.null(tu[[nm]])) tu[[nm]] else NULL
        if (is.null(tipo_dest) || tipo_dest == "excluir") next
        df[[nm]] <- switch(tipo_dest,
          "factor"  = as.factor(df[[nm]]),
          "numeric" = suppressWarnings(as.numeric(as.character(df[[nm]]))),
          "fecha"   = suppressWarnings(as.Date(as.character(df[[nm]]))),
          df[[nm]]
        )
      }
      # Excluir variables marcadas
      if (!is.null(tu)) {
        excluir <- names(tu)[sapply(tu, function(t) !is.null(t) && t == "excluir")]
        df <- df[, !names(df) %in% excluir, drop = FALSE]
      }
      df
    })

    # ── Tipo actual de variable seleccionada ─────────────────────────────────
    tipo_actual <- reactive({
      df <- datos_conv()
      req(df, input$variable)
      if (!input$variable %in% names(df)) return("Excluida")
      clasificar_variable(df[[input$variable]])
    })

    # ── Resumen de variable — estructura (renderUI solo decide qué mostrar) ──
    output$resumen_variable <- renderUI({
      df   <- datos_conv()
      req(df, input$variable)
      tipo <- tipo_actual()

      if (grepl("Numérica", tipo)) {
        tagList(
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            card(
              fill = FALSE,
              card_header(bs_icon("bullseye", class = "me-1"),
                          "Tendencia central"),
              card_body(tableOutput(ns("tbl_tendencia")))
            ),
            card(
              fill = FALSE,
              card_header(bs_icon("arrows-expand", class = "me-1"),
                          "Dispersión"),
              card_body(tableOutput(ns("tbl_dispersion")))
            )
          )
        )
      } else if (tipo == "Categórica") {
        card(
          fill = FALSE,
          card_header(bs_icon("bar-chart", class = "me-1"), "Frecuencias"),
          card_body(tableOutput(ns("tbl_frecuencias")))
        )
      } else if (tipo == "Temporal") {
        card(
          fill = FALSE,
          card_header(bs_icon("calendar", class = "me-1"), "Rango temporal"),
          card_body(tableOutput(ns("tbl_temporal")))
        )
      }
    })

    # ── Tablas de resumen — renders independientes ───────────────────────────
    output$tbl_tendencia <- renderTable({
      df <- datos_conv()
      req(df, input$variable)
      x <- df[[input$variable]]
      data.frame(
        Estadístico = c("Media", "Mediana"),
        Valor       = c(round(mean(x, na.rm = TRUE), 3),
                        round(median(x, na.rm = TRUE), 3))
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$tbl_dispersion <- renderTable({
      df <- datos_conv()
      req(df, input$variable)
      x <- df[[input$variable]]
      data.frame(
        Estadístico = c("Desv. estándar", "Mín.", "Máx.", "NAs"),
        Valor       = c(round(sd(x,  na.rm = TRUE), 3),
                        round(min(x, na.rm = TRUE), 3),
                        round(max(x, na.rm = TRUE), 3),
                        sum(is.na(x)))
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$tbl_frecuencias <- renderTable({
      df <- datos_conv()
      req(df, input$variable)
      x <- df[[input$variable]]
      as.data.frame(table(x)) |>
        dplyr::rename(Categoría = x, Frecuencia = Freq) |>
        dplyr::mutate(
          Porcentaje = paste0(round(Frecuencia / sum(Frecuencia) * 100, 1), "%")
        ) |>
        dplyr::arrange(dplyr::desc(Frecuencia))
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$tbl_temporal <- renderTable({
      df <- datos_conv()
      req(df, input$variable)
      x <- df[[input$variable]]
      data.frame(
        Estadístico = c("Fecha mínima", "Fecha máxima", "NAs"),
        Valor       = c(as.character(min(x, na.rm = TRUE)),
                        as.character(max(x, na.rm = TRUE)),
                        as.character(sum(is.na(x))))
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ── Sugerencia de gráficos ───────────────────────────────────────────────
    output$sugerencia_graficos <- renderUI({
      df   <- datos_conv()
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
        fill = FALSE,
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

    # ── Manejo de NAs ────────────────────────────────────────────────────────
    datos_final <- reactive({
      df <- datos_conv()
      req(df)
      if (isTRUE(input$manejo_na == "eliminar")) {
        df <- tidyr::drop_na(df)
      }
      df
    })

    output$na_info <- renderUI({
      df_orig  <- datos_conv()
      df_final <- datos_final()
      req(df_orig)
      n_na   <- sum(!stats::complete.cases(df_orig))
      if (n_na == 0) return(
        div(class = "alert alert-success small py-1 px-2 mt-2 mb-0",
            bs_icon("check-circle", class = "me-1"), "Sin valores perdidos.")
      )
      n_elim <- nrow(df_orig) - nrow(df_final)
      if (input$manejo_na == "eliminar")
        div(class = "alert alert-warning small py-1 px-2 mt-2 mb-0",
            bs_icon("exclamation-triangle", class = "me-1"),
            paste0(n_elim, " fila(s) eliminadas. Quedan ", nrow(df_final), " filas."))
      else
        div(class = "alert alert-info small py-1 px-2 mt-2 mb-0",
            bs_icon("info-circle", class = "me-1"),
            paste0(n_na, " fila(s) con NA. Podés eliminarlas a la izquierda."))
    })

    # ── Retornar datos ───────────────────────────────────────────────────────
    return(datos_final)

  })
}
