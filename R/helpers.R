# ============================================================
# helpers.R — Funciones y objetos compartidos entre módulos
# StatPlot — Visualización de datos con ggplot2
# Paleta: Tableau Color Blind (coherente con StatSuite)
# ============================================================

# ── Alias para no repetir prefijos en todos los módulos ───
bs_icon        <- bsicons::bs_icon
layout_columns <- bslib::layout_columns
card           <- bslib::card
card_header    <- bslib::card_header
card_body      <- bslib::card_body
nav_panel      <- bslib::nav_panel
navset_pill    <- bslib::navset_pill
navset_card_tab <- bslib::navset_card_tab

# ── Paleta de colores (idéntica a StatSuite) ───────────────
colores <- list(
  fondo       = "#F4F7FB",
  primario    = "#1170AA",
  acento      = "#FC7D0B",
  secundario  = "#5FA2CE",
  texto       = "#57606C",
  exito       = "#5FA2CE",
  advertencia = "#F1CE63",
  peligro     = "#C85200",
  borde       = "#C8D9EC",

  tableau = c(
    "#1170AA", "#FC7D0B", "#A3ACB9", "#57606C",
    "#C85200", "#7BC8ED", "#5FA2CE", "#F1CE63",
    "#9F8B75", "#B85A0D"
  )
)

# ── Tema visual (idéntico a StatSuite) ─────────────────────
tema_app <- bslib::bs_theme(
  version      = 5,
  bg           = colores$fondo,
  fg           = colores$texto,
  primary      = colores$primario,
  secondary    = colores$secundario,
  success      = colores$exito,
  danger       = colores$peligro,
  warning      = colores$advertencia,
  base_font    = bslib::font_google("Nunito"),
  heading_font = bslib::font_google("Nunito", wght = 700),
  bootswatch   = NULL
) |>
  bslib::bs_add_rules("
  .navbar { background-color: #1170AA !important; }
  .navbar-brand { color: #ffffff !important; display: flex !important;
                  align-items: center !important;
                  padding-top: 0 !important; padding-bottom: 0 !important; }
  .navbar .nav-link { color: #ffffff !important; }
  .navbar .nav-link.active { border-bottom: 2px solid #FC7D0B; }
  .nav-tabs .nav-link.active,
  .nav-tabs .nav-item .nav-link.active,
  ul.nav.nav-tabs li.nav-item a.nav-link.active {
    background-color: #1170AA !important;
    color: #ffffff !important;
    border-top-color: #1170AA !important;
    border-left-color: #1170AA !important;
    border-right-color: #1170AA !important;
    border-bottom-color: transparent !important;
    font-weight: 600 !important;
  }
  .nav-tabs .nav-link:not(.active):hover {
    background-color: #EEF3FA !important;
    color: #1170AA !important;
  }
  .btn-primary { background-color: #FC7D0B; border-color: #FC7D0B; color: #ffffff; }
  .btn-primary:hover { background-color: #d4680a; border-color: #d4680a; }
  .card > .card-header { background-color: #C8D9EC; color: #1170AA; font-weight: 700;
                         border-bottom: none; }
  .card > .card-header:has(.nav-tabs) { background-color: transparent; color: inherit;
                                        border-bottom: revert; }
  /* Código R */
  .codigo-bloque { background: #1e1e2e; color: #cdd6f4;
                   border-radius: 8px; padding: 1rem;
                   font-family: 'Fira Code', monospace;
                   font-size: 0.82rem; line-height: 1.7;
                   overflow-x: auto; white-space: pre; }
")

# ── Escalas ggplot2 (Tableau Color Blind) ─────────────────
scale_fill_tableau_cb <- function(...) {
  ggplot2::scale_fill_manual(values = colores$tableau, ...)
}
scale_color_tableau_cb <- function(...) {
  ggplot2::scale_color_manual(values = colores$tableau, ...)
}

# ── Encabezado estándar de scripts R ──────────────────────
encabezado_script <- function(modulo) {
  paste0(
    "# ============================================\n",
    "# StatPlot \u00b7 StatSuite\n",
    "# M\u00f3dulo: ", modulo, "\n",
    "# Generado: ", format(Sys.Date(), "%Y-%m-%d"), "\n",
    "# Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 UNA \u00b7 Costa Rica\n",
    "# ============================================\n\n"
  )
}

# ── Utilidad interna ───────────────────────────────────────
`%||%` <- function(x, y) if (is.null(x)) y else x
