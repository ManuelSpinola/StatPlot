# ============================================================
# golem_utils.R — Utilidades internas de golem para StatPlot
# ============================================================

#' @noRd
app_sys <- function(...) {
  system.file(..., package = "StatPlot")
}

#' @noRd
app_prod <- function() {
  isTRUE(get_golem_config("production"))
}
