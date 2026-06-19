# ============================================================
# dev/run_dev.R — Correr StatPlot en modo desarrollo
# ============================================================

# Detach package if loaded
if ("StatPlot" %in% (.packages())) {
  pkgload::unload("StatPlot")
}

# Load all
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# Run the application
run_app()
