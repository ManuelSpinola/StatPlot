# =============================================================================
# StatPlot — Prepare Example Datasets
# =============================================================================
# Saves each dataset as a named list with components:
#   $data : data.frame con los datos
#   $meta : list con información del dataset para la UI
#
# Output: inst/data/*.rds  (one file per dataset)
# Run once from project root: source("data-raw/prepare_example_data.R")
# =============================================================================

dir.create("inst/app/data", recursive = TRUE, showWarnings = FALSE)

# Helper para guardar
save_dataset <- function(data, meta, filename) {
  obj <- list(data = data, meta = meta)
  saveRDS(obj, file = file.path("inst/app/data", filename))
  message("Saved: ", filename,
          " | rows=", nrow(data),
          " | cols=", ncol(data))
}

# =============================================================================
# 1. PENGUINS (palmerpenguins)
#    344 pingüinos × 8 variables
#    Continuas: bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g
#    Categóricas: species, island, sex
#    Temporal: year
# =============================================================================
data(penguins, package = "palmerpenguins")

penguins_df <- as.data.frame(penguins)

save_dataset(
  data = penguins_df,
  meta = list(
    name        = "Pingüinos de Palmer",
    description = "Medidas morfológicas de tres especies de pingüinos en el archipiélago de Palmer, Antártida.",
    source      = "palmerpenguins",
    n_rows      = nrow(penguins_df),
    n_cols      = ncol(penguins_df),
    vars_continuas  = c("bill_length_mm", "bill_depth_mm",
                        "flipper_length_mm", "body_mass_g"),
    vars_categoricas = c("species", "island", "sex"),
    vars_temporales  = "year",
    referencia  = "Gorman et al. (2014) · PLoS ONE"
  ),
  filename = "penguins.rds"
)

# =============================================================================
# 2. GAPMINDER (gapminder)
#    1704 filas × 6 variables
#    Continuas: lifeExp, pop, gdpPercap
#    Categóricas: country, continent
#    Temporal: year (1952-2007, cada 5 años)
# =============================================================================
data(gapminder, package = "gapminder")

gapminder_df <- as.data.frame(gapminder)

save_dataset(
  data = gapminder_df,
  meta = list(
    name        = "Gapminder",
    description = "Indicadores socioeconómicos y de salud para 142 países entre 1952 y 2007.",
    source      = "gapminder",
    n_rows      = nrow(gapminder_df),
    n_cols      = ncol(gapminder_df),
    vars_continuas   = c("lifeExp", "pop", "gdpPercap"),
    vars_categoricas = c("country", "continent"),
    vars_temporales  = "year",
    referencia  = "Gapminder Foundation · gapminder.org"
  ),
  filename = "gapminder.rds"
)

# =============================================================================
# 3. MEUSE (sp)
#    155 sitios × variables de contaminación de suelos
#    Continuas: cadmium, copper, lead, zinc, elev, dist, om, ffreq
#    Categóricas: soil, lime, landuse
#    Espacial: x, y (coordenadas)
# =============================================================================
data(meuse, package = "sp")

meuse_df <- as.data.frame(meuse)

# Preservar factores
meuse_df$soil    <- factor(meuse_df$soil)
meuse_df$lime    <- factor(meuse_df$lime)
meuse_df$landuse <- factor(meuse_df$landuse)
meuse_df$ffreq   <- factor(meuse_df$ffreq)

save_dataset(
  data = meuse_df,
  meta = list(
    name        = "Contaminación río Meuse",
    description = "Concentraciones de metales pesados en suelos de la llanura aluvial del río Meuse (Países Bajos).",
    source      = "sp::meuse",
    n_rows      = nrow(meuse_df),
    n_cols      = ncol(meuse_df),
    vars_continuas   = c("cadmium", "copper", "lead", "zinc",
                         "elev", "dist", "om"),
    vars_categoricas = c("soil", "lime", "landuse", "ffreq"),
    vars_espaciales  = c("x", "y"),
    referencia  = "Rikken & Van Rijn (1993)"
  ),
  filename = "meuse.rds"
)

# =============================================================================
# 4. BIRTHWT (MASS)
#    189 nacimientos × 10 variables
#    Continuas: age, lwt, bwt
#    Categóricas: low, race, smoke, ptl, ht, ui, ftv
# =============================================================================
data(birthwt, package = "MASS")

birthwt_df <- as.data.frame(birthwt)

# Convertir variables binarias a factores con etiquetas
birthwt_df$low   <- factor(birthwt_df$low,   labels = c("Normal", "Bajo peso"))
birthwt_df$smoke <- factor(birthwt_df$smoke, labels = c("No fumadora", "Fumadora"))
birthwt_df$race  <- factor(birthwt_df$race,  labels = c("Blanca", "Negra", "Otra"))
birthwt_df$ht    <- factor(birthwt_df$ht,    labels = c("No", "Sí"))
birthwt_df$ui    <- factor(birthwt_df$ui,    labels = c("No", "Sí"))

save_dataset(
  data = birthwt_df,
  meta = list(
    name        = "Peso al nacer",
    description = "Factores de riesgo asociados al bajo peso al nacer en 189 nacimientos.",
    source      = "MASS::birthwt",
    n_rows      = nrow(birthwt_df),
    n_cols      = ncol(birthwt_df),
    vars_continuas   = c("age", "lwt", "bwt"),
    vars_categoricas = c("low", "smoke", "race", "ht", "ui"),
    referencia  = "Hosmer & Lemeshow (1989)"
  ),
  filename = "birthwt.rds"
)

# =============================================================================
# 5. MITES reducido (vegan)
#    70 sitios × 5 variables ambientales + 5 especies
#    Continuas: SubsDens, WatrCont + abundancias de especies
#    Categóricas (factores): Substrate, Shrub, Topo
# =============================================================================
data(mite,     package = "vegan")
data(mite.env, package = "vegan")

# Seleccionar 5 especies representativas (las más abundantes)
spp_sel <- names(sort(colSums(mite), decreasing = TRUE)[1:5])

mite_df <- cbind(
  as.data.frame(mite.env),
  as.data.frame(mite[, spp_sel])
)

# Preservar factores
mite_df$Substrate <- factor(mite_df$Substrate)
mite_df$Shrub     <- factor(mite_df$Shrub)
mite_df$Topo      <- factor(mite_df$Topo)

rownames(mite_df) <- paste0("sitio", seq_len(nrow(mite_df)))

save_dataset(
  data = mite_df,
  meta = list(
    name        = "Ácaros oribátidos (mites)",
    description = paste0("Abundancia de 5 especies de ácaros oribátidos en 70 sitios con variables ambientales continuas y factores."),
    source      = "vegan::mite + vegan::mite.env",
    n_rows      = nrow(mite_df),
    n_cols      = ncol(mite_df),
    vars_continuas   = c("SubsDens", "WatrCont", spp_sel),
    vars_categoricas = c("Substrate", "Shrub", "Topo"),
    especies         = spp_sel,
    referencia  = "Borcard & Legendre (1994)"
  ),
  filename = "mites.rds"
)

# =============================================================================
# Resumen
# =============================================================================
message("\n--- Todos los datasets guardados en inst/app/data/ ---")
files <- list.files("inst/data", pattern = "\\.rds$", full.names = TRUE)
for (f in files) {
  obj <- readRDS(f)
  message(basename(f), ": ", nrow(obj$data), " filas × ",
          ncol(obj$data), " columnas — ", obj$meta$name)
}
