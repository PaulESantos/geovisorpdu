suppressPackageStartupMessages({
  library(rsconnect)
})

# Generar manifest.json optimizado para Posit Connect Cloud y Posit Cloud
cat("Generando manifest.json para Posit Connect Cloud / Posit Cloud...\n")

# Archivos a incluir en el despliegue
app_files <- c(
  "app.R",
  "DESCRIPTION",
  "constancia_pdu_template.qmd",
  "R/spatial_utils.R",
  "data/pdu_capas_optimizadas.rds",
  "data/parametros_urbanisticos.rds",
  "www/custom.css",
  "www/escudo_muni_anta.png",
  "www/pdu_actualizado_sf.png",
  "logo/ESCUDO MUNI ANTA.png",
  "logo/pdu_actualizado_sf.png"
)

# Filtrar solo los que existen
app_files_exist <- app_files[file.exists(app_files)]

rsconnect::writeManifest(
  appPrimaryDoc = "app.R",
  appFiles = app_files_exist,
  appMode = "shiny"
)

cat("Manifest generado exitosamente. Existe manifest.json:", file.exists("manifest.json"), "\n")
