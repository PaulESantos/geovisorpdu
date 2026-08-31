# ==============================================================================
# VALIDACION FUNCIONAL DE DATOS Y CONSULTAS ESPACIALES - PDU ANTA
# ==============================================================================
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

source("R/spatial_utils.R")

fallar <- function(mensaje) stop(mensaje, call. = FALSE)
verificar <- function(condicion, mensaje) {
  if (!isTRUE(condicion)) fallar(mensaje)
  cat("  [OK] ", mensaje, "\n", sep = "")
}

cat("=== 1. Integridad de archivos y objetos ===\n")
archivos <- c(
  "data/pdu_capas_optimizadas.rds",
  "data/parametros_urbanisticos.rds",
  "www/custom.css",
  "app.R"
)
verificar(all(file.exists(archivos)), "Los archivos esenciales existen")

capas <- readRDS("data/pdu_capas_optimizadas.rds")
parametros <- readRDS("data/parametros_urbanisticos.rds")
capas_requeridas <- c(
  "zonificacion", "ambito", "distritos", "vias",
  "alta_tension", "alta_tension_buffer", "ferrocarril",
  "ferrocarril_buffer"
)
verificar(all(capas_requeridas %in% names(capas)), "El RDS contiene todas las capas requeridas")
verificar(nrow(capas$zonificacion) > 0, "La zonificacion contiene entidades")
verificar(nrow(parametros) > 0, "El catalogo contiene categorias urbanisticas")
verificar(all(st_is_valid(capas$zonificacion)), "Las geometrias de zonificacion son validas")

cat("\n=== 2. Trazabilidad normativa ===\n")
campos_normativos <- c("ESTADO_VALIDACION", "FUENTE_NORMATIVA", "TABLA_FUENTE")
verificar(all(campos_normativos %in% names(parametros)), "El catalogo incluye metadatos de validacion")
verificar(
  all(parametros$ESTADO_VALIDACION == "PENDIENTE DE VALIDACIÓN MUNICIPAL"),
  "Ningun parametro se presenta como aprobado sin validacion municipal"
)

cat("\n=== 3. Consulta espacial reproducible ===\n")
zonas_prueba <- capas$zonificacion[seq_len(min(3, nrow(capas$zonificacion))), ]
puntos_prueba <- suppressWarnings(st_point_on_surface(st_geometry(zonas_prueba)))
puntos_prueba <- st_sf(id = seq_along(puntos_prueba), geometry = puntos_prueba)

for (i in seq_len(nrow(puntos_prueba))) {
  punto <- puntos_prueba[i, ]
  cruces <- st_intersects(punto, capas$zonificacion)[[1]]
  verificar(length(cruces) >= 1, paste("El punto de prueba", i, "intersecta al menos una zona"))
  area <- area_interseccion_unica_m2(
    st_buffer(st_transform(punto, CRS_METRICO_ANTA), 10),
    st_transform(capas$zonificacion[cruces, ], CRS_METRICO_ANTA)
  )
  verificar(is.numeric(area) && length(area) == 1 && is.finite(area),
            paste("La interseccion del punto", i, "produce un area escalar"))
}

cat("\n=== 4. Controles de fajas y datos incompletos ===\n")
verificar(nrow(capas$alta_tension_buffer) == 4,
          "Solo cuatro lineas electricas con tension identificada generan faja")
verificar(nrow(capas$ferrocarril_buffer) > 0,
          "La capa ferroviaria referencial esta disponible")

codigos_via <- trimws(as.character(capas$vias$COD_VIA_PR))
vias_sin_codigo <- sum(
  is.na(codigos_via) | codigos_via == "" |
    grepl("^Sin c", codigos_via, ignore.case = TRUE)
)
cat("  [INFO] Vias sin codigo: ", vias_sin_codigo, " de ", nrow(capas$vias), "\n", sep = "")
cat("  [INFO] La faja ferroviaria es referencial y requiere validacion sectorial.\n")

cat("\nVALIDACION FUNCIONAL COMPLETADA SIN ERRORES\n")
