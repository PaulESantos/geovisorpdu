suppressPackageStartupMessages({
  library(sf)
  library(testthat)
})

source("R/spatial_utils.R")
capas <- readRDS("data/pdu_capas_optimizadas.rds")
parametros <- readRDS("data/parametros_urbanisticos.rds")

capas_requeridas <- c(
  "ambito", "distritos", "zonificacion", "vias", "alta_tension",
  "alta_tension_buffer", "ferrocarril", "ferrocarril_buffer"
)
stopifnot(all(capas_requeridas %in% names(capas)))
stopifnot(all(vapply(capas[capas_requeridas], inherits, logical(1), what = "sf")))
stopifnot(all(vapply(capas[capas_requeridas], function(x) all(st_is_valid(x)), logical(1))))
stopifnot(all(vapply(capas[capas_requeridas], function(x) identical(st_crs(x)$epsg, 4326L), logical(1))))
stopifnot(nrow(parametros) == 40L)
stopifnot(!anyDuplicated(parametros$COD_S_ZONA))
stopifnot(setequal(trimws(capas$zonificacion$COD_S_ZONA), trimws(parametros$COD_S_ZONA)))
stopifnot(all(c("ESTADO_VALIDACION", "FUENTE_NORMATIVA", "TABLA_FUENTE") %in% names(parametros)))
stopifnot(nrow(capas$alta_tension) == 7L)
stopifnot(nrow(capas$alta_tension_buffer) == 4L)
stopifnot(all(!is.na(capas$alta_tension_buffer$TENS_NOMIN)))

cat("Validación previa superada:\n")
cat(" -", nrow(capas$zonificacion), "polígonos de zonificación\n")
cat(" -", nrow(capas$vias), "ejes viales\n")
cat(" -", nrow(parametros), "categorías normativas pendientes de aprobación municipal\n")
cat(" -", nrow(capas$alta_tension_buffer), "tramos eléctricos con faja calculada\n")

