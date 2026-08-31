# Utilidades espaciales seguras para el GeoVisor PDU Anta.

CRS_GEOGRAFICO <- 4326
CRS_METRICO_ANTA <- 32718
BBOX_VALIDACION_ANTA <- c(xmin = -72.50, ymin = -13.80, xmax = -71.90, ymax = -13.10)

validar_coordenada_punto <- function(lng, lat) {
  valores <- suppressWarnings(as.numeric(c(lng, lat)))
  if (any(!is.finite(valores))) stop("Las coordenadas deben ser valores numéricos finitos.")
  if (valores[1] < BBOX_VALIDACION_ANTA["xmin"] || valores[1] > BBOX_VALIDACION_ANTA["xmax"] ||
      valores[2] < BBOX_VALIDACION_ANTA["ymin"] || valores[2] > BBOX_VALIDACION_ANTA["ymax"]) {
    stop("Las coordenadas se encuentran fuera del entorno geográfico admitido para Anta.")
  }
  invisible(TRUE)
}

normalizar_poligono_predio <- function(objeto_sf, max_entidades = 200, max_area_m2 = 2e8) {
  if (!inherits(objeto_sf, "sf") || nrow(objeto_sf) == 0) stop("El archivo no contiene entidades espaciales.")
  if (nrow(objeto_sf) > max_entidades) stop("El archivo excede el máximo de 200 entidades permitido.")
  if (is.na(sf::st_crs(objeto_sf))) stop("El archivo no declara un sistema de coordenadas (CRS).")
  if (any(sf::st_is_empty(objeto_sf))) objeto_sf <- objeto_sf[!sf::st_is_empty(objeto_sf), , drop = FALSE]
  if (nrow(objeto_sf) == 0) stop("Todas las geometrías del archivo están vacías.")

  objeto_sf <- sf::st_make_valid(objeto_sf)
  tipos <- unique(as.character(sf::st_geometry_type(objeto_sf)))
  if (!all(tipos %in% c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))) {
    stop("El archivo debe contener únicamente polígonos o multipolígonos de predios.")
  }

  geom_poligono <- suppressWarnings(sf::st_collection_extract(sf::st_geometry(objeto_sf), "POLYGON"))
  if (length(geom_poligono) == 0 || all(sf::st_is_empty(geom_poligono))) {
    stop("No se encontraron polígonos utilizables en el archivo.")
  }
  geom_unificada <- sf::st_union(geom_poligono)
  predio <- sf::st_sf(id = 1L, geometry = geom_unificada)
  predio <- sf::st_transform(predio, CRS_GEOGRAFICO)

  bb <- sf::st_bbox(predio)
  if (bb["xmax"] < BBOX_VALIDACION_ANTA["xmin"] || bb["xmin"] > BBOX_VALIDACION_ANTA["xmax"] ||
      bb["ymax"] < BBOX_VALIDACION_ANTA["ymin"] || bb["ymin"] > BBOX_VALIDACION_ANTA["ymax"]) {
    stop("El predio se encuentra fuera del entorno geográfico admitido para Anta.")
  }

  area_m2 <- as.numeric(sf::st_area(sf::st_transform(predio, CRS_METRICO_ANTA)))
  if (!is.finite(area_m2) || area_m2 <= 0) stop("El predio debe tener un área mayor que cero.")
  if (area_m2 > max_area_m2) stop("El predio excede el área máxima admitida para una consulta.")
  predio
}

area_interseccion_unica_m2 <- function(predio_4326, capa_4326) {
  if (is.null(capa_4326) || nrow(capa_4326) == 0) return(0)
  capa_unida <- sf::st_sf(geometry = sf::st_union(sf::st_geometry(capa_4326)))
  inter <- suppressWarnings(sf::st_intersection(predio_4326, capa_unida))
  if (nrow(inter) == 0 || all(sf::st_is_empty(inter))) return(0)
  sum(as.numeric(sf::st_area(sf::st_transform(inter, CRS_METRICO_ANTA))), na.rm = TRUE)
}

extraer_zip_seguro <- function(archivo_zip, destino, max_archivos = 100, max_descomprimido = 100 * 1024^2) {
  listado <- utils::unzip(archivo_zip, list = TRUE)
  if (nrow(listado) == 0) stop("El archivo ZIP está vacío.")
  if (nrow(listado) > max_archivos) stop("El ZIP contiene demasiados archivos.")
  nombres <- gsub("\\\\", "/", listado$Name)
  inseguro <- grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", nombres)
  if (any(inseguro)) stop("El ZIP contiene rutas no permitidas.")
  if (sum(listado$Length, na.rm = TRUE) > max_descomprimido) stop("El ZIP excede 100 MB descomprimido.")
  dir.create(destino, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(archivo_zip, exdir = destino)
  invisible(destino)
}

ejecutando_en_shinyapps <- function() {
  ruta_trabajo <- gsub("\\\\", "/", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  nzchar(Sys.getenv("RSCONNECT_APPLICATION")) ||
    grepl("/srv/connect/apps/", ruta_trabajo, fixed = TRUE)
}

quarto_disponible <- function() {
  tryCatch({
    q_path <- quarto::quarto_path()
    !is.null(q_path) && nzchar(q_path) && file.exists(q_path)
  }, error = function(e) {
    nzchar(Sys.which("quarto"))
  })
}

chrome_disponible <- function() {
  # La impresión con Chrome puede agotar o detener el contenedor de shinyapps.io.
  if (ejecutando_en_shinyapps()) return(FALSE)
  tryCatch({
    ruta <- pagedown::find_chrome()
    length(ruta) == 1 && !is.na(ruta) && nzchar(ruta) && file.exists(ruta)
  }, error = function(e) FALSE)
}

pdf_motor_disponible <- function() {
  quarto_disponible() || chrome_disponible()
}

