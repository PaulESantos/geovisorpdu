suppressPackageStartupMessages(library(sf))
source(testthat::test_path("..", "..", "R", "spatial_utils.R"))

test_that("se valida el entorno de coordenadas de Anta", {
  expect_invisible(validar_coordenada_punto(-72.1742, -13.4567))
  expect_error(validar_coordenada_punto(-13.4567, -72.1742), "fuera")
})

test_that("un polígono geográfico conserva el orden longitud-latitud", {
  xy <- matrix(c(
    -72.1750, -13.4570,
    -72.1740, -13.4570,
    -72.1740, -13.4560,
    -72.1750, -13.4560,
    -72.1750, -13.4570
  ), ncol = 2, byrow = TRUE)
  predio <- st_sf(id = 1, geometry = st_sfc(st_polygon(list(xy)), crs = 4326))
  limpio <- normalizar_poligono_predio(predio)
  expect_s3_class(limpio, "sf")
  expect_gt(as.numeric(st_area(st_transform(limpio, 32718))), 0)
})

test_that("las áreas de varias entidades se consolidan en un escalar", {
  predio <- st_as_sfc(st_bbox(c(xmin = -72.18, ymin = -13.46, xmax = -72.17, ymax = -13.45), crs = st_crs(4326)))
  predio <- st_sf(id = 1, geometry = predio)
  overlays <- rbind(
    st_sf(id = 1, geometry = st_as_sfc(st_bbox(c(xmin = -72.18, ymin = -13.46, xmax = -72.175, ymax = -13.45), crs = st_crs(4326)))),
    st_sf(id = 2, geometry = st_as_sfc(st_bbox(c(xmin = -72.175, ymin = -13.46, xmax = -72.17, ymax = -13.45), crs = st_crs(4326))))
  )
  area <- area_interseccion_unica_m2(predio, overlays)
  expect_length(area, 1)
  expect_gt(area, 0)
})

test_that("se rechazan geometrías que no son predios", {
  linea <- st_sf(id = 1, geometry = st_sfc(st_linestring(matrix(c(-72.18, -13.46, -72.17, -13.45), ncol = 2, byrow = TRUE)), crs = 4326))
  expect_error(normalizar_poligono_predio(linea), "predios")
})

test_that("la impresión PDF se deshabilita en shinyapps.io para Chrome pero detecta motor general", {
  withr::local_envvar(RSCONNECT_APPLICATION = "geovisor-pdu-anta-validacion")
  expect_true(ejecutando_en_shinyapps())
  expect_false(chrome_disponible())
  expect_type(quarto_disponible(), "logical")
  expect_type(pdf_motor_disponible(), "logical")
})

