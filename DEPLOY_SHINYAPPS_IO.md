# Prueba controlada en shinyapps.io — GeoVisor PDU Anta

## Alcance

El despliegue es un entorno de validación funcional. El reporte generado es exclusivamente informativo. El certificado oficial debe tramitarse ante la Gerencia de Desarrollo Urbano y Rural de la Municipalidad Provincial de Anta.

## 1. Verificación previa

Desde la raíz del proyecto:

```r
source("scripts/03_validar_pre_despliegue.R")
testthat::test_dir("tests/testthat")
shiny::runApp(launch.browser = TRUE)
```

Validar en escritorio y móvil:

- carga inicial y activación/desactivación de capas;
- consulta manual UTM y WGS84;
- consulta por polígono digitado, GeoJSON y Shapefile ZIP;
- punto ubicado sobre un límite de zonificación;
- advertencia de precisión GPS;
- consulta completa del reporte dentro de la aplicación;
- tiempos de respuesta y consumo de memoria con al menos cinco sesiones simultáneas.

## 2. Crear la cuenta de despliegue

1. Crear la aplicación o cuenta institucional en https://www.shinyapps.io/.
2. En el panel de shinyapps.io abrir **Tokens** y generar un token de despliegue.
3. Configurar el token solo en la estación autorizada. No guardar `secret` ni token dentro del proyecto:

```r
rsconnect::setAccountInfo(
  name = Sys.getenv("SHINYAPPS_ACCOUNT"),
  token = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)
```

## 3. Desplegar

El archivo `.rscignore` excluye fuentes pesadas, documentos de trabajo y resultados de auditoría. Solo deben publicarse `app.R`, `R/`, `data/`, `www/`, `DESCRIPTION`, `renv.lock` y archivos necesarios de configuración.

En una cuenta gratuita, configure siempre la instancia `small` antes de iniciar o reiniciar la aplicación. Una instancia `large` puede suspender la cuenta por uso de un proceso de pago:

```r
rsconnect::configureApp(
  appName = "geovisor-pdu-anta-validacion",
  account = Sys.getenv("SHINYAPPS_ACCOUNT"),
  server = "shinyapps.io",
  redeploy = FALSE,
  size = "small"
)
```

```r
archivos_publicacion <- c(
  "app.R", "DESCRIPTION", "LICENSE", "renv.lock",
  "R/spatial_utils.R",
  "data/parametros_urbanisticos.rds",
  "data/pdu_capas_optimizadas.rds",
  "www/custom.css",
  "www/escudo_muni_anta.png",
  "www/pdu_actualizado_sf.png"
)

rsconnect::deployApp(
  appDir = ".",
  appFiles = archivos_publicacion,
  appName = "geovisor-pdu-anta-validacion",
  account = Sys.getenv("SHINYAPPS_ACCOUNT"),
  forceUpdate = TRUE,
  launch.browser = TRUE
)
```

## 4. Validación posterior

Revisar los registros del servidor:

```r
rsconnect::showLogs(
  appName = "geovisor-pdu-anta-validacion",
  account = Sys.getenv("SHINYAPPS_ACCOUNT"),
  streaming = TRUE
)
```

Registrar versión desplegada, fecha, responsable, URL y resultados de prueba. No compartir públicamente la URL hasta que la Gerencia apruebe la matriz normativa.

## 5. Consideraciones del servicio

- La geolocalización funciona porque shinyapps.io usa HTTPS.
- Los mapas base dependen de OpenStreetMap y Esri; debe probarse el comportamiento ante indisponibilidad externa.
- La descarga PDF se deshabilita en shinyapps.io porque la impresión con Chrome puede detener el contenedor. Permanece disponible en instalaciones locales compatibles.
- El reporte se consulta íntegramente dentro de la aplicación alojada.
- Para producción institucional se recomienda un plan con capacidad acorde al número de usuarios, monitoreo y dominio municipal.
- Antes de producción deben aprobarse los parámetros del archivo `Matriz_validacion_normativa_PDU_Anta.xlsx` y regenerarse `data/parametros_urbanisticos.rds`.
