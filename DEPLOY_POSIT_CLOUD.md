# Guía de Despliegue y Conexión con Posit Connect Cloud / Posit Cloud

Esta guía detalla los pasos para publicar y conectar el **GeoVisor PDU Anta (2024 - 2034)** en **Posit Connect Cloud** y **Posit Cloud**.

---

## 1. Arquitectura de Despliegue

El proyecto incluye todos los elementos preparados para Posit Connect Cloud:
- **`app.R`**: Código principal optimizado con Shiny y Bslib.
- **`manifest.json`**: Manifiesto precalculado con todas las dependencias y versiones exactas capturadas desde `renv.lock`.
- **`constancia_pdu_template.qmd`**: Plantilla Quarto con compilación directa vía **Typst** (nativa en Posit Connect Cloud sin requerir dependencias de LaTeX ni Chrome).
- **`.rscignore`**: Configuración de exclusión para no sobrecargar el despliegue con archivos temporales o de desarrollo.

---

## 2. Método Recomendado: Despliegue Automático desde GitHub (Git-Backed)

Posit Connect Cloud permite conectar directamente tu repositorio de GitHub:

1. Ingresa a [Posit Connect Cloud](https://connect.posit.cloud/) e inicia sesión.
2. Haz clic en **«Publish Content»** o **«New Content»** y selecciona **«From a Git Repository»**.
3. Selecciona tu repositorio de GitHub del GeoVisor (ej. `tu-usuario/geovisor-pdu-anta`).
4. Especifica los parámetros:
   - **Branch:** `main` (o tu rama principal).
   - **Primary Document / App:** `app.R` (o selecciona el `manifest.json` pregenerado).
5. Haz clic en **«Deploy»**.
6. Posit Connect Cloud restaurará el entorno, instalará los paquetes y levantará la aplicación con SSL y dominio público gratuito.

> **Actualizaciones automáticas:** Cada vez que hagas `git push` a tu repositorio, Posit Connect Cloud actualizará automáticamente la aplicación.

---

## 3. Método Alternativo: Despliegue desde R / RStudio con `rsconnect`

Si prefieres desplegar directamente desde tu sesión de R:

### Paso 1: Configurar Credenciales de Posit Connect Cloud
```r
# Instalar / cargar rsconnect
if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
library(rsconnect)

# Configurar tu cuenta (obtén el token desde tu perfil en connect.posit.cloud)
rsconnect::setAccountInfo(
  name   = "<tu-cuenta>",
  token  = "<tu-token>",
  secret = "<tu-secret>",
  server = "posit.cloud" # o el servidor asignado
)
```

### Paso 2: Desplegar la Aplicación
```r
# Despliegue directo
rsconnect::deployApp(
  appDir      = ".",
  appName     = "geovisor-pdu-anta",
  appTitle    = "GeoVisor PDU Anta 2024-2034",
  appPrimaryDoc = "app.R",
  forceUpdate = TRUE
)
```

---

## 4. Regeneración del `manifest.json`

Si en el futuro agregas nuevos paquetes de R al proyecto, simplemente ejecuta el script automatizado:

```r
source("scripts/generate_manifest.R")
```

Esto actualizará el archivo `manifest.json` manteniendo la compatibilidad total con Posit Connect Cloud.
