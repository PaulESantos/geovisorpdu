# Documentación Técnica de Decisiones de Implementación: GeoVisor PDU Anta (2024 - 2034)

## 1. Contexto y Objetivos Institucionales
El presente documento consolida las decisiones técnicas, arquitectónicas y visuales implementadas en el **GeoVisor del Plan de Desarrollo Urbano (PDU) de Anta (2024 - 2034)**, desarrollado para la **Municipalidad Provincial de Anta** en el marco del proyecto:
> *«Mejoramiento de los Servicios de Gestión Territorial y Desarrollo Urbano Sostenible, del distrito de Anta, provincia de Anta, departamento del Cusco»*.

---

## 2. Integración de la Capa de Sistema Vial Propuesto

### Fuente de Datos
- **Archivo original:** `shp_pdu_anta/P_080301_Sistema_vial_propuesto.shp` (1,729 entidades lineales, CRS: EPSG:32718 - WGS 84 / UTM zona 18S).

### Variables Seleccionadas y Normalizadas
1. `SECCION_VI`: Sección transversal de vía propuesta (ancho normativo en metros lineales, ej. `SV=12.00 ml`, `SV=24.00 ml`, `SV=10.00 ml`).
2. `SIS_VIAL`: Jerarquía de la red vial en el PDU:
   - **Sistema Vial Provincial-Metropolitano**
   - **Sistema Vial Primario**
   - **Sistema Vial Secundario**
   - **Sistema Vial Especial**
3. `COD_VIA_PR`: Código identificador asignado por el PDU (ej. `S/N-13'1`, `S/N-26`, etc.).

### Procesamiento y Optimización
- **Script:** [`scripts/01_preprocesar_datos.R`](file:///d:/geovisorpdu/scripts/01_preprocesar_datos.R)
- **Topología y Proyección:** Corrección de geometrías con `sf::st_make_valid()` y reproyección a WGS84 (`EPSG:4326`) para renderizado dinámico en Leaflet y UTM 18S (`EPSG:32718`) para cálculos métricos de distancia.
- **Empaquetado:** Almacenado como lista optimizada en [`data/pdu_capas_optimizadas.rds`](file:///d:/geovisorpdu/data/pdu_capas_optimizadas.rds) con compresión `xz`.

### Lógica de Consulta Espacial
- **Consulta Puntual:** Se evalúa la distancia métrica a todas las vías del sistema propuesto. Si el punto se encuentra a 25 metros o menos de un eje vial, se identifica como vía próxima, extrayendo su sección normativa y código.
- **Consulta por Polígono:** Se evalúa la intersección y franja de proximidad de 25 m del lote respecto a los ejes viales, desglosando las vías que intersectan o se encuentran próximas al predio.

---

## 3. Identidad Gráfica y Paleta de Colores Oficial de Zonificación

Se actualizó e integró la paleta cromática exacta por cada subzona normada de acuerdo a las láminas oficiales entregadas por la Gerencia:

### A. INTENSIDADES (Residenciales)
- `ZDA - C`: `#C86868` (Rojo ladrillo medio)
- `ZDA - S`: `#F49494` (Rosa rojizo)
- `ZDM - C - I`: `#F67777` (Rojo salmón vivo)
- `ZDM - C - II`: `#F5AA7A` (Salmón anaranjado)
- `ZDM - S - I`: `#FCA8A8` (Rosa pastel claro)
- `ZDM - S - II`: `#FDE0DC` (Rosa pálido)
- `ZDM - CR - I`: `#ECA080` (Salmón anaranjado con trama)
- `ZDM - CR - II`: `#F5CAA8` (Salmón pálido con trama)
- `ZDB - C - I`: `#F2964C` (Naranja cálido medio)
- `ZDB - C - II`: `#F7B865` (Naranja amarillento)
- `ZDB - S - I`: `#FDE4B0` (Crema anaranjado suave)
- `ZDB - S - II`: `#FFF0C2` (Amarillo crema claro)
- `ZDB - CR - I`: `#F2B262` (Naranja ocre con trama)
- `ZDB - CR - II`: `#F8DEAC` (Crema ocre con trama)
- `ZDMB - S - I`: `#E5E64B` (Amarillo lima)
- `ZDMB - S - II`: `#F6F255` (Amarillo brillante)
- `ZDMB - S - III`: `#FFF7A4` (Amarillo pastel suave)
- `ZDMB - CR`: `#FBF4BA` (Amarillo suave con trama)

### B. SEGÚN USOS (Equipamientos e Industria)
- `E1` (Educación Básica): `#D4E7F5` (Celeste muy claro)
- `E1?` / `CETPRO` (Educación Técnico Productivo): `#D0E8D7` (Verde agua claro)
- `E2` (Educación Superior Tecnológica): `#A9CEEC` (Celeste medio)
- `H1` (Posta Médica): `#C6E2E1` (Turquesa pastel claro)
- `H2` (Centro de Salud): `#99D5D2` (Turquesa medio)
- `H3` (Hospital General): `#80BDBB` (Turquesa oscuro)
- `ZRP` (Zona de Recreación Pública): `#BEDB92` (Verde manzana claro)
- `ZOU` (Zona de Otros Usos): `#BCBDBE` (Gris medio)
- `I1` (Industria Elemental): `#E2D4E4` (Lila pastel)
- `I2` (Industria Liviana): `#B2AECB` (Lavanda medio)
- `ZA` (Zona Agraria): `#EFF6D3` (Amarillo verdoso marfil)

### C. CARACTERÍSTICAS PARTICULARES (Patrimonio y Reglamentación Especial)
- `ZM` (Zona Monumental): `#DDD4CE` (Beige arena)
- `ZPA - CE` (Conservación Ecológica): `#80C880` (Verde esmeralda)
- `ZPA - RH` (Recursos Hídricos / Fajas Marginales): `#80C8F6` (Azul hídrico)
- `ZPA - IERE` (Recuperación de Ecosistemas): `#E5F69A` (Verde lima claro)
- `ZRE - NU - P` (No Urbanizable por Peligro Alto): `#7F8285` (Gris)
- `ZRE - NU - RE` (No Urbanizable por Riesgo Eléctrico): `#686B6E` (Gris oscuro)
- `ZRE - PMA - S` (Peligro Muy Alto por Sismicidad): `#CB7B9E` (Rosa magenta)
- `ZRE - PA - C` (Protección Calpitocasa): `#AFA39A` (Marrón grisáceo)
- `ZRE - PA - HYI` (Protección Humedal Yungaqui-In): `#B6FFF5` (Aguamarina claro)
- `ZRE - VHC - CH` (Centro Histórico): `#5D5E60` (Carbón oscuro)
- `ZRE - CBIP` (Bien Inmueble Prehispánico): `#CBA362` (Ocre dorado)

---

## 4. Distribución de Logos y Formato del Reporte Informativo

> **Alcance institucional:** El documento generado por el geovisor es exclusivamente informativo y no constituye un certificado oficial. El certificado oficial debe tramitarse ante la Gerencia de Desarrollo Urbano y Rural de la Municipalidad Provincial de Anta.

1. **Distribución en Extremos del Navbar:** Escudo de la Municipalidad Provincial de Anta al extremo izquierdo junto al título principal, y Logo del Proyecto PDU al extremo derecho mediante `nav_spacer()` y `nav_item()`.
2. **Carga Universal de Archivos Espaciales:** Soporte para archivos Shapefile comprimidos en `.zip`, selección múltiple de componentes (`.shp`, `.dbf`, `.shx`, `.prj`), `.geojson`, `.kml`, `.csv` y `.txt`.
3. **Plano Cartográfico Oficial en Página Completa (Página 1) — Cero Pérdida de Espacio:**
   - **Sincronización de Aspect-Ratio del Lienzo (1.464):** Se calcula dinámicamente la extensión en UTM ($X_{\text{span}} = Y_{\text{span}} \times \frac{8.2}{5.6}$) de modo que las coordenadas de visualización coincidan con la relación de aspecto de la lámina (`8.2 x 5.6 pulgadas`), eliminando por completo el efecto de *letterboxing* (bandas o márgenes blancos laterales).
   - **Tarjetas de Fondo Antisolapamiento:** La Rosa de los Vientos (Norte) y la Escala Gráfica se ubican sobre tarjetas bimetálicas con fondo blanco translúcido (`alpha = 0.90`) y bordes nítidos, evitando superposiciones de textos y garantizando 100% de legibilidad sobre cualquier color de zonificación.
   - **Aprovechamiento Integral de la Hoja:** En la primera página del PDF y en el visor, el plano cartográfico se expande al 100% del ancho imprimible (`max-height: 640px`), logrando una lámina catastral de alta definición que utiliza todo el espacio disponible.
4. **Buffer Estricto de 25 Metros y Nota Metodológica en el Sistema Vial:**
   - La recuperación de vías propuestas del PDU se limita a una distancia máxima de **25 metros** (`dists_vias <= 25`) respecto al perímetro del predio o punto consultado.
   - Se excluye cualquier longitud de vía individual.
   - Se incluye de manera obligatoria la siguiente **Nota Metodológica** bajo la tabla vial:
     > *«Nota Metodológica: Las vías del Sistema Vial Propuesto mostradas corresponden a los ejes normativos identificados dentro de un área de influencia directa (buffer de 25 metros) respecto al perímetro del predio o punto consultado.»*
5. **Estandarización Numérica (Máximo 3 Decimales):**
   - Todos los cálculos cuantitativos (área en m² y hectáreas, perímetro, distancias viales, porcentajes de ocupación del lote, afectaciones de servidumbre y coordenadas UTM) se presentan formateados a un **máximo de 3 decimales**.
6. **Aprovechamiento Integral del Espacio en el Panel del Visor y PDF (2 Hojas Completas):**
   - En la aplicación, la pestaña *«Reporte Informativo»* utiliza `container-fluid px-lg-5` y una tarjeta expandida a `max-width: 1200px`, eliminando márgenes vacíos y aprovechando todo el ancho de pantalla.
   - En PDF, se utiliza una paginación limpia de 2 páginas exactas:
     - **Página 1:** Membrete Oficial + Proyecto e Instrumento + Resumen de Identificación + Gran Plano Cartográfico a Página Completa.
     - **Página 2:** 1. Datos Métricos Detallados + 2. Sistema Vial Propuesto (Buffer 25m + Nota Metodológica) + 3. Desglose de Zonificación + 4. Afectaciones por Infraestructura + 5. Parámetros Urbanísticos Normativos por Zona + Pie Legal.

---

## 5. Scripts de Mantenimiento y Validación
- [`scripts/01_preprocesar_datos.R`](file:///d:/geovisorpdu/scripts/01_preprocesar_datos.R): Script reproducible para reprocesar shapefiles si se actualizan capas de zonificación o sistema vial.
- [`scripts/02_validar_app.R`](file:///d:/geovisorpdu/scripts/02_validar_app.R): Suite de pruebas automatizadas para comprobar la integridad de datos, atributos viales, topología y recorte cartográfico.

---

## 6. Motor PDF de Alta Eficiencia: Quarto + Typst
- **Motor Primario:** Compilación nativa con **Quarto CLI + Typst**, eliminando dependencias pesadas de LaTeX y eliminando fallas por falta de navegadores headless (Chrome/Chromium) en entornos de servidor como Posit Connect Cloud.
- **Plantilla:** [`constancia_pdu_template.qmd`](file:///d:/geovisorpdu/constancia_pdu_template.qmd) con paginación A4 exacta de 2 páginas.
- **Trazabilidad sin QR:** Conforme a las directrices institucionales, se prescindió del uso de códigos QR, manteniendo una estructura sobria, clara e institucional con identificador único de consulta.
- **Seguridad:** Los secretos y configuraciones se manejan mediante `Sys.getenv()` y exclusión en `.gitignore`.

