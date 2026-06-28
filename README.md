# Explorador de Datos EMTP — Datos Públicos

Aplicación Shiny para explorar datos de la Educación Media Técnico-Profesional (EMTP) de Chile.

**App en línea:** https://datostp.shinyapps.io/datospublicosEMTP/

## Contenido

- **app.R** — Aplicación principal Shiny
- **scripts/preparar_datos.R** — Pipeline de preparación de datos desde fuentes brutas
- **data/app/** — Datos procesados en formato `.rds` (generados por `preparar_datos.R`)
- **templates/** — Plantillas Quarto para minutas PDF por establecimiento
- **docs/** — Reportes de matrícula EMTP 2018–2025 por región y zona
- **www/** — Recursos estáticos (CSS, imágenes)

## Datos

Los datos procesados (`data/app/*.rds`) se generan ejecutando:

```r
source("scripts/preparar_datos.R")
```

Las fuentes brutas provienen de bases públicas del MINEDUC, JUNAEB y la Agencia de Calidad de la Educación.

## Variables de entorno

Para el chatbot integrado se requiere una clave de API de Groq. Crear un archivo `.Renviron` en la raíz con:

```
GROQ_API_KEY=tu_clave_aqui
```

(Este archivo **no** está versionado por seguridad.)

## Despliegue

```r
library(rsconnect)
rsconnect::deployApp(appDir = ".", appName = "datospublicosEMTP", account = "datostp")
```

## Fuentes de datos

- Matrícula oficial EMTP — MINEDUC 2025
- SIMCE 2° Medio — Agencia de Calidad 2025
- IDPS 2° Medio — Agencia de Calidad 2025
- Docentes — CPEIP / MINEDUC 2025
- IVE — JUNAEB 2025
- Titulados TP — MINEDUC 2024
