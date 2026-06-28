# Explorador de Datos EMTP — Chile

Aplicación web interactiva construida en R Shiny para explorar datos públicos de la **Educación Media Técnico-Profesional (EMTP)** de Chile. Integra matrícula, docentes, egresados, titulados, resultados SIMCE e IDPS, y un chatbot con acceso a los datos.

**App en línea:** https://datostp.shinyapps.io/datospublicosEMTP/

---

## ¿Qué hace esta app?

La app consolida en un solo lugar múltiples bases de datos públicas del sistema educativo chileno, permitiendo explorarlas de forma visual y descargar reportes por establecimiento.

### Pestañas principales

| Pestaña | Contenido |
|---|---|
| **Inicio** | KPIs nacionales, navegación temática, notas metodológicas y registro de cambios |
| **Establecimientos** | Buscador de liceos EMTP con ficha por establecimiento: mapa, especialidades, SIMCE, IDPS, IVE y descarga de minutas en PDF |
| **Visualizaciones Matrícula** | Gráficos interactivos de matrícula por región, dependencia, especialidad, sector económico y género |
| **Docentes** | Análisis del cuerpo docente EMTP: edad, experiencia, función, titulación, género y tipo de contrato |
| **Egresados y Titulados** | Egresados EMTP, continuidad en educación superior y titulados técnico-profesionales |
| **Asistente EMTP** | Chatbot en lenguaje natural con acceso a datos de matrícula, docentes y establecimientos |

---

## Estructura del repositorio

```
├── app.R                        # Aplicación principal Shiny
├── R/
│   ├── chatbot_rag.R            # Lógica del chatbot (RAG con Groq API)
│   ├── bg_jobs.R                # Generación de minutas en background
│   ├── filtros_docentes.R       # Lógica de filtros para la pestaña Docentes
│   ├── ui_rft.R                 # Componentes UI auxiliares
│   └── utils.R                  # Funciones utilitarias
├── scripts/
│   ├── preparar_datos.R         # Pipeline completo de preparación de datos
│   └── config.R                 # Configuración de rutas y parámetros
├── data/
│   ├── app/                     # Datos procesados (.rds) listos para la app
│   └── geographic/              # Shapefile de comunas simplificado
├── templates/
│   ├── minuta_establecimiento.Rmd   # Plantilla de minuta PDF por liceo
│   ├── resumen_territorio.Rmd       # Plantilla de reporte territorial
│   └── reporte_docentes_completo.Rmd
├── docs/
│   └── Reportes 2018-2025/      # PDFs de análisis histórico de matrícula
└── www/
    ├── custom.css               # Estilos de la app
    └── pdfs/                    # Documentos de referencia metodológica
```

---

## Cómo reproducir el pipeline de datos

Los datos procesados en `data/app/` se generan a partir de bases públicas. El script `scripts/preparar_datos.R` realiza todo el proceso de limpieza, cruce y transformación.

**Requisito previo:** tener descargadas las bases brutas desde las fuentes oficiales (ver sección de fuentes más abajo) y configurar las rutas en `scripts/config.R`.

```r
source("scripts/preparar_datos.R")
```

El script genera los siguientes archivos `.rds` en `data/app/`:

| Archivo | Contenido |
|---|---|
| `matricula.rds` | Matrícula EMTP 2025 a nivel de estudiante |
| `docentes.rds` / `docentes_long.rds` | Dotación docente EMTP 2025 |
| `base_apoyo.rds` | SIMCE, IVE, GSE y significancia estadística por RBD |
| `idps_dimensiones.rds` | IDPS por dimensión y subdimensión |
| `egresados.rds` | Egresados EMTP 2024 |
| `continuidad.rds` | Continuidad en educación superior 2025 |
| `titulados.rds` | Titulados técnico-profesionales 2024 |
| `comunas.rds` | Directorio de establecimientos con coordenadas |

---

## Fuentes de datos

Todos los datos utilizados son de acceso público:

| Dato | Fuente | Año |
|---|---|---|
| Matrícula por estudiante | MINEDUC — Centro de Estudios | 2025 |
| Directorio oficial de establecimientos | MINEDUC — SIGE | 2025 |
| Dotación docente | MINEDUC — CPEIP | 2025 |
| SIMCE 2° Medio | Agencia de Calidad de la Educación | 2025 |
| IDPS 2° Medio | Agencia de Calidad de la Educación | 2025 |
| Índice de Vulnerabilidad Escolar (IVE) | JUNAEB | 2025 |
| Egresados Educación Media | MINEDUC — Centro de Estudios | 2024 |
| Titulados Técnico-Profesional | MINEDUC — Centro de Estudios | 2024 |
| Matrícula Educación Superior | SIES — MINEDUC | 2025 |

---

## Requisitos técnicos

- R ≥ 4.2
- Los paquetes principales se declaran al inicio de `app.R`. Los más relevantes: `shiny`, `shinydashboard`, `plotly`, `leaflet`, `DT`, `openxlsx`, `dplyr`, `tidyr`, `httr2`.

---

## Chatbot (Asistente EMTP)

El chatbot usa un esquema RAG sencillo sobre los datos procesados de la app, con llamadas a la API de [Groq](https://groq.com). Para habilitarlo es necesario crear un archivo `.Renviron` en la raíz del proyecto con tu propia clave:

```
GROQ_API_KEY=tu_clave_aqui
```

Este archivo **no está versionado**. Sin la clave el resto de la app funciona normalmente; solo el chatbot queda deshabilitado.

---

## Despliegue en shinyapps.io

```r
library(rsconnect)
rsconnect::deployApp(
  appDir    = ".",
  appName   = "nombre-de-tu-app",
  account   = "tu-cuenta-shinyapps"
)
```

---

## Licencia y uso

Los datos son de acceso público y propiedad del Ministerio de Educación de Chile, JUNAEB y la Agencia de Calidad de la Educación. Esta aplicación no tiene afiliación oficial con ninguna de estas instituciones. El código está disponible libremente como referencia para proyectos similares de análisis educativo.
