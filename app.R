# ============================================================================
# VISUALIZADOR EMTP - Aplicación Shiny
# ============================================================================
# Para actualizar datos: editar scripts/config.R y correr
#   Rscript scripts/preparar_datos.R
# ============================================================================

# Locale UTF-8 para evitar warnings de traducción de tildes al leer los .rds
suppressWarnings(try(Sys.setlocale("LC_ALL", "en_US.UTF-8"), silent = TRUE))

library(shiny)
library(shinyjs)
library(leaflet)
library(dplyr)
library(sf)
library(colorspace)
library(stringr)
library(tidyr)
library(zip)
library(purrr)
library(rmarkdown)
library(plotly)
library(DT)
library(shinythemes)
library(openxlsx)

source("R/chatbot_rag.R")

# scripts/config.R busca los CSV en 'datos brutos/' (es config del PIPELINE) y haría
# stop() en el servidor, donde esos archivos no se despliegan. La app solo necesita
# las constantes de año, que se definen ANTES de las búsquedas de archivos. Por eso
# lo cargamos de forma TOLERANTE en un entorno aislado: las constantes quedan
# disponibles aunque la búsqueda de rutas falle por falta de 'datos brutos/'.
.cfg_env <- new.env()
try(sys.source("scripts/config.R", envir = .cfg_env), silent = TRUE)
ANIO_TITULADOS <- if (!is.null(.cfg_env$ANIO_TITULADOS)) .cfg_env$ANIO_TITULADOS else 2024L

# =============================================================================
# CARGA DE DATOS (pre-procesados por scripts/preparar_datos_deployment.R)
# Para actualizar: editar scripts/config.R → correr el script → deployApp()
# =============================================================================

suppressWarnings({
matricula_raw              <- readRDS("data/app/matricula.rds")
base_apoyo                 <- readRDS("data/app/base_apoyo.rds")
docentes_raw               <- readRDS("data/app/docentes.rds")
docentes_idich             <- readRDS("data/app/docentes_idich.rds")
docentes_especialidad_long <- readRDS("data/app/docentes_long.rds")
egresados_2024             <- readRDS("data/app/egresados.rds")
continuidad_es             <- readRDS("data/app/continuidad.rds")
comunas                    <- readRDS("data/app/comunas.rds")
idps_dimensiones           <- readRDS("data/app/idps_dimensiones.rds")
titulados                  <- readRDS("data/app/titulados.rds")
.meta                      <- readRDS("data/app/meta.rds")
})

indicadores_continuidad    <- .meta$indicadores_continuidad
dic_especialidades         <- .meta$dic_especialidades
mapeo_dependencias         <- .meta$mapeo_dependencias
choices_especialidades_doc <- .meta$choices_especialidades_doc
choices_especialidades     <- .meta$choices_especialidades

rm(.meta); gc(verbose = FALSE)

cat("✓ Datos cargados — matrícula:", format(nrow(matricula_raw), big.mark = "."),
    "| docentes:", format(nrow(docentes_raw), big.mark = "."),
    "| egresados:", format(nrow(egresados_2024), big.mark = "."), "\n")

# Función auxiliar para convertir códigos a nombres de dependencia
obtener_nombre_dependencia <- function(cod) {
  if (is.na(cod) || cod == "") return("Desconocida")
  cod_char <- as.character(cod)
  nom <- mapeo_dependencias$nom_depe[mapeo_dependencias$cod_depe2 == cod_char]
  if (length(nom) == 0) return("Desconocida")
  return(nom[1])
}

# ---- Tema Global Plotly ----
# Tema global Plotly
# Tipografía y paleta de gráficos (estándar editorial: OWID / Datawrapper)
PLOTLY_FONT <- list(family = "Inter, Roboto, 'Segoe UI', system-ui, sans-serif",
                    size = 13, color = "#3A4754")
# Paleta categórica institucional armónica (hasta 8 series)
PALETA_CAT <- c("#34536A", "#B35A5A", "#3C7F6D", "#C2A869",
                "#6E5F80", "#5A6E79", "#7FA8B5", "#9C7A6B")

plotly_theme <- list(
  font = PLOTLY_FONT,
  paper_bgcolor = "white",
  plot_bgcolor = "white",
  margin = list(l = 60, r = 24, t = 28, b = 44),
  xaxis = list(showgrid = TRUE, gridcolor = "#EEF1F4", gridwidth = 1,
               zeroline = FALSE, showline = FALSE, ticks = "outside",
               tickcolor = "#D7DEE6", ticklen = 4,
               tickfont = list(size = 12, color = "#6B7785"),
               title = list(font = list(size = 12.5, color = "#6B7785")),
               automargin = TRUE),
  yaxis = list(showgrid = TRUE, gridcolor = "#EEF1F4", gridwidth = 1,
               zeroline = FALSE, showline = FALSE, ticks = "",
               tickfont = list(size = 12, color = "#6B7785"),
               title = list(font = list(size = 12.5, color = "#6B7785")),
               automargin = TRUE),
  legend = list(bgcolor = "rgba(0,0,0,0)", bordercolor = "rgba(0,0,0,0)",
                font = list(size = 12, color = "#3A4754")),
  hoverlabel = list(bgcolor = "white", bordercolor = "#D7DEE6",
                    font = list(family = "Inter, Roboto, sans-serif", size = 12.5, color = "#2B3440")),
  colorway = PALETA_CAT
)

# Aplica el tema editorial y limpia la barra de herramientas (look estático tipo Datawrapper)
apply_plotly_theme <- function(p) {
  p <- layout(p,
              font = plotly_theme$font,
              paper_bgcolor = plotly_theme$paper_bgcolor,
              plot_bgcolor = plotly_theme$plot_bgcolor,
              margin = plotly_theme$margin,
              xaxis = plotly_theme$xaxis,
              yaxis = plotly_theme$yaxis,
              legend = plotly_theme$legend,
              hoverlabel = plotly_theme$hoverlabel,
              colorway = plotly_theme$colorway,
              separators = ",.")   # decimal "," y miles "." (formato chileno)
  p <- plotly::config(p, displayModeBar = FALSE, responsive = TRUE,
                      locale = "es")
  return(p)
}

# --- UI Shiny
ui <- fluidPage(
  useShinyjs(),  # Necesario para show/hide
  
  # Pantalla de carga (se muestra mientras cargan los datos)
  div(id = "loading-screen",
      style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; 
               background: linear-gradient(135deg, #34536A, #2A4255); 
               z-index: 9999; display: flex; align-items: center; justify-content: center;
               flex-direction: column;",
      tags$div(
        style = "text-align: center; color: white;",
        tags$div(
          class = "spinner",
          style = "border: 8px solid rgba(255,255,255,0.2);
                   border-top: 8px solid white;
                   border-radius: 50%;
                   width: 80px;
                   height: 80px;
                   animation: spin 1s linear infinite;
                   margin: 0 auto 30px auto;"
        ),
        tags$h2("Cargando Explorador de Datos EMTP", 
                style = "font-family: 'Roboto', sans-serif; font-weight: 300; margin-bottom: 10px;"),
        tags$p("Preparando datos 2025...", 
               style = "font-size: 16px; opacity: 0.9;"),
        tags$style(HTML("
          @keyframes spin {
            0% { transform: rotate(0deg); }
            100% { transform: rotate(360deg); }
          }
        "))
      )
  ),
  
  # Contenido principal (oculto inicialmente)
  shinyjs::hidden(
    div(id = "main-content",
        navbarPage(
          title = "Explorador de Datos EMTP 2025",
          id = "navbar",
          theme = shinytheme("flatly"),
          windowTitle = "Explorador EMTP",
          collapsible = TRUE,
          
          header = tagList(
            tags$head(
              # Tipografía (Inter para UI, Roboto de respaldo)
              tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
              tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&family=Roboto:wght@300;400;500;700&display=swap"),
              # Font Awesome para iconos
              tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
              
              
              tags$style(HTML("
    /* PALETA ÚNICA (fuente de verdad de la app). Nota: www/custom.css NO se
       carga; este bloque en línea es el único stylesheet propio activo. */
    :root {
      --color-bg:#F4F6F7; --color-text:#2C3E50; --color-primary:#34536A; --color-primary-dark:#2A4255;
      --color-accent-green:#5A6E79; --color-accent-red:#B35A5A; --color-accent-yellow:#C2A869; --color-accent-purple:#6E5F80;
      --color-border-light:#ECF0F1; --color-panel-border:#BDC3C7; --color-muted:#7F8C8D; --color-neutral:#FAFAFA;
      /* Nombres SEMÁNTICOS unificados (antes dispersos en custom.css) */
      --color-success:#3C7F6D; --color-warning:#C27D2A; --color-danger:#963A3A; --color-info:#3E6E8E;
      --focus-ring:#FFB347;
    }
    body { font-family:'Roboto',sans-serif; background:var(--color-bg); color:var(--color-text); }
    h1,h2,h3,h4,h5 { color:var(--color-text); font-weight:700; }
    .well { background:var(--color-neutral); border:none; border-radius:10px; box-shadow:0 2px 4px rgba(0,0,0,.08); transition:box-shadow .3s; }
    .well:hover { box-shadow:0 4px 8px rgba(0,0,0,.12); }
    .btn-primary { background:var(--color-primary); border:none; font-weight:600; border-radius:6px; transition:all .25s; }
    .btn-primary:hover { background:var(--color-primary-dark); transform:translateY(-1px); }
    /* btn-success ahora ES verde (acción positiva, ej. Buscar). El rojo se
       reserva para descargas por tipo de archivo y acciones de alerta. */
    .btn-success { background:var(--color-success); border:none; font-weight:600; border-radius:6px; transition:all .25s; }
    .btn-success:hover { background:#326b5c; transform:translateY(-1px); }
    .btn-secondary { background:var(--color-text); border:none; color:#fff; }
    .btn-secondary:hover { background:#1b2730; }
    .navbar { background:var(--color-primary)!important; box-shadow:0 2px 4px rgba(0,0,0,.1); }
    .navbar-default .navbar-nav>li>a, .navbar-brand { color:#ECF0F1!important; }
    .navbar-default .navbar-nav>li>a:hover { color:var(--color-accent-red)!important; }
    
    .info-card { background:linear-gradient(135deg,var(--color-primary),var(--color-accent-red)); color:#fff; padding:20px; border-radius:10px; margin-bottom:20px; box-shadow:0 4px 6px rgba(0,0,0,.12); }
    .metric-card { background:#fff; padding:18px 16px; border-radius:10px; text-align:center; box-shadow:0 2px 6px rgba(52,83,106,.08); border-left:4px solid var(--color-primary); transition:transform .2s, box-shadow .2s; }
    .metric-card:hover { transform:translateY(-3px); box-shadow:0 8px 18px rgba(52,83,106,.14); }
    .metric-number { font-size:2.1em; font-weight:800; color:var(--color-primary); line-height:1.1; letter-spacing:-.5px; }
    .metric-label { font-size:.78em; color:var(--color-muted); text-transform:uppercase; letter-spacing:.6px; font-weight:600; margin-top:4px; }
    .metric-ico { font-size:1.1em; opacity:.85; margin-bottom:2px; }
    /* Acento de color por métrica (borde, número e ícono) */
    .metric-mat { border-left-color:var(--color-primary); }
    .metric-mat .metric-number, .metric-mat .metric-ico { color:var(--color-primary); }
    .metric-est { border-left-color:var(--color-success); }
    .metric-est .metric-number, .metric-est .metric-ico { color:var(--color-success); }
    .metric-h { border-left-color:var(--color-info); }
    .metric-h .metric-number, .metric-h .metric-ico { color:var(--color-info); }
    .metric-m { border-left-color:var(--color-accent-purple); }
    .metric-m .metric-number, .metric-m .metric-ico { color:var(--color-accent-purple); }
    .loading-spinner { border:4px solid #f3f3f3; border-top:4px solid var(--color-primary); border-radius:50%; width:40px; height:40px; animation:spin 2s linear infinite; margin:20px auto; }
    @keyframes spin { 0%{transform:rotate(0deg);} 100%{transform:rotate(360deg);} }
    .update-box { border-left:5px solid var(--color-primary); padding:15px; background:var(--color-neutral); border-radius:6px; margin-bottom:20px; }
  .alert-info { background:#EEF3F5; border:1px solid #D3DBDF; color:#2C3E50; padding:15px; border-radius:6px; margin-bottom:20px; }
  .hero-title { color: var(--color-neutral); letter-spacing:.5px; text-shadow:0 1px 2px rgba(0,0,0,.35); }
  .hero-sub { opacity:.92; }
    table.dataTable thead th { background:var(--color-primary); color:#fff; }
    table.dataTable tbody tr.selected { background:rgba(52,83,106,0.15); }
  #descargar_minuta_pdf.btn, #descargar_minuta_pdf.shiny-download-link { opacity:1!important; pointer-events:auto!important; cursor:pointer!important; filter:none!important; }
  #descargar_minuta_pdf.btn.disabled, #descargar_minuta_pdf.shiny-download-link.disabled { background:var(--color-primary)!important; color:#fff!important; opacity:1!important; }
  #descargar_minuta_excel.btn, #descargar_minuta_excel.shiny-download-link { opacity:1!important; pointer-events:auto!important; cursor:pointer!important; filter:none!important; }
  #descargar_minuta_excel.btn.disabled, #descargar_minuta_excel.shiny-download-link.disabled { background:var(--color-primary)!important; color:#fff!important; opacity:1!important; }

  /* ===== Componentes UX (jun 2026): tarjetas de inicio, acordeones,
     badge de vigencia y ayudas. Movidos aquí desde custom.css (que no se carga). ===== */
  .nav-card { background:#fff; border:1px solid var(--color-border-light); border-left:4px solid var(--color-primary); border-radius:10px; padding:14px 16px; height:100%; box-shadow:0 1px 3px rgba(52,83,106,.06); transition:box-shadow .2s, transform .2s; }
  .nav-card:hover { box-shadow:0 6px 16px rgba(52,83,106,.12); transform:translateY(-2px); }
  .nav-card-title { margin:0 0 6px; font-size:16px; font-weight:700; }
  .nav-card p { margin:0; color:var(--color-muted); font-size:13px; line-height:1.5; }
  .nav-title-link, a.nav-title-link { color:var(--color-primary)!important; text-decoration:none!important; font-weight:700; cursor:pointer; border-bottom:1px solid transparent; transition:border-color .15s; }
  .nav-title-link:hover, a.nav-title-link:hover { text-decoration:none!important; border-bottom-color:currentColor; }
  .nav-title-link:focus { outline:2px solid var(--focus-ring); outline-offset:3px; border-radius:2px; }
  details.emtp-acc { border:1px solid var(--color-border-light); border-radius:8px; background:var(--color-neutral); margin-bottom:16px; overflow:hidden; }
  details.emtp-acc>summary { cursor:pointer; list-style:none; padding:14px 18px; font-weight:700; color:var(--color-text); background:#fff; display:flex; align-items:center; justify-content:space-between; }
  details.emtp-acc>summary::-webkit-details-marker { display:none; }
  details.emtp-acc>summary::after { content:'\\25BC'; font-size:11px; color:var(--color-muted); transition:transform .2s; }
  details.emtp-acc[open]>summary::after { transform:rotate(180deg); }
  details.emtp-acc>summary:hover { background:#f4f7fa; }
  details.emtp-acc>.emtp-acc-body { padding:4px 18px 16px; }
  .data-vintage-badge { display:inline-block; margin:0 6px 4px 0; padding:3px 9px; border-radius:12px; font-size:11px; font-weight:600; letter-spacing:.3px; background:rgba(255,255,255,.18); color:#fff; vertical-align:middle; }
  .help-tip { display:inline-block; width:15px; height:15px; line-height:15px; text-align:center; border-radius:50%; background:var(--color-muted); color:#fff; font-size:10px; font-weight:700; cursor:help; margin-left:4px; }
  .help-tip:hover { background:var(--color-primary); }

  /* ================= REFINAMIENTO UI PRO (jun 2026) — estándar editorial ================= */
  body, .container-fluid { font-family:'Inter','Roboto','Segoe UI',system-ui,-apple-system,sans-serif; -webkit-font-smoothing:antialiased; -moz-osx-font-smoothing:grayscale; color:#2B3440; font-size:15px; line-height:1.55; }
  h1,h2,h3,h4,h5,h6 { letter-spacing:-.01em; }

  /* Paneles tipo tarjeta (.panel-custom no tenía estilo propio) */
  .panel-custom { background:#fff; border:1px solid #E8ECF1; border-radius:14px; box-shadow:0 1px 2px rgba(16,36,53,.04), 0 4px 16px rgba(16,36,53,.04); padding:22px; margin-bottom:22px; }
  .panel-custom > h4:first-child, .well > h4:first-child { font-size:.9rem; text-transform:uppercase; letter-spacing:.5px; color:#51606E!important; font-weight:700; padding-bottom:11px; border-bottom:1px solid #EEF1F4; margin:0 0 16px; }
  .well { background:#fff; border:1px solid #E8ECF1; border-radius:14px; box-shadow:0 1px 2px rgba(16,36,53,.04), 0 4px 16px rgba(16,36,53,.04); padding:20px; }

  /* Métricas */
  .metric-card { border:1px solid #E8ECF1; border-radius:14px; box-shadow:0 1px 2px rgba(16,36,53,.04), 0 3px 12px rgba(16,36,53,.05); }
  .metric-card h2, .metric-card h3 { color:#1F2A37; font-weight:800; letter-spacing:-.02em; font-variant-numeric:tabular-nums; }
  .metric-card h5 { color:#6B7785; font-size:12px; text-transform:uppercase; letter-spacing:.4px; font-weight:600; margin:0 0 2px; }
  .metric-number { font-variant-numeric:tabular-nums; }

  /* Navbar */
  .navbar { background:linear-gradient(180deg,#3A5C75,#34536A)!important; box-shadow:0 2px 12px rgba(16,36,53,.18)!important; min-height:56px; border:none!important; }
  .navbar-default .navbar-nav>li>a { font-size:14px; padding:18px 16px; transition:background .15s; }
  .navbar-default .navbar-nav>li>a:hover { background:rgba(255,255,255,.10)!important; color:#fff!important; }
  .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover { background:rgba(255,255,255,.16)!important; color:#fff!important; box-shadow:inset 0 -3px 0 var(--color-accent-red); }
  .navbar-brand { font-weight:700; letter-spacing:.3px; }

  /* Sub-pestañas (tabsetPanel) */
  .nav-tabs { border-bottom:1px solid #E2E8EE; margin-bottom:18px; }
  .nav-tabs>li>a { background:transparent!important; color:#5A6B7B!important; border:none!important; border-radius:8px 8px 0 0; padding:9px 16px; font-weight:600; margin-right:2px; }
  .nav-tabs>li>a:hover { background:#EEF2F6!important; color:var(--color-primary)!important; }
  .nav-tabs>li.active>a, .nav-tabs>li.active>a:hover, .nav-tabs>li.active>a:focus { background:transparent!important; color:var(--color-primary)!important; border:none!important; box-shadow:inset 0 -3px 0 var(--color-primary); }

  /* Formularios */
  .control-label, .shiny-input-container>label { font-size:12.5px!important; font-weight:600!important; color:#51606E!important; letter-spacing:.2px; margin-bottom:5px; }
  .form-control, .selectize-input { min-height:40px!important; border:1px solid #D9E0E7!important; border-radius:9px!important; font-size:13.5px!important; color:#2B3440!important; box-shadow:none!important; }
  .selectize-input { display:flex; align-items:center; flex-wrap:wrap; padding:7px 12px; }
  .form-control:focus, .selectize-input.focus { border-color:var(--color-primary)!important; box-shadow:0 0 0 3px rgba(52,83,106,.12)!important; }
  .selectize-dropdown { border-radius:9px; border:1px solid #D9E0E7; font-size:13.5px; box-shadow:0 8px 24px rgba(16,36,53,.12); }
  .selectize-dropdown .active { background:#EDF2F6; color:var(--color-primary); }
  .selectize-input>.item { background:#EAEFF3; border:1px solid #D9E0E7!important; border-radius:6px; color:#34536A; }

  /* Botones */
  .btn { border-radius:9px; font-weight:600; letter-spacing:.2px; }
  .btn-warning { background:#F2F5F8!important; color:#51606E!important; border:1px solid #D9E0E7!important; }
  .btn-warning:hover { background:#E7ECF1!important; color:#34536A!important; }

  /* Foco accesible (solo teclado) */
  *:focus:not(:focus-visible) { outline:none!important; }
  *:focus-visible { outline:2px solid var(--color-primary)!important; outline-offset:2px; }

  /* Tablas DT */
  .dataTables_wrapper { font-size:13.5px; }
  table.dataTable thead th { background:#F3F6F9!important; color:#33455A!important; font-weight:700; font-size:12px; text-transform:uppercase; letter-spacing:.3px; border-bottom:2px solid #E2E8EE!important; }
  table.dataTable tbody td { padding:9px 12px!important; color:#2B3440; }
  table.dataTable tbody tr:hover { background:#F5F8FB!important; }
  table.dataTable tbody tr.selected { background:rgba(52,83,106,.12)!important; color:#1F2A37; }
  .dataTables_wrapper .dataTables_paginate .paginate_button.current { background:var(--color-primary)!important; color:#fff!important; border:none!important; border-radius:6px; }
  .dataTables_wrapper .dataTables_filter input { border:1px solid #D9E0E7; border-radius:7px; padding:4px 10px; }

  /* Gráficos / mapa */
  .js-plotly-plot, .leaflet-container { border-radius:10px; }
  .leaflet-popup-content-wrapper { border-radius:10px; }
  .leaflet-popup-content { font-family:'Inter','Roboto',sans-serif; margin:12px 14px; }

  /* Alertas */
  .alert { border-radius:10px; }
  .alert-warning { background:#FBF3E4!important; border:1px solid #EAD9B6!important; color:#5A4B2E!important; }

  /* Scrollbar */
  ::-webkit-scrollbar { width:10px; height:10px; }
  ::-webkit-scrollbar-thumb { background:#C7D0DA; border-radius:6px; border:2px solid #F4F6F7; }
  ::-webkit-scrollbar-thumb:hover { background:#A9B6C4; }
  "))
            ),
          ),
          
          # --- Pestaña Inicio ---
          tabPanel(
            title = tagList(icon("home"), "Inicio"),
            value = "tab_inicio",
            fluidPage(
              div(class = "info-card",
                  fluidRow(
                    column(8,
                           h1("Explorador de Datos EMTP 2025", class="hero-title", style="margin:0;"),
                           p("Sistema integrado de visualización y análisis de datos de Educación Media Técnico Profesional",
                             class="hero-sub", style="margin:5px 0 0 0;"),
                           # NUEVO: badge de vigencia de datos siempre visible en el inicio
                           tags$div(style="margin-top:10px;",
                             tags$span(class="data-vintage-badge",
                               tags$i(class="fas fa-database"), " Matrícula y docentes 2025"),
                             tags$span(class="data-vintage-badge",
                               tags$i(class="fas fa-chart-bar"), " SIMCE · IDPS 2025 · IVE 2026"),
                             tags$span(class="data-vintage-badge",
                               tags$i(class="fas fa-user-graduate"), " Egresados y titulados 2024")
                           )
                    ),
                    column(4,
                           tags$div(tags$i(class="fas fa-chart-line fa-3x", style="float:right; opacity:0.7;"))
                    )
                  )
              ),
              # Métricas principales (con acento de color e ícono por métrica)
              fluidRow(
                column(3,
                       div(class="metric-card metric-mat",
                           div(class="metric-ico", tags$i(class="fas fa-users")),
                           div(class="metric-number", textOutput("kpi_total_matricula_inicio")),
                           div(class="metric-label", "Matrícula EMTP")
                       )
                ),
                column(3,
                       div(class="metric-card metric-est",
                           div(class="metric-ico", tags$i(class="fas fa-school")),
                           div(class="metric-number", textOutput("kpi_establecimientos_inicio")),
                           div(class="metric-label", "Establecimientos")
                       )
                ),
                column(3,
                       div(class="metric-card metric-h",
                           div(class="metric-ico", tags$i(class="fas fa-mars")),
                           div(class="metric-number", textOutput("kpi_hombres_inicio")),
                           div(class="metric-label", "Hombres")
                       )
                ),
                column(3,
                       div(class="metric-card metric-m",
                           div(class="metric-ico", tags$i(class="fas fa-venus")),
                           div(class="metric-number", textOutput("kpi_mujeres_inicio")),
                           div(class="metric-label", "Mujeres")
                       )
                )
              ),
              br(),
              # Segunda fila: indicadores de trayectoria EMTP
              fluidRow(
                column(3, div(class="metric-card", style="border-left:6px solid #C2A869;",
                  div(class="metric-number", style="color:var(--color-text)",
                      n_distinct(matricula_raw$nom_espe[!is.na(matricula_raw$nom_espe)])),
                  div(class="metric-label", "Especialidades",
                      tags$br(),
                      tags$small(style="font-weight:400;color:#8a8a8a;font-size:0.78em;",
                                 "incluye especialidades EPJA")))),
                column(3, div(class="metric-card", style="border-left:6px solid #5A6E79;",
                  div(class="metric-number", style="color:var(--color-text)",
                      format(nrow(egresados_2024), big.mark=".")),
                  div(class="metric-label", "Egresados EMTP 2024"))),
                column(3, div(class="metric-card", style="border-left:6px solid #3C7F6D;",
                  div(class="metric-number", style="color:#3C7F6D",
                      paste0(gsub("\\.", ",", as.character(indicadores_continuidad$pct_continuidad)), "%")),
                  div(class="metric-label", "Continuidad en Ed. Superior"))),
                column(3, div(class="metric-card", style="border-left:6px solid #6E5F80;",
                  div(class="metric-number", style="color:var(--color-text)",
                      format(nrow(titulados), big.mark=".")),
                  div(class="metric-label", "Titulados TP 2024")))
              ),

              br(),

              # ============================================================
              # 1. ACERCA DEL SISTEMA
              # ============================================================
              wellPanel(
                h3(tags$i(class="fas fa-info-circle"), " Acerca del Sistema"),
                p("Esta aplicación permite visualizar y descargar información completa de la matrícula y otros datos de apoyo 
          de la Educación Media Técnico Profesional (EMTP) correspondientes al año 2025."),
                
                tags$div(class = "alert-info",
                         tags$strong("Importante: "), 
                         "Los datos presentados corresponden a estudiantes de 3° y 4° medio asociados a una especialidad EMTP, 
                 salvo en el caso de estudiantes adultos, donde se incluyen también los del 1° Nivel (equivalente a 1° y 2° medio)."
                ),
                
                tags$div(class = "alert-info", style = "margin-top: 10px;",
                         tags$strong("Criterio de selección: "), 
                         "Solo se incluyen establecimientos educacionales declarados como 'Funcionando' según la base de datos oficial."
                ),
                 tags$div(class = "alert-info", style = "margin-top: 10px;",
                         tags$strong("Nota: "), 
                         "La matrícula total incluye tanto la formación regular como la de personas jóvenes y adultas (EPJA). Los filtros permiten desagregar esta información según corresponda. Asimismo, el total de especialidades se construye a partir de los registros del SIGE para formación regular y EPJA, por lo que se reportan, según código, más de las 35 especialidades definidas en el Currículum Nacional de la EMTP para el ciclo regular."
                )
              ),
              
              # ============================================================
              # 2. ADVERTENCIA: DATOS 2024-2025
              # ============================================================
              tags$div(
                class = "alert alert-warning",
                style = "margin: 20px 0; padding: 15px; background-color: #fff3cd; border: 2px solid #ff9800; border-radius: 4px;",
                tags$div(
                  style = "display: flex; align-items: flex-start;",
                  tags$i(class="fas fa-calendar-check fa-2x", style="color: #cc6600; margin-right: 15px; margin-top: 3px;"),
                  tags$div(
                    tags$h4(style="margin-top: 0; color: #662200; font-weight: bold; font-size: 16px;",
                            tags$strong("Vigencia de los datos por fuente")),
                    tags$p("Cada indicador usa la versión más reciente publicada por su fuente oficial. La continuidad y la titulación se miden sobre la cohorte de egreso del año anterior, por lo que provienen de bases distintas:",
                           style="margin-bottom: 8px; color: #333333; font-weight: 500; line-height: 1.5;"),
                    tags$ul(style="margin: 0 0 0 4px; color:#333; font-weight:500; line-height:1.6;",
                      tags$li(tags$strong("2025: "), "Matrícula EMTP, docentes, matrícula en educación superior, SIMCE 2° medio e IDPS 2° medio."),
                      tags$li(tags$strong("2026: "), "Índice de Vulnerabilidad Escolar (IVE-JUNAEB)."),
                      tags$li(tags$strong("2024: "), "Egresados de enseñanza media y titulados Técnico-Profesional (cohorte base de continuidad y titulación)."))
                  )
                )
              ),
              
              # ============================================================
              # 3. FUENTES DE DATOS
              # ============================================================
              wellPanel(
                h3(tags$i(class="fas fa-database"), " Fuentes de Datos"),
                p("Los datos utilizados en esta aplicación provienen de fuentes oficiales del Ministerio de Educación de Chile:"),
                tags$ul(
                  tags$li(
                    tags$strong("Centro de Estudios MINEDUC - Datos Abiertos: "),
                    tags$a(href = "https://datosabiertos.mineduc.cl", 
                           target = "_blank",
                           "https://datosabiertos.mineduc.cl",
                           style = "color: var(--color-primary); font-weight: 600;")
                  ),
                  tags$li(
                    tags$strong("Agencia de Calidad de la Educación - Bases de Datos de acceso público: "),
                    tags$a(href = "https://informacionestadistica.agenciaeducacion.cl/#/bases", 
                           target = "_blank",
                           "https://informacionestadistica.agenciaeducacion.cl/#/bases",
                           style = "color: var(--color-primary); font-weight: 600;")
                  )
                ),
                tags$p(
                  style = "margin-top: 10px; font-size: 0.9em; color: var(--color-muted);",
                  tags$em("Esta aplicación procesa y visualiza datos públicos con fines de análisis educativo.")
                )
              ),
              
              # ============================================================
              # 4. NAVEGACIÓN DEL SISTEMA
              # ============================================================
              wellPanel(
                h3(tags$i(class="fas fa-compass"), " Navegación del Sistema"),
                # NUEVO: solo el TÍTULO de cada tarjeta es el enlace a la pestaña
                tags$p(style="font-size:12px; color:var(--color-muted); margin:-6px 0 12px;",
                       tags$i(class="fas fa-hand-pointer"), " Haz clic en el título de una tarjeta para ir a esa sección."),
                fluidRow(
                  column(3,
                         tags$div(class="nav-card", style="border-left-color: var(--color-primary);",
                           h4(class="nav-card-title",
                              actionLink("go_mapa", class="nav-title-link",
                                label = tagList(tags$i(class="fas fa-map-location-dot"), " Análisis Territorial"))),
                           p("Mapa de matrícula e indicadores del territorio (SIMCE, IDPS, IVE, GSE, asistencia) según filtros, con descarga de minutas territoriales.")
                         )
                  ),
                  column(3,
                         tags$div(class="nav-card", style="border-left-color: var(--color-accent-red);",
                           h4(class="nav-card-title",
                              actionLink("go_buscador", class="nav-title-link",
                                label = tagList(tags$i(class="fas fa-school"), " Establecimientos"))),
                           p("Busca por RBD, nombre o filtros; consulta la ficha de cada liceo (ubicación, especialidades, SIMCE e IDPS) y descarga minutas.")
                         )
                  ),
                  column(3,
                         tags$div(class="nav-card", style="border-left-color: var(--color-primary-dark);",
                           h4(class="nav-card-title",
                              actionLink("go_viz", class="nav-title-link",
                                label = tagList(tags$i(class="fas fa-chart-bar"), " Visualizaciones"))),
                           p("Explora datos mediante gráficos interactivos, tablas dinámicas y análisis comparativos.")
                         )
                  ),
                  column(3,
                         tags$div(class="nav-card", style="border-left-color: #B35A5A;",
                           h4(class="nav-card-title",
                              actionLink("go_docentes", class="nav-title-link",
                                label = tagList(tags$i(class="fas fa-chalkboard-teacher"), " Docentes"))),
                           p("Analiza información de docentes EMTP: género, dependencia, ruralidad, experiencia y distribución territorial.")
                         )
                  )
                ),
                fluidRow(
                  column(6,
                         tags$div(class="nav-card", style="border-left-color: #3C7F6D; margin-top: 15px;",
                           h4(class="nav-card-title",
                              actionLink("go_egresados", class="nav-title-link",
                                label = tagList(tags$i(class="fas fa-user-graduate"), " Egresados y Titulados"))),
                           p("Analiza egresados EMTP 2024, continuidad de estudios en educación superior 2025 y titulación Técnico-Profesional (tasa al año de egreso, especialidad y sector económico).")
                         )
                  ),
                  column(6,
                         # La tarjeta del asistente abre el chatbot flotante (no es una pestaña)
                         tags$div(class="nav-card", style="border-left-color: #7B1FA2; margin-top: 15px;",
                           h4(class="nav-card-title",
                              tags$a(href="#", class="nav-title-link", onclick="chatbotToggle(); return false;",
                                     tags$i(class="fas fa-robot"), " Asistente EMTP"),
                              tags$span("PROTOTIPO", style="font-size:10px; background:#7B1FA2; color:white; border-radius:3px; padding:1px 5px; margin-left:6px; vertical-align:middle;")),
                           p("Chatbot con acceso a datos de matrícula, docentes y establecimientos. También responde preguntas sobre ",
                             tags$strong("normativa oficial"), " (Decreto 452, Ley 20370, REX 1080, entre otros).",
                             tags$em(style="color:#888; font-size:12px;", " Actualmente en fase de desarrollo — las respuestas pueden contener errores."))
                         )
                  )
                )
              ),
              
              # ============================================================
              # 5. REPORTES HISTÓRICOS 2018-2025
              # ============================================================
              wellPanel(
                h3(tags$i(class="fas fa-chart-line"), " Reportes Históricos de Matrícula EMTP 2018-2025"),
                p("Descarga análisis detallados de evolución de matrícula EMTP con datos históricos 2018-2025. Selecciona el nivel de análisis:"),
                
                fluidRow(
                  # NIVEL NACIONAL
                  column(6,
                         tags$div(style="text-align: center; padding: 15px; border: 2px solid #34536A; border-radius: 8px; margin: 5px; background: #f8f9fa;",
                                  tags$i(class="fas fa-flag fa-2x", style="color: #34536A; margin-bottom: 10px;"),
                                  h5(tags$strong("Nacional"), style="color: #34536A;"),
                                  p("Análisis agregado nacional EMTP", style="font-size: 12px; color: #555;"),
                                  downloadButton("download_reporte_nacional_pdf", "Descargar PDF", class = "btn-primary btn-sm", style="width: 98%; margin: 2px;")
                         )
                  ),
                  
                  # NIVEL REGIÓN
                  column(6,
                         tags$div(style="text-align: center; padding: 15px; border: 2px solid #B35A5A; border-radius: 8px; margin: 5px; background: #f8f9fa;",
                                  tags$i(class="fas fa-map-marked-alt fa-2x", style="color: #B35A5A; margin-bottom: 10px;"),
                                  h5(tags$strong("Por Región"), style="color: #B35A5A;"),
                                  selectInput("select_region_reporte", "Selecciona Región:", 
                                              choices = c("Antofagasta" = "Antofagasta",
                                                          "Arica y Parinacota" = "Arica y Parinacota",
                                                          "Atacama" = "Atacama",
                                                          "Aysén" = "Aysén",
                                                          "Biobío" = "Biobío",
                                                          "Coquimbo" = "Coquimbo",
                                                          "La Araucanía" = "La Araucanía",
                                                          "Los Lagos" = "Los Lagos",
                                                          "Los Ríos" = "Los Ríos",
                                                          "Magallanes" = "Magallanes",
                                                          "Maule" = "Maule",
                                                          "Metropolitana" = "Metropolitana",
                                                          "O'Higgins" = "O'Higgins",
                                                          "Tarapacá" = "Tarapacá",
                                                          "Valparaíso" = "Valparaíso",
                                                          "Ñuble" = "Ñuble"),
                                              selected = "Metropolitana"),
                                  downloadButton("download_reporte_region_pdf", "Descargar PDF", class = "btn-primary btn-sm", style="width: 98%; margin: 2px;")
                         )
                  )
                ),
                
                tags$div(class = "alert alert-info", style="margin-top: 15px;",
                         icon("info-circle"), " ",
                         strong("Nota: "), "Estos reportes contienen análisis de tendencias 2018-2025 con gráficos, tablas y estadísticas descriptivas de la evolución de la matrícula EMTP."
                )
              ),
              
              # ============================================================
              # 6. DESCARGA DE BASES DE DATOS
              # ============================================================
              wellPanel(
                h3(tags$i(class="fas fa-download"), " Descarga de Bases de Datos"),
                p("Descarga las bases completas en diferentes formatos:"),
                
                fluidRow(
                  column(4,
                         tags$div(style="text-align: center; padding: 15px; border: 1px solid #ddd; border-radius: 8px; margin: 5px;",
                                  tags$i(class="fas fa-users fa-2x", style="color: var(--color-primary);"),
                                  h5("Matrícula EMTP 2025"),
                                  downloadButton("descargar_matricula_csv", "Descargar (.csv)", class = "btn-primary btn-sm")
                         )
                  ),
                  column(4,
                         tags$div(style="text-align: center; padding: 15px; border: 1px solid #ddd; border-radius: 8px; margin: 5px;",
                                  tags$i(class="fas fa-school fa-2x", style="color: var(--color-accent-red);"),
                                  h5("Base de Establecimientos 2025"),
                                  downloadButton("descargar_base_apoyo_csv", "Descargar (.csv)", class = "btn-primary btn-sm")
                         )
                  ),
                  column(4,
                         tags$div(style="text-align: center; padding: 15px; border: 1px solid #ddd; border-radius: 8px; margin: 5px;",
                                  tags$i(class="fas fa-chalkboard-teacher fa-2x", style="color: var(--color-primary-dark);"),
                                  h5("Docentes EMTP 2025"),
                                  downloadButton("descargar_docentes_emtp_csv", "Descargar (.csv)", class = "btn-primary btn-sm")
                         )
                  )
                )
              ),
              
              # ============================================================
              # 6b. NOTAS METODOLÓGICAS Y CORRECCIONES DE DATOS
              # ============================================================
              # NUEVO: notas metodológicas colapsables (acordeón)
              tags$details(
                class = "emtp-acc",
                tags$summary(tags$span(tags$i(class="fas fa-exclamation-triangle", style="color:#e67e22;"), " Notas Metodológicas y Correcciones de Datos")),
                tags$div(class = "emtp-acc-body",
                tags$p(style="font-size:13px; color:#555; margin-bottom:10px;",
                  "Las siguientes notas documentan limitaciones conocidas de las bases oficiales Mineduc 2025 y correcciones aplicadas en esta plataforma."
                ),
                tags$div(class="alert alert-warning", style="margin-bottom:10px;",
                  tags$strong(tags$i(class="fas fa-book"), " Diferencias de taxonomía entre bases Mineduc (Matrícula vs. Docentes)"),
                  tags$br(), tags$br(),
                  HTML("La Matrícula Única 2025 y el Directorio de Docentes 2025 utilizan <strong>dos catálogos distintos para la variable Especialidad/Subsector</strong>. Esto impide cruzar directamente ambas fuentes en algunos casos. Casos detectados:
                  <ul style='margin:6px 0 4px 20px; font-size:13px;'>
                    <li><strong>Gastronomía</strong> (61003 en matrícula, ~14.857 estudiantes): figura como <em>Servicio de Alimentación Colectiva</em> (61002) en el directorio docente.</li>
                    <li><strong>Programación</strong> (58034) y <strong>Conectividad y Redes</strong> (58033): ambos figuran bajo 58035 <em>Telecomunicaciones (Redes)</em> en docentes.</li>
                  </ul>")
                ),
                tags$div(class="alert alert-secondary", style="font-size:12px; padding:8px 12px;",
                  HTML("<strong><i class='fas fa-database'></i> Corrección puntual aplicada — RBD 25824:</strong> El Liceo Sergio Silva Bascuñán (La Pintana) figura en las bases oficiales Mineduc 2025 con dependencia SLEP (COD_DEPE2=5) por error de codificación en la fuente. Se ha corregido a Particular Subvencionado (COD_DEPE2=2), en concordancia con la matrícula 2024 y su reconocimiento oficial. Esta corrección aplica a todas las pestañas de la plataforma.")
                )
              )),

              # ============================================================
              # 7. ACTUALIZACIONES RECIENTES (CHANGELOG)
              # ============================================================
              # NUEVO: changelog colapsable (acordeón) para acortar el scroll del Inicio
              tags$details(
                class = "emtp-acc",
                tags$summary(tags$span(tags$i(class="fas fa-history"), " Historial de Actualizaciones")),
                tags$div(class = "emtp-acc-body",
                tags$ul(
                  tags$li(tags$strong("18 de junio de 2026:"), " Reconstrucción completa del pipeline de datos desde las bases brutas oficiales. Se incorporaron ",
                    tags$strong("SIMCE 2° medio (por Estándares de Aprendizaje), IDPS, IVE y Titulados TP"), ". La pestaña ", tags$em("Establecimientos"),
                    " unifica búsqueda y ficha por liceo (ubicación en mapa, especialidades, SIMCE e IDPS). Nueva sub-pestaña de ", tags$em("Titulados TP"),
                    " con tasa de titulación al año de egreso y sector económico de la práctica."),
                  tags$li(tags$strong("9 de junio de 2026:"), " Se agregó ",
                    tags$strong("Asistente EMTP (chatbot)"), ": consultas en lenguaje natural sobre matrícula, docentes, establecimientos y normativa. Responde preguntas por región, comuna, dependencia, especialidad y RBD específico. Integra documentos oficiales (Decreto 452, Ley 20370, REX 1080, entre otros). Funciona como prototipo en desarrollo."),
                  tags$li(tags$strong("4 de mayo de 2026:"), " Correcciones en pestaña Docentes: (1) universo corregido a 18.957 cargos / 18.766 personas en 969 EE con matrícula activa, (2) KPIs ahora muestran totales generales independientes de filtros, (3) corrección de dependencia RBD 25824 (SLEP→Part. Subvencionado), (4) notas metodológicas sobre diferencias de taxonomía Mineduc."),
                  tags$li(tags$strong("9 de abril de 2026:"), " Se agregaron fuentes de datos oficiales y licencia MIT"),
                  tags$li(tags$strong("19 de enero de 2026:"), " Nueva pestaña ", tags$em("Egresados y Titulados"), " con 2 sub-pestañas: (1) Egresados EMTP 2024, (2) Continuidad de Estudios en ES 2025 con análisis de género"),
                  tags$li(tags$strong("19 de enero de 2026:"), " Tabla de Egresados agrupada por RBD mostrando cantidad de egresados y promedio de notas por establecimiento"),
                  tags$li(tags$strong("15 de enero de 2026:"), " Se actualizó Base de Establecimientos con datos 2025: matrícula por especialidad, dependencia administrativa y nombre SLEP (970 EE activos)"),
                  tags$li(tags$strong("9 de diciembre de 2025:"), " Se corrigió filtro de establecimientos, dejando solo EE activos según BBDD oficial (970 RBDs)"),
                  tags$li(tags$strong("5 de diciembre de 2025:"), " Se actualizó base de matrícula completa a 2025 (datos al 30/04/2025)"),
                  tags$li("14 de octubre de 2025: Se mejoró interfaz para navegación. Se agregó pestaña de análisis docentes EMTP"),
                  tags$li("28 de agosto de 2025: Se complementó la pestaña de Visualización con más opciones de filtro e información"),
                  tags$li("25 de agosto de 2025: Se agregó pestaña de Visualización con gráficos interactivos y tabla descargable por tipo de enseñanza, grado y especialidad")
                )
              )),
              
              # ============================================================
              # 8. CRÉDITOS Y LICENCIA
              # ============================================================
              tags$div(
                style = "margin: 30px 0 10px 0; padding: 15px 20px; background: #f0f4f8; border-top: 2px solid #d0dae6; border-radius: 4px; text-align: center;",
                tags$p(style = "margin: 0; color: #555; font-size: 13px; line-height: 1.8;",
                  tags$strong("Desarrollado por:"), " Andrés Lazcano A. — Sociólogo (UDP) · Magíster en Psicología Social (UAH) · Estudiante Magíster en Ciencias Sociales (U. de Chile)",
                  tags$br(),
                  tags$a(href = "https://www.linkedin.com/in/andr%C3%A9s-lazcano-455363202/",
                         target = "_blank",
                         style = "color: #0077b5; text-decoration: none; font-size: 13px;",
                         tags$i(class = "fab fa-linkedin", style = "margin-right: 5px;"),
                         "linkedin.com/in/andrés-lazcano-455363202")
                ),
                tags$p(style = "margin: 10px 0 0 0; color: #666; font-size: 12px; line-height: 1.6;",
                  tags$strong("Licencia:"), " MIT License — Esta aplicación es de código abierto y uso gratuito.",
                  tags$br(),
                  tags$em("Los datos utilizados son de acceso público y propiedad del Ministerio de Educación de Chile. Esta aplicación no tiene afiliación oficial con MINEDUC ni con la Agencia de Calidad de la Educación. Uso de datos con fines de análisis educativo y divulgación."),
                  tags$br(),
                  tags$span(style = "color: #888; font-size: 11px;",
                    tags$i(class = "fab fa-github", style = "margin-right: 5px;"),
                    "Repositorio en proceso de publicación en GitHub")
                )
              )
            )
          ),
          
          # --- Pestaña Análisis Territorial (mapa + indicadores + minutas) ---
          tabPanel(
            title = tagList(icon("map-location-dot"), "Análisis Territorial"),
            value = "tab_mapa",
            fluidPage(
              div(style = "text-align: center; margin-bottom: 20px;",
                  h3(icon("map-location-dot"), "Análisis Territorial de la EMTP", style = "color: #2C3E50;"),
                  p(style = "color:#666;", "Matrícula, resultados (SIMCE, IDPS), vulnerabilidad (IVE), grupo socioeconómico y asistencia por territorio, con descarga de minutas. Usa los filtros para acotar región, comuna, sector económico y más.")
              ),
              
              # Panel de filtros y descarga
              fluidRow(
                column(12, 
                       div(class = "panel-custom",
                           h4(icon("filter"), "Filtros Territoriales y Descarga de Reportes", style = "color: #2C3E50;"),
                           fluidRow(
                             # Filtros territoriales
                             column(8,
                                    fluidRow(
                                      column(3,
                                             selectInput("rft", tagList(icon("globe"), "RFT:",
                                                           tags$span(class="help-tip", title="Red Futuro Técnico: política que articula liceos EMTP con educación superior y el mundo del trabajo.", "?")),
                                                         choices = c("Todas", sort(unique(comunas$rft))),
                                                         selected = "Todas")
                                      ),
                                      column(3,
                                             selectInput("region", tagList(icon("map-marker-alt"), "Región:"), 
                                                         choices = c("Todas", sort(unique(comunas$nom_reg_rbd_a))), 
                                                         selected = "Todas")
                                      ),
                                      column(3,
                                             selectInput("provincia", tagList(icon("map-pin"), "DEPROV:",
                                                           tags$span(class="help-tip", title="Departamento Provincial de Educación (Mineduc): subdivisión administrativa territorial, distinta a la provincia política.", "?")),
                                                         choices = c("Todas", sort(unique(comunas$nom_deprov_rbd))),
                                                         selected = "Todas")
                                      ),
                                      column(3,
                                             selectInput("comuna", tagList(icon("city"), "Comuna:"), 
                                                         choices = c("Todas", sort(unique(comunas$nom_com_rbd))), 
                                                         selected = "Todas")
                                      )
                                    ),
                                    fluidRow(
                                      column(3,
                                             selectInput("sector_mapa",
                                                         tagList(icon("industry"), "Sector Económico:",
                                                           tags$span(class="help-tip", title="Agrupación oficial de especialidades EMTP (COD_SEC): p. ej. Metalmecánico agrupa Mecánica Industrial, Automotriz, Construcciones Metálicas, etc.", "?")),
                                                         choices = c("Todos", sort(unique(matricula_raw$nom_sector[!is.na(matricula_raw$nom_sector)]))),
                                                         selected = "Todos")
                                      ),
                                      column(3,
                                             selectizeInput(
                                               "especialidad",
                                               tagList(icon("cogs"), "Especialidad:"),
                                               choices = c("", sort(unique(comunas$nom_espe))),
                                               selected = "",
                                               multiple = TRUE,
                                               options = list(
                                                 placeholder = 'Especialidad...',
                                                 plugins = list('remove_button')
                                               )
                                             )
                                      ),
                                      column(3,
                                             selectInput("dependencia", tagList(icon("building"), "Dependencia:"),
                                                         choices = c(
                                                           "Todas" = "Todas",
                                                           "Municipal" = "1",
                                                           "Particular Subvencionado" = "2",
                                                           "Corporación de Administración Delegada" = "4",
                                                           "Servicio Local de Educación Pública" = "5"
                                                         ),
                                                         selected = "Todas")
                                      ),
                                      column(3,
                                             selectInput("sostenedor_mapa", tagList(icon("users"), "Sostenedor:"),
                                                         choices = c("Todos", sort(unique(matricula_raw$nombre_sost))),
                                                         selected = "Todos")
                                      )
                                    ),
                                    fluidRow(
                                      column(12,
                                             div(style = "margin-top: 15px;",
                                                 actionButton("reset_filtros_mapa", tagList(icon("redo"), " Reiniciar filtros"), 
                                                              class = "btn-warning", style = "width: 100%; padding: 12px;")
                                             )
                                      )
                                    )
                             ),
                             
                             # Panel de descarga
                             column(4,
                                    div(style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid var(--color-primary);",
                                        h5(icon("file-word"), "Minutas Territoriales", style = "color: #2C3E50; margin-bottom: 10px;"),
                                        p(style = "font-size: 12px; color: #666; margin-bottom: 10px;",
                                          icon("info-circle"), " Descarga reportes detallados del territorio seleccionado"),
                                        downloadButton("descargar_resumen_territorial_pdf", 
                                                       tagList(icon("file-pdf"), " Descargar minuta (.pdf)"), 
                                                       class = "btn-danger", style = "width: 100%; margin-bottom: 8px;"),
                                        downloadButton("descargar_resumen_territorial_excel", 
                                                       tagList(icon("file-excel"), " Descargar Excel (.xlsx)"), 
                                                       class = "btn-info", style = "width: 100%; margin-bottom: 8px;"),
                                        checkboxInput("incluir", tagList(icon("list"), " Incluir lista de establecimientos"), 
                                                      value = FALSE, width = "100%")
                                    )
                             )
                           )
                       )
                )
              ),
              
              # Mapa principal
              fluidRow(
                column(12,
                       div(class = "panel-custom",
                           h4(icon("map"), "Mapa Interactivo de Matrícula", style = "color: #2C3E50;"),
                           leafletOutput("mapa_matricula", height = "650px")
                       )
                )
              ),
              
              # Indicadores del territorio (reactivos a los filtros) — estilo ficha
              fluidRow(
                column(12, div(class = "panel-custom",
                  h4(icon("gauge-high"), " Indicadores del territorio filtrado"),
                  p(class = "text-muted", style = "margin-top:-6px;",
                    "Vulnerabilidad (IVE), aprendizajes (SIMCE 2° medio por estándares) y desarrollo personal y social (IDPS) ",
                    "agregados según los filtros aplicados arriba."),
                  uiOutput("mapa_ind_kpis"),
                  br(),
                  fluidRow(
                    column(7, div(class = "metric-card",
                      h5(icon("chart-bar"), " SIMCE 2° medio — distribución por estándar"),
                      plotlyOutput("mapa_simce_dist", height = "280px"),
                      div(style = "font-size:11px;color:#777;margin-top:6px;",
                          HTML("<b style='color:#C0392B'>Insuficiente</b> · <b style='color:#D4A017'>Elemental</b> · <b style='color:#1E8449'>Adecuado</b>. Ponderado por nº de estudiantes evaluados del territorio."))) ),
                    column(5, div(class = "metric-card",
                      h5(icon("hands-helping"), " IDPS por dimensión"),
                      plotlyOutput("mapa_idps_plot", height = "280px"),
                      div(style = "font-size:11px;color:#777;margin-top:6px;",
                          "Promedio del territorio (0–100). <60 bajo · 60–74 medio · ≥75 alto.")))
                  ),
                  br(),
                  fluidRow(
                    column(5, div(class = "metric-card",
                      h5(icon("layer-group"), " Grupo socioeconómico (GSE)"),
                      plotlyOutput("mapa_gse_plot", height = "270px"),
                      div(style = "font-size:11px;color:#777;margin-top:6px;",
                          "GSE SIMCE de los establecimientos del territorio."))),
                    column(7, div(class = "metric-card",
                      h5(icon("user-check"), " Asistencia anual de los estudiantes EMTP",
                         tags$span(style = "float:right;font-weight:700;color:#1E8449;", textOutput("mapa_asis_prom", inline = TRUE))),
                      plotlyOutput("mapa_asis_plot", height = "270px"),
                      div(style = "font-size:11px;color:#777;margin-top:6px;",
                          "Categoría de asistencia 2025: Crítica (<50%), Grave (50–84%), Reiterada (85–89%), Esperada (≥90%).")))
                  )
                ))
              ),
              # Tabla de resumen debajo del mapa
              fluidRow(
                column(12,
                       div(class = "panel-custom",
                           div(style = "text-align: center; margin-bottom: 15px;",
                               h4(icon("chart-bar"), "Resumen Detallado de Matrícula Filtrada", style = "color: #2C3E50;")
                           ),
                           div(style = "background: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                               uiOutput("resumen_matricula")
                           )
                       )
                )
              )
            )
          ),
          
          # --- Pestaña Establecimientos (búsqueda + ficha + minutas) ---
          tabPanel(
            title = tagList(icon("school"), "Establecimientos"),
            value = "tab_buscador",
            fluidPage(
              div(style = "text-align: center; margin-bottom: 20px;",
                  h3(icon("school"), "Establecimientos EMTP — Búsqueda, Ficha y Minutas", style = "color: #2C3E50;"),
                  p(style = "color:#666;", "Filtra y descarga minutas; haz clic en un establecimiento de la tabla para ver su ficha completa (contexto, mapa, especialidades, SIMCE e IDPS).")
              ),
              
              # Panel de filtros y descarga
              fluidRow(
                column(12, 
                       div(class = "panel-custom",
                           h4(icon("filter"), "Filtros de Búsqueda y Descarga de Reportes", style = "color: #2C3E50;"),
                           fluidRow(
                             # Filtros de búsqueda
                             column(8,
                                    fluidRow(
                                      column(3,
                                             selectInput("rft_busqueda", tagList(icon("globe"), "RFT:",
                                                           tags$span(class="help-tip", title="Red Futuro Técnico: política que articula liceos EMTP con educación superior y el mundo del trabajo.", "?")),
                                                         choices = c("Todas", sort(unique(matricula_raw$rft))),
                                                         selected = "Todas")
                                      ),
                                      column(3,
                                             selectInput("region_busqueda", tagList(icon("map-marker-alt"), "Región:"), 
                                                         choices = c("Todas", sort(unique(matricula_raw$nom_reg_rbd_a))), 
                                                         selected = "Todas")
                                      ),
                                      column(3,
                                             selectInput("provincia_busqueda", tagList(icon("map-pin"), "DEPROV:",
                                                           tags$span(class="help-tip", title="Departamento Provincial de Educación (Mineduc): subdivisión administrativa territorial, distinta a la provincia política.", "?")),
                                                         choices = c("Todas", sort(unique(matricula_raw$nom_deprov_rbd))),
                                                         selected = "Todas")
                                      ),
                                      column(3,
                                             selectInput("comuna_busqueda", tagList(icon("city"), "Comuna:"), 
                                                         choices = c("Todas", sort(unique(matricula_raw$nom_com_rbd))), 
                                                         selected = "Todas")
                                      )
                                    ),
                                    fluidRow(
                                      column(4,
                                             selectInput("dependencia_busqueda", tagList(icon("building"), "Dependencia:"), 
                                                         choices = c(
                                                           "Todas" = "Todas",
                                                           "Municipal" = "1",
                                                           "Particular Subvencionado" = "2",
                                                           "Corporación de Administración Delegada" = "4",
                                                           "Servicio Local de Educación Pública" = "5"
                                                         ), 
                                                         selected = "Todas")
                                      ),
                                      column(4,
                                             selectInput("sostenedor_busqueda", tagList(icon("users"), "Sostenedor:"), 
                                                         choices = c("Todos", sort(unique(matricula_raw$nombre_sost))), 
                                                         selected = "Todos")
                                      ),
                                      column(4,
                                             selectizeInput(
                                               "especialidad_busqueda",
                                               tagList(icon("cogs"), "Especialidad:"),
                                               choices = c("", sort(unique(matricula_raw$nom_espe))),
                                               selected = "",
                                               multiple = TRUE,
                                               options = list(
                                                 placeholder = 'Selecciona especialidad...',
                                                 plugins = list('remove_button')
                                               )
                                             )
                                      )
                                    ),
                                    fluidRow(
                                      column(6,
                                             textInput("rbd_busqueda", tagList(icon("id-card"), "Buscar por RBD (1234, 5678)"), "")
                                      ),
                                      column(6,
                                             textInput("nombre_busqueda", tagList(icon("school"), "Buscar por Nombre"), "")
                                      )
                                    ),
                                    fluidRow(
                                      column(6,
                                             div(style = "margin-top: 15px;",
                                                 actionButton("buscar", tagList(icon("search"), " Buscar Establecimientos"), 
                                                              class = "btn-success", style = "width: 100%; padding: 12px;")
                                             )
                                      ),
                                      column(6,
                                             div(style = "margin-top: 15px;",
                                                 actionButton("reset_filtros", tagList(icon("redo"), " Reiniciar filtros"), 
                                                              class = "btn-warning", style = "width: 100%; padding: 12px;")
                                             )
                                      )
                                    )
                             ),
                             
                             # Panel de descarga
                             column(4,
                                    div(style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid var(--color-primary);",
                                        h5(icon("file-alt"), "Minutas por Establecimiento", style = "color: #2C3E50; margin-bottom: 10px;"),
                                        p(style = "font-size: 12px; color: #666; margin-bottom: 10px;",
                                          icon("info-circle"), " Descarga reportes detallados de establecimientos específicos"),
                                        p(style = "font-size: 11px; color: #856404; background: #fff3cd; padding: 8px; border-radius: 4px; margin-bottom: 10px;",
                                          icon("exclamation-triangle"), " Importante: Se descargará una minuta individual por cada establecimiento filtrado o buscado. No se genera un PDF agregado, sino un archivo ZIP con una minuta por cada RBD seleccionado."),
                                        downloadButton("descargar_minuta_pdf", 
                                                       tagList(icon("file-pdf"), " Descargar minutas PDF (.zip)"), 
                                                       class = "btn-danger", style = "width: 100%; margin-bottom: 8px;", disabled = TRUE),
                                        downloadButton("descargar_minuta_excel", 
                                                       tagList(icon("file-excel"), " Descargar minutas Excel (.xlsx)"), 
                                                       class = "btn-info", style = "width: 100%; margin-bottom: 8px;", disabled = TRUE),
                                        p(style = "font-size: 11px; color: #666;",
                                          icon("file-archive"), " Archivo ZIP con documentos PDF (.pdf) y Excel (.xlsx) individuales para cada establecimiento")
                                    )
                             )
                           )
                       )
                )
              ),
              
              # Tabla de resultados
              fluidRow(
                column(12,
                       div(class = "panel-custom",
                           h4(icon("table"), "Resultados de Búsqueda", style = "color: #2C3E50;"),
                           div(style = "background: #ffffff; padding: 12px 20px 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                               div(style = "display:flex; flex-wrap:wrap; gap:8px; align-items:center; margin-bottom:10px;",
                                   tags$strong(icon("info-circle"), " Selección de establecimientos:"),
                                   span(style="font-size:12px; color:#555;", "Por defecto todos quedan seleccionados tras la búsqueda. Puedes deseleccionar filas o usar los botones."),
                                   actionButton("seleccionar_todos_est", label = tagList(icon("check-square"), "Seleccionar todos"), class = "btn btn-sm btn-success"),
                                   actionButton("deseleccionar_todos_est", label = tagList(icon("square"), "Quitar selección"), class = "btn btn-sm btn-secondary"),
                                   span(style="margin-left:auto; font-size:12px;", textOutput("contador_seleccion", inline = TRUE))
                               ),
                               DT::dataTableOutput("tabla_establecimientos")
                           )
                       )
                )
              ),

              # =====================================================
              # FICHA DEL ESTABLECIMIENTO (se activa al elegir un RBD)
              # =====================================================
              hr(),
              h3(icon("id-card"), " Ficha del Establecimiento", style = "color:#2C3E50;"),
              fluidRow(
                column(4,
                  wellPanel(
                    h4(icon("search"), " Ver ficha por RBD"),
                    selectizeInput("ee_rbd", "RBD del establecimiento:",
                      choices = NULL, width = "100%",
                      options = list(placeholder = "Escribe el RBD (ej: 279)...")),
                    helpText("Escribe el RBD/nombre o haz clic en una fila de la tabla de arriba.")
                  )
                ),
                column(8, uiOutput("ee_ficha"))
              ),
              br(),
              fluidRow(
                column(5, div(class = "metric-card",
                  h4(icon("map-marked-alt"), " Ubicación"),
                  leafletOutput("ee_mapa", height = "300px"))),
                column(7, div(class = "metric-card",
                  h4(icon("cogs"), " Especialidades impartidas"),
                  plotlyOutput("ee_especialidades", height = "300px"),
                  div(style="font-size:11px;color:#777;", "Matrícula EMTP por especialidad (barra) y % de mujeres.")))
              ),
              br(),
              div(class = "metric-card",
                h4(icon("user-check"), " Asistencia anual de los estudiantes EMTP",
                   tags$span(style = "float:right;font-size:1rem;font-weight:700;color:#1E8449;", textOutput("ee_asis_prom", inline = TRUE))),
                plotlyOutput("ee_asistencia", height = "230px"),
                div(style="font-size:11px;color:#777;margin-top:6px;",
                    "Categoría de asistencia 2025: Crítica (<50%), Grave (50–84%), Reiterada (85–89%), Esperada (≥90%).")),
              br(),
              div(class = "metric-card",
                h4(icon("chart-bar"), " Aprendizajes — SIMCE 2° medio"),
                p(class = "text-muted", style = "margin-bottom:6px;",
                  HTML("Porcentaje de estudiantes en cada <b>Estándar de Aprendizaje</b>. ",
                       "El objetivo de política es reducir el % en <b>Insuficiente</b> y aumentar <b>Adecuado</b>.")),
                fluidRow(
                  column(7, plotlyOutput("ee_simce_dist", height = "300px")),
                  column(5, uiOutput("ee_simce_cards"))
                ),
                div(style = "font-size:11px;color:#777;margin-top:8px;",
                    HTML("Estándares: <b style='color:#C0392B'>Insuficiente</b> · ",
                         "<b style='color:#D4A017'>Elemental</b> · <b style='color:#1E8449'>Adecuado</b>. ",
                         "Comparaciones: vs. medición anterior y vs. establecimientos del ",
                         "<b>mismo grupo socioeconómico (GSE)</b>; ▲/▼ = diferencia significativa."))
              ),
              br(),
              div(class = "metric-card",
                h4(icon("hands-helping"), " Desarrollo Personal y Social — IDPS 2° medio"),
                p(class = "text-muted", style = "margin-bottom:6px;",
                  HTML("Escala 0–100 en cuatro dimensiones. Referencia: <b>&lt;60</b> bajo · ",
                       "<b>60–74</b> medio · <b>≥75</b> alto. La flecha indica la tendencia vs. la medición anterior.")),
                fluidRow(
                  column(7, plotlyOutput("ee_idps_plot", height = "380px")),
                  column(5, uiOutput("ee_idps_interp"))
                )
              )
            )
          ),

          # --- Pestaña Visualizaciones ---
          tabPanel(
            title = tagList(icon("chart-line"), "Visualizaciones Matrícula"),
            value = "tab_viz",
            fluidPage(
              div(style = "text-align: center; margin-bottom: 20px;",
                  h3(icon("chart-bar"), "Exploración Visual de la Matrícula EMTP", style = "color: #2C3E50;")
              ),
              
              fluidRow(
                column(12, 
                       div(class = "panel-custom",
                           h4(icon("filter"), "Filtros Territoriales y de Categorización", style = "color: #2C3E50;"),
                           fluidRow(
                             column(3,
                                    selectInput("filtro_rft", tagList(icon("globe"), "RFT:",
                                                  tags$span(class="help-tip", title="Red Futuro Técnico: política que articula liceos EMTP con educación superior y el mundo del trabajo.", "?")),
                                                choices = c("Todas", sort(unique(matricula_raw$rft))),
                                                selected = "Todas")
                             ),
                             column(3,
                                    selectInput("filtro_region_viz", tagList(icon("map-marker-alt"), "Región:"), 
                                                choices = c("Todas", sort(unique(matricula_raw$nom_reg_rbd_a))), 
                                                selected = "Todas")
                             ),
                             column(3,
                                    selectInput("filtro_provincia", tagList(icon("map-pin"), "DEPROV:",
                                                  tags$span(class="help-tip", title="Departamento Provincial de Educación (Mineduc): subdivisión administrativa territorial, distinta a la provincia política.", "?")),
                                                choices = c("Todas", sort(unique(matricula_raw$nom_deprov_rbd))),
                                                selected = "Todas")
                             ),
                             column(3,
                                    selectInput("filtro_comuna", tagList(icon("city"), "Comuna:"), 
                                                choices = c("Todas", sort(unique(matricula_raw$nom_com_rbd))), 
                                                selected = "Todas")
                             )
                           ),
                           fluidRow(
                             column(3,
                                    selectizeInput(
                                      "filtro_especialidad",
                                      tagList(icon("cogs"), "Especialidad:"),
                                      choices = c("", sort(unique(matricula_raw$nom_espe))),
                                      selected = "",
                                      multiple = TRUE,
                                      options = list(
                                        placeholder = 'Selecciona especialidad...',
                                        plugins = list('remove_button')
                                      )
                                    )
                             ),
                             column(3,
                                    selectInput("filtro_dependencia", tagList(icon("building"), "Dependencia:"), 
                                                choices = c(
                                                  "Todas" = "Todas",
                                                  "Municipal" = "1",
                                                  "Particular Subvencionado" = "2",
                                                  "Corporación de Administración Delegada" = "4",
                                                  "Servicio Local de Educación Pública" = "5"
                                                ), 
                                                selected = "Todas")
                             ),
                             column(3,
                                    selectInput("filtro_sostenedor", tagList(icon("users"), "Sostenedor:"), 
                                                choices = c("Todos", sort(unique(matricula_raw$nombre_sost))), 
                                                selected = "Todos")
                             ),
                             column(3,
                                    selectInput("nivel", tagList(icon("graduation-cap"), "Tipo de enseñanza:"), 
                                                choices = c("Ambos", "Niños y Jóvenes", "Adultos"), 
                                                selected = "Ambos")
                             )
                           ),
                           fluidRow(
                             column(6, uiOutput("grado_ui")),
                             column(6, 
                                    div(style = "margin-top: 25px;",
                                        h5(icon("info-circle"), "Matrícula Total Filtrada: ", 
                                           textOutput("total_matricula_filtrada", inline = TRUE), 
                                           style = "color: #2C3E50; font-weight: bold;")
                                    )
                             )
                           ),
                           fluidRow(
                             column(12,
                                    div(style = "margin-top: 15px;",
                                        actionButton("reset_filtros_viz", tagList(icon("redo"), " Reiniciar filtros"), 
                                                     class = "btn-warning", style = "width: 100%; padding: 12px;")
                                    )
                             )
                           )
                       )
                )
              ),
              
              tabsetPanel(
                id = "viz_tabs",
                
                # Sub-pestaña: Resumen General
                tabPanel(
                  title = tagList(icon("tachometer-alt"), "Resumen General"),
                  br(),
                  
                  # Primera fila: Métricas principales
                  fluidRow(
                    column(3,
                           div(class = "metric-card", style = "padding:20px; border-left:6px solid #34536A;",
                               h5("Total Matrícula", style = "margin:0; font-size:14px; color: var(--color-muted); text-transform:uppercase; letter-spacing:.5px;"),
                               h2(textOutput("total_matricula"), style = "margin:8px 0 0 0; font-weight:700; color: var(--color-text);")
                           )),
                    column(3,
                           div(class = "metric-card", style = "padding:20px; border-left:6px solid #3B5268;",
                               h5("Hombres", style = "margin:0; font-size:14px; color: var(--color-muted); text-transform:uppercase; letter-spacing:.5px;"),
                               h2(textOutput("total_hombres"), style = "margin:8px 0 0 0; font-weight:700; color: var(--color-text);"),
                               p(textOutput("pct_hombres"), style = "margin:0; font-size:12px; color: #3B5268; font-weight:600;")
                           )),
                    column(3,
                           div(class = "metric-card", style = "padding:20px; border-left:6px solid #A75F5D;",
                               h5("Mujeres", style = "margin:0; font-size:14px; color: var(--color-muted); text-transform:uppercase; letter-spacing:.5px;"),
                               h2(textOutput("total_mujeres"), style = "margin:8px 0 0 0; font-weight:700; color: var(--color-text);"),
                               p(textOutput("pct_mujeres"), style = "margin:0; font-size:12px; color: #A75F5D; font-weight:600;")
                           )),
                    column(3,
                           div(class = "metric-card", style = "padding:20px; border-left:6px solid #6E5F80;",
                               h5("Establecimientos", style = "margin:0; font-size:14px; color: var(--color-muted); text-transform:uppercase; letter-spacing:.5px;"),
                               h2(textOutput("total_establecimientos"), style = "margin:8px 0 0 0; font-weight:700; color: var(--color-text);")
                           ))
                  ),
                  
                  # Segunda fila: Gráficos y tablas
                  fluidRow(
                    column(6,
                           div(class = "panel-custom",
                               h4(icon("chart-pie"), "Distribución por Género", style = "color: #2C3E50; margin-bottom: 15px;"),
                               plotlyOutput("grafico_torta", height = "300px")
                           )
                    ),
                    column(6,
                           div(class = "panel-custom",
                               h4(icon("building"), "Establecimientos por Dependencia", style = "color: #2C3E50; margin-bottom: 15px;"),
                               DTOutput("tabla_dependencia", height = "300px")
                           )
                    )
                  ),
                  fluidRow(
                    column(12,
                           div(class = "panel-custom",
                               h4(icon("map-location-dot"), "Matrícula EMTP por Región (Top 10)", style = "color: #2C3E50; margin-bottom: 15px;"),
                               plotlyOutput("grafico_regional", height = "360px")
                           )
                    )
                  )
                ),

                # Sub-pestaña: Análisis por Grado
                tabPanel(
                  title = tagList(icon("layer-group"), "Análisis por Grado"),
                  br(),
                  fluidRow(
                    column(12,
                           div(class = "panel-custom",
                               h4(icon("chart-column"), "Matrícula por Nivel y Grado", style = "color: #2C3E50;"),
                               plotlyOutput("grafico_nivel_grado", height = "500px")
                           )
                    )
                  )
                ),
                
                # Sub-pestaña: Análisis por Especialidad
                tabPanel(
                  title = tagList(icon("cogs"), "Especialidades y Sectores"),
                  br(),
                  fluidRow(
                    column(12,
                           div(class = "panel-custom",
                               h4(icon("industry"), "Matrícula por Sector Económico", style = "color: #2C3E50;"),
                               plotlyOutput("grafico_sector", height = "380px")
                           )
                    )
                  ),
                  fluidRow(
                    column(12,
                           div(class = "panel-custom",
                               h4(icon("chart-bar"), "Matrícula por Especialidad (% por Género)", style = "color: #2C3E50;"),
                               plotlyOutput("grafico_barras", height = "600px")
                           )
                    )
                  )
                ),
                
                # Sub-pestaña: Tabla Detallada
                tabPanel(
                  title = tagList(icon("table"), "Tabla Detallada"),
                  br(),
                  fluidRow(
                    column(12,
                           div(class = "panel-custom",
                               h4(icon("list"), "Detalle de la Matrícula por RBD-Especialidad", style = "color: #2C3E50;"),
                               DTOutput("tabla_matricula"),
                               br(),
                               downloadButton("descargar_csv", 
                                              tagList(icon("download"), " Descargar lista filtrada"), 
                                              class = "btn-primary")
                           )
                    )
                  )
                )
              )
            )
          ),
          
          # --- Pestaña Docentes (reestructurada flujo ID_ICH -> SUBSECTOR) ---
          tabPanel(
            title = tagList(icon("chalkboard-teacher"), "Docentes"),
            value = "tab_docentes",
            fluidPage(
              div(style="text-align:center;margin-bottom:15px;",
                  h3(icon("chalkboard-teacher"), "Docentes EMTP")
              ),
              div(class="alert-info", style="margin-bottom:15px;",
                  HTML("
                    <strong>¿Qué docentes se incluyen?</strong> Esta pestaña considera a todos los docentes con al menos una hora asignada en establecimientos con matrícula TP (códigos de enseñanza 410–863).<br><br>
                    Se distinguen dos tipos según el subsector en que imparten clases:<br>
                    <ul style='margin:6px 0 4px 20px;'>
                      <li><strong>Módulos de Especialidad</strong> — imparten asignaturas propias de la formación técnico-profesional (objeto central de análisis). <em>Filtro activo por defecto.</em></li>
                      <li><strong>Formación General</strong> — imparten asignaturas del currículum común (lenguaje, matemática, historia, etc.) en cursos EMTP.</li>
                    </ul>
                    <strong>Unidad de análisis:</strong> Docente–RBD–Especialidad. Una misma persona puede aparecer más de una vez si trabaja en varios establecimientos o especialidades.
                  ")),
              # KPIs: los dos conteos clave (personas únicas)
              fluidRow(
                column(4, div(class="metric-card", style="border-left:6px solid #34536A;",
                  h5("Docentes EMTP (total)", style="text-transform:uppercase;font-size:13px;color:var(--color-muted);"),
                  h2(format(dplyr::n_distinct(docentes_raw$MRUN), big.mark="."), style="font-weight:700;margin:6px 0 0;"),
                  tags$small(style="color:#777","Con ≥1 hora en enseñanza EMTP (410–863)"))),
                column(4, div(class="metric-card", style="border-left:6px solid #1E8449;",
                  h5("Docentes de módulos de especialidad", style="text-transform:uppercase;font-size:13px;color:var(--color-muted);"),
                  h2(format(dplyr::n_distinct(docentes_especialidad_long$MRUN[docentes_especialidad_long$es_especialidad]), big.mark="."), style="font-weight:700;margin:6px 0 0;color:#1E8449;"),
                  tags$small(style="color:#777","Dictan SUBSECTOR > 40000 (especialidad TP)"))),
                column(4, div(class="metric-card", style="border-left:6px solid #6E5F80;",
                  h5("% que dicta especialidad", style="text-transform:uppercase;font-size:13px;color:var(--color-muted);"),
                  h2(paste0(round(100 * dplyr::n_distinct(docentes_especialidad_long$MRUN[docentes_especialidad_long$es_especialidad]) / dplyr::n_distinct(docentes_raw$MRUN)), "%"), style="font-weight:700;margin:6px 0 0;"),
                  tags$small(style="color:#777","del total de docentes EMTP")))
              ),
              br(),
              div(class="alert alert-warning", style="margin-bottom:15px;",
                  HTML("<strong><i class='fas fa-exclamation-triangle'></i> Nota metodológica: diferencias de taxonomía entre bases Mineduc</strong><br><br>
                    Una auditoría cruzada entre el Directorio de Docentes 2025 y la Matrícula Única 2025 reveló que ambas fuentes utilizan <strong>dos catálogos distintos para la variable Especialidad/Subsector</strong>: la matrícula refleja el currículum actualizado (Decreto 452/2013 con ajustes posteriores), mientras que el directorio de docentes opera con un catálogo previo en el que ciertas especialidades aparecen colapsadas o renombradas. Esto debe tenerse presente al cruzar ambos productos.<br><br>
                    <strong>Casos detectados:</strong>
                    <ul style='margin:6px 0 4px 20px;'>
                      <li><strong>Gastronomía</strong> (61003 en matrícula, ~14.857 estudiantes): no existe como código en el directorio de docentes, donde el área figura bajo 61002 <em>Servicio de Alimentación Colectiva</em>. Es un renombramiento curricular no propagado al maestro de docentes.</li>
                      <li><strong>Programación</strong> (58034, ~3.682 estudiantes) y <strong>Conectividad y Redes</strong> (58033, ~2.128 estudiantes): en docentes ambos códigos figuran vacíos; los profesionales aparecen bajo 58035 <em>Telecomunicaciones (Redes)</em>. La separación curricular vigente no se refleja en la nómina docente.</li>
                    </ul>
                  ")),
              div(class="alert alert-secondary", style="margin-bottom:15px; font-size:12px; padding:8px 12px;",
                  HTML("<strong><i class='fas fa-info-circle'></i> Corrección de dato puntual — RBD 25824:</strong> El Liceo Sergio Silva Bascuñán (La Pintana) figura en las bases oficiales Mineduc 2025 con dependencia SLEP (COD_DEPE2=5) por error de codificación en la fuente. Se ha corregido a Particular Subvencionado (COD_DEPE2=2), en concordancia con la matrícula 2024 y su reconocimiento oficial.")
              ),
              div(class="panel-custom", style="margin-bottom:15px;",
                  h4(icon("filter"), "Filtros"),
                  fluidRow(
                    column(3, selectInput("doc_f_region", tagList(icon("map-marker-alt"), "Región"), choices = c("Todas", sort(unique(docentes_especialidad_long$NOM_REG_RBD_A))), selected="Todas")),
                    column(3, selectInput("doc_f_comuna", tagList(icon("map-pin"), "Comuna"), choices = c("Todas", sort(unique(docentes_especialidad_long$NOM_COM_RBD))), selected="Todas")),
                    column(2, selectInput("doc_f_poblacion", tagList(icon("graduation-cap"), "Enseñanza"), choices = c("Todas","Jóvenes","Adultos"), selected="Todas")),
                    column(2, selectInput("doc_f_tipo", tagList(icon("chalkboard"), "Tipo Docente"), 
                                          choices = c("Todos" = "Todos",
                                                      "Formación General" = "Formación General",
                                                      "Módulos de Especialidad" = "Módulos de Especialidad"),
                                          selected = "Módulos de Especialidad")),
                    column(2, selectInput("doc_f_genero", tagList(icon("venus-mars"), "Género"), choices = c("Todos","Femenino","Masculino"), selected="Todos"))
                  ),
                  fluidRow(
                    column(3, selectizeInput("doc_f_especialidad", tagList(icon("cogs"), "Especialidad"), choices = choices_especialidades_doc, multiple=TRUE, options=list(placeholder='Todas',plugins=list('remove_button')))),
                    column(3, selectInput("doc_f_dependencia", tagList(icon("building"), "Dependencia Adm."), 
                                          choices = c("Todas","Municipal","Particular Subvencionado",
                                                      "Corporación de Administración Delegada","Servicio Local de Educación Pública"),
                                          selected="Todas")),
                    column(3, selectInput("doc_f_ruralidad", tagList(icon("map"), "Ruralidad"), choices = c("Todas","Urbana","Rural"), selected="Todas")),
                    column(3, selectInput("doc_f_tramo", tagList(icon("award"), "Tramo Carrera"), 
                                          choices = c("Todos","0: Sin Información","1: Acceso","2: Inicial","3: Temprano","4: Avanzado","5: Experto I","6: Experto II"),
                                          selected="Todos"))
                  ),
                  fluidRow(
                    column(3, actionButton("doc_reset", tagList(icon("redo"), " Reiniciar filtros"), class="btn-warning", width="100%")),
                    column(4, downloadButton("doc_descarga", tagList(icon("download"), " Descargar CSV"), class="btn-primary", style="width:100%")),
                    column(4, downloadButton("doc_descarga_pdf", tagList(icon("file-pdf"), " Descargar Reporte PDF"), class="btn-danger", style="width:100%")),
                    column(1, div(style="padding-top:8px;font-size:10px;color:#555;text-align:center;", HTML("MRUN–RBD–Esp")))
                  )
              ),
              # Mensaje de filtros activos
              uiOutput("doc_filtros_mensaje"),
              tabsetPanel(
                id="doc_subtabs",
                tabPanel(tagList(icon("tachometer-alt"), "Resumen"),
                         fluidRow(
                           column(3, div(class="metric-card", h5("Cargos EMTP"), h2(textOutput("kpi_registros")))),
                           column(3, div(class="metric-card", h5("Personas docentes (MRUN únicos)"), h2(textOutput("kpi_personas")))),
                           column(3, div(class="metric-card", h5("Personas que dictan Módulos de Especialidad"), h2(textOutput("kpi_modulos_especialidad")))),
                           column(3, div(class="metric-card", h5("Mediana edad"), h2(textOutput("kpi_mediana_edad"))))
                         ),
                         fluidRow(
                           column(6, div(class="panel-custom", h4(icon("table"), "Resumen por Especialidad"), DTOutput("doc_tab_resumen"))),
                           column(6, div(class="panel-custom", h4(icon("chart-pie"), "% Mujeres (personas)"), h2(textOutput("kpi_mujeres")), plotlyOutput("doc_plot_genero", height="300px")))
                         )
                ),
                tabPanel(tagList(icon("venus-mars"), "Género"),
                         tabsetPanel(
                           id = "doc_genero_tabs",
                           tabPanel(
                             tagList(icon("cogs"), "Por especialidad"),
                             fluidRow(
                               column(12, div(class="panel-custom", h4(icon("chart-bar"), "Género por Especialidad (% interno)"), plotlyOutput("doc_plot_genero_especialidad", height="520px")))
                             )
                           ),
                           tabPanel(
                             tagList(icon("map-marker-alt"), "Por región"),
                             fluidRow(
                               column(12, div(class="panel-custom", h4(icon("chart-bar"), "Género por Región (% interno)"), plotlyOutput("doc_plot_genero_region", height="420px")))
                             ),
                             fluidRow(
                               column(12, div(class="panel-custom", h4(icon("chart-column"), "Cantidad de Docentes por Región"), plotlyOutput("doc_plot_cantidad_region", height="420px")))
                             )
                           )
                         )
                ),
                tabPanel(tagList(icon("building"), "Dependencia"),
                         fluidRow(
                           column(12, div(class="panel-custom", h4(icon("building"), "Distribución por Dependencia"),
                                          plotlyOutput("doc_plot_dependencia", height="420px"),
                                          DTOutput("doc_tab_dependencia")))
                         )
                ),
                tabPanel(tagList(icon("map"), "Ruralidad"),
                         fluidRow(
                           column(12, div(class="panel-custom", h4(icon("map"), "Distribución por Ruralidad"),
                                          plotlyOutput("doc_plot_ruralidad", height="420px"),
                                          div(style="font-size:11px;color:#555;", "Ruralidad según RURAL_RBD (0=Urbana,1=Rural).")))
                         )
                ),
                tabPanel(tagList(icon("graduation-cap"), "Experiencia y Carrera"),
                         fluidRow(
                           column(4, div(class="panel-custom", h4(icon("chart-column"), "Tramo Carrera Docente (personas únicas)"), plotlyOutput("doc_plot_tramo", height="440px"),
                                         div(style="font-size:11px;color:#555;", "Fuente: TRAMO_CARR_DOCENTE. Muestra distribución porcentual de docentes con tramo reportado."))),
                           column(4, div(class="panel-custom", h4(icon("chart-column"), "Distribución de Edades (personas)"), plotlyOutput("doc_plot_edad", height="440px"),
                                         div(style="font-size:11px;color:#555;", "Edad calculada desde DOC_FEC_NAC (yyyymm). Línea vertical = mediana."))),
                           column(4, div(class="panel-custom", h4(icon("chart-column"), "Años en el Sistema (personas)"), plotlyOutput("doc_plot_servicio", height="440px"),
                                         div(style="font-size:11px;color:#555;", "Años según ANO_SERVICIO_SISTEMA truncado a 0–60. Líneas: mediana (línea continua), promedio (línea punteada).")))
                         )
                ),
                tabPanel(tagList(icon("user-graduate"), "Títulos y Función"),
                         fluidRow(
                           column(4, div(class="panel-custom", 
                                         h4(icon("graduation-cap"), "Área de Titulación"),
                                         plotlyOutput("doc_plot_tipo_institucion", height="380px"),
                                         div(style="font-size:11px;color:#555;", "Fuente: TIT_ID_1 y TIT_ID_2. Clasificación: 0=Sin información, 1=Educación, 2=Otras áreas, 3=No titulado. Se consideran ambos títulos.")
                           )),
                           column(4, div(class="panel-custom", 
                                         h4(icon("certificate"), "Tipo de Título (solo Educación)"),
                                         plotlyOutput("doc_plot_tipo_titulo", height="380px"),
                                         div(style="font-size:11px;color:#555;", "Fuente: TIP_TIT_ID_1 y TIP_TIT_ID_2. Solo docentes con título en Educación. % del total de títulos (un docente con 2 títulos aporta 2 registros).")
                           )),
                           column(4, div(class="panel-custom", 
                                         h4(icon("briefcase"), "Función Principal"),
                                         plotlyOutput("doc_plot_funcion", height="380px"),
                                         div(style="font-size:11px;color:#555;", "Fuente: ID_IFP2. Función principal agrupada del docente.")
                           ))
                         ),
                         fluidRow(
                           column(6, div(class="panel-custom",
                                         h4(icon("building-columns"), "Tipo de Institución donde Estudió"),
                                         plotlyOutput("doc_plot_tipo_institucion_estudio", height="380px"),
                                         div(style="font-size:11px;color:#555;", "Fuente: TIP_INSTI_ID_1 y TIP_INSTI_ID_2. Tipo de institución donde obtuvo su(s) título(s). Un docente con 2 títulos puede aparecer en 2 categorías.")
                           )),
                           column(6, div(class="panel-custom",
                                          h4(icon("table"), "Resumen: Títulos y Formación"),
                                          DTOutput("doc_tab_titulos_resumen"),
                                          div(style="font-size:11px;color:#555;margin-top:10px;", 
                                              "Tabla muestra cantidad de docentes según área de titulación y tipo de institución donde estudiaron.")
                           ))
                         )
                ),
                tabPanel(tagList(icon("list"), "Detalle"),
                         fluidRow(
                           column(12, div(class="panel-custom", h4(icon("table"), "Detalle Registros"), DTOutput("doc_tab_detalle")))
                         )
                )
              )
            )
          ),
          

          # --- PESTAÑA EGRESADOS Y TITULADOS EMTP ---
          tabPanel(
            title = tagList(icon("graduation-cap"), "Egresados y Titulados"),
            value = "egresados_tab",
            fluidPage(
              div(class="info-card", style="margin:20px;",
                  h2(icon("graduation-cap"), "Seguimiento de Egresados EMTP", style="color: white;"),
                  p("Análisis de egreso, continuidad de estudios y titulación de estudiantes de Educación Media Técnico-Profesional.")
              ),
              
              # SUB-PESTAÑAS
              tabsetPanel(
                
                # ============================================================
                # SUB-PESTAÑA 1: EGRESADOS EMTP
                # ============================================================
                tabPanel(
                  title = tagList(icon("user-graduate"), "Egresados EMTP"),
                  
                  h4(icon("user-graduate"), " Egresados EMTP 2024"),
                  p("Estudiantes que completaron Educación Media Técnico-Profesional en 2024."),
                  
                  div(class = "alert alert-info", style = "margin: 10px 0;",
                      icon("info-circle"), 
                      strong(" Nota: "), 
                      "Datos del año 2024. Cuando estén disponibles los datos de 2025, se agregará un selector de año."
                  ),
                  
                  # KPIs principales
                  fluidRow(
                    column(3, div(class="metric-card", 
                                  h5("Total Egresados"), 
                                  h2(textOutput("egr_total_egresados")))),
                    column(3, div(class="metric-card", 
                                  h5("Establecimientos"), 
                                  h2(textOutput("egr_total_ee")))),
                    column(3, div(class="metric-card", 
                                  h5("% Urbano"), 
                                  h2(textOutput("egr_pct_urbano")))),
                    column(3, div(class="metric-card", 
                                  h5("% Jóvenes"), 
                                  h2(textOutput("egr_pct_jovenes"))))
                  ),
                  
                  br(),
                  
                  # Panel de filtros
                  div(class = "well", style = "background-color: #f0f8ff; padding: 15px; margin-bottom: 20px;",
                      h5(icon("filter"), " Filtros de Análisis"),
                      fluidRow(
                        column(3,
                               selectInput("egr_region", "Región:",
                                           choices = NULL,
                                           selected = "Todas")
                        ),
                        column(3,
                               selectInput("egr_comuna", "Comuna:",
                                           choices = NULL,
                                           selected = "Todas")
                        ),
                        column(3,
                               selectInput("egr_dependencia", "Dependencia:",
                                           choices = c("Todas"),
                                           selected = "Todas")
                        ),
                        column(3,
                               selectInput("egr_ruralidad", "Ruralidad:",
                                           choices = c("Todas", "Urbano", "Rural"),
                                           selected = "Todas")
                        )
                      ),
                      fluidRow(
                        column(3,
                               selectInput("egr_tipo_ense", "Tipo Enseñanza:",
                                           choices = c("Todas", "Jóvenes", "Adultos"),
                                           selected = "Todas")
                        ),
                        column(9,
                               div(style = "margin-top: 25px;",
                                   actionButton("egr_limpiar_filtros", tagList(icon("redo"), " Reiniciar filtros"), 
                                                class = "btn-warning")
                               )
                        )
                      )
                  ),
                  
                  br(),
                  
                  # Visualizaciones
                  fluidRow(
                    column(6, 
                           div(class = "metric-card",
                               h5("Egresados por Dependencia"),
                               plotlyOutput("egr_plot_dependencia", height = "350px")
                           )
                    ),
                    column(6, 
                           div(class = "metric-card",
                               h5("Egresados por Región"),
                               plotlyOutput("egr_plot_region", height = "350px")
                           )
                    )
                  ),
                  
                  br(),
                  
                  fluidRow(
                    column(6, 
                           div(class = "metric-card",
                               h5("Distribución Urbano-Rural"),
                               plotlyOutput("egr_plot_ruralidad", height = "350px")
                           )
                    ),
                    column(6, 
                           div(class = "metric-card",
                               h5("Tipo de Enseñanza"),
                               plotlyOutput("egr_plot_tipo_ense", height = "350px")
                           )
                    )
                  ),
                  
                  br(),
                  
                  # Tabla de datos
                  h5(icon("table"), " Detalle de Egresados"),
                  DT::dataTableOutput("egr_tabla_detalle")
                ),
                
                # ============================================================
                # SUB-PESTAÑA 2: CONTINUIDAD DE ESTUDIOS
                # ============================================================
                tabPanel(
                  title = tagList(icon("university"), "Continuidad de Estudios"),
                  
                  h4(icon("university"), " Continuidad en Educación Superior (al año de egreso)"),
                  p("Análisis de egresados EMTP 2024 que se matricularon en educación superior en 2025."),
                  
                  div(class = "alert alert-info", style = "margin: 10px 0;",
                      icon("info-circle"), 
                      strong(" Metodología: "), 
                      "Se cruza MRUN de egresados EMTP 2024 con matrícula en educación superior 2025 (año t+1). ",
                      "Permite identificar qué porcentaje continúa estudios al año siguiente de egresar y en qué tipo de institución."
                  ),
                  
                  # Panel de filtros
                  div(class = "well", style = "background-color: #f0f8ff; padding: 15px; margin-bottom: 20px;",
                      h5(icon("filter"), " Filtros de Análisis"),
                      fluidRow(
                        column(3,
                               selectInput("cont_filtro_region", "Región:",
                                           choices = NULL,
                                           selected = "Todas")
                        ),
                        column(3,
                               selectInput("cont_filtro_comuna", "Comuna:",
                                           choices = NULL,
                                           selected = "Todas")
                        ),
                        column(3,
                               selectInput("cont_filtro_dependencia", "Dependencia:",
                                           choices = c("Todas",
                                                       "Municipal DAEM",
                                                       "Municipal Corporación",
                                                       "Particular Subvencionado",
                                                       "De Administración Delegada"),
                                           selected = "Todas")
                        ),
                        column(3,
                               selectInput("cont_filtro_ruralidad", "Ruralidad:",
                                           choices = c("Todas", "Urbano", "Rural"),
                                           selected = "Todas")
                        )
                      ),
                      fluidRow(
                        column(3,
                               selectInput("cont_filtro_tipo_ense", "Tipo de Enseñanza:",
                                           choices = c("Todas", "Jóvenes", "Adultos"),
                                           selected = "Todas")
                        ),
                        column(3,
                               selectInput("cont_filtro_genero", "Género:",
                                           choices = c("Todos", "Mujeres", "Hombres"),
                                           selected = "Todos")
                        ),
                        column(6,
                               div(style = "margin-top: 25px;",
                                   actionButton("cont_limpiar_filtros", tagList(icon("redo"), " Reiniciar filtros"), 
                                                class = "btn-warning")
                               )
                        )
                      )
                  ),
                  
                  # KPIs de continuidad
                  fluidRow(
                    column(3, div(class="metric-card", 
                                  h5("Egresados EMTP 2024"), 
                                  h2(textOutput("cont_total_egresados")))),
                    column(3, div(class="metric-card", 
                                  h5("Continúan en ES 2025"), 
                                  h2(textOutput("cont_total_continuan")))),
                    column(3, div(class="metric-card", 
                                  h5("% Continuidad"), 
                                  h2(textOutput("cont_pct_continuidad")))),
                    column(3, div(class="metric-card", 
                                  h5("% Continuidad Mujeres"), 
                                  h2(textOutput("cont_pct_mujeres"))))
                  ),
                  
                  br(),
                  
                  # Información sobre formas de ingreso
                  div(class = "well", style = "background-color: #f8f9fa;",
                      h5(icon("info-circle"), " Datos de Matrícula Educación Superior 2025"),
                      p("Variables analizadas: tipo de institución (Universidad, IP, CFT), modalidad (presencial/no presencial), ",
                        "área de conocimiento, acreditación de carrera e institución, y forma de ingreso."),
                      p(strong("Formas de ingreso:"), " Ingreso directo, PACE, articulación TNS, RAP, programas de inclusión, entre otros.")
                  ),
                  
                  br(),
                  
                  # Gráficos de continuidad
                  fluidRow(
                    column(6,
                           div(class = "metric-card",
                               h5("Continuidad por Tipo de Institución"),
                               plotlyOutput("cont_plot_tipo_inst", height = "350px")
                           )
                    ),
                    column(6,
                           div(class = "metric-card",
                               h5("Top 10 Áreas de Conocimiento"),
                               plotlyOutput("cont_plot_areas", height = "350px")
                           )
                    )
                  ),
                  
                  br(),
                  
                  fluidRow(
                    column(6,
                           div(class = "metric-card",
                               h5("Tasa de Continuidad por Dependencia"),
                               plotlyOutput("cont_plot_dependencia", height = "350px")
                           )
                    ),
                    column(6,
                           div(class = "metric-card",
                               h5("Tasa de Continuidad Urbano vs Rural"),
                               plotlyOutput("cont_plot_ruralidad", height = "350px")
                           )
                    )
                  ),
                  
                  br(),
                  
                  # Tabla de continuidad
                  h5(icon("table"), " Detalle de Continuidad por Dependencia y Ruralidad"),
                  DT::dataTableOutput("cont_tabla_resumen")
                ),
                
                # ============================================================
                # SUB-PESTAÑA 3: TITULADOS TP
                # ============================================================
                tabPanel(
                  title = tagList(icon("award"), "Titulados TP"),
                  br(),
                  div(class = "alert alert-info",
                      icon("info-circle"),
                      HTML(sprintf(" <b>Titulados Técnico-Profesionales %d</b> (con práctica profesional aprobada, <code>ESTADO_PRACTICA=1</code>). %d es el último año disponible en la base de Prácticas y Titulados TP del Mineduc. ",
                                   ANIO_TITULADOS, ANIO_TITULADOS)),
                      HTML(sprintf("La titulación TP ocurre tras aprobar la práctica profesional, habitualmente 1–2 años después del egreso; por eso la <b>tasa de titulación al año de egreso</b> cruza los titulados %d con los egresados <b>%d</b>.",
                                   ANIO_TITULADOS, ANIO_TITULADOS - 1))),
                  # Filtros horizontales (ancho completo)
                  div(class = "panel-custom",
                    h4(icon("filter"), " Filtros"),
                    fluidRow(
                      column(4, selectInput("tit_region", "Región:",
                        c("Todas", sort(unique(titulados$NOM_REG_RBD_A[!is.na(titulados$NOM_REG_RBD_A)]))), width = "100%")),
                      column(4, selectInput("tit_dependencia", "Dependencia:",
                        c("Todas","Municipal","Particular Subvencionado",
                          "Corporación de Administración Delegada","Servicio Local de Educación"), width = "100%")),
                      column(4, selectizeInput("tit_especialidad", "Especialidad:",
                        choices = c("Todas", sort(unique(titulados$NOM_ESPE[!is.na(titulados$NOM_ESPE)]))),
                        selected = "Todas", width = "100%",
                        options = list(placeholder = "Todas las especialidades")))
                    )
                  ),
                  # KPIs (ancho completo)
                  fluidRow(
                    column(3, div(class="metric-card",
                      h5("Titulados TP"), h3(textOutput("tit_kpi_total")))),
                    column(3, div(class="metric-card",
                      h5(sprintf("Tasa titulación (egreso %d)", ANIO_TITULADOS - 1)), h3(textOutput("tit_kpi_tasa")),
                      tags$small(style="color:#777", sprintf("titulados %d / egresados %d", ANIO_TITULADOS, ANIO_TITULADOS - 1)))),
                    column(3, div(class="metric-card",
                      h5("Tiempo a titulación"), h3(textOutput("tit_kpi_tiempo")),
                      tags$small(style="color:#777","años desde el egreso"))),
                    column(3, div(class="metric-card",
                      h5("% Mujeres"), h3(textOutput("tit_kpi_mujeres"))))
                  ),
                  br(),
                  # Gráficos a ancho completo (mitad de pantalla cada uno)
                  fluidRow(
                    column(6, div(class="panel-custom",
                      h4(icon("cogs")," Titulados por especialidad"),
                      plotlyOutput("tit_plot_especialidad", height="460px"))),
                    column(6, div(class="panel-custom",
                      h4(icon("industry")," Sector económico de la práctica (top 12)"),
                      plotlyOutput("tit_plot_rubro", height="460px")))
                  ),
                  fluidRow(
                    column(6, div(class="panel-custom",
                      h4(icon("map-location-dot")," Titulados por región"),
                      plotlyOutput("tit_plot_region", height="430px"))),
                    column(6, div(class="panel-custom",
                      h4(icon("building")," Titulados por dependencia y género"),
                      plotlyOutput("tit_plot_genero", height="430px")))
                  )
                )
              )
            )
          ),

        ) # Fin navbarPage
    ) # Fin div main-content
  ) # Fin shinyjs::hidden
  ,
  # Burbuja de chat flotante (disponible en todas las pestañas)
  chatbot_floating_ui()
) # Fin fluidPage (UI principal)


# --- Server Shiny
server <- function(input, output, session) {
  
  # 🎬 PANTALLA DE CARGA - Ocultar cuando los datos estén listos
  observe({
    # Esperar a que los datos estén cargados (verificar que existan las variables globales)
    req(exists("matricula_raw"), exists("base_apoyo"))
    
    # Delay adicional para asegurar que todo esté completamente cargado y renderizado
    Sys.sleep(5)
    
    # Ocultar pantalla de carga
    shinyjs::hide("loading-screen", anim = TRUE, animType = "fade", time = 1)
    
    # Mostrar contenido principal
    shinyjs::show("main-content", anim = TRUE, animType = "fade", time = 1)

  })

  # ============================================================
  # NUEVO — FILTROS GEOGRÁFICOS EN CASCADA (Región → DEPROV → Comuna)
  # No modifica datos: solo ACOTA las opciones de los desplegables según la
  # región/DEPROV elegida (antes Comuna mostraba las ~346 comunas del país).
  # Conserva la selección si sigue siendo válida; si no, vuelve a "Todas".
  # ============================================================
  .cascada_geo <- function(df, reg_in, dep_in, com_in,
                           reg_col = "nom_reg_rbd_a",
                           dep_col = "nom_deprov_rbd",
                           com_col = "nom_com_rbd") {
    # Región cambia → recalcular DEPROV y Comuna
    observeEvent(input[[reg_in]], {
      d <- df
      if (!is.null(input[[reg_in]]) && input[[reg_in]] != "Todas")
        d <- d[!is.na(d[[reg_col]]) & d[[reg_col]] == input[[reg_in]], ]
      dep_choices <- c("Todas", sort(unique(d[[dep_col]][!is.na(d[[dep_col]])])))
      com_choices <- c("Todas", sort(unique(d[[com_col]][!is.na(d[[com_col]])])))
      dep_sel <- if (!is.null(input[[dep_in]]) && input[[dep_in]] %in% dep_choices) input[[dep_in]] else "Todas"
      com_sel <- if (!is.null(input[[com_in]]) && input[[com_in]] %in% com_choices) input[[com_in]] else "Todas"
      updateSelectInput(session, dep_in, choices = dep_choices, selected = dep_sel)
      updateSelectInput(session, com_in, choices = com_choices, selected = com_sel)
    }, ignoreInit = TRUE)

    # DEPROV cambia → recalcular Comuna (respetando la región activa)
    observeEvent(input[[dep_in]], {
      d <- df
      if (!is.null(input[[reg_in]]) && input[[reg_in]] != "Todas")
        d <- d[!is.na(d[[reg_col]]) & d[[reg_col]] == input[[reg_in]], ]
      if (!is.null(input[[dep_in]]) && input[[dep_in]] != "Todas")
        d <- d[!is.na(d[[dep_col]]) & d[[dep_col]] == input[[dep_in]], ]
      com_choices <- c("Todas", sort(unique(d[[com_col]][!is.na(d[[com_col]])])))
      com_sel <- if (!is.null(input[[com_in]]) && input[[com_in]] %in% com_choices) input[[com_in]] else "Todas"
      updateSelectInput(session, com_in, choices = com_choices, selected = com_sel)
    }, ignoreInit = TRUE)
  }
  # Aplicar a los tres grupos de filtros geográficos de la app
  tryCatch({
    if (exists("comunas"))       .cascada_geo(comunas,       "region",           "provincia",          "comuna")
    if (exists("matricula_raw")) .cascada_geo(matricula_raw, "region_busqueda",  "provincia_busqueda", "comuna_busqueda")
    if (exists("matricula_raw")) .cascada_geo(matricula_raw, "filtro_region_viz","filtro_provincia",   "filtro_comuna")
  }, error = function(e) cat("[cascada] aviso:", conditionMessage(e), "\n"))

  # ============================================================
  # NUEVO — Navegación por tarjetas del Inicio (saltan a la pestaña)
  # ============================================================
  observeEvent(input$go_mapa,      updateNavbarPage(session, "navbar", "tab_mapa"))
  observeEvent(input$go_buscador,  updateNavbarPage(session, "navbar", "tab_buscador"))
  observeEvent(input$go_viz,       updateNavbarPage(session, "navbar", "tab_viz"))
  observeEvent(input$go_docentes,  updateNavbarPage(session, "navbar", "tab_docentes"))
  observeEvent(input$go_egresados, updateNavbarPage(session, "navbar", "egresados_tab"))

  # --- Nueva lógica reactiva pestaña Docentes ---
  observeEvent(input$doc_reset, {
    updateSelectInput(session, "doc_f_region", selected = "Todas")
    updateSelectInput(session, "doc_f_comuna", selected = "Todas")
    updateSelectInput(session, "doc_f_poblacion", selected = "Todas")
    updateSelectInput(session, "doc_f_tipo", selected = "Todos")
    updateSelectizeInput(session, "doc_f_especialidad", selected = NULL)
    updateSelectInput(session, "doc_f_genero", selected = "Todos")
    updateSelectInput(session, "doc_f_dependencia", selected = "Todas")
    updateSelectInput(session, "doc_f_ruralidad", selected = "Todas")
    updateSelectInput(session, "doc_f_tramo", selected = "Todos")
  })
  
  doc_datos <- reactive({
    # Filtros automáticos - sin necesidad de botón "Aplicar"
    df <- docentes_especialidad_long
    
    # Enriquecimiento dinámico (solo una vez por sesión se puede cachear si fuera necesario)
    df <- df %>% mutate(
      ANOS_SERV = suppressWarnings(as.numeric(ANO_SERVICIO_SISTEMA)),
      # DOC_FEC_NAC formato yyyymm -> edad
      FEC_NAC = suppressWarnings(as.character(DOC_FEC_NAC)),
      FEC_NAC = ifelse(nchar(FEC_NAC)==6, FEC_NAC, NA),
      NAC_YEAR = suppressWarnings(as.numeric(substr(FEC_NAC,1,4))),
      NAC_MONTH = suppressWarnings(as.numeric(substr(FEC_NAC,5,6))),
      NAC_MONTH = ifelse(NAC_MONTH>=1 & NAC_MONTH<=12, NAC_MONTH, NA),
      EDAD = ifelse(!is.na(NAC_YEAR), {
        hoy <- Sys.Date();
        age <- as.numeric(format(hoy, '%Y')) - NAC_YEAR - ifelse(!is.na(NAC_MONTH) & NAC_MONTH > as.numeric(format(hoy,'%m')), 1, 0);
        ifelse(age<15 | age>90, NA, age)
      }, NA),
      TRAMO_LABEL = dplyr::case_when(
        TRAMO_CARR_DOCENTE == 0 ~ '0: Sin Información',
        TRAMO_CARR_DOCENTE == 1 ~ '1: Acceso',
        TRAMO_CARR_DOCENTE == 2 ~ '2: Inicial',
        TRAMO_CARR_DOCENTE == 3 ~ '3: Temprano',
        TRAMO_CARR_DOCENTE == 4 ~ '4: Avanzado',
        TRAMO_CARR_DOCENTE == 5 ~ '5: Experto I',
        TRAMO_CARR_DOCENTE == 6 ~ '6: Experto II',
        TRUE ~ 'Sin dato'
      ),
      Dependencia_label = dplyr::case_when(
        COD_DEPE2 == 1 ~ 'Municipal',
        COD_DEPE2 == 2 ~ 'Particular Subvencionado',
        COD_DEPE2 == 4 ~ 'Corporación de Administración Delegada',
        COD_DEPE2 == 5 ~ 'Servicio Local de Educación Pública',
        TRUE ~ 'Otra/No informado'
      ),
      Ruralidad_label = dplyr::case_when(
        RURAL_RBD == 1 ~ 'Rural',
        RURAL_RBD == 0 ~ 'Urbana',
        TRUE ~ 'Sin dato'
      )
    )
    
    # Aplicar filtros automáticamente
    if(!is.null(input$doc_f_region) && input$doc_f_region != "Todas") {
      df <- df %>% filter(NOM_REG_RBD_A == input$doc_f_region)
    }
    if(!is.null(input$doc_f_comuna) && input$doc_f_comuna != "Todas") {
      df <- df %>% filter(NOM_COM_RBD == input$doc_f_comuna)
    }
    if(!is.null(input$doc_f_poblacion) && input$doc_f_poblacion != "Todas") {
      df <- df %>% filter(Poblacion == input$doc_f_poblacion)
    }
    if(!is.null(input$doc_f_tipo) && input$doc_f_tipo != "Todos") {
      df <- df %>% filter(tipo_docente == input$doc_f_tipo)
    }
    if(!is.null(input$doc_f_especialidad) && length(input$doc_f_especialidad) > 0) {
      df <- df %>% filter(SUBSECTOR %in% as.integer(input$doc_f_especialidad))
    }
    if(!is.null(input$doc_f_genero) && input$doc_f_genero != "Todos") {
      if(input$doc_f_genero == 'Femenino') {
        df <- df %>% filter(DOC_GENERO == 2)
      } else {
        df <- df %>% filter(DOC_GENERO == 1)
      }
    }
    if(!is.null(input$doc_f_dependencia) && input$doc_f_dependencia != 'Todas') {
      df <- df %>% filter(Dependencia_label == input$doc_f_dependencia)
    }
    if(!is.null(input$doc_f_ruralidad) && input$doc_f_ruralidad != 'Todas') {
      df <- df %>% filter(Ruralidad_label == input$doc_f_ruralidad)
    }
    if(!is.null(input$doc_f_tramo) && input$doc_f_tramo != 'Todos') {
      df <- df %>% filter(TRAMO_LABEL == input$doc_f_tramo)
    }
    
    df
  })
  
  # Mensaje dinámico de filtros activos
  output$doc_filtros_mensaje <- renderUI({
    filtros_activos <- list()
    
    # Recopilar filtros activos
    if(!is.null(input$doc_f_region) && input$doc_f_region != "Todas") {
      filtros_activos <- c(filtros_activos, paste0("<strong>Región:</strong> ", input$doc_f_region))
    }
    if(!is.null(input$doc_f_comuna) && input$doc_f_comuna != "Todas") {
      filtros_activos <- c(filtros_activos, paste0("<strong>Comuna:</strong> ", input$doc_f_comuna))
    }
    if(!is.null(input$doc_f_poblacion) && input$doc_f_poblacion != "Todas") {
      filtros_activos <- c(filtros_activos, paste0("<strong>Enseñanza:</strong> ", input$doc_f_poblacion))
    }
    if(!is.null(input$doc_f_tipo) && input$doc_f_tipo != "Todos") {
      filtros_activos <- c(filtros_activos, paste0("<strong>Tipo:</strong> ", input$doc_f_tipo))
    }
    if(!is.null(input$doc_f_genero) && input$doc_f_genero != "Todos") {
      filtros_activos <- c(filtros_activos, paste0("<strong>Género:</strong> ", input$doc_f_genero))
    }
    if(!is.null(input$doc_f_especialidad) && length(input$doc_f_especialidad) > 0) {
      esp_nombres <- names(choices_especialidades_doc)[choices_especialidades_doc %in% input$doc_f_especialidad]
      filtros_activos <- c(filtros_activos, paste0("<strong>Especialidad:</strong> ", paste(esp_nombres, collapse=", ")))
    }
    if(!is.null(input$doc_f_dependencia) && input$doc_f_dependencia != "Todas") {
      filtros_activos <- c(filtros_activos, paste0("<strong>Dependencia:</strong> ", input$doc_f_dependencia))
    }
    if(!is.null(input$doc_f_ruralidad) && input$doc_f_ruralidad != "Todas") {
      filtros_activos <- c(filtros_activos, paste0("<strong>Ruralidad:</strong> ", input$doc_f_ruralidad))
    }
    if(!is.null(input$doc_f_tramo) && input$doc_f_tramo != "Todos") {
      filtros_activos <- c(filtros_activos, paste0("<strong>Tramo:</strong> ", input$doc_f_tramo))
    }
    
    # Generar mensaje
    if(length(filtros_activos) == 0) {
      div(class="alert alert-success", style="margin-bottom:15px; padding:12px;",
          icon("check-circle"), " ",
          HTML("<strong>Mostrando todos los docentes EMTP</strong> (sin filtros aplicados)")
      )
    } else {
      div(class="alert alert-warning", style="margin-bottom:15px; padding:12px;",
          icon("filter"), " ",
          HTML(paste0("<strong>Filtros aplicados:</strong> ", paste(filtros_activos, collapse=" • ")))
      )
    }
  })
  
  output$kpi_registros <- renderText({ format(nrow(docentes_raw), big.mark='.', decimal.mark=',') })
  output$kpi_personas  <- renderText({ format(dplyr::n_distinct(docentes_raw$MRUN), big.mark='.', decimal.mark=',') })
  output$kpi_modulos_especialidad <- renderText({
    n <- docentes_especialidad_long %>%
      dplyr::filter(tipo_docente == "Módulos de Especialidad") %>%
      dplyr::distinct(MRUN) %>% nrow()
    format(n, big.mark='.', decimal.mark=',')
  })
  output$kpi_promedio <- renderText({ '' })
  output$kpi_mujeres <- renderText({
    df <- doc_datos()
    pers <- df %>% dplyr::filter(!is.na(MRUN)) %>% dplyr::distinct(MRUN, DOC_GENERO)
    if(nrow(pers)==0) '-' else paste0(round(100*sum(pers$DOC_GENERO==2, na.rm=TRUE)/nrow(pers),1),'%')
  })
  output$kpi_mediana_edad <- renderText({
    df <- doc_datos()
    pers <- df %>%
      mutate(
        FEC_NAC = suppressWarnings(as.character(DOC_FEC_NAC)),
        FEC_NAC = ifelse(nchar(FEC_NAC)==6, FEC_NAC, NA),
        NAC_YEAR = suppressWarnings(as.numeric(substr(FEC_NAC,1,4))),
        NAC_MONTH = suppressWarnings(as.numeric(substr(FEC_NAC,5,6))),
        EDAD = ifelse(!is.na(NAC_YEAR), {
          hoy <- Sys.Date()
          age <- as.numeric(format(hoy,'%Y')) - NAC_YEAR - ifelse(!is.na(NAC_MONTH) & NAC_MONTH > as.numeric(format(hoy,'%m')), 1, 0)
          ifelse(age<15 | age>90, NA, age)
        }, NA)
      ) %>%
      dplyr::distinct(MRUN, EDAD) %>%
      dplyr::filter(!is.na(EDAD))
    if(nrow(pers)==0) '-' else stats::median(pers$EDAD)
  })
  output$kpi_prom_servicio <- renderText({
    df <- doc_datos()
    pers <- df %>%
      mutate(ANOS_SERV = suppressWarnings(as.numeric(ANO_SERVICIO_SISTEMA))) %>%
      dplyr::distinct(MRUN, ANOS_SERV) %>%
      dplyr::filter(!is.na(ANOS_SERV))
    if(nrow(pers)==0) '-' else round(mean(pers$ANOS_SERV),1)
  })
  
  output$doc_tab_resumen <- DT::renderDT({
    df <- doc_datos(); if(nrow(df)==0) return(DT::datatable(data.frame(Mensaje='Sin datos'), options=list(dom='t')))
    resumen <- df %>%
      mutate(Genero = ifelse(DOC_GENERO==2,'Mujeres', ifelse(DOC_GENERO==1,'Hombres','Desconocido'))) %>%
      group_by(SUBSECTOR, Especialidad) %>%
      summarise(Total = n(), Hombres = sum(Genero=='Hombres'), Mujeres = sum(Genero=='Mujeres'), .groups='drop') %>%
      mutate(`% Hombres` = round(100*Hombres/Total,1), `% Mujeres` = round(100*Mujeres/Total,1)) %>% arrange(desc(Total))
    DT::datatable(resumen, extensions='Buttons',
                  options=list(pageLength=12, scrollX=TRUE, dom='Bfrtip',
                               buttons=list('copy','csv',list(extend='excel', title='docentes_resumen'))),
                  rownames=FALSE)
  })
  
  output$doc_plot_genero <- renderPlotly({
    df <- doc_datos(); if(nrow(df)==0) return(NULL)
    # Calcular sobre PERSONAS únicas (no registros) para coincidir con kpi_mujeres
    datos <- df %>%
      filter(!is.na(MRUN)) %>%
      distinct(MRUN, DOC_GENERO) %>%
      mutate(Genero = ifelse(DOC_GENERO==2,'Mujeres', ifelse(DOC_GENERO==1,'Hombres','Desconocido'))) %>%
      count(Genero, name='n') %>%
      mutate(
        pct = round(100*n/sum(n), 1),
        color = dplyr::case_when(
          Genero == 'Mujeres' ~ '#B35A5A',
          Genero == 'Hombres' ~ '#34536A',
          TRUE ~ '#888888'
        )
      ) %>%
      arrange(match(Genero, c('Hombres','Mujeres','Desconocido')))
    plotly::plot_ly(
      datos,
      labels = ~Genero,
      values = ~n,
      type = 'pie',
      text = ~paste0(Genero,': ',pct,'% (',n,' personas)'),
      textinfo = 'text',
      hoverinfo = 'text',
      marker = list(colors = datos$color)
    ) %>%
      layout(showlegend=FALSE, margin=list(l=10,r=10,b=10,t=10))
  })
  
  # Género por Especialidad (% interno registros)
  output$doc_plot_genero_especialidad <- renderPlotly({
    df <- doc_datos(); if(nrow(df)==0) return(NULL)
    datos <- df %>% mutate(Genero = ifelse(DOC_GENERO==2,'Mujeres', ifelse(DOC_GENERO==1,'Hombres','Desconocido'))) %>%
      count(Especialidad, Genero, name='n') %>% group_by(Especialidad) %>% mutate(total=sum(n), pct=round(100*n/total,1)) %>% ungroup()
    orden <- datos %>% filter(Genero=='Mujeres') %>% arrange(desc(pct)) %>% pull(Especialidad)
    # Si alguna especialidad no tiene Mujeres (solo Hombres), la agregamos al final manteniendo orden previo
    restantes <- setdiff(unique(datos$Especialidad), orden)
    niveles <- c(orden, restantes)
    datos$Especialidad <- factor(datos$Especialidad, levels=niveles)
    pal_genero <- c('Hombres'='#34536A', 'Mujeres'='#B35A5A', 'Desconocido'='#888888')
    plotly::plot_ly(
      datos,
      x = ~Especialidad,
      y = ~pct,
      color = ~Genero,
      colors = pal_genero,
      type = 'bar',
      text = ~paste0(pct, '% (n=', n, ')'),
      textposition = 'outside',
      texttemplate = '%{text}',
      customdata = ~paste0(Genero, ': ', pct, '% (n=', n, ')'),
      hovertemplate = '<b>%{x}</b><br>%{customdata}<extra></extra>'
    ) %>%
      layout(
        barmode='stack',
        xaxis=list(title='Especialidad', tickangle=-45),
        yaxis=list(title='% dentro de especialidad', range = c(0, 100)),
        height = 520
      )
  })
  
  # Género por Región (% interno registros)
  output$doc_plot_genero_region <- renderPlotly({
    df <- doc_datos(); if(nrow(df)==0) return(NULL)
    datos <- df %>% mutate(Genero = ifelse(DOC_GENERO==2,'Mujeres', ifelse(DOC_GENERO==1,'Hombres','Desconocido'))) %>%
      count(NOM_REG_RBD_A, Genero, name='n') %>% group_by(NOM_REG_RBD_A) %>% mutate(total=sum(n), pct=round(100*n/total,1)) %>% ungroup()
    # Orden específico de regiones solicitado
    orden_deseado <- c("AYP", "TPCA", "ANTOF", "ATCMA", "COQ", "VALPO", "RM", "LGBO", "MAULE", "NUBLE", "BBIO", "ARAUC", "RIOS", "LAGOS", "AYSEN", "MAG")
    regiones_presentes <- intersect(orden_deseado, unique(datos$NOM_REG_RBD_A))
    regiones_faltantes <- setdiff(unique(datos$NOM_REG_RBD_A), orden_deseado)
    niveles_region <- c(regiones_presentes, regiones_faltantes)
    datos$NOM_REG_RBD_A <- factor(datos$NOM_REG_RBD_A, levels = niveles_region)
    pal_genero <- c('Hombres'='#34536A', 'Mujeres'='#B35A5A', 'Desconocido'='#888888')
    plotly::plot_ly(
      datos,
      x = ~NOM_REG_RBD_A,
      y = ~pct,
      color = ~Genero,
      colors = pal_genero,
      type = 'bar',
      text = ~paste0(pct, '% (n=', n, ')'),
      textposition = 'outside',
      texttemplate = '%{text}',
      customdata = ~paste0(Genero, ': ', pct, '% (n=', n, ')'),
      hovertemplate = '<b>%{x}</b><br>%{customdata}<extra></extra>'
    ) %>%
      layout(
        barmode='stack',
        xaxis=list(title='Región', tickangle=-45),
        yaxis=list(title='% dentro de región', range = c(0, 100)),
        height = 420
      )
  })
  
  # Cantidad de Docentes por Región
  output$doc_plot_cantidad_region <- renderPlotly({
    df <- doc_datos(); if(nrow(df)==0) return(NULL)
    datos <- df %>% 
      count(NOM_REG_RBD_A, name='total') %>%
      arrange(desc(total))
    # Aplicar el mismo orden de regiones que en el gráfico de género
    orden_deseado <- c("AYP", "TPCA", "ANTOF", "ATCMA", "COQ", "VALPO", "RM", "LGBO", "MAULE", "NUBLE", "BBIO", "ARAUC", "RIOS", "LAGOS", "AYSEN", "MAG")
    regiones_presentes <- intersect(orden_deseado, unique(datos$NOM_REG_RBD_A))
    regiones_faltantes <- setdiff(unique(datos$NOM_REG_RBD_A), orden_deseado)
    niveles_region <- c(regiones_presentes, regiones_faltantes)
    datos$NOM_REG_RBD_A <- factor(datos$NOM_REG_RBD_A, levels = niveles_region)
    
    plotly::plot_ly(
      datos,
      x = ~NOM_REG_RBD_A,
      y = ~total,
      type = 'bar',
      marker = list(color = '#5A6E79'),
      text = ~total,
      textposition = 'outside',
      texttemplate = '%{text}',
      hovertemplate = '<b>%{x}</b><br>Total docentes: %{y}<extra></extra>'
    ) %>%
      layout(
        xaxis = list(title = 'Región', tickangle = -45),
        yaxis = list(title = 'Número de Docentes'),
        height = 420
      )
  })
  
  # Distribución por Dependencia administrativa (registros)
  output$doc_plot_dependencia <- renderPlotly({
    df <- doc_datos(); if(nrow(df)==0) return(NULL)
    datos <- df %>% mutate(Dependencia = Dependencia_label) %>%
      count(Dependencia, name='n') %>% mutate(pct = round(100*n/sum(n),1)) %>% arrange(desc(n))
    plotly::plot_ly(datos, x=~pct, y=~reorder(Dependencia, pct), type='bar', orientation='h', text=~paste0(pct,'% (',n,')'), hoverinfo='text', marker=list(color='#34536A')) %>%
      layout(xaxis=list(title='% de registros'), yaxis=list(title='Dependencia'))
  })
  
  output$doc_tab_dependencia <- DT::renderDT({
    df <- doc_datos(); if(nrow(df)==0) return(DT::datatable(data.frame(Mensaje='Sin datos'), options=list(dom='t')))
    tabla <- df %>% mutate(Dependencia = Dependencia_label) %>%
      count(Dependencia, name='Registros') %>% mutate(`%` = round(100*Registros/sum(Registros),1)) %>% arrange(desc(Registros))
    DT::datatable(tabla, options=list(pageLength=8, dom='tip'), rownames=FALSE)
  })
  
  # Distribución por Ruralidad (registros)
  output$doc_plot_ruralidad <- renderPlotly({
    df <- doc_datos(); if(nrow(df)==0) return(NULL)
    datos <- df %>% mutate(Ruralidad = Ruralidad_label) %>% filter(Ruralidad %in% c('Urbana','Rural')) %>%
      count(Ruralidad, name='n') %>% mutate(pct=round(100*n/sum(n),1))
    plotly::plot_ly(datos, labels=~Ruralidad, values=~n, type='pie', text=~paste0(Ruralidad,': ',pct,'% (',n,')'), textinfo='text', hoverinfo='text', marker=list(colors=c('#34536A','#B35A5A'))) %>%
      layout(showlegend=FALSE, margin=list(l=10,r=10,b=10,t=10))
  })
  
  # Tramo Carrera Docente
  output$doc_plot_tramo <- renderPlotly({
    df <- doc_datos(); if(nrow(df)==0) return(NULL)
    datos <- df %>% distinct(MRUN, TRAMO_LABEL) %>% filter(!is.na(TRAMO_LABEL)) %>% count(TRAMO_LABEL, name='n') %>% mutate(pct=round(100*n/sum(n),1)) %>% arrange(n)
    plotly::plot_ly(datos, x=~pct, y=~TRAMO_LABEL, type='bar', orientation='h', text=~paste0(pct,'%'), marker=list(color='#34536A')) %>%
      layout(xaxis=list(title='% de docentes'), yaxis=list(title='Tramo'), margin=list(l=120))
  })
  
  # Distribución de Edades
  output$doc_plot_edad <- renderPlotly({
    df <- doc_datos(); if(nrow(df)==0) return(NULL)
    edades <- df %>% distinct(MRUN, EDAD) %>% filter(!is.na(EDAD))
    if(nrow(edades)==0) return(NULL)
    med <- stats::median(edades$EDAD)
    plotly::plot_ly(edades, x=~EDAD, type='histogram', nbinsx=30, marker=list(color='#34536A')) %>%
      layout(
        shapes=list(list(type='line', x0=med, x1=med, y0=0, y1=1, xref='x', yref='paper', line=list(color='#B35A5A', dash='dash'))),
        annotations=list(list(x=med, y=1.02, xref='x', yref='paper', text=paste('Mediana:', med),
                              showarrow=FALSE, font=list(color='#B35A5A', size=12), yanchor='bottom')),
        xaxis=list(title='Edad'), yaxis=list(title='Docentes (personas)')
      )
  })
  
  # Años de servicio
  output$doc_plot_servicio <- renderPlotly({
    df <- doc_datos(); if(nrow(df)==0) return(NULL)
    serv <- df %>% distinct(MRUN, ANOS_SERV) %>% filter(!is.na(ANOS_SERV), ANOS_SERV>=0, ANOS_SERV<=60)
    if(nrow(serv)==0) return(NULL)
    med <- stats::median(serv$ANOS_SERV); prom <- mean(serv$ANOS_SERV)
    plotly::plot_ly(serv, x=~ANOS_SERV, type='histogram', nbinsx=30, marker=list(color='#B35A5A')) %>%
      layout(
        shapes=list(
          list(type='line', x0=med, x1=med, y0=0, y1=1, xref='x', yref='paper', line=list(color='#34536A', width=2)),
          list(type='line', x0=prom, x1=prom, y0=0, y1=1, xref='x', yref='paper', line=list(color='#34536A', dash='dash'))
        ),
        annotations=list(
          list(x=med, y=1.02, xref='x', yref='paper', text=paste('Mediana:', med),
               showarrow=FALSE, font=list(color='#34536A', size=12), yanchor='bottom'),
          list(x=prom, y=0.92, xref='x', yref='paper', text=paste('Promedio:', round(prom,1)),
               showarrow=FALSE, font=list(color='#34536A', size=12), yanchor='top')
        ),
        xaxis=list(title='Años de servicio'), yaxis=list(title='Docentes (personas)')
      )
  })
  
  # --- Nuevos outputs para pestaña "Títulos y Función" ---
  
  output$doc_plot_tipo_institucion <- renderPlotly({
    df <- doc_datos(); if(nrow(df)==0) return(NULL)
    
    # Clasificación correcta según codificación TIT_ID:
    # 0 = Sin Información
    # 1 = Titulado en Educación
    # 2 = Titulado en Otras Áreas
    # 3 = No titulado
    df_tit <- df %>%
      distinct(MRUN, TIT_ID_1, TIT_ID_2) %>%
      mutate(
        # Categorías según codificación oficial
        Categoria = case_when(
          # Tiene título en Educación (con o sin otro título)
          TIT_ID_1 == 1 | TIT_ID_2 == 1 ~ "Titulado en Educación",
          # Solo tiene título en otras áreas
          (TIT_ID_1 == 2 | TIT_ID_2 == 2) ~ "Titulado en Otras Áreas",
          # No titulado
          TIT_ID_1 == 3 ~ "No titulado",
          # Sin información
          TIT_ID_1 == 0 | is.na(TIT_ID_1) ~ "Sin información",
          # Otros casos
          TRUE ~ "Otra situación"
        )
      ) %>%
      count(Categoria) %>%
      mutate(
        pct = round(100 * n / sum(n), 1),
        # Orden para el gráfico
        Categoria = factor(Categoria, levels = c(
          "Titulado en Educación",
          "Titulado en Otras Áreas",
          "No titulado",
          "Sin información",
          "Otra situación"
        ))
      ) %>%
      filter(n > 0) %>%  # Solo mostrar categorías con datos
      arrange(Categoria)
    
    if(nrow(df_tit)==0) return(NULL)
    
    # Colores diferenciados
    colores_map <- c(
      "Titulado en Educación" = "#3C7F6D",           # Verde
      "Titulado en Otras Áreas" = "#34536A",         # Azul
      "No titulado" = "#C62828",                      # Rojo
      "Sin información" = "#757575",                  # Gris
      "Otra situación" = "#9E9E9E"                    # Gris claro
    )
    
    df_tit <- df_tit %>%
      mutate(color = colores_map[as.character(Categoria)])
    
    # Gráfico de torta con porcentajes
    plotly::plot_ly(df_tit, 
                    labels = ~Categoria, 
                    values = ~n,
                    type = 'pie',
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    marker = list(colors = ~color),
                    hoverinfo = 'text',
                    text = ~paste0(Categoria, '<br>', n, ' docentes<br>', pct, '%')) %>%
      layout(showlegend = FALSE,
             margin = list(l=20, r=20, t=20, b=20))
  })
  
  output$doc_plot_tipo_titulo <- renderPlotly({
    df <- doc_datos(); if(nrow(df)==0) return(NULL)
    
    # Mapeo TIP_TIT_ID para títulos en Educación
    tip_tit_map <- c(
      "11" = "De Párvulos",
      "12" = "Diferencial",
      "13" = "Básica",
      "14" = "Media",
      "15" = "Parvularia y Básica",
      "16" = "Básica y Media"
    )
    
    # Considerar AMBOS títulos si están en Educación
    # Título 1 en Educación
    df_tip1 <- df %>%
      filter(TIT_ID_1 == 1) %>%  # Título 1 en Educación
      distinct(MRUN, TIP_TIT_ID_1) %>%
      filter(TIP_TIT_ID_1 %in% c(11, 12, 13, 14, 15, 16)) %>%
      select(MRUN, TIP_TIT = TIP_TIT_ID_1)
    
    # Título 2 en Educación
    df_tip2 <- df %>%
      filter(TIT_ID_2 == 1) %>%  # Título 2 en Educación
      distinct(MRUN, TIP_TIT_ID_2) %>%
      filter(TIP_TIT_ID_2 %in% c(11, 12, 13, 14, 15, 16)) %>%
      select(MRUN, TIP_TIT = TIP_TIT_ID_2)
    
    # Combinar ambos títulos (un docente puede aparecer dos veces si tiene 2 títulos en Educación)
    df_tip_combined <- bind_rows(df_tip1, df_tip2) %>%
      mutate(TIPO_TIT_LABEL = factor(tip_tit_map[as.character(TIP_TIT)], 
                                     levels = tip_tit_map)) %>%
      filter(!is.na(TIPO_TIT_LABEL)) %>%
      count(TIPO_TIT_LABEL) %>%
      mutate(pct = round(100 * n / sum(n), 1)) %>%
      arrange(n)  # Orden ascendente para barras horizontales
    
    if(nrow(df_tip_combined)==0) {
      # Mensaje si no hay datos
      return(plotly::plot_ly() %>%
        layout(annotations = list(
          text = "Sin datos de títulos en Educación",
          xref = "paper", yref = "paper",
          x = 0.5, y = 0.5, showarrow = FALSE,
          font = list(size = 14, color = "#757575")
        )))
    }
    
    # Gráfico de barras horizontal con porcentajes
    plotly::plot_ly(df_tip_combined, 
                    x = ~pct, 
                    y = ~TIPO_TIT_LABEL, 
                    type = 'bar',
                    orientation = 'h',
                    text = ~paste0(pct, '%'),
                    textposition = 'outside',
                    marker = list(color = '#34536A')) %>%
      layout(xaxis = list(title = '% del total de títulos en Educación'), 
             yaxis = list(title = ''),
             margin = list(l = 150, r = 50))
  })
  
  output$doc_plot_funcion <- renderPlotly({
    df <- doc_datos(); if(nrow(df)==0) return(NULL)
    
    # Mapeo de funciones
    funcion_map <- c(
      "1" = "Docente de Aula",
      "2" = "Directiva",
      "3" = "Docente de Apoyo",
      "4" = "Otras funciones",
      "5" = "Sostenedor"
    )
    
    # Colores para cada función
    colores_funcion <- c(
      "Docente de Aula" = "#B35A5A",
      "Directiva" = "#34536A",
      "Docente de Apoyo" = "#7FAD84",
      "Otras funciones" = "#D4A574",
      "Sostenedor" = "#9B8AA4"
    )
    
    # Contar por docente único (MRUN)
    df_func <- df %>%
      distinct(MRUN, ID_IFP2) %>%
      mutate(FUNCION_LABEL = factor(funcion_map[as.character(ID_IFP2)], 
                                     levels = funcion_map)) %>%
      filter(!is.na(FUNCION_LABEL)) %>%
      count(FUNCION_LABEL) %>%
      mutate(
        pct = round(100 * n / sum(n), 1),
        color = colores_funcion[as.character(FUNCION_LABEL)]
      ) %>%
      arrange(desc(n))
    
    if(nrow(df_func)==0) return(NULL)
    
    # Gráfico de torta con porcentajes
    plotly::plot_ly(df_func, 
                    labels = ~FUNCION_LABEL, 
                    values = ~n,
                    type = 'pie',
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    marker = list(colors = ~color),
                    hoverinfo = 'text',
                    text = ~paste0(FUNCION_LABEL, '<br>', n, ' docentes<br>', pct, '%')) %>%
      layout(showlegend = FALSE,
             margin = list(l=20, r=20, t=20, b=20))
  })
  
  # Nuevo gráfico: Tipo de Institución donde estudió
  output$doc_plot_tipo_institucion_estudio <- renderPlotly({
    df <- doc_datos(); if(nrow(df)==0) return(NULL)
    
    # Mapeo de tipos de institución
    tipo_insti_map <- c(
      "0" = "No posee título",
      "1" = "Universidad",
      "2" = "Centro de Formación Técnica (CFT)",
      "3" = "Instituto Profesional (IP)",
      "4" = "Escuela Normal",
      "5" = "Otro tipo de institución"
    )
    
    # Colores para cada tipo de institución
    colores_insti <- c(
      "Universidad" = "#34536A",
      "Centro de Formación Técnica (CFT)" = "#7FAD84",
      "Instituto Profesional (IP)" = "#D4A574",
      "Escuela Normal" = "#9B8AA4",
      "Otro tipo de institución" = "#B35A5A",
      "No posee título" = "#757575"
    )
    
    # Procesar institución 1
    df_insti1 <- df %>%
      distinct(MRUN, TIP_INSTI_ID_1) %>%
      filter(!is.na(TIP_INSTI_ID_1)) %>%
      select(MRUN, TIP_INSTI = TIP_INSTI_ID_1)
    
    # Procesar institución 2
    df_insti2 <- df %>%
      distinct(MRUN, TIP_INSTI_ID_2) %>%
      filter(!is.na(TIP_INSTI_ID_2), TIP_INSTI_ID_2 != 0) %>%
      select(MRUN, TIP_INSTI = TIP_INSTI_ID_2)
    
    # Combinar ambas instituciones
    df_insti_combined <- bind_rows(df_insti1, df_insti2) %>%
      mutate(TIPO_INSTI_LABEL = tipo_insti_map[as.character(TIP_INSTI)]) %>%
      filter(!is.na(TIPO_INSTI_LABEL)) %>%
      count(TIPO_INSTI_LABEL) %>%
      mutate(
        pct = round(100 * n / sum(n), 1),
        color = colores_insti[TIPO_INSTI_LABEL]
      ) %>%
      arrange(desc(n))
    
    if(nrow(df_insti_combined)==0) {
      return(plotly::plot_ly() %>%
        layout(annotations = list(
          text = "Sin datos de tipo de institución",
          xref = "paper", yref = "paper",
          x = 0.5, y = 0.5, showarrow = FALSE,
          font = list(size = 14, color = "#757575")
        )))
    }
    
    # Gráfico de barras horizontal con porcentajes
    plotly::plot_ly(df_insti_combined, 
                    x = ~pct, 
                    y = ~reorder(TIPO_INSTI_LABEL, pct), 
                    type = 'bar',
                    orientation = 'h',
                    text = ~paste0(pct, '%'),
                    textposition = 'outside',
                    marker = list(color = ~color)) %>%
      layout(xaxis = list(title = '% del total de títulos'), 
             yaxis = list(title = ''),
             margin = list(l = 250, r = 50),
             showlegend = FALSE)
  })
  
  output$doc_tab_titulos_resumen <- DT::renderDT({
    df <- doc_datos(); if(nrow(df)==0) return(NULL)
    
    # Mapeos
    tit_id_map <- c(
      "0" = "Sin Información",
      "1" = "Titulado en Educación",
      "2" = "Titulado en Otras Áreas",
      "3" = "No titulado"
    )
    
    tipo_institucion_map <- c(
      "0" = "No posee título",
      "1" = "Universidad",
      "2" = "CFT",
      "3" = "Instituto Profesional",
      "4" = "Escuela Normal",
      "5" = "Otro tipo"
    )
    
    # Resumen cruzado: Área de Titulación x Tipo de Institución
    df_resumen <- df %>%
      distinct(MRUN, TIT_ID_1, TIP_INSTI_ID_1) %>%
      mutate(
        Area_Titulo = tit_id_map[as.character(TIT_ID_1)],
        Tipo_Institucion = tipo_institucion_map[as.character(TIP_INSTI_ID_1)],
        Area_Titulo = ifelse(is.na(Area_Titulo), "Sin Información", Area_Titulo),
        Tipo_Institucion = ifelse(is.na(Tipo_Institucion), "Sin Información", Tipo_Institucion)
      ) %>%
      count(Area_Titulo, Tipo_Institucion) %>%
      tidyr::pivot_wider(names_from = Tipo_Institucion, values_from = n, values_fill = 0) %>%
      mutate(Total = rowSums(across(where(is.numeric)))) %>%
      arrange(desc(Total))
    
    DT::datatable(df_resumen, 
                  options=list(pageLength=10, scrollX=TRUE, dom='t'), 
                  rownames=FALSE,
                  colnames = c('Área de Titulación', names(df_resumen)[-1]))
  })
  
  # --- Fin nuevos outputs ---
  
  output$doc_tab_detalle <- DT::renderDT({
    df <- doc_datos();
    cols <- c('MRUN','RBD','NOM_RBD','NOM_REG_RBD_A','NOM_COM_RBD','Dependencia_label','Ruralidad_label','Poblacion','SUBSECTOR','Especialidad','HORAS','TITULO')
    cols <- intersect(cols, names(df))
    DT::datatable(df[cols], extensions='Buttons',
                  options=list(pageLength=20, scrollX=TRUE, dom='Bfrtip',
                               buttons=list('copy','csv',list(extend='excel', title='docentes_detalle'))),
                  rownames=FALSE)
  })
  
  output$doc_descarga <- downloadHandler(
    filename = function(){ paste0('docentes_especialidad_', Sys.Date(), '.csv') },
    content = function(file){ write.csv(doc_datos(), file, row.names=FALSE, fileEncoding='UTF-8') }
  )
  
  # Descarga de reporte PDF de docentes
  output$doc_descarga_pdf <- downloadHandler(
    filename = function() {
      paste0("reporte_docentes_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Mostrar mensaje de progreso
      showNotification("Generando reporte PDF... Por favor espere.", 
                       type = "message", duration = NULL, id = "pdf_progress")
      
      tryCatch({
        # Obtener datos filtrados
        datos <- doc_datos()
        
        if (nrow(datos) == 0) {
          showNotification("No hay datos para generar el reporte", type = "error")
          return(NULL)
        }
        
        # Calcular ANOS_SERV si no existe
        if (!"ANOS_SERV" %in% names(datos) && "ANO_SERVICIO_SISTEMA" %in% names(datos)) {
          datos <- datos %>%
            dplyr::mutate(ANOS_SERV = suppressWarnings(as.numeric(ANO_SERVICIO_SISTEMA)))
        }
        
        # Calcular EDAD si no existe (DOC_FEC_NAC es string YYYYMM de 6 dígitos)
        if (!"EDAD" %in% names(datos) && "DOC_FEC_NAC" %in% names(datos)) {
          datos <- datos %>%
            dplyr::mutate(
              .fec = suppressWarnings(as.character(DOC_FEC_NAC)),
              .fec = ifelse(nchar(.fec) == 6, .fec, NA),
              .nac_year  = suppressWarnings(as.numeric(substr(.fec, 1, 4))),
              .nac_month = suppressWarnings(as.numeric(substr(.fec, 5, 6))),
              EDAD = ifelse(!is.na(.nac_year), {
                hoy <- Sys.Date()
                age <- as.numeric(format(hoy, "%Y")) - .nac_year -
                  ifelse(!is.na(.nac_month) & .nac_month > as.numeric(format(hoy, "%m")), 1, 0)
                ifelse(age < 15 | age > 90, NA_real_, age)
              }, NA_real_)
            ) %>%
            dplyr::select(-.fec, -.nac_year, -.nac_month)
        }
        
        # Preparar KPIs
        personas_unicas <- datos %>% dplyr::distinct(MRUN) %>% nrow()
        registros_totales <- nrow(datos)
        
        # Calcular mediana de edad
        mediana_edad_val <- datos %>% 
          dplyr::distinct(MRUN, .keep_all = TRUE) %>% 
          dplyr::pull(EDAD) %>% 
          stats::median(na.rm = TRUE)
        
        # Calcular promedio años de servicio
        prom_servicio_val <- datos %>% 
          dplyr::distinct(MRUN, .keep_all = TRUE) %>% 
          dplyr::pull(ANOS_SERV) %>% 
          mean(na.rm = TRUE)
        
        resumen_kpis <- list(
          registros = registros_totales,
          personas = personas_unicas,
          promedio_rbd = if(personas_unicas > 0) registros_totales / personas_unicas else 0,
          mediana_edad = if(!is.na(mediana_edad_val)) mediana_edad_val else 0,
          prom_servicio = if(!is.na(prom_servicio_val)) prom_servicio_val else 0,
          pct_mujeres = if(personas_unicas > 0) {
            datos %>% dplyr::distinct(MRUN, .keep_all = TRUE) %>%
              dplyr::summarise(pct = sum(DOC_GENERO == 2, na.rm = TRUE) / dplyr::n() * 100) %>%
              dplyr::pull(pct)
          } else 0
        )
        
        # Resumen por especialidad - verificar que existan las columnas
        if ("Especialidad" %in% names(datos)) {
          resumen_especialidad <- datos %>%
            dplyr::group_by(Especialidad) %>%
            dplyr::summarise(
              Registros = dplyr::n(),
              Docentes = dplyr::n_distinct(MRUN),
              Pct_Mujeres = sum(DOC_GENERO == 2, na.rm = TRUE) / dplyr::n() * 100,
              .groups = 'drop'
            ) %>%
            dplyr::arrange(desc(Docentes))
          
          # Renombrar columna para evitar problemas con %
          names(resumen_especialidad)[4] <- "Porcentaje_Mujeres"
          
        } else {
          resumen_especialidad <- data.frame(
            Especialidad = "No disponible",
            Registros = nrow(datos),
            Docentes = personas_unicas,
            Porcentaje_Mujeres = 0
          )
        }
        
        # Filtros aplicados
        filtros <- list(
          Región = input$doc_f_region,
          Comuna = input$doc_f_comuna,
          Enseñanza = input$doc_f_poblacion,
          Especialidad = paste(input$doc_f_especialidad, collapse = ", "),
          Género = input$doc_f_genero,
          Dependencia = input$doc_f_dependencia,
          Ruralidad = input$doc_f_ruralidad
        )
        
        # Generar plots (versión estática para PDF)
        # Plot género general
        plot_genero <- NULL
        if (personas_unicas > 0 && "DOC_GENERO_label" %in% names(datos)) {
          datos_genero <- datos %>%
            dplyr::distinct(MRUN, .keep_all = TRUE) %>%
            dplyr::count(DOC_GENERO_label) %>%
            dplyr::mutate(pct = n / sum(n) * 100)
          
          plot_genero <- ggplot(datos_genero, aes(x = "", y = pct, fill = DOC_GENERO_label)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y") +
            labs(fill = "Género", title = "Distribución por Género") +
            theme_minimal() +
            theme(axis.text = element_blank(),
                  axis.title = element_blank())
        }
        
        # Plot género por especialidad
        plot_genero_esp <- NULL
        if (nrow(datos) > 0 && "Especialidad" %in% names(datos)) {
          datos_esp <- datos %>%
            dplyr::group_by(Especialidad) %>%
            dplyr::summarise(
              Total = dplyr::n(),
              Mujeres = sum(DOC_GENERO == 2, na.rm = TRUE),
              Pct_Mujeres = Mujeres / Total * 100,
              .groups = 'drop'
            ) %>%
            dplyr::arrange(desc(Total)) %>%
            head(15)
          
          plot_genero_esp <- ggplot(datos_esp, aes(x = reorder(Especialidad, Pct_Mujeres), y = Pct_Mujeres)) +
            geom_col(fill = "#B35A5A") +
            coord_flip() +
            labs(x = NULL, y = "% Mujeres", title = "Género por Especialidad (Top 15)") +
            theme_minimal()
        }
        
        # Renderizar el RMarkdown
        rmarkdown::render(
          input = "templates/reporte_docentes_completo.Rmd",
          output_file = basename(file),
          output_dir = dirname(file),
          params = list(
            datos = datos,
            resumen_kpis = resumen_kpis,
            filtros_aplicados = filtros,
            resumen_especialidad = resumen_especialidad,
            plot_genero = plot_genero,
            plot_genero_esp = plot_genero_esp,
            plot_genero_region = NULL,
            plot_dependencia = NULL,
            plot_ruralidad = NULL,
            plot_edad = NULL,
            plot_servicio = NULL,
            plot_tramo = NULL,
            muestra_datos = datos
          ),
          envir = new.env(parent = globalenv())
        )
        
        removeNotification(id = "pdf_progress")
        showNotification("¡Reporte PDF generado exitosamente!", type = "message", duration = 5)
        
      }, error = function(e) {
        removeNotification(id = "pdf_progress")
        showNotification(paste("Error al generar PDF:", e$message), type = "error", duration = 10)
      })
    }
  )
  
  # Glosario/ayuda para la pestaña docentes
  output$glosario_docentes <- renderUI({
    tagList(
      div(class = "alert-info", style = "margin-bottom: 15px;",
          tags$strong("¿Qué muestra esta tabla?"),
          p("La base contiene docentes por establecimiento (RBD) y especialidad. Cada docente puede impartir Formación General (SUBSECTOR1/SUBSECTOR2 entre 31001 y 39501) o Módulos de Especialidad (otros códigos). Aquí se muestran principalmente los docentes que imparten módulos de especialidad.")
      ),
      div(class = "alert-info", style = "margin-bottom: 15px;",
          tags$strong("Códigos relevantes:"),
          tags$ul(
            tags$li("SUBSECTOR1/SUBSECTOR2: Código de la asignatura o módulo que imparte el docente."),
            tags$li("31001-39501: Formación General en una especialidad."),
            tags$li("Otros códigos: Módulos de especialidad EMTP.")
          )
      )
    )
  })
  # --- Botón para reiniciar filtros en Mapa de Matrícula ---
  observeEvent(input$reset_filtros_mapa, {
    updateSelectInput(session, "rft", selected = "Todas")
    updateSelectInput(session, "region", selected = "Todas")
    updateSelectInput(session, "provincia", selected = "Todas")
    updateSelectInput(session, "comuna", selected = "Todas")
    updateSelectizeInput(session, "especialidad", selected = "")
    updateSelectInput(session, "sector_mapa", selected = "Todos")
    updateSelectInput(session, "dependencia", selected = "Todas")
    updateSelectInput(session, "sostenedor_mapa", selected = "Todos")
  })
  
  # --- Botón para reiniciar filtros en Visualizaciones ---
  observeEvent(input$reset_filtros_viz, {
    updateSelectInput(session, "filtro_rft", selected = "Todas")
    updateSelectInput(session, "filtro_region_viz", selected = "Todas")
    updateSelectInput(session, "filtro_provincia", selected = "Todas")
    updateSelectInput(session, "filtro_comuna", selected = "Todas")
    updateSelectizeInput(session, "filtro_especialidad", selected = "")
    updateSelectInput(session, "nivel", selected = "Ambos")
    updateSelectInput(session, "filtro_dependencia", selected = "Todas")
    updateSelectInput(session, "filtro_sostenedor", selected = "Todos")
    # Si hay otros filtros personalizados, agregarlos aquí
    # Ejemplo: updateSelectInput(session, "grado", selected = "Todas")
  })
  # --- Botón para reiniciar filtros en Buscador de Establecimientos ---
  observeEvent(input$reset_filtros, {
    updateSelectInput(session, "rft_busqueda", selected = "Todas")
    updateSelectInput(session, "region_busqueda", selected = "Todas")
    updateSelectInput(session, "provincia_busqueda", selected = "Todas")
    updateSelectInput(session, "comuna_busqueda", selected = "Todas")
    updateSelectInput(session, "dependencia_busqueda", selected = "Todas")
    updateSelectInput(session, "sostenedor_busqueda", selected = "Todos")
    updateSelectizeInput(session, "especialidad_busqueda", selected = "")
    updateTextInput(session, "rbd_busqueda", value = "")
    updateTextInput(session, "nombre_busqueda", value = "")
  })
  
  # --- Habilitar/Deshabilitar botón de descarga según resultados ---
  observe({
    # Obtener la tabla de establecimientos filtrados
    tabla <- tryCatch({
      datos <- datos_visual()
      datos %>%
        group_by(rbd, nom_rbd, nom_reg_rbd_a, nom_com_rbd, nom_espe, cod_men) %>%
        summarise(Total = n(), .groups = "drop")
    }, error = function(e) NULL)
    
    habilitar <- !is.null(tabla) && nrow(tabla) > 0
    session$sendCustomMessage(type = "toggleDownloadBtn", message = list(enabled = habilitar))
  })
  # --- JS para habilitar/deshabilitar el botón de descarga ---
  shiny::addResourcePath("customjs", "www")
  tags$head(tags$script(src = "customjs/toggleDownload.js"))
  
  # Valores reactivos para el estado de la aplicación
  values <- reactiveValues(
    cargando = FALSE,
    mensaje_estado = "Listo"
  )
  
  # Función para mostrar mensajes de estado
  mostrar_estado <- function(mensaje) {
    values$mensaje_estado <- mensaje
    showNotification(mensaje, type = "message", duration = 3)
  }
  
  # Validación de datos al inicio
  observe({
    if(nrow(matricula_raw) == 0) {
      showNotification("Error: No se pudieron cargar los datos de matrícula", 
                       type = "error", duration = NULL)
    } else {
      mostrar_estado(paste("Datos cargados correctamente:", 
                           format(nrow(matricula_raw), big.mark = ","), 
                           "registros de matrícula"))
    }
  })
  
  # ========== FILTROS REACTIVOS PARA MAPA DE MATRÍCULA ==========
  # Los filtros se actualizan dinámicamente según las selecciones actuales
  
  # Helper function para obtener datos filtrados según selección actual
  get_datos_mapa_filtrados <- reactive({
    datos <- matricula_raw
    
    # Aplicar filtros acumulativos
    if (!is.null(input$rft) && input$rft != "Todas") {
      datos <- datos %>% filter(rft == input$rft)
    }
    if (!is.null(input$region) && input$region != "Todas") {
      datos <- datos %>% filter(nom_reg_rbd_a == input$region)
    }
    if (!is.null(input$provincia) && input$provincia != "Todas") {
      datos <- datos %>% filter(nom_deprov_rbd == input$provincia)
    }
    if (!is.null(input$comuna) && input$comuna != "Todas") {
      datos <- datos %>% filter(nom_com_rbd == input$comuna)
    }
    if (!is.null(input$dependencia) && input$dependencia != "Todas") {
      datos <- datos %>% filter(cod_depe2 == input$dependencia)
    }
    if (!is.null(input$sostenedor_mapa) && input$sostenedor_mapa != "Todos") {
      datos <- datos %>% filter(nombre_sost == input$sostenedor_mapa)
    }
    if (!is.null(input$especialidad) && length(input$especialidad) > 0) {
      datos <- datos %>% filter(nom_espe %in% input$especialidad)
    }
    
    return(datos)
  })
  
  # 1. Al cambiar RFT → actualizar región, provincia, comuna, dependencia, sostenedor
  observeEvent(input$rft, {
    datos <- matricula_raw
    if (input$rft != "Todas") {
      datos <- datos %>% filter(rft == input$rft)
    }
    
    # Mantener otros filtros si están activos
    if (!is.null(input$dependencia) && input$dependencia != "Todas") {
      datos <- datos %>% filter(cod_depe2 == input$dependencia)
    }
    if (!is.null(input$especialidad) && length(input$especialidad) > 0) {
      datos <- datos %>% filter(nom_espe %in% input$especialidad)
    }
    
    updateSelectInput(session, "region",
                      choices = c("Todas", sort(unique(datos$nom_reg_rbd_a))),
                      selected = "Todas")
    updateSelectInput(session, "provincia",
                      choices = c("Todas", sort(unique(datos$nom_deprov_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "comuna",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "sostenedor_mapa",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 2. Al cambiar Región → actualizar provincia, comuna, sostenedor
  observeEvent(input$region, {
    datos <- get_datos_mapa_filtrados()
    
    updateSelectInput(session, "provincia",
                      choices = c("Todas", sort(unique(datos$nom_deprov_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "comuna",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "sostenedor_mapa",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 3. Al cambiar Provincia → actualizar comuna, sostenedor
  observeEvent(input$provincia, {
    datos <- get_datos_mapa_filtrados()
    
    updateSelectInput(session, "comuna",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "sostenedor_mapa",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 4. Al cambiar Comuna → actualizar sostenedor
  observeEvent(input$comuna, {
    datos <- get_datos_mapa_filtrados()
    
    updateSelectInput(session, "sostenedor_mapa",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 5. Al cambiar Dependencia → actualizar sostenedor y verificar compatibilidad territorial
  observeEvent(input$dependencia, {
    datos <- get_datos_mapa_filtrados()
    
    updateSelectInput(session, "sostenedor_mapa",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 6. Al cambiar Especialidad → actualizar todos los filtros
  observeEvent(input$especialidad, {
    datos <- get_datos_mapa_filtrados()
    
    updateSelectInput(session, "rft",
                      choices = c("Todas", sort(unique(datos$rft))),
                      selected = input$rft)
    updateSelectInput(session, "region",
                      choices = c("Todas", sort(unique(datos$nom_reg_rbd_a))),
                      selected = input$region)
    updateSelectInput(session, "provincia",
                      choices = c("Todas", sort(unique(datos$nom_deprov_rbd))),
                      selected = input$provincia)
    updateSelectInput(session, "comuna",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = input$comuna)
    updateSelectInput(session, "sostenedor_mapa",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = input$sostenedor_mapa)
  })
  
  # 7. Al cambiar Sostenedor → actualizar filtros territoriales compatibles
  observeEvent(input$sostenedor_mapa, {
    datos <- get_datos_mapa_filtrados()
    
    updateSelectInput(session, "rft",
                      choices = c("Todas", sort(unique(datos$rft))),
                      selected = input$rft)
    updateSelectInput(session, "region",
                      choices = c("Todas", sort(unique(datos$nom_reg_rbd_a))),
                      selected = input$region)
    updateSelectInput(session, "provincia",
                      choices = c("Todas", sort(unique(datos$nom_deprov_rbd))),
                      selected = input$provincia)
    updateSelectInput(session, "comuna",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = input$comuna)
  })
  
  # ========== FIN FILTROS REACTIVOS MAPA ==========
  
  # ========== FILTROS REACTIVOS PARA BUSCADOR DE ESTABLECIMIENTOS ==========
  
  # Helper function para obtener datos filtrados según selección actual
  get_datos_busqueda_filtrados <- reactive({
    datos <- matricula_raw
    
    # Aplicar filtros acumulativos
    if (!is.null(input$rft_busqueda) && input$rft_busqueda != "Todas") {
      datos <- datos %>% filter(rft == input$rft_busqueda)
    }
    if (!is.null(input$region_busqueda) && input$region_busqueda != "Todas") {
      datos <- datos %>% filter(nom_reg_rbd_a == input$region_busqueda)
    }
    if (!is.null(input$provincia_busqueda) && input$provincia_busqueda != "Todas") {
      datos <- datos %>% filter(nom_deprov_rbd == input$provincia_busqueda)
    }
    if (!is.null(input$comuna_busqueda) && input$comuna_busqueda != "Todas") {
      datos <- datos %>% filter(nom_com_rbd == input$comuna_busqueda)
    }
    if (!is.null(input$dependencia_busqueda) && input$dependencia_busqueda != "Todas") {
      datos <- datos %>% filter(cod_depe2 == input$dependencia_busqueda)
    }
    if (!is.null(input$sostenedor_busqueda) && input$sostenedor_busqueda != "Todos") {
      datos <- datos %>% filter(nombre_sost == input$sostenedor_busqueda)
    }
    if (!is.null(input$especialidad_busqueda) && length(input$especialidad_busqueda) > 0) {
      datos <- datos %>% filter(nom_espe %in% input$especialidad_busqueda)
    }
    
    return(datos)
  })
  
  # 1. Al cambiar RFT → actualizar región, provincia, comuna, dependencia, sostenedor
  observeEvent(input$rft_busqueda, {
    datos <- matricula_raw
    if (input$rft_busqueda != "Todas") {
      datos <- datos %>% filter(rft == input$rft_busqueda)
    }
    
    # Mantener otros filtros si están activos
    if (!is.null(input$dependencia_busqueda) && input$dependencia_busqueda != "Todas") {
      datos <- datos %>% filter(cod_depe2 == input$dependencia_busqueda)
    }
    if (!is.null(input$especialidad_busqueda) && length(input$especialidad_busqueda) > 0) {
      datos <- datos %>% filter(nom_espe %in% input$especialidad_busqueda)
    }
    
    updateSelectInput(session, "region_busqueda",
                      choices = c("Todas", sort(unique(datos$nom_reg_rbd_a))),
                      selected = "Todas")
    updateSelectInput(session, "provincia_busqueda",
                      choices = c("Todas", sort(unique(datos$nom_deprov_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "comuna_busqueda",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "sostenedor_busqueda",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 2. Al cambiar Región → actualizar provincia, comuna, sostenedor
  observeEvent(input$region_busqueda, {
    datos <- get_datos_busqueda_filtrados()
    
    updateSelectInput(session, "provincia_busqueda",
                      choices = c("Todas", sort(unique(datos$nom_deprov_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "comuna_busqueda",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "sostenedor_busqueda",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 3. Al cambiar Provincia → actualizar comuna, sostenedor
  observeEvent(input$provincia_busqueda, {
    datos <- get_datos_busqueda_filtrados()
    
    updateSelectInput(session, "comuna_busqueda",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "sostenedor_busqueda",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 4. Al cambiar Comuna → actualizar sostenedor
  observeEvent(input$comuna_busqueda, {
    datos <- get_datos_busqueda_filtrados()
    
    updateSelectInput(session, "sostenedor_busqueda",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 5. Al cambiar Dependencia → actualizar sostenedor
  observeEvent(input$dependencia_busqueda, {
    datos <- get_datos_busqueda_filtrados()
    
    updateSelectInput(session, "sostenedor_busqueda",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 6. Al cambiar Especialidad → actualizar todos los filtros
  observeEvent(input$especialidad_busqueda, {
    datos <- get_datos_busqueda_filtrados()
    
    updateSelectInput(session, "rft_busqueda",
                      choices = c("Todas", sort(unique(datos$rft))),
                      selected = input$rft_busqueda)
    updateSelectInput(session, "region_busqueda",
                      choices = c("Todas", sort(unique(datos$nom_reg_rbd_a))),
                      selected = input$region_busqueda)
    updateSelectInput(session, "provincia_busqueda",
                      choices = c("Todas", sort(unique(datos$nom_deprov_rbd))),
                      selected = input$provincia_busqueda)
    updateSelectInput(session, "comuna_busqueda",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = input$comuna_busqueda)
    updateSelectInput(session, "sostenedor_busqueda",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = input$sostenedor_busqueda)
  })
  
  # 7. Al cambiar Sostenedor → actualizar filtros territoriales
  observeEvent(input$sostenedor_busqueda, {
    datos <- get_datos_busqueda_filtrados()
    
    updateSelectInput(session, "rft_busqueda",
                      choices = c("Todas", sort(unique(datos$rft))),
                      selected = input$rft_busqueda)
    updateSelectInput(session, "region_busqueda",
                      choices = c("Todas", sort(unique(datos$nom_reg_rbd_a))),
                      selected = input$region_busqueda)
    updateSelectInput(session, "provincia_busqueda",
                      choices = c("Todas", sort(unique(datos$nom_deprov_rbd))),
                      selected = input$provincia_busqueda)
    updateSelectInput(session, "comuna_busqueda",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = input$comuna_busqueda)
  })
  
  # ========== FIN FILTROS REACTIVOS BUSCADOR ==========
  
  # ========== FILTROS REACTIVOS PARA VISUALIZACIONES ==========
  
  # Helper function para obtener datos filtrados según selección actual
  get_datos_viz_filtrados <- reactive({
    datos <- matricula_raw
    
    # Aplicar filtros acumulativos
    if (!is.null(input$filtro_rft) && input$filtro_rft != "Todas") {
      datos <- datos %>% filter(rft == input$filtro_rft)
    }
    if (!is.null(input$filtro_region_viz) && input$filtro_region_viz != "Todas") {
      datos <- datos %>% filter(nom_reg_rbd_a == input$filtro_region_viz)
    }
    if (!is.null(input$filtro_provincia) && input$filtro_provincia != "Todas") {
      datos <- datos %>% filter(nom_deprov_rbd == input$filtro_provincia)
    }
    if (!is.null(input$filtro_comuna) && input$filtro_comuna != "Todas") {
      datos <- datos %>% filter(nom_com_rbd == input$filtro_comuna)
    }
    if (!is.null(input$filtro_dependencia) && input$filtro_dependencia != "Todas") {
      datos <- datos %>% filter(cod_depe2 == input$filtro_dependencia)
    }
    if (!is.null(input$filtro_sostenedor) && input$filtro_sostenedor != "Todos") {
      datos <- datos %>% filter(nombre_sost == input$filtro_sostenedor)
    }
    if (!is.null(input$filtro_especialidad) && length(input$filtro_especialidad) > 0) {
      datos <- datos %>% filter(nom_espe %in% input$filtro_especialidad)
    }
    
    return(datos)
  })
  
  # 1. Al cambiar RFT → actualizar región, provincia, comuna, dependencia, sostenedor
  observeEvent(input$filtro_rft, {
    datos <- matricula_raw
    if (input$filtro_rft != "Todas") {
      datos <- datos %>% filter(rft == input$filtro_rft)
    }
    
    # Mantener otros filtros si están activos
    if (!is.null(input$filtro_dependencia) && input$filtro_dependencia != "Todas") {
      datos <- datos %>% filter(cod_depe2 == input$filtro_dependencia)
    }
    if (!is.null(input$filtro_especialidad) && length(input$filtro_especialidad) > 0) {
      datos <- datos %>% filter(nom_espe %in% input$filtro_especialidad)
    }
    
    updateSelectInput(session, "filtro_region_viz",
                      choices = c("Todas", sort(unique(datos$nom_reg_rbd_a))),
                      selected = "Todas")
    updateSelectInput(session, "filtro_provincia",
                      choices = c("Todas", sort(unique(datos$nom_deprov_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "filtro_comuna",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "filtro_sostenedor",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 2. Al cambiar Región → actualizar provincia, comuna, sostenedor
  observeEvent(input$filtro_region_viz, {
    datos <- get_datos_viz_filtrados()
    
    updateSelectInput(session, "filtro_provincia",
                      choices = c("Todas", sort(unique(datos$nom_deprov_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "filtro_comuna",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "filtro_sostenedor",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 3. Al cambiar Provincia → actualizar comuna, sostenedor
  observeEvent(input$filtro_provincia, {
    datos <- get_datos_viz_filtrados()
    
    updateSelectInput(session, "filtro_comuna",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = "Todas")
    updateSelectInput(session, "filtro_sostenedor",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 4. Al cambiar Comuna → actualizar sostenedor
  observeEvent(input$filtro_comuna, {
    datos <- get_datos_viz_filtrados()
    
    updateSelectInput(session, "filtro_sostenedor",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 5. Al cambiar Dependencia → actualizar sostenedor
  observeEvent(input$filtro_dependencia, {
    datos <- get_datos_viz_filtrados()
    
    updateSelectInput(session, "filtro_sostenedor",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = "Todos")
  })
  
  # 6. Al cambiar Especialidad → actualizar todos los filtros
  observeEvent(input$filtro_especialidad, {
    datos <- get_datos_viz_filtrados()
    
    updateSelectInput(session, "filtro_rft",
                      choices = c("Todas", sort(unique(datos$rft))),
                      selected = input$filtro_rft)
    updateSelectInput(session, "filtro_region_viz",
                      choices = c("Todas", sort(unique(datos$nom_reg_rbd_a))),
                      selected = input$filtro_region_viz)
    updateSelectInput(session, "filtro_provincia",
                      choices = c("Todas", sort(unique(datos$nom_deprov_rbd))),
                      selected = input$filtro_provincia)
    updateSelectInput(session, "filtro_comuna",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = input$filtro_comuna)
    updateSelectInput(session, "filtro_sostenedor",
                      choices = c("Todos", sort(unique(datos$nombre_sost))),
                      selected = input$filtro_sostenedor)
  })
  
  # 7. Al cambiar Sostenedor → actualizar filtros territoriales
  observeEvent(input$filtro_sostenedor, {
    datos <- get_datos_viz_filtrados()
    
    updateSelectInput(session, "filtro_rft",
                      choices = c("Todas", sort(unique(datos$rft))),
                      selected = input$filtro_rft)
    updateSelectInput(session, "filtro_region_viz",
                      choices = c("Todas", sort(unique(datos$nom_reg_rbd_a))),
                      selected = input$filtro_region_viz)
    updateSelectInput(session, "filtro_provincia",
                      choices = c("Todas", sort(unique(datos$nom_deprov_rbd))),
                      selected = input$filtro_provincia)
    updateSelectInput(session, "filtro_comuna",
                      choices = c("Todas", sort(unique(datos$nom_com_rbd))),
                      selected = input$filtro_comuna)
  })
  
  # ========== FIN FILTROS REACTIVOS VISUALIZACIONES ==========
  
  
  
  # --- Optimización: Crear índices para joins más rápidos ---
  # Después de cargar los datos, antes del UI
  matricula_raw <- matricula_raw %>%
    arrange(rbd, cod_com_rbd) # Ordenar para mejorar joins
  
  base_apoyo <- base_apoyo %>%
    arrange(rbd)
  
  # Cache de datos frecuentemente usados
  especialidades_unicas <- sort(unique(matricula_raw$nom_espe))
  regiones_unicas <- sort(unique(matricula_raw$nom_reg_rbd_a))
  comunas_unicas <- sort(unique(matricula_raw$nom_com_rbd))
  sostenedores_unicos <- sort(unique(matricula_raw$nombre_sost))
  
  # Mejorar datos_filtrados con debounce para evitar recálculos excesivos
  datos_filtrados <- debounce(reactive({
    values$cargando <- TRUE
    on.exit(values$cargando <- FALSE)
    
    comunas_f <- comunas
    matricula_f <- matricula_raw
    
    # Aplicar filtros de manera más eficiente
    filtros <- list(
      region = if(input$region != "Todas") input$region else NULL,
      provincia = if(input$provincia != "Todas") input$provincia else NULL,
      comuna = if(input$comuna != "Todas") input$comuna else NULL,
      rft = if(input$rft != "Todas") input$rft else NULL,
      dependencia = if(input$dependencia != "Todas") input$dependencia else NULL,
      sostenedor = if(input$sostenedor_mapa != "Todos") input$sostenedor_mapa else NULL
    )
    
    # Aplicar filtros solo si existen
    if(!is.null(filtros$region)) {
      comunas_f <- comunas_f %>% filter(nom_reg_rbd_a == filtros$region)
      matricula_f <- matricula_f %>% filter(nom_reg_rbd_a == filtros$region)
    }
    
    if(!is.null(filtros$provincia)) {
      comunas_f <- comunas_f %>% filter(nom_deprov_rbd == filtros$provincia)
      matricula_f <- matricula_f %>% filter(nom_deprov_rbd == filtros$provincia)
    }
    
    if(!is.null(filtros$comuna)) {
      comunas_f <- comunas_f %>% filter(nom_com_rbd == filtros$comuna)
      matricula_f <- matricula_f %>% filter(nom_com_rbd == filtros$comuna)
    }
    
    if(!is.null(filtros$rft)) {
      comunas_f <- comunas_f %>% filter(rft == filtros$rft)
      matricula_f <- matricula_f %>% filter(rft == filtros$rft)
    }
    
    if(!is.null(filtros$dependencia)) {
      matricula_f <- matricula_f %>% filter(cod_depe2 == filtros$dependencia)
    }
    
    if(!is.null(filtros$sostenedor)) {
      matricula_f <- matricula_f %>% filter(nombre_sost == filtros$sostenedor)
      comunas_f <- comunas_f %>% filter(cod_comuna %in% unique(matricula_f$cod_comuna))
    }
    
    # Filtro por Sector Económico (agrupación oficial de especialidades, COD_SEC)
    if(!is.null(input$sector_mapa) && input$sector_mapa != "Todos") {
      matricula_f <- matricula_f %>% filter(nom_sector == input$sector_mapa)
      comunas_f <- comunas_f %>% filter(cod_comuna %in% unique(matricula_f$cod_comuna))
    }

    # Filtro por especialidad
    if(!is.null(input$especialidad) && length(input$especialidad) > 0 && !"Todas" %in% input$especialidad) {
      comunas_f <- comunas_f %>% filter(nom_espe %in% input$especialidad)
      matricula_f <- matricula_f %>% filter(nom_espe %in% input$especialidad)
    }
    
    # Actualizar colores del mapa
    comunas_actual <- comunas %>%
      mutate(
        fill_color_final = ifelse(cod_comuna %in% comunas_f$cod_comuna, fill_color_final, "#BBBBBB"),
        fill_opacity = ifelse(cod_comuna %in% comunas_f$cod_comuna, fill_opacity, 0.2)
      )
    
    mostrar_estado(paste("Filtros aplicados:", format(nrow(matricula_f), big.mark = ","), "registros"))
    
    list(comunas = comunas_actual, matricula = matricula_f)
  }), 500) # Debounce de 500ms

  # ===== Indicadores del territorio (Mapa) — reactivos a los filtros =====
  mapa_base <- reactive({
    rbds <- tryCatch(unique(datos_filtrados()$matricula$rbd), error = function(e) NULL)
    if (is.null(rbds) || length(rbds) == 0) return(base_apoyo[0, ])
    base_apoyo %>% dplyr::filter(as.character(rbd) %in% as.character(rbds))
  })
  .mnum <- function(x) suppressWarnings(as.numeric(x))

  output$mapa_ind_kpis <- renderUI({
    d <- mapa_base()
    n_ee    <- nrow(d)
    n_simce <- sum(!is.na(.mnum(d$prom_lect2m_rbd)))
    ive <- mean(.mnum(d$IVE), na.rm = TRUE)
    pl  <- mean(.mnum(d$prom_lect2m_rbd), na.rm = TRUE)
    pm  <- mean(.mnum(d$prom_mate2m_rbd), na.rm = TRUE)
    fmt <- function(x) if (is.na(x) || is.nan(x)) "s/i" else round(x)
    card <- function(t, v, sub) column(3, div(class = "metric-card",
      h5(t), h2(v), tags$small(style = "color:#777", sub)))
    fluidRow(
      card("Establecimientos", format(n_ee, big.mark = "."), paste0(format(n_simce, big.mark="."), " con SIMCE")),
      card("IVE promedio", if (is.nan(ive)) "s/i" else paste0(round(ive), "%"), "vulnerabilidad"),
      card("SIMCE Lectura", fmt(pl), "promedio del territorio"),
      card("SIMCE Matemática", fmt(pm), "promedio del territorio")
    )
  })

  output$mapa_simce_dist <- renderPlotly({
    d <- mapa_base()
    agg <- function(suf) {
      ins <- sum(.mnum(d[[paste0("n_palu_eda_ins_", suf)]]), na.rm = TRUE)
      ele <- sum(.mnum(d[[paste0("n_palu_eda_ele_", suf)]]), na.rm = TRUE)
      ade <- sum(.mnum(d[[paste0("n_palu_eda_ade_", suf)]]), na.rm = TRUE)
      tot <- ins + ele + ade
      if (tot == 0) return(c(NA, NA, NA)); round(c(ins, ele, ade) / tot * 100, 1)
    }
    L <- agg("lect2m_rbd"); M <- agg("mate2m_rbd")
    if (all(is.na(c(L, M))))
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = list(text = "Sin datos SIMCE en el territorio filtrado")))
    df <- data.frame(Prueba = factor(c("Matemática", "Lectura"), levels = c("Matemática", "Lectura")),
                     Insuficiente = c(M[1], L[1]), Elemental = c(M[2], L[2]), Adecuado = c(M[3], L[3]))
    lbl <- function(v) ifelse(is.na(v) | v == 0, "", paste0(round(v), "%"))
    p <- plot_ly(df, y = ~Prueba, x = ~Insuficiente, type = "bar", orientation = "h", name = "Insuficiente",
                 marker = list(color = "#C0392B"), text = ~lbl(Insuficiente), textposition = "inside",
                 insidetextfont = list(color = "white"), hoverinfo = "name+x") %>%
      add_trace(x = ~Elemental, name = "Elemental", marker = list(color = "#D4A017"), text = ~lbl(Elemental)) %>%
      add_trace(x = ~Adecuado, name = "Adecuado", marker = list(color = "#1E8449"), text = ~lbl(Adecuado)) %>%
      layout(barmode = "stack", xaxis = list(title = "% de estudiantes", range = c(0, 100), ticksuffix = "%"),
             yaxis = list(title = ""), legend = list(orientation = "h", x = 0, y = 1.2), margin = list(l = 80, t = 28))
    apply_plotly_theme(p)
  })

  output$mapa_idps_plot <- renderPlotly({
    d <- mapa_base()
    lab <- c("Autoestima y Motivación", "Clima de Convivencia",
             "Participación y F. Ciudadana", "Hábitos de Vida Saludable")
    vals <- sapply(1:4, function(i) round(mean(.mnum(d[[paste0("IDPS", i, "_Puntaje")]]), na.rm = TRUE)))
    if (all(is.na(vals)))
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = list(text = "Sin datos IDPS en el territorio filtrado")))
    g <- data.frame(Ind = lab, val = vals)
    g <- g[order(g$val), ]; g$Ind <- factor(g$Ind, levels = g$Ind)
    band <- ifelse(is.na(g$val), "#BBBBBB", ifelse(g$val < 60, "#C0392B", ifelse(g$val < 75, "#D4A017", "#1E8449")))
    p <- plot_ly(g, x = ~val, y = ~Ind, type = "bar", orientation = "h", marker = list(color = band),
                 text = ~val, textposition = "outside", hoverinfo = "x+y") %>%
      layout(xaxis = list(title = "Puntaje (0–100)", range = c(0, 110)), yaxis = list(title = ""), margin = list(l = 10))
    apply_plotly_theme(p)
  })

  output$mapa_gse_plot <- renderPlotly({
    d <- mapa_base()
    niveles  <- c("Bajo","Medio Bajo","Medio","Medio Alto","Alto")
    gse_map  <- setNames(niveles, c("1","2","3","4","5"))
    gse_col  <- c("Bajo"="#963A3A","Medio Bajo"="#B35A5A","Medio"="#C2A869",
                  "Medio Alto"="#5A6E79","Alto"="#34536A")
    g <- d %>% dplyr::mutate(GSE = gse_map[as.character(cod_grupo)]) %>%
      dplyr::filter(!is.na(GSE)) %>% dplyr::count(GSE)
    if (nrow(g) == 0)
      return(plotly_empty() %>% layout(title = list(text = "Sin clasificación GSE")))
    g <- g %>% dplyr::mutate(GSE = factor(GSE, levels = niveles)) %>% dplyr::arrange(GSE)
    p <- plot_ly(g, labels = ~GSE, values = ~n, type = "pie", hole = 0.55, sort = FALSE,
                 direction = "clockwise",
                 marker = list(colors = gse_col[as.character(g$GSE)], line = list(color = "#fff", width = 1.5)),
                 textinfo = "label+percent", textposition = "outside",
                 hovertemplate = "%{label}: %{value} EE (%{percent})<extra></extra>") %>%
      layout(showlegend = FALSE,
             annotations = list(list(text = paste0("<b>", sum(g$n), "</b><br>EE"),
                                     showarrow = FALSE, font = list(size = 15, color = "#34536A"))))
    apply_plotly_theme(p)
  })

  output$mapa_asis_prom <- renderText({
    d <- tryCatch(datos_filtrados()$matricula, error = function(e) NULL)
    if (is.null(d)) return("")
    m <- mean(suppressWarnings(as.numeric(d$tasa_asis_anual)), na.rm = TRUE)
    if (is.nan(m) || is.na(m)) "" else paste0("Promedio: ", gsub("\\.", ",", sprintf("%.1f", m)), "%")
  })

  output$mapa_asis_plot <- renderPlotly({
    d <- tryCatch(datos_filtrados()$matricula, error = function(e) NULL)
    if (is.null(d)) return(plotly_empty())
    lab <- c("1"="Crítica (<50%)","2"="Grave (50-84%)","3"="Reiterada (85-89%)","4"="Esperada (>=90%)")
    col <- c("Crítica (<50%)"="#963A3A","Grave (50-84%)"="#C0392B",
             "Reiterada (85-89%)"="#D4A017","Esperada (>=90%)"="#1E8449")
    g <- d %>% dplyr::filter(!is.na(categoria_asis_anual)) %>%
      dplyr::mutate(Cat = lab[as.character(categoria_asis_anual)]) %>%
      dplyr::filter(!is.na(Cat)) %>% dplyr::count(Cat)
    if (nrow(g) == 0)
      return(plotly_empty() %>% layout(title = list(text = "Sin datos de asistencia en el territorio")))
    g <- g %>% dplyr::mutate(pct = round(100 * n / sum(n), 1),
                             Cat = factor(Cat, levels = rev(c("Crítica (<50%)","Grave (50-84%)","Reiterada (85-89%)","Esperada (>=90%)"))))
    p <- plot_ly(g, x = ~pct, y = ~Cat, type = "bar", orientation = "h",
                 marker = list(color = col[as.character(g$Cat)]),
                 text = ~paste0(pct, "%"), textposition = "outside",
                 hovertext = ~paste0(Cat, "<br>", format(n, big.mark="."), " estudiantes (", pct, "%)"),
                 hoverinfo = "text") %>%
      layout(xaxis = list(title = "% de estudiantes EMTP", range = c(0, 100), ticksuffix = "%"),
             yaxis = list(title = ""), margin = list(l = 10))
    apply_plotly_theme(p)
  })
  
  # ========================= KPIs Inicio (reubicados dentro del server) =========================
  # KPIs de Inicio: SIEMPRE fijos (totales EMTP del sistema). No dependen de los
  # filtros de Análisis Territorial ni de ninguna otra pestaña → usan el dataset
  # completo (matricula_raw), no datos_filtrados().
  output$kpi_total_matricula_inicio <- renderText({
    format(nrow(matricula_raw), big.mark = ".")
  })
  output$kpi_establecimientos_inicio <- renderText({
    format(dplyr::n_distinct(matricula_raw$rbd), big.mark = ".")
  })
  output$kpi_hombres_inicio <- renderText({
    format(sum(matricula_raw$gen_alu == 1, na.rm = TRUE), big.mark = ".")
  })
  output$kpi_mujeres_inicio <- renderText({
    format(sum(matricula_raw$gen_alu == 2, na.rm = TRUE), big.mark = ".")
  })
  
  # Filtro dinámico para grado según el nivel
  # UI dinámica para grado (no cambia)
  output$grado_ui <- renderUI({
    if (input$nivel == "Niños y Jóvenes") {
      selectInput("grado", "Grado:",
                  choices = c("Todos" = "Todos",
                              "3° medio" = "3",
                              "4° medio" = "4"),
                  selected = "Todos")
    } else if (input$nivel == "Adultos") {
      selectInput("grado", "Grado:",
                  choices = c("Todos" = "Todos",
                              "1° nivel (1° y 2° medio sólo adultos)" = "1"),
                  selected = "Todos")
    } else {
      selectInput("grado", "Grado:",
                  choices = c("Todos" = "Todos",
                              "1° nivel (1° y 2° medio sólo adultos)" = "1",
                              "3° medio" = "3",
                              "4° medio" = "4"),
                  selected = "Todos")
    }
  })
  
  # --- Datos filtrados con TODOS los filtros territoriales y de categorización ---
  datos_visual <- reactive({
    datos <- matricula_raw
    
    # Filtros territoriales
    if (!is.null(input$filtro_rft) && input$filtro_rft != "Todas") {
      datos <- datos %>% filter(rft == input$filtro_rft)
    }
    
    if (!is.null(input$filtro_region_viz) && input$filtro_region_viz != "Todas") {
      datos <- datos %>% filter(nom_reg_rbd_a == input$filtro_region_viz)
    }
    
    if (!is.null(input$filtro_provincia) && input$filtro_provincia != "Todas") {
      datos <- datos %>% filter(nom_deprov_rbd == input$filtro_provincia)
    }
    
    if (!is.null(input$filtro_comuna) && input$filtro_comuna != "Todas") {
      datos <- datos %>% filter(nom_com_rbd == input$filtro_comuna)
    }
    
    # Filtro por especialidad
    if (!is.null(input$filtro_especialidad) && length(input$filtro_especialidad) > 0) {
      datos <- datos %>% filter(nom_espe %in% input$filtro_especialidad)
    }
    
    # Filtro por dependencia
    if (!is.null(input$filtro_dependencia) && input$filtro_dependencia != "Todas") {
      datos <- datos %>% filter(cod_depe2 == input$filtro_dependencia)
    }
    
    # Filtro por sostenedor
    if (!is.null(input$filtro_sostenedor) && input$filtro_sostenedor != "Todos") {
      datos <- datos %>% filter(nombre_sost == input$filtro_sostenedor)
    }
    
    # Filtro por nivel
    if (input$nivel == "Niños y Jóvenes") {
      datos <- datos %>% filter(cod_ense2 == 7)
    } else if (input$nivel == "Adultos") {
      datos <- datos %>% filter(cod_ense2 == 8)
    }
    
    # Filtro por grado
    if (!is.null(input$grado) && input$grado != "Todos") {
      datos <- datos %>% filter(cod_grado == as.numeric(input$grado))
    }
    
    # Etiquetas para grado
    datos <- datos %>%
      mutate(grado_etiqueta = case_when(
        cod_grado == 1 ~ "1° nivel (1° y 2° medio sólo adultos)",
        cod_grado == 3 ~ "3° medio",
        cod_grado == 4 ~ "4° medio",
        TRUE ~ as.character(cod_grado)
      ))
    
    datos
  })
  
  # --- Resúmenes para las tarjetas ---
  output$total_matricula <- renderText({
    nrow(datos_visual())
  })
  
  output$total_matricula_filtrada <- renderText({
    format(nrow(datos_visual()), big.mark = ".")
  })
  
  output$total_hombres <- renderText({
    sum(datos_visual()$gen_alu == 1, na.rm = TRUE)
  })
  
  output$total_mujeres <- renderText({
    sum(datos_visual()$gen_alu == 2, na.rm = TRUE)
  })
  
  # Total establecimientos
  output$total_establecimientos <- renderText({
    n_distinct(datos_visual()$rbd)
  })
  
  output$tabla_dependencia <- renderDT({
    datos_dep <- datos_visual() %>%
      group_by(cod_depe2) %>%
      summarise(N = n_distinct(rbd), .groups = "drop") %>%
      mutate(
        "Dependencia" = sapply(cod_depe2, obtener_nombre_dependencia),
        "Pct" = round(N / sum(N) * 100, 1)
      ) %>%
      select("Dependencia", "N", "Pct")
    
    datatable(
      datos_dep,
      colnames = c("Dependencia", "Establecimientos", "%"),
      options = list(
        dom = 't',          # Solo la tabla sin filtros ni paginación
        paging = FALSE,
        ordering = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      rownames = FALSE
    )
  }, server = FALSE)
  
  # Porcentaje de Hombres
  output$pct_hombres <- renderText({
    total <- nrow(datos_visual())
    if (total == 0) return("0%")
    hombres <- sum(datos_visual()$gen_alu == 1, na.rm = TRUE)
    paste0(round(hombres / total * 100, 1), "%")
  })
  
  # Porcentaje de Mujeres
  output$pct_mujeres <- renderText({
    total <- nrow(datos_visual())
    if (total == 0) return("0%")
    mujeres <- sum(datos_visual()$gen_alu == 2, na.rm = TRUE)
    paste0(round(mujeres / total * 100, 1), "%")
  })
  
  # Número de especialidades distintas
  output$num_especialidades <- renderText({
    length(unique(datos_visual()$nom_espe))
  })
  
  # Número de establecimientos (RBD) distintos
  output$num_establecimientos <- renderText({
    length(unique(datos_visual()$rbd))
  })
  
  # --- Outputs para pestaña Inicio ---
  output$total_matricula_inicio <- renderText({
    format(nrow(matricula_raw), big.mark = ".")
  })
  
  output$total_establecimientos_inicio <- renderText({
    format(n_distinct(matricula_raw$rbd), big.mark = ".")
  })
  
  output$total_especialidades_inicio <- renderText({
    format(n_distinct(matricula_raw$nom_espe), big.mark = ".")
  })
  
  
  # Gráfico: matrícula por sector económico (nombres oficiales ANEXO VII)
  output$grafico_sector <- renderPlotly({
    d <- datos_visual() %>%
      dplyr::filter(!is.na(nom_sector)) %>%
      dplyr::count(nom_sector, sort = TRUE) %>%
      dplyr::arrange(n)
    if (nrow(d) == 0)
      return(plotly_empty() %>% layout(title = list(text = "Sin datos para los filtros seleccionados")))
    d$nom_sector <- factor(d$nom_sector, levels = d$nom_sector)
    p <- plot_ly(d, x = ~n, y = ~nom_sector, type = "bar", orientation = "h",
                 marker = list(color = "#34536A"),
                 text = ~format(n, big.mark = "."), textposition = "outside",
                 hovertext = ~paste0(nom_sector, "<br>", format(n, big.mark="."), " estudiantes"),
                 hoverinfo = "text") %>%
      layout(xaxis = list(title = "Matrícula EMTP"), yaxis = list(title = ""),
             margin = list(l = 10))
    apply_plotly_theme(p)
  })

  # Gráfico de barras: matrícula por especialidad con % Hombres y Mujeres
  output$grafico_barras <- renderPlotly({
    datos_barras <- datos_visual() %>%
      group_by(nom_espe, gen_alu) %>%
      summarise(Total = n(), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = gen_alu, values_from = Total, values_fill = 0) %>%
      mutate(
        Hombres = `1`,
        Mujeres = `2`,
        Total = Hombres + Mujeres,
        pct_Hombres = Hombres / Total * 100,
        pct_Mujeres = Mujeres / Total * 100
      ) %>%
      arrange(pct_Mujeres) %>% 
      mutate(nom_espe = factor(nom_espe, levels = nom_espe))
    
    p <- plot_ly(
      datos_barras,
      x = ~nom_espe,
      y = ~pct_Hombres,
      type = 'bar',
      name = 'Hombres',
      marker = list(color = "#3B5268")
    ) %>%
      add_trace(y = ~pct_Mujeres, name = 'Mujeres', marker = list(color = "#A75F5D")) %>%
      layout(
        title = "Matrícula por Especialidad (% por Género)",
        barmode = 'stack',
        margin = list(b = 150),
        xaxis = list(title = "Especialidad", tickangle = -45),
        yaxis = list(title = "% de Estudiantes")
      )
    
    return(apply_plotly_theme(p))
  })
  
  # Gráfico de torta: proporción por género (total general)
  output$grafico_torta <- renderPlotly({
    datos_torta <- datos_visual() %>%
      mutate(Genero = case_when(
        gen_alu == 1 ~ "Hombres",
        gen_alu == 2 ~ "Mujeres",
        TRUE ~ "Desconocido"
      )) %>%
      group_by(Genero) %>%
      summarise(Total = n(), .groups = "drop")
    
    datos_torta <- datos_torta %>%
      mutate(color = dplyr::case_when(
        Genero == "Hombres" ~ "#3B5268",
        Genero == "Mujeres" ~ "#A75F5D",
        TRUE ~ "#C0C8D2"
      ))
    p <- plot_ly(
      datos_torta,
      labels = ~Genero,
      values = ~Total,
      type = "pie",
      marker = list(colors = datos_torta$color)
    ) %>%
      layout(title = "Distribución por Género")
    
    return(apply_plotly_theme(p))
  })
  
  # Gráfico de barras: matrícula por nivel y grado
  output$grafico_nivel_grado <- renderPlotly({
    datos <- datos_visual() %>%
      mutate(
        Nivel = case_when(cod_ense2 == 7 ~ "Niños y Jóvenes",
                          cod_ense2 == 8 ~ "Adultos"),
        Grado = case_when(cod_grado == 1 ~ "1° nivel",
                          cod_grado == 3 ~ "3° medio",
                          cod_grado == 4 ~ "4° medio")
      ) %>%
      group_by(Nivel, Grado, gen_alu) %>%
      summarise(Total = n(), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = gen_alu, values_from = Total, values_fill = 0) %>%
      mutate(Hombres = `1`, Mujeres = `2`, Total_Grado = Hombres + Mujeres)
    
    fig <- plot_ly(
      datos,
      x = ~Grado,
      y = ~Hombres,
      type = 'bar',
      name = 'Hombres',
      color = I("#3B5268")
    ) %>%
      add_trace(y = ~Mujeres, name = 'Mujeres', color = I("#A75F5D")) %>%
      layout(
        title = "Matrícula por Grado",
        barmode = 'stack',
        xaxis = list(title = "Grado"),
        yaxis = list(title = "Número de Estudiantes")
      ) %>%
      add_text(
        x = ~Grado,
        y = ~Total_Grado,
        text = ~Total_Grado,
        textposition = "outside",
        showlegend = FALSE,
        textfont = list(color = "black", size = 14),
        inherit = FALSE  # <-- Esto es clave
      )
    
    return(apply_plotly_theme(fig))
  })
  
  # Gráfico por establecimientos y dependencia
  output$establecimientos_dependencia <- renderPlotly({
    datos_est <- datos_visual() %>%
      group_by(cod_depe2) %>%
      summarise(N = n_distinct(rbd), .groups = "drop") %>%
      mutate("Dependencia" = sapply(cod_depe2, obtener_nombre_dependencia)) %>%
      select("Dependencia", "N")
    
    p <- plot_ly(
      datos_est,
      x = ~Dependencia,
      y = ~N,
      type = 'bar',
      marker = list(color = "#34536A")
    ) %>%
      layout(
        title = "Establecimientos por Dependencia",
        xaxis = list(title = "Dependencia"),
        yaxis = list(title = "Número de Establecimientos")
      )
    
    return(apply_plotly_theme(p))
  })
  
  # Gráfico comparativo por región
  output$grafico_regional <- renderPlotly({
    datos_regional <- datos_visual() %>%
      group_by(nom_reg_rbd_a) %>%
      summarise(
        Matricula = n(),
        Establecimientos = n_distinct(rbd),
        Especialidades = n_distinct(nom_espe),
        .groups = "drop"
      ) %>%
      arrange(desc(Matricula)) %>%
      slice_head(n = 10)  # Top 10 regiones
    
    p <- plot_ly(
      datos_regional,
      x = ~reorder(nom_reg_rbd_a, Matricula),
      y = ~Matricula,
      type = 'bar',
      marker = list(color = "#5A6E79"),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Matrícula: %{y:,}<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "Top 10 Regiones por Matrícula EMTP",
        xaxis = list(title = "Región", tickangle = -45),
        yaxis = list(title = "Matrícula"),
        margin = list(b = 150)
      )
    
    return(apply_plotly_theme(p))
  })
  
  # Crear tabla de menciones
  menciones <- tibble(
    cod_men = c(41005001,41005002,41005003,
                51007001,51007002,51007003,51007004,
                52013001,52013002,52013003,52013004,
                56027001,56027002,56027003,
                61003001,61003002,61003003,
                64003001,64003002,64003003,
                72007001,72007002,72007003,72007004),
    Glosa_Mencion = c("Plan Común","Logística","Recursos Humanos",
                      "Plan Común","Edificación","Terminaciones de la Construcción","Obras Viales e Infraestructura",
                      "Plan Común","Mantenimiento Electromecánico","Máquinas-Herramientas","Matricería",
                      "Plan Común","Planta Química","Laboratorio Químico",
                      "Plan Común","Cocina","Pastelería y Repostería",
                      "Adultos Mayores","Enfermería","Plan Común",
                      "Plan Común","Agricultura","Pecuaria","Vitivinícola")
  )
  
  # Tabla interactiva agregada por RBD y especialidad
  output$tabla_matricula <- renderDT({
    datos_visual() %>%
      group_by(rbd, nom_rbd, nom_reg_rbd_a, nom_com_rbd, nom_espe, cod_men) %>%
      summarise(
        Total = n(),
        Total_Hombres = sum(gen_alu == 1, na.rm = TRUE),
        Total_Mujeres = sum(gen_alu == 2, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(menciones, by = "cod_men") %>%
      mutate(
        Glosa_Mencion = ifelse(is.na(Glosa_Mencion), "Sin Mención", Glosa_Mencion)
      ) %>%
      select(rbd, nom_rbd, nom_reg_rbd_a, nom_com_rbd, nom_espe, Glosa_Mencion, 
             Total, Total_Hombres, Total_Mujeres)
  }, 
  options = list(pageLength = 10, scrollX = TRUE),  # opciones de visualización
  filter = "top",  # filtrado por columna
  rownames = FALSE)
  
  # Descargar datos filtrados en csv
  output$descargar_csv <- downloadHandler(
    filename = function() {
      paste0("lista_liceos_filtrada_", Sys.Date(), ".csv")
    },
    content = function(file) {
      datos_a_descargar <- datos_visual() %>%
        group_by(rbd, nom_rbd, nom_reg_rbd_a, nom_com_rbd, nom_espe, cod_men, cod_ense2, cod_grado) %>%
        summarise(
          Total = n(),
          Total_Hombres = sum(gen_alu == 1, na.rm = TRUE),
          Total_Mujeres = sum(gen_alu == 2, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        left_join(menciones, by = "cod_men") %>%
        mutate(
          Glosa_Mencion = ifelse(is.na(Glosa_Mencion), "Sin Mención", Glosa_Mencion),
          Nivel = case_when(
            cod_ense2 == 7 ~ "Niños y Jóvenes",
            cod_ense2 == 8 ~ "Adultos",
            TRUE ~ "Desconocido"
          ),
          Grado = case_when(
            cod_grado == 1 ~ "1° nivel (1° y 2° medio sólo adultos)",
            cod_grado == 3 ~ "3° medio",
            cod_grado == 4 ~ "4° medio",
            TRUE ~ as.character(cod_grado)
          )
        ) %>%
        select(rbd, nom_rbd, nom_reg_rbd_a, nom_com_rbd, nom_espe, Glosa_Mencion,
               Nivel, Grado, Total, Total_Hombres, Total_Mujeres)
      
      write.csv2(datos_a_descargar, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # Descargar matrícula en CSV (sin columnas de equipamiento)
  output$descargar_matricula_csv <- downloadHandler(
    filename = function() {
      paste0("Matricula_EMTP_2025_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Excluir columnas de equipamiento y columnas internas
      matricula_descarga <- matricula_raw %>%
        select(-any_of(c("EquipamientoRegular_TOTAL", "EquipamientoRegular_2020",
                         "EquipamientoRegular_2021", "EquipamientoRegular_2022",
                         "EquipamientoRegular_2023", "EquipamientoRegular_2024",
                         "EquipamientoRegular_Adjudica", "EquipamientoSLEP_2023",
                         "EquipamientoSLEP_2024", "EquipamientoSLEP_TOTAL",
                         "rft_ejecutor", "categoria_asis_anual")))
      
      write.csv2(matricula_descarga, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # Reactive compartido: construye base_2025 una sola vez para CSV y Excel
  base_ee_2025_descarga <- reactive({
    matriculas_2025 <- matricula_raw %>%
      group_by(rbd) %>%
      summarise(
        MatriculaEMTP_2025 = n(),
        MatriculaMujeresEMTP_2025 = sum(gen_alu == 2, na.rm = TRUE),
        MatriculaHombresEMTP_2025 = sum(gen_alu == 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(rbd = as.character(rbd))
    
    datos_admin_2025 <- matricula_raw %>%
      group_by(rbd) %>%
      summarise(
        COD_DEPE2_2025 = first(cod_depe2),
        RURAL_RBD_2025 = first(rural_rbd),
        NOMBRE_SLEP = first(nombre_slep),
        .groups = "drop"
      ) %>%
      mutate(rbd = as.character(rbd))
    
    rbds_activos_2025 <- unique(matricula_raw$rbd)
    
    matriculas_por_especialidad_2025 <- matricula_raw %>%
      filter(!is.na(nom_espe)) %>%
      group_by(rbd, nom_espe) %>%
      summarise(matricula_espe = n(), .groups = "drop") %>%
      mutate(rbd = as.character(rbd)) %>%
      tidyr::pivot_wider(
        names_from = nom_espe,
        values_from = matricula_espe,
        values_fill = 0
      )
    
    base_2025 <- base_apoyo %>%
      mutate(rbd = as.character(rbd)) %>%
      filter(rbd %in% rbds_activos_2025) %>%
      left_join(matriculas_2025, by = "rbd") %>%
      left_join(datos_admin_2025, by = "rbd") %>%
      left_join(matriculas_por_especialidad_2025, by = "rbd", suffix = c("_OLD", "")) %>%
      mutate(
        COD_DEPE2_2025 = as.numeric(COD_DEPE2_2025),
        RURAL_RBD_2025 = as.numeric(RURAL_RBD_2025),
        cod_depe2 = as.numeric(cod_depe2),
        RuralidadRBD = as.numeric(RuralidadRBD),
        cod_depe2 = coalesce(COD_DEPE2_2025, cod_depe2),
        RuralidadRBD = coalesce(RURAL_RBD_2025, RuralidadRBD)
      ) %>%
      select(
        # Identificación
        rbd, Nombre, NombreRegión, Provincia, NombreComuna,
        # Información del establecimiento
        cod_depe2, nombre_sost, RutSostenedor, NOMBRE_SLEP, direccion,
        IVE, RuralidadRBD, RuralidadRBD_2025,
        # Modalidad EMTP
        `EMTP para Jóvenes ciclo diferenciado`,
        `EMTP para Adultos ciclo diferenciado`,
        # Programas
        Bicentenario,
        CONVENIO_PIE_2025, PACE_2025,
        # Matrícula 2025
        MatriculaEMTP_2025, MatriculaMujeresEMTP_2025, MatriculaHombresEMTP_2025,
        `MatrículaTotal del Establecimiento`,
        MATRICULA_OFICIAL_2025,
        N_ESPECIALIDADES,
        # Docentes EMTP
        DocentesEMTP_Total, DocentesEMTP_Hombres, DocentesEMTP_Mujeres,
        # Resultados académicos
        gse_agencia, prom_lect2m_rbd, prom_mate2m_rbd
      ) %>%
      # Excluir columnas temporales y duplicados _OLD en paso separado
      select(-matches("_OLD$"))
    
    # Agregar RBD 10210 si falta (está en matrícula pero no en base_apoyo)
    if(10210 %in% rbds_activos_2025 && !"10210" %in% base_2025$rbd) {
      info_10210 <- matricula_raw %>%
        filter(rbd == 10210) %>%
        select(rbd, nom_rbd, nom_reg_rbd_a, nom_com_rbd, cod_depe2, nombre_slep) %>%
        distinct() %>%
        mutate(
          rbd = as.character(rbd),
          Nombre = as.character(nom_rbd),
          NombreRegión = as.character(nom_reg_rbd_a),
          NombreComuna = as.character(nom_com_rbd),
          cod_depe2 = as.numeric(cod_depe2),
          NOMBRE_SLEP = as.character(nombre_slep)
        ) %>%
        select(rbd, Nombre, NombreRegión, NombreComuna, cod_depe2, NOMBRE_SLEP) %>%
        left_join(matriculas_2025, by = "rbd")
      
      columnas_faltantes <- setdiff(names(base_2025), names(info_10210))
      valores_na <- setNames(
        lapply(columnas_faltantes, function(col) {
          tipo <- class(base_2025[[col]])[1]
          if(tipo == "numeric") return(as.numeric(NA))
          if(tipo == "integer") return(as.integer(NA))
          if(tipo == "logical") return(as.logical(NA))
          return(as.character(NA))
        }),
        columnas_faltantes
      )
      base_2025 <- bind_rows(base_2025, bind_cols(info_10210, valores_na))
    }
    
    base_2025
  })
  
  # Descargar base de apoyo en CSV
  output$descargar_base_apoyo_csv <- downloadHandler(
    filename = function() {
      paste0("Base_EE_EMTP_2025_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Escribir CSV en temporal, luego anteponer BOM UTF-8 para que Excel lea tildes correctamente
      tmp <- tempfile(fileext = ".csv")
      write.csv2(base_ee_2025_descarga(), tmp, row.names = FALSE, fileEncoding = "UTF-8")
      raw_content <- readBin(tmp, "raw", n = file.info(tmp)$size)
      writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), raw_content), file)
      file.remove(tmp)
    }
  )
  

  
  output$descargar_docentes_emtp_csv <- downloadHandler(
    filename = function() {
      paste0("Base_docentes_2025_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv2(docentes_raw, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # ======== DOWNLOAD HANDLERS PARA REPORTES HISTÓRICOS 2018-2025 ========
  
  # Mapeo de nombres de región a códigos
  region_codes <- c(
    "Arica y Parinacota" = "15",
    "Tarapacá" = "01",
    "Antofagasta" = "02",
    "Atacama" = "03",
    "Coquimbo" = "04",
    "Valparaíso" = "05",
    "Metropolitana" = "13",
    "O'Higgins" = "06",
    "Maule" = "07",
    "Ñuble" = "16",
    "Biobío" = "08",
    "La Araucanía" = "09",
    "Los Ríos" = "14",
    "Los Lagos" = "10",
    "Aysén" = "11",
    "Magallanes" = "12"
  )
  
  # 1. REPORTE NACIONAL - PDF
  output$download_reporte_nacional_pdf <- downloadHandler(
    filename = function() {
      "Analisis_EMTP_2018_2025_Nacional.pdf"
    },
    content = function(file) {
      file_path <- "docs/Reportes 2018-2025/Analisis_Matricula_Nacional/01. Documentos Finales/Analisis_EMTP_2018_2025.pdf"
      if (file.exists(file_path)) {
        file.copy(file_path, file)
      } else {
        showNotification("Archivo no encontrado", type = "error")
      }
    }
  )
  
  # 2. REPORTE REGIÓN - PDF
  output$download_reporte_region_pdf <- downloadHandler(
    filename = function() {
      region_seleccionada <- input$select_region_reporte
      paste0("Analisis_EMTP_2018_2025_", region_seleccionada, ".pdf")
    },
    content = function(file) {
      region_seleccionada <- input$select_region_reporte
      file_path <- paste0("docs/Reportes 2018-2025/Analisis_Matricula_por_Region/", 
                          region_seleccionada, 
                          "/01. Documentos Finales/Analisis_EMTP_2018_2025_", 
                          region_seleccionada, ".pdf")
      if (file.exists(file_path)) {
        file.copy(file_path, file)
      } else {
        showNotification("Archivo no encontrado", type = "error")
      }
    }
  )
  

  # Download handler para minuta territorial en PDF
  output$descargar_resumen_territorial_pdf <- downloadHandler(
    filename = function() {
      nombre <- input$comuna
      if (nombre == "Todas") nombre <- "Territorio"
      paste0("Resumen_", nombre, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      notif_id <- "res_terr_pdf"
      showNotification("Generando reporte PDF... Por favor espere.", type = "message", duration = NULL, id = notif_id)
      on.exit({ removeNotification(id = notif_id) }, add = TRUE)
      incluir_val <- isTRUE(input$incluir)
      
      nombre <- if (input$comuna != "Todas") {
        input$comuna
      } else if (input$provincia != "Todas") {
        input$provincia
      } else if (input$region != "Todas") {
        input$region
      } else if (input$rft != "Todas") {
        input$rft
      } else {
        "Territorio_Nacional"
      }
      
      territorio_filtrado <- datos_filtrados()$matricula
      rbd_filtrados <- unique(territorio_filtrado$rbd)
      
      # Preprocesamiento docentes similar al DOCX
      cod_ens_tp <- 410:863
      docentes_tp <- docentes_raw %>%
        dplyr::filter(
          rbd %in% rbd_filtrados &
            ((COD_ENS_1 %in% cod_ens_tp & HORAS1 > 0) |
               (COD_ENS_2 %in% cod_ens_tp & HORAS2 > 0))
        ) %>%
        dplyr::mutate(
          # Normalizar SUBSECTOR: eliminar 0s y duplicados
          SUBSECTOR1 = dplyr::na_if(SUBSECTOR1, "0"),
          SUBSECTOR2 = dplyr::na_if(SUBSECTOR2, "0"),
          SUBSECTOR2 = ifelse(!is.na(SUBSECTOR1) & SUBSECTOR2 == SUBSECTOR1, NA, SUBSECTOR2)
        ) %>%
        # Pivot ANTES de mapear a string (para poder filtrar por rango numérico)
        tidyr::pivot_longer(
          cols = c(SUBSECTOR1, SUBSECTOR2),
          names_to = "col_sub",
          values_to = "SUBSECTOR"
        ) %>%
        dplyr::mutate(SUBSECTOR = as.numeric(SUBSECTOR)) %>%
        # Filtrar por rango de especialidades (igual que pestaña Docentes)
        dplyr::filter(
          !is.na(SUBSECTOR),
          SUBSECTOR >= 31001,  # Incluye Formación General (31001-39501) y Especialidades (40000-81004)
          SUBSECTOR <= 81004
        ) %>%
        # AHORA mapear a especialidades o "Formación General"
        dplyr::mutate(
          Especialidad = dplyr::if_else(
            startsWith(as.character(SUBSECTOR), "3"),
            "Formación General",
            as.character(SUBSECTOR)
          )
        ) %>%
        dplyr::distinct(MRUN, rbd, Especialidad, .keep_all = TRUE)
      if(nrow(docentes_tp) == 0) docentes_tp <- tibble::tibble(Especialidad = character())
      
      base_apoyo_filtrada <- base_apoyo %>% dplyr::filter(rbd %in% rbd_filtrados)
      
      # Renderizar a PDF cambiando el output_format (usar xelatex por soporte Unicode)
      suppressWarnings(rmarkdown::render(
        input = "templates/resumen_territorio.Rmd",
        output_file = file,
        output_format = rmarkdown::pdf_document(latex_engine = "xelatex"),
        quiet = TRUE,
        params = list(
          nombre_territorio = nombre,
          territorio_filtrado = territorio_filtrado,
          resumen_matricula = resumen_matricula(),
          detalle_especialidades = NULL,
          resumen_etnias = NULL,
          extranjeros = NULL,
          origen_extranjeros = NULL,
          rural_dependencia = NULL,
          base_apoyo = base_apoyo_filtrada,
          docentes_emtp = docentes_tp,
          resumen_subsector = docentes_tp,
          incluir_tabla = incluir_val
        ),
        envir = new.env(parent = globalenv())
      ))
    }
  )
  
  # Download handler para resumen territorial en Excel
  output$descargar_resumen_territorial_excel <- downloadHandler(
    filename = function() {
      nombre <- input$comuna
      if (nombre == "Todas") nombre <- "Territorio"
      paste0("Resumen_", nombre, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      notif_id <- "res_terr_excel"
      showNotification("Generando Excel territorial completo... Por favor espere.", type = "message", duration = NULL, id = notif_id)
      on.exit({ removeNotification(id = notif_id) }, add = TRUE)
      
      territorio_filtrado <- datos_filtrados()$matricula
      
      # ===== HOJA 1: RESUMEN GENERAL =====
      resumen_territorio_excel <- territorio_filtrado %>%
        summarise(
          "Total Matrícula" = n(),
          "Hombres" = sum(gen_alu == 1, na.rm = TRUE),
          "Mujeres" = sum(gen_alu == 2, na.rm = TRUE),
          "Establecimientos" = n_distinct(rbd),
          "Jóvenes (3° y 4° Medio)" = sum(cod_ense2 == 7 & cod_grado %in% c(3, 4), na.rm = TRUE),
          "Adultos Total" = sum(cod_ense2 == 8, na.rm = TRUE),
          "Adultos 1° Nivel" = sum(cod_ense2 == 8 & cod_grado %in% c(1, 2), na.rm = TRUE)
        ) %>%
        mutate(
          "% Hombres" = round(100 * `Hombres` / `Total Matrícula`, 1),
          "% Mujeres" = round(100 * `Mujeres` / `Total Matrícula`, 1)
        )
      
      # ===== HOJA 2: MATRÍCULA POR NIVEL Y GRADO =====
      detalle_nivel_grado <- territorio_filtrado %>%
        mutate(
          Nivel = case_when(
            cod_ense2 == 7 ~ "Jóvenes",
            cod_ense2 == 8 ~ "Adultos",
            TRUE ~ NA_character_
          ),
          Grado_Label = case_when(
            cod_grado == 1 ~ "1° Medio",
            cod_grado == 2 ~ "2° Medio",
            cod_grado == 3 ~ "3° Medio",
            cod_grado == 4 ~ "4° Medio",
            TRUE ~ NA_character_
          )
        ) %>%
        filter(!is.na(Nivel), !is.na(Grado_Label)) %>%
        group_by(Nivel, Grado_Label) %>%
        summarise(
          "Total" = n(),
          "Hombres" = sum(gen_alu == 1, na.rm = TRUE),
          "Mujeres" = sum(gen_alu == 2, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate("% Mujeres" = round(100 * Mujeres / Total, 1)) %>%
        arrange(Nivel, desc(Grado_Label))
      
      # ===== HOJA 3: MATRÍCULA POR ESPECIALIDAD (DATOS 2025) =====
      detalle_especialidades_terr <- territorio_filtrado %>%
        filter(!is.na(nom_espe)) %>%
        group_by(nom_espe) %>%
        summarise(
          "Total" = n(),
          "Hombres" = sum(gen_alu == 1, na.rm = TRUE),
          "Mujeres" = sum(gen_alu == 2, na.rm = TRUE),
          "Jóvenes" = sum(cod_ense2 == 7, na.rm = TRUE),
          "Adultos" = sum(cod_ense2 == 8, na.rm = TRUE),
          "3° Medio" = sum(cod_grado == 3, na.rm = TRUE),
          "4° Medio" = sum(cod_grado == 4, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          "% Hombres" = round(100 * Hombres / Total, 1),
          "% Mujeres" = round(100 * Mujeres / Total, 1)
        ) %>%
        rename("Especialidad" = nom_espe) %>%
        select("Especialidad", "Total", "Hombres", "Mujeres", "% Hombres", "% Mujeres", "Jóvenes", "Adultos", "3° Medio", "4° Medio") %>%
        arrange(desc(Total))
      
      # ===== HOJA 4: ESTABLECIMIENTOS CON DEPENDENCIA LEGIBLE (DATOS 2025) =====
      detalle_establecimientos_terr <- territorio_filtrado %>%
        group_by(rbd, nom_rbd, nom_com_rbd, cod_depe2) %>%
        summarise(
          Matricula = n(),
          Hombres = sum(gen_alu == 1, na.rm = TRUE),
          Mujeres = sum(gen_alu == 2, na.rm = TRUE),
          Especialidades = n_distinct(nom_espe, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          "Dependencia" = sapply(cod_depe2, obtener_nombre_dependencia),
          "% Hombres" = round(100 * Hombres / Matricula, 1)
        ) %>%
        rename(
          "RBD" = rbd,
          "Establecimiento" = nom_rbd,
          "Comuna" = nom_com_rbd,
          "Matrícula" = Matricula
        ) %>%
        select("RBD", "Establecimiento", "Comuna", "Dependencia", "Matrícula", "Hombres", "Mujeres", "% Hombres", "Especialidades") %>%
        arrange(desc(Matrícula))
      
      # ===== HOJA 5: ETNIAS (DATOS 2024) =====
      detalle_etnias_terr <- territorio_filtrado %>%
        filter(!is.na(cod_etnia_alu), cod_etnia_alu != "") %>%
        group_by(cod_etnia_alu) %>%
        summarise(
          "Total" = n(),
          "Hombres" = sum(gen_alu == 1, na.rm = TRUE),
          "Mujeres" = sum(gen_alu == 2, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate("% del Total" = round(100 * Total / sum(Total), 1)) %>%
        rename("Etnia" = cod_etnia_alu) %>%
        arrange(desc(Total))
      
      # ===== HOJA 6: NACIONALIDAD (DATOS 2024) =====
      detalle_nacionalidad_terr <- territorio_filtrado %>%
        group_by(cod_nac_alu) %>%
        summarise(
          "Total" = n(),
          "Hombres" = sum(gen_alu == 1, na.rm = TRUE),
          "Mujeres" = sum(gen_alu == 2, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          "Nacionalidad" = case_when(
            cod_nac_alu == "C" ~ "Chilena",
            cod_nac_alu == "E" ~ "Extranjera",
            cod_nac_alu == "N" ~ "No especificado",
            TRUE ~ "Desconocida"
          ),
          "% del Total" = round(100 * Total / sum(Total), 1)
        ) %>%
        select("Nacionalidad", "Total", "Hombres", "Mujeres", "% del Total")
      
      # ===== ASISTENCIA ANUAL (DATOS 2024) =====
      detalle_asistencia_terr <- territorio_filtrado %>%
        filter(!is.na(categoria_asis_anual)) %>%
        group_by(categoria_asis_anual) %>%
        summarise(
          "Total" = n(),
          "Hombres" = sum(gen_alu == 1, na.rm = TRUE),
          "Mujeres" = sum(gen_alu == 2, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          "Categoría de Asistencia" = case_when(
            categoria_asis_anual == 1 ~ "Inasistencia crítica (<50%)",
            categoria_asis_anual == 2 ~ "Inasistencia grave (50%-84%)",
            categoria_asis_anual == 3 ~ "Inasistencia reiterada (85%-89%)",
            categoria_asis_anual == 4 ~ "Asistencia esperada (≥90%)",
            TRUE ~ "Desconocida"
          ),
          "% del Total" = round(100 * Total / sum(Total), 1)
        ) %>%
        select("Categoría de Asistencia", "Total", "Hombres", "Mujeres", "% del Total")
      
      # --- Crear archivo Excel COMPLETO
      wb <- openxlsx::createWorkbook()
      
      # Agregar todas las hojas
      openxlsx::addWorksheet(wb, "Resumen General")
      openxlsx::writeData(wb, "Resumen General", resumen_territorio_excel, rowNames = FALSE)
      
      openxlsx::addWorksheet(wb, "Nivel y Grado 2025")
      openxlsx::writeData(wb, "Nivel y Grado 2025", detalle_nivel_grado, rowNames = FALSE)
      
      openxlsx::addWorksheet(wb, "Especialidades 2025")
      openxlsx::writeData(wb, "Especialidades 2025", detalle_especialidades_terr, rowNames = FALSE)
      
      openxlsx::addWorksheet(wb, "Establecimientos 2025")
      openxlsx::writeData(wb, "Establecimientos 2025", detalle_establecimientos_terr, rowNames = FALSE)
      
      if (nrow(detalle_etnias_terr) > 0) {
        openxlsx::addWorksheet(wb, "Etnias (2024)", tabColour = "#E67E22")
        openxlsx::writeData(wb, "Etnias (2024)", detalle_etnias_terr, rowNames = FALSE)
      }
      
      if (nrow(detalle_nacionalidad_terr) > 0) {
        openxlsx::addWorksheet(wb, "Nacionalidad (2024)", tabColour = "#E67E22")
        openxlsx::writeData(wb, "Nacionalidad (2024)", detalle_nacionalidad_terr, rowNames = FALSE)
      }
      
      if (nrow(detalle_asistencia_terr) > 0) {
        openxlsx::addWorksheet(wb, "Asistencia Anual (2024)", tabColour = "#E67E22")
        openxlsx::writeData(wb, "Asistencia Anual (2024)", detalle_asistencia_terr, rowNames = FALSE)
      }
      
      # ===== ESTILOS EXCEL =====
      estilo_hdr_t <- openxlsx::createStyle(
        fontColour = "#FFFFFF", fgFill = "#2C3E50", textDecoration = "bold",
        halign = "center", border = "TopBottomLeftRight", borderColour = "#1A252F", fontSize = 11
      )
      estilo_par_t <- openxlsx::createStyle(
        fontColour = "#1A1A1A", fgFill = "#FDFEFE",
        halign = "center", border = "TopBottomLeftRight", borderColour = "#BDC3C7", fontSize = 10
      )
      estilo_imp_t <- openxlsx::createStyle(
        fontColour = "#1A1A1A", fgFill = "#EAF2F8",
        halign = "center", border = "TopBottomLeftRight", borderColour = "#BDC3C7", fontSize = 10
      )
      aplicar_est_terr <- function(hoja, df) {
        nc <- ncol(df); nr <- nrow(df)
        openxlsx::addStyle(wb, hoja, estilo_hdr_t, rows = 1, cols = 1:nc, gridExpand = TRUE)
        if (nr > 0) {
          filas <- 2:(nr+1)
          even <- filas[filas %% 2 == 0]; odd <- filas[filas %% 2 != 0]
          if (length(even) > 0) openxlsx::addStyle(wb, hoja, estilo_par_t, rows = even, cols = 1:nc, gridExpand = TRUE)
          if (length(odd)  > 0) openxlsx::addStyle(wb, hoja, estilo_imp_t, rows = odd,  cols = 1:nc, gridExpand = TRUE)
        }
        openxlsx::setColWidths(wb, hoja, cols = 1:nc, widths = "auto")
        openxlsx::setRowHeights(wb, hoja, rows = 1, heights = 24)
        openxlsx::freezePane(wb, hoja, firstRow = TRUE)
      }
      aplicar_est_terr("Resumen General",     resumen_territorio_excel)
      aplicar_est_terr("Nivel y Grado 2025",  detalle_nivel_grado)
      aplicar_est_terr("Especialidades 2025", detalle_especialidades_terr)
      aplicar_est_terr("Establecimientos 2025", detalle_establecimientos_terr)
      if (nrow(detalle_etnias_terr)        > 0) aplicar_est_terr("Etnias (2024)",          detalle_etnias_terr)
      if (nrow(detalle_nacionalidad_terr)  > 0) aplicar_est_terr("Nacionalidad (2024)",    detalle_nacionalidad_terr)
      if (nrow(detalle_asistencia_terr)    > 0) aplicar_est_terr("Asistencia Anual (2024)", detalle_asistencia_terr)
      
      # Guardar workbook
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  

  # Download handler para minutas por RBD en PDF (ZIP)
  output$descargar_minuta_pdf <- downloadHandler(
    filename = function() {
      paste0("minutas_establecimientos_pdf_", Sys.Date(), ".zip")
    },
    content = function(file) {
      notif_id <- "minuta_pdf"
      showNotification("Generando minutas PDF... Por favor espere.", type = "message", duration = NULL, id = notif_id)
      on.exit({ removeNotification(id = notif_id) }, add = TRUE)
      resultados <- resultado_busqueda()
      if (nrow(resultados) == 0) {
        stop("No se encontró ningún RBD para generar minutas.")
      }
      rbd_lista <- rbd_seleccionados()
      if (length(rbd_lista) == 0) {
        stop("Seleccione al menos un establecimiento antes de descargar.")
      }
      
      temp_dir <- tempdir()
      archivos_generados <- c()
      
      matricula_rbd_lista <- matricula_raw %>%
        dplyr::filter(rbd %in% rbd_lista) %>%
        dplyr::group_split(rbd) %>%
        setNames(purrr::map_chr(., ~ as.character(unique(.x$rbd))))
      
      for (rbd_seleccionado in rbd_lista) {
        matricula_actual <- matricula_rbd_lista[[as.character(rbd_seleccionado)]]
        
        datos_generales <- matricula_raw %>%
          dplyr::filter(rbd == rbd_seleccionado) %>%
          dplyr::select(rbd, nom_rbd, cod_depe2, rft, nom_reg_rbd_a, nom_deprov_rbd, nombre_sost, nom_com_rbd, RuralidadRBD) %>%
          dplyr::distinct()
        
        datos_generales1 <- matricula_raw %>%
          dplyr::filter(rbd == rbd_seleccionado) %>%
          dplyr::select(rbd, nom_rbd, cod_depe2, rft, nom_reg_rbd_a, nom_deprov_rbd, nombre_sost, nom_com_rbd, RuralidadRBD, categoria_asis_anual)
        
        resumen_matricula <- matricula_actual %>%
          dplyr::filter(rbd == rbd_seleccionado) %>%
          dplyr::summarise(
            total = dplyr::n(),
            hombres = sum(gen_alu == 1, na.rm = TRUE),
            mujeres = sum(gen_alu == 2, na.rm = TRUE),
            matricula_jovenes = sum(cod_ense2 == 7 & cod_grado %in% c(3, 4), na.rm = TRUE),
            matricula_adultos = sum(cod_ense2 == 8, na.rm = TRUE),
            matricula_adultos_1nivel = sum(cod_ense2 == 8 & cod_grado %in% c(1, 2), na.rm = TRUE),
            matricula_3medio = sum(cod_grado == 3, na.rm = TRUE),
            matricula_4medio = sum(cod_grado == 4, na.rm = TRUE)
          ) %>% as.list()
        
        detalle_por_nivel <- matricula_actual %>%
          dplyr::filter(rbd == rbd_seleccionado) %>%
          dplyr::group_by(cod_ense2, cod_grado) %>%
          dplyr::summarise(
            total = dplyr::n(),
            hombres = sum(gen_alu == 1, na.rm = TRUE),
            mujeres = sum(gen_alu == 2, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            nivel = dplyr::case_when(
              cod_ense2 == 7 ~ "Jóvenes",
              cod_ense2 == 8 ~ "Adultos",
              TRUE ~ "Otro"
            ),
            grado = cod_grado
          )
        
        detalle_especialidades <- matricula_actual %>%
          dplyr::filter(rbd == rbd_seleccionado, !is.na(nom_espe)) %>%
          dplyr::group_by(nom_espe) %>%
          dplyr::summarise(
            Total = dplyr::n(),
            Hombres = sum(gen_alu == 1, na.rm = TRUE),
            Mujeres = sum(gen_alu == 2, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            `% Hombres` = round(100 * Hombres / Total, 1),
            `% Mujeres` = round(100 * Mujeres / Total, 1)
          ) %>%
          dplyr::rename(`Especialidad` = nom_espe)
        
        total_matricula_rbd <- resumen_matricula$total
        
        resumen_etnias <- matricula_actual %>%
          dplyr::filter(rbd == rbd_seleccionado, !is.na(cod_etnia_alu)) %>%
          dplyr::group_by(Etnia = cod_etnia_alu) %>%
          dplyr::summarise(
            Total = dplyr::n(),
            Porcentaje = round(100 * Total / total_matricula_rbd, 1),
            .groups = "drop"
          )
        
        extranjeros <- matricula_actual %>%
          dplyr::filter(rbd == rbd_seleccionado) %>%
          dplyr::group_by(Nacionalidad = dplyr::case_when(
            cod_nac_alu == "E" ~ "Extranjero",
            cod_nac_alu == "N" ~ "Nacionalizado",
            cod_nac_alu == "C" ~ "Chileno",
            TRUE ~ "Desconocido"
          )) %>%
          dplyr::summarise(
            Total = dplyr::n(),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            Porcentaje = round(100 * Total / sum(Total), 1)
          )
        
        total_extranjeros <- matricula_actual %>%
          dplyr::filter(rbd == rbd_seleccionado, cod_nac_alu == "E") %>%
          nrow()
        
        origen_extranjeros <- matricula_actual %>%
          dplyr::filter(rbd == rbd_seleccionado, cod_nac_alu == "E") %>%
          dplyr::mutate(pais_origen_alu = ifelse(pais_origen_alu == "Chile" | is.na(pais_origen_alu), "Desconocido", pais_origen_alu)) %>%
          dplyr::group_by(País = pais_origen_alu) %>%
          dplyr::summarise(Total = dplyr::n(), .groups = "drop") %>%
          dplyr::mutate(
            Porcentaje_extranjeros = round(100 * Total / sum(Total), 1),
            Porcentaje_total = round(100 * Total / total_matricula_rbd, 1)
          ) %>%
          dplyr::arrange(dplyr::desc(Total))
        
        embarazo <- matricula_actual %>%
          dplyr::filter(rbd == rbd_seleccionado, gen_alu == 2) %>%
          dplyr::group_by(Embarazo = dplyr::case_when(
            emb_alu == 1 ~ "Sí",
            emb_alu == 0 ~ "No",
            TRUE ~ "Desconocido"
          )) %>%
          dplyr::summarise(
            Total = dplyr::n(),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            Porcentaje = ifelse(sum(Total) > 0,
                                round(100 * Total / sum(Total), 1),
                                NA_real_)
          )
        
        # Datos establecimiento
        datos_establecimiento <- base_apoyo %>% dplyr::filter(rbd == rbd_seleccionado)
        datos_docentes <- docentes_raw %>%
          dplyr::filter(rbd == rbd_seleccionado) %>%
          dplyr::summarise(
            rbd = as.character(dplyr::first(rbd)),   # convertir a character para join con base_apoyo
            Docentes_Total   = dplyr::n(),
            Docentes_Hombres = sum(DOC_GENERO == 1, na.rm = TRUE),
            Docentes_Mujeres = sum(DOC_GENERO == 2, na.rm = TRUE)
          )
        
        # Construir resumen_subsector (docentes_tp) con columna "Especialidad" como en la versión DOCX
        cod_ens_tp <- 410:863
        docentes_tp <- docentes_raw %>%
          dplyr::filter(
            rbd == rbd_seleccionado &
              ((COD_ENS_1 %in% cod_ens_tp & HORAS1 > 0) |
                 (COD_ENS_2 %in% cod_ens_tp & HORAS2 > 0))
          ) %>%
          dplyr::mutate(
            # Normalizar SUBSECTOR: eliminar 0s y duplicados
            SUBSECTOR1 = dplyr::na_if(SUBSECTOR1, "0"),
            SUBSECTOR2 = dplyr::na_if(SUBSECTOR2, "0"),
            SUBSECTOR2 = ifelse(!is.na(SUBSECTOR1) & SUBSECTOR2 == SUBSECTOR1, NA, SUBSECTOR2)
          ) %>%
          # Pivot ANTES de mapear a string (para poder filtrar por rango numérico)
          tidyr::pivot_longer(
            cols = c(SUBSECTOR1, SUBSECTOR2),
            names_to = "col_sub",
            values_to = "SUBSECTOR"
          ) %>%
          dplyr::mutate(SUBSECTOR = as.numeric(SUBSECTOR)) %>%
          # Filtrar por rango de especialidades (igual que pestaña Docentes)
          dplyr::filter(
            !is.na(SUBSECTOR),
            SUBSECTOR >= 31001,  # Incluye Formación General (31001-39501) y Especialidades (40000-81004)
            SUBSECTOR <= 81004
          ) %>%
          # AHORA mapear a especialidades o "Formación General"
          dplyr::mutate(
            Especialidad = dplyr::if_else(
              startsWith(as.character(SUBSECTOR), "3"),
              "Formación General",
              as.character(SUBSECTOR)
            )
          ) %>%
          dplyr::distinct(MRUN, rbd, Especialidad, .keep_all = TRUE)
        
        # Render a PDF usando el Rmd existente cambiando el formato
        nombre_archivo_pdf <- file.path(temp_dir, paste0("minuta_", rbd_seleccionado, "_", stringr::str_sub(gsub("[^[:alnum:][:space:]]", "", datos_generales$nom_rbd[1]) %>% stringr::str_replace_all("\\s+", "_"), 1, 40), "_", Sys.Date(), ".pdf"))
        
        suppressWarnings(rmarkdown::render(
          input = "templates/minuta_establecimiento.Rmd",
          output_file = nombre_archivo_pdf,
          output_format = rmarkdown::pdf_document(latex_engine = "xelatex"),
          clean = TRUE,
          intermediates_dir = temp_dir,
          params = list(
            datos_generales = datos_generales,
            datos_generales1 = datos_generales1,
            resumen_matricula = resumen_matricula,
            detalle_especialidades = detalle_especialidades,
            resumen_etnias = resumen_etnias,
            extranjeros = extranjeros,
            origen_extranjeros = origen_extranjeros,
            datos_establecimiento = datos_establecimiento,
            detalle_por_nivel = detalle_por_nivel,
            datos_docentes = datos_docentes,
            resumen_subsector = if (nrow(docentes_tp) > 0) docentes_tp else tibble::tibble(Especialidad = character()),
            asistencia = matricula_raw %>%
              dplyr::filter(rbd == rbd_seleccionado, !is.na(categoria_asis_anual)) %>%
              dplyr::count(categoria_asis_anual),
            asis_prom = mean(matricula_raw$tasa_asis_anual[matricula_raw$rbd == rbd_seleccionado], na.rm = TRUE)
          ),
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        ))
        
        archivos_generados <- c(archivos_generados, nombre_archivo_pdf)
      }
      
      # Crear ZIP
      zipfile <- file.path(temp_dir, paste0("minutas_pdf_", Sys.Date(), ".zip"))
      suppressWarnings(zip::zip(zipfile, files = archivos_generados, mode = "cherry-pick"))
      file.copy(zipfile, file)
    }
  )
  
  # Download handler para minutas Excel por RBD
  output$descargar_minuta_excel <- downloadHandler(
    filename = function() {
      paste0("minutas_establecimientos_", Sys.Date(), ".zip")
    },
    content = function(file) {
      resultados <- resultado_busqueda()
      if (nrow(resultados) == 0) {
        stop("No se encontró ningún RBD para generar minutas.")
      }
      rbd_lista <- rbd_seleccionados()
      if (length(rbd_lista) == 0) {
        stop("Seleccione al menos un establecimiento antes de descargar.")
      }
      
      notif_id <- "minuta_excel"
      showNotification("Generando minutas Excel completas... Por favor espere.", type = "message", duration = NULL, id = notif_id)
      on.exit({ removeNotification(id = notif_id) }, add = TRUE)
      
      # Pre-filtramos todos los datos necesarios una vez
      matricula_rbd_lista <- matricula_raw %>%
        filter(rbd %in% rbd_lista) %>%
        group_split(rbd) %>%
        setNames(map_chr(., ~ as.character(unique(.x$rbd))))
      
      temp_dir <- tempdir()
      archivos_generados <- c()
      
      for (rbd_seleccionado in rbd_lista) {
        
        matricula_actual <- matricula_rbd_lista[[as.character(rbd_seleccionado)]]
        
        datos_generales <- matricula_raw %>%
          filter(rbd == rbd_seleccionado) %>%
          select(rbd, nom_rbd, cod_depe2, rft, nom_reg_rbd_a, nom_deprov_rbd, nombre_sost, nom_com_rbd, RuralidadRBD) %>%
          distinct()
        
        # ===== HOJA 1: RESUMEN GENERAL =====
        resumen_matricula_excel <- matricula_actual %>%
          filter(rbd == rbd_seleccionado) %>%
          summarise(
            "Total Matrícula" = n(),
            "Hombres" = sum(gen_alu == 1, na.rm = TRUE),
            "Mujeres" = sum(gen_alu == 2, na.rm = TRUE),
            "Jóvenes (3° y 4° Medio)" = sum(cod_ense2 == 7 & cod_grado %in% c(3, 4), na.rm = TRUE),
            "Adultos Total" = sum(cod_ense2 == 8, na.rm = TRUE),
            "Adultos 1° Nivel" = sum(cod_ense2 == 8 & cod_grado %in% c(1, 2), na.rm = TRUE),
            "Matrícula 3° Medio" = sum(cod_grado == 3, na.rm = TRUE),
            "Matrícula 4° Medio" = sum(cod_grado == 4, na.rm = TRUE)
          ) %>%
          mutate(
            "Dependencia" = obtener_nombre_dependencia(datos_generales$cod_depe2[1]),
            "Ruralidad" = datos_generales$RuralidadRBD[1],
            "RFT" = datos_generales$rft[1]
          )
        
        # ===== HOJA 2: MATRÍCULA POR ESPECIALIDAD (DATOS 2025) =====
        detalle_especialidades_excel <- matricula_actual %>%
          filter(rbd == rbd_seleccionado, !is.na(nom_espe)) %>%
          group_by(nom_espe) %>%
          summarise(
            "Total" = n(),
            "Hombres" = sum(gen_alu == 1, na.rm = TRUE),
            "Mujeres" = sum(gen_alu == 2, na.rm = TRUE),
            "Jóvenes" = sum(cod_ense2 == 7, na.rm = TRUE),
            "Adultos" = sum(cod_ense2 == 8, na.rm = TRUE),
            "3° Medio" = sum(cod_grado == 3, na.rm = TRUE),
            "4° Medio" = sum(cod_grado == 4, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            "% Hombres" = round(100 * Hombres / Total, 1),
            "% Mujeres" = round(100 * Mujeres / Total, 1)
          ) %>%
          rename("Especialidad" = nom_espe) %>%
          select("Especialidad", "Total", "Hombres", "Mujeres", "% Hombres", "% Mujeres", "Jóvenes", "Adultos", "3° Medio", "4° Medio")
        
        # ===== HOJA 3: MATRÍCULA POR NIVEL Y GRADO =====
        detalle_por_nivel <- matricula_actual %>%
          filter(rbd == rbd_seleccionado) %>%
          mutate(
            Nivel = case_when(
              cod_ense2 == 7 ~ "Jóvenes",
              cod_ense2 == 8 ~ "Adultos",
              TRUE ~ NA_character_
            ),
            Grado_Label = case_when(
              cod_grado == 1 ~ "1° Medio",
              cod_grado == 2 ~ "2° Medio",
              cod_grado == 3 ~ "3° Medio",
              cod_grado == 4 ~ "4° Medio",
              TRUE ~ NA_character_
            )
          ) %>%
          filter(!is.na(Nivel), !is.na(Grado_Label)) %>%
          group_by(Nivel, Grado_Label) %>%
          summarise(
            "Total" = n(),
            "Hombres" = sum(gen_alu == 1, na.rm = TRUE),
            "Mujeres" = sum(gen_alu == 2, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(Nivel, desc(Grado_Label)) %>%
          mutate("% Mujeres" = round(100 * Mujeres / Total, 1))
        
        # ===== HOJA 4: ETNIAS (DATOS 2024) =====
        detalle_etnias <- matricula_actual %>%
          filter(rbd == rbd_seleccionado, !is.na(cod_etnia_alu), cod_etnia_alu != "") %>%
          group_by(cod_etnia_alu) %>%
          summarise(
            "Total" = n(),
            "Hombres" = sum(gen_alu == 1, na.rm = TRUE),
            "Mujeres" = sum(gen_alu == 2, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate("% del Total" = round(100 * Total / sum(Total), 1)) %>%
          rename("Etnia" = cod_etnia_alu) %>%
          arrange(desc(Total))
        
        # ===== HOJA 6: NACIONALIDAD Y EXTRANJERÍA (DATOS 2024) =====
        detalle_extranjeros <- matricula_actual %>%
          filter(rbd == rbd_seleccionado) %>%
          group_by(cod_nac_alu) %>%
          summarise(
            "Total" = n(),
            "Hombres" = sum(gen_alu == 1, na.rm = TRUE),
            "Mujeres" = sum(gen_alu == 2, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            "Nacionalidad" = case_when(
              cod_nac_alu == "C" ~ "Chilena",
              cod_nac_alu == "E" ~ "Extranjera",
              cod_nac_alu == "N" ~ "No especificado",
              TRUE ~ "Desconocida"
            ),
            "% del Total" = round(100 * Total / sum(Total), 1)
          ) %>%
          select("Nacionalidad", "Total", "Hombres", "Mujeres", "% del Total")
        
        # ===== HOJA 7: ASISTENCIA ANUAL (DATOS 2024) =====
        detalle_asistencia <- matricula_actual %>%
          filter(rbd == rbd_seleccionado, !is.na(categoria_asis_anual)) %>%
          group_by(categoria_asis_anual) %>%
          summarise(
            "Total" = n(),
            "Hombres" = sum(gen_alu == 1, na.rm = TRUE),
            "Mujeres" = sum(gen_alu == 2, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            "Categoría de Asistencia" = case_when(
              categoria_asis_anual == 1 ~ "Inasistencia crítica (<50%)",
              categoria_asis_anual == 2 ~ "Inasistencia grave (50%-84%)",
              categoria_asis_anual == 3 ~ "Inasistencia reiterada (85%-89%)",
              categoria_asis_anual == 4 ~ "Asistencia esperada (≥90%)",
              TRUE ~ "Desconocida"
            ),
            "% del Total" = round(100 * Total / sum(Total), 1)
          ) %>%
          select("Categoría de Asistencia", "Total", "Hombres", "Mujeres", "% del Total")
        
        # ===== HOJA 6b: ESTUDIANTES EXTRANJEROS POR PAÍS (DATOS 2024) =====
        total_matricula_rbd_excel <- matricula_actual %>% dplyr::filter(rbd == rbd_seleccionado) %>% nrow()
        detalle_origen_extranjeros <- matricula_actual %>%
          dplyr::filter(rbd == rbd_seleccionado, cod_nac_alu == "E") %>%
          dplyr::mutate(pais_origen_alu = ifelse(pais_origen_alu == "Chile" | is.na(pais_origen_alu), "Desconocido", pais_origen_alu)) %>%
          dplyr::group_by("País de Origen" = pais_origen_alu) %>%
          dplyr::summarise(
            "Total" = dplyr::n(),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            "% s/Extranjeros" = round(100 * Total / sum(Total), 1),
            "% s/Total Matrícula" = round(100 * Total / total_matricula_rbd_excel, 1)
          ) %>%
          dplyr::arrange(dplyr::desc(Total))

        # ===== HOJA 8: INTERNADO (DATOS 2024) =====
        detalle_internado <- matricula_actual %>%
          filter(rbd == rbd_seleccionado) %>%
          group_by(int_alu) %>%
          summarise(
            "Total" = n(),
            "Hombres" = sum(gen_alu == 1, na.rm = TRUE),
            "Mujeres" = sum(gen_alu == 2, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            "Condición" = case_when(
              int_alu == 0 ~ "No internado",
              int_alu == 1 ~ "Internado",
              TRUE ~ "Desconocido"
            ),
            "% del Total" = round(100 * Total / sum(Total), 1)
          ) %>%
          select("Condición", "Total", "Hombres", "Mujeres", "% del Total")
        
        # ===== HOJA 8: DATOS DEL ESTABLECIMIENTO =====
        detalle_establecimiento_2025 <- data.frame(
          "Campo" = c(
            "RBD", "Nombre", "Dependencia", "Región", "Provincia", "Comuna", 
            "Sostenedor", "Ruralidad", "RFT/Zona Territorial"
          ),
          "Valor" = c(
            rbd_seleccionado,
            datos_generales$nom_rbd[1],
            obtener_nombre_dependencia(datos_generales$cod_depe2[1]),
            datos_generales$nom_reg_rbd_a[1],
            datos_generales$nom_deprov_rbd[1],
            datos_generales$nom_com_rbd[1],
            datos_generales$nombre_sost[1],
            datos_generales$RuralidadRBD[1],
            datos_generales$rft[1]
          ),
          stringsAsFactors = FALSE
        )
        
        # --- Crear archivo Excel COMPLEJO
        nombre_archivo <- paste0(as.character(rbd_seleccionado), "_minuta_completa.xlsx")
        ruta_archivo <- file.path(temp_dir, nombre_archivo)
        
        # Crear workbook
        wb <- openxlsx::createWorkbook()
        
        # ===== AGREGAR TODAS LAS HOJAS =====
        # Hoja 1: Establecimiento
        openxlsx::addWorksheet(wb, "Establecimiento", tabColour = "#6E5F80")
        openxlsx::writeData(wb, "Establecimiento", detalle_establecimiento_2025, rowNames = FALSE)
        
        # Hoja 2: Resumen
        openxlsx::addWorksheet(wb, "Resumen General", tabColour = "#2C3E50")
        openxlsx::writeData(wb, "Resumen General", resumen_matricula_excel, rowNames = FALSE)
        
        # Hoja 3: Especialidades
        openxlsx::addWorksheet(wb, "Especialidades 2025", tabColour = "#3C7F6D")
        openxlsx::writeData(wb, "Especialidades 2025", detalle_especialidades_excel, rowNames = FALSE)
        
        # Hoja 4: Nivel y Grado
        openxlsx::addWorksheet(wb, "Nivel y Grado 2025", tabColour = "#3C7F6D")
        openxlsx::writeData(wb, "Nivel y Grado 2025", detalle_por_nivel, rowNames = FALSE)
        
        # Hoja 5: Etnias
        if (nrow(detalle_etnias) > 0) {
          openxlsx::addWorksheet(wb, "Etnias (2024)", tabColour = "#E67E22")
          openxlsx::writeData(wb, "Etnias (2024)", detalle_etnias, rowNames = FALSE)
        }
        
        # Hoja 6: Nacionalidad
        if (nrow(detalle_extranjeros) > 0) {
          openxlsx::addWorksheet(wb, "Nacionalidad (2024)", tabColour = "#E67E22")
          openxlsx::writeData(wb, "Nacionalidad (2024)", detalle_extranjeros, rowNames = FALSE)
        }
        
        # Hoja 7: Estudiantes Extranjeros por país
        if (nrow(detalle_origen_extranjeros) > 0) {
          openxlsx::addWorksheet(wb, "Estudiantes Extranjeros (2024)", tabColour = "#E67E22")
          openxlsx::writeData(wb, "Estudiantes Extranjeros (2024)", detalle_origen_extranjeros, rowNames = FALSE)
        }
        
        # Hoja 8: Asistencia Anual
        if (nrow(detalle_asistencia) > 0) {
          openxlsx::addWorksheet(wb, "Asistencia Anual (2024)", tabColour = "#E67E22")
          openxlsx::writeData(wb, "Asistencia Anual (2024)", detalle_asistencia, rowNames = FALSE)
        }
        
        # ===== ESTILOS EXCEL =====
        estilo_hdr_e <- openxlsx::createStyle(
          fontColour = "#FFFFFF", fgFill = "#2C3E50", textDecoration = "bold",
          halign = "center", border = "TopBottomLeftRight", borderColour = "#1A252F", fontSize = 11
        )
        estilo_par_e <- openxlsx::createStyle(
          fontColour = "#1A1A1A", fgFill = "#FDFEFE",
          halign = "center", border = "TopBottomLeftRight", borderColour = "#BDC3C7", fontSize = 10
        )
        estilo_imp_e <- openxlsx::createStyle(
          fontColour = "#1A1A1A", fgFill = "#EAF2F8",
          halign = "center", border = "TopBottomLeftRight", borderColour = "#BDC3C7", fontSize = 10
        )
        aplicar_est_rbd <- function(hoja, df) {
          nc <- ncol(df); nr <- nrow(df)
          openxlsx::addStyle(wb, hoja, estilo_hdr_e, rows = 1, cols = 1:nc, gridExpand = TRUE)
          if (nr > 0) {
            filas <- 2:(nr+1)
            even <- filas[filas %% 2 == 0]; odd <- filas[filas %% 2 != 0]
            if (length(even) > 0) openxlsx::addStyle(wb, hoja, estilo_par_e, rows = even, cols = 1:nc, gridExpand = TRUE)
            if (length(odd)  > 0) openxlsx::addStyle(wb, hoja, estilo_imp_e, rows = odd,  cols = 1:nc, gridExpand = TRUE)
          }
          openxlsx::setColWidths(wb, hoja, cols = 1:nc, widths = "auto")
          openxlsx::setRowHeights(wb, hoja, rows = 1, heights = 24)
          openxlsx::freezePane(wb, hoja, firstRow = TRUE)
        }
        aplicar_est_rbd("Establecimiento", detalle_establecimiento_2025)
        aplicar_est_rbd("Resumen General",     resumen_matricula_excel)
        aplicar_est_rbd("Especialidades 2025", detalle_especialidades_excel)
        aplicar_est_rbd("Nivel y Grado 2025",  detalle_por_nivel)
        if (nrow(detalle_etnias)       > 0) aplicar_est_rbd("Etnias (2024)",                  detalle_etnias)
        if (nrow(detalle_extranjeros)  > 0) aplicar_est_rbd("Nacionalidad (2024)",            detalle_extranjeros)
        if (nrow(detalle_origen_extranjeros) > 0) aplicar_est_rbd("Estudiantes Extranjeros (2024)", detalle_origen_extranjeros)
        if (nrow(detalle_asistencia)   > 0) aplicar_est_rbd("Asistencia Anual (2024)",        detalle_asistencia)
        
        # Guardar workbook
        openxlsx::saveWorkbook(wb, ruta_archivo, overwrite = TRUE)
        
        archivos_generados <- c(archivos_generados, ruta_archivo)
      }
      
      # --- Crear ZIP con todos los archivos
      if (length(archivos_generados) > 0) {
        zip::zip(zipfile = file, files = archivos_generados, mode = "cherry-pick")
      }
    }
  )
  
  resumen_matricula <- reactive({
    datos <- datos_filtrados()$matricula
    
    total <- nrow(datos)
    hombres <- sum(datos$gen_alu == 1, na.rm = TRUE)
    mujeres <- sum(datos$gen_alu == 2, na.rm = TRUE)
    
    # Por tipo de enseñanza
    jovenes_3_4 <- datos %>% filter(cod_ense2 == 7, cod_grado %in% c(3, 4)) %>% nrow()
    jovenes_1_2 <- datos %>% filter(cod_ense2 == 7, cod_grado %in% c(1, 2)) %>% nrow()
    adultos_total <- datos %>% filter(cod_ense2 == 8) %>% nrow()
    adultos_1nivel <- datos %>% filter(cod_ense2 == 8, cod_grado %in% c(1, 2)) %>% nrow()
    adultos_3_4 <- datos %>% filter(cod_ense2 == 8, cod_grado %in% c(3, 4)) %>% nrow()
    
    # Desglose detallado por tipo, grado y género
    # Jóvenes por grado y género
    jovenes_h_1_2 <- datos %>% filter(cod_ense2 == 7, cod_grado %in% c(1, 2), gen_alu == 1) %>% nrow()
    jovenes_m_1_2 <- datos %>% filter(cod_ense2 == 7, cod_grado %in% c(1, 2), gen_alu == 2) %>% nrow()
    jovenes_h_3 <- datos %>% filter(cod_ense2 == 7, cod_grado == 3, gen_alu == 1) %>% nrow()
    jovenes_m_3 <- datos %>% filter(cod_ense2 == 7, cod_grado == 3, gen_alu == 2) %>% nrow()
    jovenes_h_4 <- datos %>% filter(cod_ense2 == 7, cod_grado == 4, gen_alu == 1) %>% nrow()
    jovenes_m_4 <- datos %>% filter(cod_ense2 == 7, cod_grado == 4, gen_alu == 2) %>% nrow()
    
    # Adultos por grado y género
    adultos_h_1_2 <- datos %>% filter(cod_ense2 == 8, cod_grado %in% c(1, 2), gen_alu == 1) %>% nrow()
    adultos_m_1_2 <- datos %>% filter(cod_ense2 == 8, cod_grado %in% c(1, 2), gen_alu == 2) %>% nrow()
    adultos_h_3 <- datos %>% filter(cod_ense2 == 8, cod_grado == 3, gen_alu == 1) %>% nrow()
    adultos_m_3 <- datos %>% filter(cod_ense2 == 8, cod_grado == 3, gen_alu == 2) %>% nrow()
    adultos_h_4 <- datos %>% filter(cod_ense2 == 8, cod_grado == 4, gen_alu == 1) %>% nrow()
    adultos_m_4 <- datos %>% filter(cod_ense2 == 8, cod_grado == 4, gen_alu == 2) %>% nrow()
    
    # Por grados (totales)
    tercero_medio <- datos %>% filter(cod_grado == 3) %>% nrow()
    cuarto_medio <- datos %>% filter(cod_grado == 4) %>% nrow()
    primer_nivel <- datos %>% filter(cod_grado %in% c(1, 2)) %>% nrow()
    
    data.frame(
      Categoría = c(
        "🎯 RESUMEN GENERAL",
        "Total Matrícula",
        "Matrícula Hombres", 
        "Matrícula Mujeres",
        "",
        "👥 POR TIPO DE ENSEÑANZA",
        "Jóvenes - 1° y 2° Nivel",
        "Jóvenes - 3° y 4° Medio", 
        "Adultos - Total",
        "Adultos - 1° Nivel",
        "Adultos - 3° y 4° Medio",
        "",
        "📚 DESGLOSE DETALLADO POR GRADO Y GÉNERO",
        "--- JÓVENES ---",
        "Jóvenes Hombres - 1° y 2° Nivel",
        "Jóvenes Mujeres - 1° y 2° Nivel",
        "Jóvenes Hombres - 3° Medio",
        "Jóvenes Mujeres - 3° Medio",
        "Jóvenes Hombres - 4° Medio",
        "Jóvenes Mujeres - 4° Medio",
        "",
        "--- ADULTOS ---",
        "Adultos Hombres - 1° Nivel",
        "Adultos Mujeres - 1° Nivel",
        "Adultos Hombres - 3° Medio",
        "Adultos Mujeres - 3° Medio",
        "Adultos Hombres - 4° Medio",
        "Adultos Mujeres - 4° Medio",
        "",
        "📊 TOTALES POR GRADO",
        "Total 1° Nivel",
        "Total 3° Medio",
        "Total 4° Medio"
      ),
      Valor = c(
        "",
        format(total, big.mark = "."),
        format(hombres, big.mark = "."),
        format(mujeres, big.mark = "."),
        "",
        "",
        format(jovenes_1_2, big.mark = "."),
        format(jovenes_3_4, big.mark = "."),
        format(adultos_total, big.mark = "."),
        format(adultos_1nivel, big.mark = "."),
        format(adultos_3_4, big.mark = "."),
        "",
        "",
        "",
        format(jovenes_h_1_2, big.mark = "."),
        format(jovenes_m_1_2, big.mark = "."),
        format(jovenes_h_3, big.mark = "."),
        format(jovenes_m_3, big.mark = "."),
        format(jovenes_h_4, big.mark = "."),
        format(jovenes_m_4, big.mark = "."),
        "",
        "",
        format(adultos_h_1_2, big.mark = "."),
        format(adultos_m_1_2, big.mark = "."),
        format(adultos_h_3, big.mark = "."),
        format(adultos_m_3, big.mark = "."),
        format(adultos_h_4, big.mark = "."),
        format(adultos_m_4, big.mark = "."),
        "",
        "",
        format(primer_nivel, big.mark = "."),
        format(tercero_medio, big.mark = "."),
        format(cuarto_medio, big.mark = ".")
      ),
      check.names = FALSE
    )
  })
  
  # --- Resumen detallado de matrícula filtrada (sin emojis, con %) ---
  output$resumen_matricula <- renderUI({
    datos <- datos_filtrados()$matricula
    n <- nrow(datos)
    if (n == 0) return(tags$div(class = "text-muted", style = "padding:20px;", "Sin datos para los filtros seleccionados."))
    fmt <- function(x) format(x, big.mark = ".")
    pc  <- function(x) paste0(gsub("\\.", ",", sprintf("%.1f", 100 * x / n)), "%")
    H  <- sum(datos$gen_alu == 1, na.rm = TRUE); M <- sum(datos$gen_alu == 2, na.rm = TRUE)
    jov <- sum(datos$cod_ense2 == 7, na.rm = TRUE); adu <- sum(datos$cod_ense2 == 8, na.rm = TRUE)
    jov34 <- sum(datos$cod_ense2 == 7 & datos$cod_grado %in% c(3,4), na.rm = TRUE)
    adu1  <- sum(datos$cod_ense2 == 8 & datos$cod_grado %in% c(1,2), na.rm = TRUE)
    adu34 <- sum(datos$cod_ense2 == 8 & datos$cod_grado %in% c(3,4), na.rm = TRUE)
    t1 <- sum(datos$cod_grado %in% c(1,2), na.rm = TRUE)
    t3 <- sum(datos$cod_grado == 3, na.rm = TRUE); t4 <- sum(datos$cod_grado == 4, na.rm = TRUE)
    gg <- function(ens, gr, gen) sum(datos$cod_ense2 == ens & datos$cod_grado %in% gr & datos$gen_alu == gen, na.rm = TRUE)

    fila <- function(lbl, val, pct = TRUE) tags$div(
      style = "display:flex;justify-content:space-between;align-items:center;padding:7px 14px;border-bottom:1px solid #EEF1F4;",
      tags$span(lbl, style = "color:#51606E;font-size:13.5px;"),
      tags$span(style = "display:flex;gap:12px;align-items:baseline;",
        tags$span(fmt(val), style = "font-weight:700;color:#1F2A37;font-variant-numeric:tabular-nums;"),
        if (pct) tags$span(pc(val), style = "font-size:12px;color:#6B7785;min-width:48px;text-align:right;")))
    subhdr <- function(txt) tags$div(style = "padding:6px 14px;background:#F3F6F9;color:#51606E;font-size:11.5px;font-weight:700;text-transform:uppercase;letter-spacing:.4px;", txt)
    seccion <- function(ic, titulo, ...) tags$div(class = "metric-card", style = "padding:0;overflow:hidden;margin-bottom:18px;",
      tags$div(style = "background:#34536A;color:#fff;padding:10px 14px;font-weight:700;font-size:12.5px;text-transform:uppercase;letter-spacing:.4px;",
               tags$i(class = paste("fas", ic), style = "margin-right:8px;opacity:.85;"), titulo),
      tags$div(...))

    fluidRow(
      column(6,
        seccion("fa-users", "Resumen general",
          fila("Total matrícula", n, FALSE),
          fila("Hombres", H), fila("Mujeres", M)),
        seccion("fa-graduation-cap", "Por tipo de enseñanza",
          fila("Jóvenes (total)", jov),
          fila("Jóvenes · 3° y 4° medio", jov34),
          fila("Adultos (total)", adu),
          fila("Adultos · 1° nivel (1°-2° medio)", adu1),
          fila("Adultos · 3° y 4° medio", adu34)),
        seccion("fa-layer-group", "Totales por grado",
          fila("1° nivel (sólo adultos)", t1),
          fila("3° medio", t3), fila("4° medio", t4))
      ),
      column(6,
        seccion("fa-venus-mars", "Detalle por grado y género",
          subhdr("Educación de Jóvenes"),
          fila("Hombres · 3° medio", gg(7,3,1)), fila("Mujeres · 3° medio", gg(7,3,2)),
          fila("Hombres · 4° medio", gg(7,4,1)), fila("Mujeres · 4° medio", gg(7,4,2)),
          subhdr("Educación de Adultos"),
          fila("Hombres · 1° nivel", gg(8,c(1,2),1)), fila("Mujeres · 1° nivel", gg(8,c(1,2),2)),
          fila("Hombres · 3° medio", gg(8,3,1)), fila("Mujeres · 3° medio", gg(8,3,2)),
          fila("Hombres · 4° medio", gg(8,4,1)), fila("Mujeres · 4° medio", gg(8,4,2)))
      )
    )
  })
  
  # Mejorar el renderLeaflet
  output$mapa_matricula <- renderLeaflet({
    datos <- datos_filtrados()
    
    # Popups del mapa (estética del visualizador, con %)
    .mt <- ifelse(is.na(datos$comunas$matricula), 0, datos$comunas$matricula)
    .mh <- ifelse(is.na(datos$comunas$matricula_hombres), 0, datos$comunas$matricula_hombres)
    .mm <- ifelse(is.na(datos$comunas$matricula_mujeres), 0, datos$comunas$matricula_mujeres)
    .ph <- ifelse(.mt > 0, paste0(round(100 * .mh / .mt), "%"), "—")
    .pm <- ifelse(.mt > 0, paste0(round(100 * .mm / .mt), "%"), "—")
    .ne <- ifelse(is.na(datos$comunas$n_establecimientos), 0, datos$comunas$n_establecimientos)
    fila_pop <- function(lbl, val, extra = "") paste0(
      "<tr><td style='color:#51606E;padding:2px 0;'>", lbl,
      "</td><td style='text-align:right;font-weight:700;color:#1F2A37;padding:2px 0;'>", val,
      "</td><td style='text-align:right;color:#6B7785;padding:2px 0 2px 10px;font-size:11px;'>", extra, "</td></tr>")
    popups <- paste0(
      "<div style='font-family:Inter,Roboto,sans-serif;min-width:215px;'>",
      "<div style='font-weight:700;font-size:14px;color:#34536A;'>", datos$comunas$Comuna, "</div>",
      "<div style='color:#6B7785;font-size:11px;margin-bottom:6px;'>", datos$comunas$Region, "</div>",
      "<hr style='margin:6px 0;border:none;border-top:1px solid #E8ECF1;'>",
      "<table style='width:100%;font-size:12.5px;border-collapse:collapse;'>",
      fila_pop("Matrícula EMTP", format(.mt, big.mark = "."), ""),
      fila_pop("Hombres", format(.mh, big.mark = "."), .ph),
      fila_pop("Mujeres", format(.mm, big.mark = "."), .pm),
      fila_pop("Establecimientos", format(.ne, big.mark = "."), ""),
      "</table></div>"
    )
    
    # Crear el mapa base
    mapa <- leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = datos$comunas,
        fillColor = ~fill_color_final,
        fillOpacity = ~fill_opacity,
        color = "white",
        weight = 1,
        popup = popups,
        highlight = highlightOptions(
          weight = 3,
          color = "#2C3E50",
          fillOpacity = 0.8,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        title = "Matrícula EMTP por Comuna",
        labels = c("Sin datos", "Con datos"),
        colors = c("#BBBBBB", "#34536A"),
        opacity = 0.8
      )
    
    # Aplicar zoom automático a las comunas con datos filtrados
    comunas_con_datos <- datos$comunas %>% filter(!is.na(matricula) & matricula > 0)
    
    if (nrow(comunas_con_datos) > 0) {
      # Verificar que hay geometría válida
      if (!is.null(comunas_con_datos$geometry) && any(!st_is_empty(comunas_con_datos))) {
        # Calcular el bounding box de las comunas con datos
        bbox <- st_bbox(comunas_con_datos)
        
        # Agregar fitBounds para hacer zoom automático con un margen
        mapa <- mapa %>%
          fitBounds(
            lng1 = bbox[["xmin"]] - 0.1, lat1 = bbox[["ymin"]] - 0.1,
            lng2 = bbox[["xmax"]] + 0.1, lat2 = bbox[["ymax"]] + 0.1
          )
      }
    }
    
    mapa
  })
  
  # ---------------------------------------------------------------------------
  # Tabla seleccionable de establecimientos (Buscador)
  # ---------------------------------------------------------------------------
  output$tabla_establecimientos <- DT::renderDataTable({
    req(input$buscar)
    resultado_busqueda()
  }, selection = list(mode = 'multiple', target = 'row'), options = list(
    pageLength = 15,
    scrollX = TRUE,
    language = list(
      search = "Buscar:",
      lengthMenu = "Mostrar _MENU_ registros por página",
      zeroRecords = "No se encontraron registros",
      info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
      infoEmpty = "Mostrando 0 a 0 de 0 registros",
      infoFiltered = "(filtrado de _MAX_ registros totales)",
      paginate = list(
        first = "Primero",
        previous = "Anterior",
        `next` = "Siguiente",
        last = "Último"
      )
    )
  ))
  
  proxy_tabla_est <- DT::dataTableProxy("tabla_establecimientos")
  
  # Seleccionar todas las filas tras nueva búsqueda
  observeEvent(resultado_busqueda(), {
    dat <- resultado_busqueda()
    if (nrow(dat) > 0) {
      DT::selectRows(proxy_tabla_est, 1:nrow(dat))
      # Habilitar botones de descarga cuando hay resultados
      shinyjs::removeClass("descargar_minuta_pdf", "disabled")
      shinyjs::removeClass("descargar_minuta_excel", "disabled")
    } else {
      # Deshabilitar botones si no hay resultados
      shinyjs::addClass("descargar_minuta_pdf", "disabled")
      shinyjs::addClass("descargar_minuta_excel", "disabled")
    }
  })
  
  observeEvent(input$seleccionar_todos_est, {
    dat <- resultado_busqueda()
    if (nrow(dat) > 0) DT::selectRows(proxy_tabla_est, 1:nrow(dat))
  })
  observeEvent(input$deseleccionar_todos_est, {
    DT::selectRows(proxy_tabla_est, NULL)
  })
  
  `%||%` <- function(a, b) if (is.null(a)) b else a
  output$contador_seleccion <- renderText({
    total <- tryCatch(nrow(resultado_busqueda()), error = function(e) 0)
    sel <- length(input$tabla_establecimientos_rows_selected %||% integer())
    if (total == 0) return("0 seleccionados")
    paste0(sel, " seleccionados de ", total)
  })
  
  rbd_seleccionados <- reactive({
    dat <- resultado_busqueda()
    idx <- input$tabla_establecimientos_rows_selected
    if (is.null(idx) || length(idx) == 0) return(dat$rbd) # fallback a todos
    dat$rbd[idx]
  })
  
  resultado_busqueda <- eventReactive(input$buscar, {
    datos <- matricula_raw
    
    # Aplicar filtros (ya están bien)
    if (input$rft_busqueda != "Todas") {
      datos <- datos %>% filter(rft == input$rft_busqueda)
    }
    if (input$region_busqueda != "Todas") {
      datos <- datos %>% filter(nom_reg_rbd_a == input$region_busqueda)
    }
    if (input$provincia_busqueda != "Todas") {
      datos <- datos %>% filter(nom_deprov_rbd == input$provincia_busqueda)
    }
    if (input$comuna_busqueda != "Todas") {
      datos <- datos %>% filter(nom_com_rbd == input$comuna_busqueda)
    }
    
    # Filtro por especialidad in los datos
    if (!is.null(input$especialidad_busqueda) && length(input$especialidad_busqueda) > 0) {
      datos <- datos %>% filter(nom_espe %in% input$especialidad_busqueda)
    }
    
    if (input$rbd_busqueda != "") {
      rbd_input <- str_split(input$rbd_busqueda, ",")[[1]] %>%
        str_trim() %>%
        discard(~ .x == "") %>%
        as.character()
      datos <- datos %>% filter(rbd %in% rbd_input)
    }
    if (input$nombre_busqueda != "") {
      datos <- datos %>% filter(str_detect(str_to_lower(nom_rbd), str_to_lower(input$nombre_busqueda)))
    }
    if (input$dependencia_busqueda != "Todas") {
      datos <- datos %>% filter(cod_depe2 == input$dependencia_busqueda)
    }
    if (input$sostenedor_busqueda != "Todos") {
      datos <- datos %>% filter(nombre_sost == input$sostenedor_busqueda)
    }
    
    # Agrupar y mantener rbd
    datos %>%
      group_by(rbd, nom_rbd, nom_com_rbd, nom_deprov_rbd, nom_reg_rbd_a, cod_depe2) %>%
      summarise(
        especialidades = str_c(unique(na.omit(nom_espe)), collapse = ", "),
        .groups = "drop"
      ) %>%
      arrange(nom_rbd)
  })
  
  # ===========================================================================
  # OUTPUTS PARA BUSCADOR AVANZADO
  # ===========================================================================
  
  # (Eliminado output$tabla_establecimientos_dt redundante)
  
  # ===========================================================================
  # OUTPUTS PARA ANÁLISIS VISUAL
  # ===========================================================================
  
  # Métricas del análisis visual
  output$total_matricula <- renderText({
    format(nrow(datos_visual()), big.mark = ".")
  })
  
  output$total_hombres <- renderText({
    format(sum(datos_visual()$gen_alu == 1, na.rm = TRUE), big.mark = ".")
  })
  
  output$total_mujeres <- renderText({
    format(sum(datos_visual()$gen_alu == 2, na.rm = TRUE), big.mark = ".")
  })
  
  output$pct_hombres <- renderText({
    total <- nrow(datos_visual())
    if (total == 0) return("0%")
    hombres <- sum(datos_visual()$gen_alu == 1, na.rm = TRUE)
    paste0(round(hombres / total * 100, 1), "%")
  })
  
  output$pct_mujeres <- renderText({
    total <- nrow(datos_visual())
    if (total == 0) return("0%")
    mujeres <- sum(datos_visual()$gen_alu == 2, na.rm = TRUE)
    paste0(round(mujeres / total * 100, 1), "%")
  })
  
  output$total_establecimientos <- renderText({
    format(n_distinct(datos_visual()$rbd), big.mark = ".")
  })
  
  # Gráfico principal del análisis visual
  output$grafico_principal <- renderPlotly({
    datos <- datos_visual()
    
    if (nrow(datos) == 0) {
      p <- plot_ly() %>%
        add_text(x = 0.5, y = 0.5, text = "No hay datos para mostrar", textsize = 16) %>%
        layout(
          title = "Sin datos disponibles",
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
      return(p)
    }
    
    if (input$tipo_grafico == "barras_especialidad") {
      # Gráfico de barras por especialidad
      datos_barras <- datos %>%
        group_by(nom_espe, gen_alu) %>%
        summarise(Total = n(), .groups = "drop") %>%
        pivot_wider(names_from = gen_alu, values_from = Total, values_fill = 0) %>%
        mutate(
          Hombres = `1`,
          Mujeres = `2`,
          Total_est = Hombres + Mujeres,
          pct_Hombres = ifelse(Total_est > 0, Hombres / Total_est * 100, 0),
          pct_Mujeres = ifelse(Total_est > 0, Mujeres / Total_est * 100, 0)
        ) %>%
        arrange(pct_Mujeres) %>% 
        mutate(nom_espe = factor(nom_espe, levels = nom_espe))
      
      p <- plot_ly(
        datos_barras,
        x = ~nom_espe,
        y = ~pct_Hombres,
        type = 'bar',
        name = 'Hombres',
        marker = list(color = "#3B5268"),
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Hombres: %{y:.1f}%<br>",
          "<extra></extra>"
        )
      ) %>%
        add_trace(
          y = ~pct_Mujeres, 
          name = 'Mujeres', 
          marker = list(color = "#A75F5D"),
          hovertemplate = paste(
            "<b>%{x}</b><br>",
            "Mujeres: %{y:.1f}%<br>",
            "<extra></extra>"
          )
        ) %>%
        layout(
          title = "Distribución por Género en Especialidades EMTP",
          barmode = 'stack',
          margin = list(b = 150),
          xaxis = list(title = "Especialidad", tickangle = -45),
          yaxis = list(title = "Porcentaje de Estudiantes")
        )
      
    } else if (input$tipo_grafico == "distribucion_genero") {
      # Gráfico circular de distribución por género
      datos_genero <- datos %>%
        group_by(gen_alu) %>%
        summarise(Total = n(), .groups = "drop") %>%
        mutate(
          Género = case_when(
            gen_alu == 1 ~ "Hombres",
            gen_alu == 2 ~ "Mujeres",
            TRUE ~ "No especificado"
          ),
          Porcentaje = round(Total / sum(Total) * 100, 1)
        )
      
      datos_genero <- datos_genero %>%
        mutate(color = dplyr::case_when(
          Género == "Hombres" ~ "#3B5268",
          Género == "Mujeres" ~ "#A75F5D",
          TRUE ~ "#7F8C8D"
        ))
      p <- plot_ly(
        datos_genero,
        labels = ~Género,
        values = ~Total,
        type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        marker = list(colors = datos_genero$color),
        hovertemplate = paste(
          "<b>%{label}</b><br>",
          "Total: %{value:,}<br>",
          "Porcentaje: %{percent}<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(title = "Distribución de Matrícula por Género")
      
    } else if (input$tipo_grafico == "matricula_region") {
      # Gráfico de barras por región
      datos_region <- datos %>%
        group_by(nom_reg_rbd_a) %>%
        summarise(Total = n(), .groups = "drop") %>%
        arrange(desc(Total)) %>%
        slice_head(n = 15)  # Top 15 regiones
      
      p <- plot_ly(
        datos_region,
        x = ~reorder(nom_reg_rbd_a, Total),
        y = ~Total,
        type = 'bar',
        marker = list(color = "#5A6E79"),
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Matrícula: %{y:,}<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          title = "Matrícula EMTP por Región (Top 15)",
          xaxis = list(title = "Región"),
          yaxis = list(title = "Número de Estudiantes"),
          margin = list(b = 100)
        )
      
    } else if (input$tipo_grafico == "dependencia_matricula") {
      # Gráfico de barras por dependencia
      datos_dep <- datos %>%
        group_by(cod_depe2) %>%
        summarise(Total = n(), .groups = "drop") %>%
        arrange(desc(Total))
      
      p <- plot_ly(
        datos_dep,
        x = ~reorder(cod_depe2, Total),
        y = ~Total,
        type = 'bar',
        marker = list(color = "#6E5F80"),
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Matrícula: %{y:,}<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          title = "Matrícula EMTP por Dependencia",
          xaxis = list(title = "Dependencia"),
          yaxis = list(title = "Número de Estudiantes")
        )
    }
    
    return(p)
  })
  
  # ============================================================================
  # MÓDULO: EGRESADOS Y TITULADOS EMTP
  # ============================================================================
  
  # Datos reactivos filtrados de egresados
  egresados_filtrados <- reactive({
    datos <- egresados_2024
    
    # Aplicar filtros
    if (!is.null(input$egr_region) && input$egr_region != "Todas") {
      datos <- datos %>% filter(NOM_REG_RBD_A == input$egr_region)
    }
    
    if (!is.null(input$egr_comuna) && input$egr_comuna != "Todas") {
      datos <- datos %>% filter(NOM_COM_RBD == input$egr_comuna)
    }
    
    if (!is.null(input$egr_dependencia) && input$egr_dependencia != "Todas") {
      datos <- datos %>% filter(DEPENDENCIA_label == input$egr_dependencia)
    }
    
    if (!is.null(input$egr_ruralidad) && input$egr_ruralidad != "Todas") {
      datos <- datos %>% filter(RURALIDAD_label == input$egr_ruralidad)
    }
    
    if (!is.null(input$egr_tipo_ense) && input$egr_tipo_ense != "Todas") {
      datos <- datos %>% filter(TIPO_ENSE_label == input$egr_tipo_ense)
    }
    
    datos
  })
  
  # Actualizar opciones de filtros dinámicamente
  observe({
    regiones <- c("Todas", sort(unique(egresados_2024$NOM_REG_RBD_A)))
    updateSelectInput(session, "egr_region", choices = regiones)
    
    dependencias <- c("Todas", sort(unique(egresados_2024$DEPENDENCIA_label)))
    updateSelectInput(session, "egr_dependencia", choices = dependencias)
  })
  
  # Actualizar comunas según región seleccionada
  observe({
    if (!is.null(input$egr_region) && input$egr_region != "Todas") {
      comunas <- egresados_2024 %>%
        filter(NOM_REG_RBD_A == input$egr_region) %>%
        pull(NOM_COM_RBD) %>%
        unique() %>%
        sort()
      comunas <- c("Todas", comunas)
    } else {
      comunas <- c("Todas", sort(unique(egresados_2024$NOM_COM_RBD)))
    }
    updateSelectInput(session, "egr_comuna", choices = comunas)
  })
  
  # Reiniciar filtros
  observeEvent(input$egr_limpiar_filtros, {
    updateSelectInput(session, "egr_region", selected = "Todas")
    updateSelectInput(session, "egr_comuna", selected = "Todas")
    updateSelectInput(session, "egr_dependencia", selected = "Todas")
    updateSelectInput(session, "egr_ruralidad", selected = "Todas")
    updateSelectInput(session, "egr_tipo_ense", selected = "Todas")
  })
  
  # KPIs
  output$egr_total_egresados <- renderText({
    format(nrow(egresados_filtrados()), big.mark = ".")
  })
  
  output$egr_total_ee <- renderText({
    format(n_distinct(egresados_filtrados()$RBD), big.mark = ".")
  })
  
  # KPIs adicionales para pestaña de Egresados
  output$egr_pct_urbano <- renderText({
    datos <- egresados_filtrados()
    pct <- (sum(datos$RURALIDAD_label == "Urbano", na.rm = TRUE) / nrow(datos)) * 100
    paste0(round(pct, 1), "%")
  })
  
  output$egr_pct_jovenes <- renderText({
    datos <- egresados_filtrados()
    pct <- (sum(datos$TIPO_ENSE_label == "Jóvenes", na.rm = TRUE) / nrow(datos)) * 100
    paste0(round(pct, 1), "%")
  })
  
  # KPI: % Continuidad en Educación Superior
  # ACTUALIZADO: 19 de enero de 2026
  # Fuente: Cruce egresados EMTP 2024 (73,931) con matrícula ES 2025 (1.4M)
  # Resultado: 37,170 continúan (50.3%)
  # Datos: data/processed/continuidad_app_optimizado.rds
  # Ver: HALLAZGOS_CONTINUIDAD_ES_2024_2025.md para análisis completo
  output$egr_pct_continuidad <- renderText({
    paste0(round(indicadores_continuidad$pct_continuidad, 1), "%")
  })
  
  # KPI: % Titulados en Educación Superior
  # PENDIENTE: Requiere base titulados SIES año t+2 a t+4 (2026-2028 para egresados 2024)
  # Variables clave: MRUN, AÑO_TITULACION, CARRERA, NIVEL_CARRERA
  # Metodología: Seguimiento de cohorte de egresados 2024 a lo largo del tiempo
  # Nota: Primeros titulados esperados en 2026 (carreras cortas 2-3 años)
  # Ver docs/METODOLOGIA_EGRESADOS_TEMPORAL.md para plan de implementación
  output$egr_pct_titulados <- renderText({
    "N/D *"
  })
  
  # Gráfico: Distribución por Dependencia
  output$egr_plot_dependencia <- renderPlotly({
    datos <- egresados_filtrados() %>%
      count(DEPENDENCIA_label) %>%
      mutate(pct = n / sum(n) * 100) %>%
      arrange(desc(n))
    
    plot_ly(datos, 
            x = ~n, 
            y = ~reorder(DEPENDENCIA_label, n),
            type = 'bar',
            orientation = 'h',
            marker = list(color = '#34536A'),
            text = ~paste0(format(n, big.mark = "."), " (", round(pct, 1), "%)"),
            textposition = 'outside',
            hoverinfo = 'text',
            hovertext = ~paste0(DEPENDENCIA_label, "<br>",
                                format(n, big.mark = "."), " egresados<br>",
                                round(pct, 1), "%")
    ) %>%
      layout(
        xaxis = list(title = "Número de Egresados"),
        yaxis = list(title = ""),
        margin = list(l = 200),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Gráfico: Top 10 Regiones
  output$egr_plot_region <- renderPlotly({
    datos <- egresados_filtrados() %>%
      count(NOM_REG_RBD_A) %>%
      arrange(desc(n)) %>%
      mutate(pct = n / sum(n) * 100)
    
    plot_ly(datos, 
            x = ~n, 
            y = ~reorder(NOM_REG_RBD_A, n),
            type = 'bar',
            orientation = 'h',
            marker = list(color = '#3C7F6D'),
            text = ~paste0(format(n, big.mark = "."), " (", round(pct, 1), "%)"),
            textposition = 'outside',
            hoverinfo = 'text',
            hovertext = ~paste0(NOM_REG_RBD_A, "<br>",
                                format(n, big.mark = "."), " egresados<br>",
                                round(pct, 1), "%")
    ) %>%
      layout(
        xaxis = list(title = "Número de Egresados"),
        yaxis = list(title = ""),
        margin = list(l = 100),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Gráfico: Ruralidad (Torta)
  output$egr_plot_ruralidad <- renderPlotly({
    datos <- egresados_filtrados() %>%
      count(RURALIDAD_label) %>%
      mutate(pct = n / sum(n) * 100)
    
    plot_ly(datos, 
            labels = ~RURALIDAD_label, 
            values = ~n,
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            marker = list(colors = c('#3C7F6D', '#C0392B')),
            hoverinfo = 'text',
            hovertext = ~paste0(RURALIDAD_label, "<br>",
                                format(n, big.mark = "."), " egresados<br>",
                                round(pct, 1), "%")
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(orientation = 'h', y = -0.1)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Gráfico: Tipo de Enseñanza (Torta)
  output$egr_plot_tipo_ense <- renderPlotly({
    datos <- egresados_filtrados() %>%
      count(TIPO_ENSE_label) %>%
      mutate(pct = n / sum(n) * 100)
    
    plot_ly(datos, 
            labels = ~TIPO_ENSE_label, 
            values = ~n,
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            marker = list(colors = c('#34536A', '#B35A5A')),
            hoverinfo = 'text',
            hovertext = ~paste0(TIPO_ENSE_label, "<br>",
                                format(n, big.mark = "."), " egresados<br>",
                                round(pct, 1), "%")
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(orientation = 'h', y = -0.1)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Tabla de detalle
  output$egr_tabla_detalle <- DT::renderDataTable({
    egresados_filtrados() %>%
      group_by(
        RBD,
        NOM_REG_RBD_A,
        NOM_COM_RBD,
        DEPENDENCIA_label,
        RURALIDAD_label,
        TIPO_ENSE_label
      ) %>%
      summarise(
        n_egresados = n(),
        promedio_notas = round(mean(PROM_NOTAS_ALU, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(n_egresados)) %>%
      rename(
        "RBD" = RBD,
        "Región" = NOM_REG_RBD_A,
        "Comuna" = NOM_COM_RBD,
        "Dependencia" = DEPENDENCIA_label,
        "Ruralidad" = RURALIDAD_label,
        "Tipo Enseñanza" = TIPO_ENSE_label,
        "N° Egresados" = n_egresados,
        "Promedio Notas" = promedio_notas
      ) %>%
      datatable(
        extensions = 'Buttons',
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = list('copy','csv', list(extend='excel', title='egresados_por_establecimiento')),
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
        ),
        filter = 'top',
        rownames = FALSE
      )
  })

  # ============================================================================
  # OUTPUTS PARA SUB-PESTAÑA: CONTINUIDAD DE ESTUDIOS
  # ============================================================================
  
  # Datos reactivos filtrados de continuidad
  continuidad_filtrada <- reactive({
    datos <- continuidad_es
    
    # Aplicar filtros
    if (!is.null(input$cont_filtro_region) && input$cont_filtro_region != "Todas") {
      datos <- datos %>% filter(NOM_REG_RBD_A == input$cont_filtro_region)
    }
    
    if (!is.null(input$cont_filtro_comuna) && input$cont_filtro_comuna != "Todas") {
      datos <- datos %>% filter(NOM_COM_RBD == input$cont_filtro_comuna)
    }
    
    if (!is.null(input$cont_filtro_dependencia) && input$cont_filtro_dependencia != "Todas") {
      datos <- datos %>% filter(DEPENDENCIA_label == input$cont_filtro_dependencia)
    }
    
    if (!is.null(input$cont_filtro_ruralidad) && input$cont_filtro_ruralidad != "Todas") {
      datos <- datos %>% filter(RURALIDAD_label == input$cont_filtro_ruralidad)
    }
    
    if (!is.null(input$cont_filtro_tipo_ense) && input$cont_filtro_tipo_ense != "Todas") {
      datos <- datos %>% filter(TIPO_ENSE_label == input$cont_filtro_tipo_ense)
    }
    
    if (!is.null(input$cont_filtro_genero) && input$cont_filtro_genero != "Todos") {
      if(input$cont_filtro_genero == "Mujeres") {
        datos <- datos %>% filter(GEN_ALU == 2)
      } else if(input$cont_filtro_genero == "Hombres") {
        datos <- datos %>% filter(GEN_ALU == 1)
      }
    }
    
    datos
  })
  
  # Actualizar opciones de filtros dinámicamente
  observe({
    regiones <- c("Todas", sort(unique(continuidad_es$NOM_REG_RBD_A)))
    updateSelectInput(session, "cont_filtro_region", choices = regiones)
  })
  
  # Actualizar comunas según región seleccionada
  observe({
    if (!is.null(input$cont_filtro_region) && input$cont_filtro_region != "Todas") {
      comunas <- continuidad_es %>%
        filter(NOM_REG_RBD_A == input$cont_filtro_region) %>%
        pull(NOM_COM_RBD) %>%
        unique() %>%
        sort()
      comunas <- c("Todas", comunas)
    } else {
      comunas <- c("Todas", sort(unique(continuidad_es$NOM_COM_RBD)))
    }
    updateSelectInput(session, "cont_filtro_comuna", choices = comunas)
  })
  
  # Reiniciar filtros
  observeEvent(input$cont_limpiar_filtros, {
    updateSelectInput(session, "cont_filtro_region", selected = "Todas")
    updateSelectInput(session, "cont_filtro_comuna", selected = "Todas")
    updateSelectInput(session, "cont_filtro_dependencia", selected = "Todas")
    updateSelectInput(session, "cont_filtro_ruralidad", selected = "Todas")
    updateSelectInput(session, "cont_filtro_tipo_ense", selected = "Todas")
    updateSelectInput(session, "cont_filtro_genero", selected = "Todos")
  })
  
  # KPIs de continuidad
  output$cont_total_egresados <- renderText({
    format(nrow(continuidad_filtrada()), big.mark = ".")
  })
  
  output$cont_total_continuan <- renderText({
    n_continua <- sum(continuidad_filtrada()$continua_es, na.rm = TRUE)
    format(n_continua, big.mark = ".")
  })
  
  output$cont_pct_continuidad <- renderText({
    datos <- continuidad_filtrada()
    if(nrow(datos) == 0) return("N/D")
    
    n_continua <- sum(datos$continua_es, na.rm = TRUE)
    pct <- (n_continua / nrow(datos)) * 100
    paste0(round(pct, 1), "%")
  })
  
  output$cont_pct_mujeres <- renderText({
    # % de continuidad de mujeres egresadas (usa GEN_ALU de matrícula)
    datos_mujeres <- continuidad_filtrada() %>% filter(GEN_ALU == 2)
    if(nrow(datos_mujeres) == 0) return("N/D")
    
    n_mujeres_continuan <- sum(datos_mujeres$continua_es, na.rm = TRUE)
    pct_mujeres <- (n_mujeres_continuan / nrow(datos_mujeres)) * 100
    paste0(round(pct_mujeres, 1), "%")
  })
  
  # Gráfico: Tipo de Institución ES
  output$cont_plot_tipo_inst <- renderPlotly({
    datos <- continuidad_filtrada() %>%
      filter(continua_es) %>%
      count(tipo_inst_3, sort = TRUE) %>%
      mutate(pct = n / sum(n) * 100)
    
    if(nrow(datos) == 0) {
      return(plotly_empty() %>% 
               layout(title = list(text = "No hay datos para mostrar con los filtros seleccionados")))
    }
    
    plot_ly(datos, 
            x = ~n, 
            y = ~reorder(tipo_inst_3, n),
            type = 'bar',
            orientation = 'h',
            marker = list(color = '#6E5F80'),
            text = ~paste0(format(n, big.mark = "."), " (", round(pct, 1), "%)"),
            textposition = 'outside',
            hoverinfo = 'text',
            hovertext = ~paste0(tipo_inst_3, "<br>",
                                format(n, big.mark = "."), " estudiantes<br>",
                                round(pct, 1), "%")
    ) %>%
      layout(
        xaxis = list(title = "Número de Estudiantes"),
        yaxis = list(title = ""),
        margin = list(l = 200),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Gráfico: Top 10 Áreas de Conocimiento
  output$cont_plot_areas <- renderPlotly({
    datos_cont <- continuidad_filtrada()
    datos <- datos_cont %>%
      filter(continua_es) %>%
      count(area_conocimiento, sort = TRUE) %>%
      head(10) %>%
      mutate(pct = n / sum(datos_cont$continua_es, na.rm = TRUE) * 100)
    
    if(nrow(datos) == 0) {
      return(plotly_empty() %>% 
               layout(title = list(text = "No hay datos para mostrar con los filtros seleccionados")))
    }
    
    plot_ly(datos, 
            x = ~n, 
            y = ~reorder(area_conocimiento, n),
            type = 'bar',
            orientation = 'h',
            marker = list(color = '#E67E22'),
            text = ~paste0(format(n, big.mark = "."), " (", round(pct, 1), "%)"),
            textposition = 'outside',
            hoverinfo = 'text',
            hovertext = ~paste0(area_conocimiento, "<br>",
                                format(n, big.mark = "."), " estudiantes<br>",
                                round(pct, 1), "%")
    ) %>%
      layout(
        xaxis = list(title = "Número de Estudiantes"),
        yaxis = list(title = ""),
        margin = list(l = 150),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Gráfico: Continuidad por Dependencia
  output$cont_plot_dependencia <- renderPlotly({
    datos <- continuidad_filtrada() %>%
      group_by(DEPENDENCIA_label) %>%
      summarise(
        total = n(),
        continua = sum(continua_es, na.rm = TRUE),
        tasa = (sum(continua_es, na.rm = TRUE) / n()) * 100,
        .groups = "drop"
      ) %>%
      arrange(desc(tasa))
    
    if(nrow(datos) == 0) {
      return(plotly_empty() %>% 
               layout(title = list(text = "No hay datos para mostrar con los filtros seleccionados")))
    }
    
    plot_ly(datos, 
            x = ~tasa, 
            y = ~reorder(DEPENDENCIA_label, tasa),
            type = 'bar',
            orientation = 'h',
            marker = list(color = '#34536A'),
            text = ~paste0(round(tasa, 1), "%"),
            textposition = 'outside',
            hoverinfo = 'text',
            hovertext = ~paste0(DEPENDENCIA_label, "<br>",
                                "Tasa: ", round(tasa, 1), "%<br>",
                                "Continúan: ", format(continua, big.mark = "."), " / ", format(total, big.mark = "."))
    ) %>%
      layout(
        xaxis = list(title = "Tasa de Continuidad (%)"),
        yaxis = list(title = ""),
        margin = list(l = 200),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Gráfico: Continuidad Urbano vs Rural
  output$cont_plot_ruralidad <- renderPlotly({
    datos <- continuidad_filtrada() %>%
      group_by(RURALIDAD_label) %>%
      summarise(
        total = n(),
        continua = sum(continua_es, na.rm = TRUE),
        tasa = (sum(continua_es, na.rm = TRUE) / n()) * 100,
        .groups = "drop"
      ) %>%
      arrange(desc(tasa))
    
    if(nrow(datos) == 0) {
      return(plotly_empty() %>% 
               layout(title = list(text = "No hay datos para mostrar con los filtros seleccionados")))
    }
    
    plot_ly(datos, 
            x = ~tasa, 
            y = ~reorder(RURALIDAD_label, tasa),
            type = 'bar',
            orientation = 'h',
            marker = list(color = '#3C7F6D'),
            text = ~paste0(round(tasa, 1), "%"),
            textposition = 'outside',
            hoverinfo = 'text',
            hovertext = ~paste0(RURALIDAD_label, "<br>",
                                "Tasa: ", round(tasa, 1), "%<br>",
                                "Continúan: ", format(continua, big.mark = "."), " / ", format(total, big.mark = "."))
    ) %>%
      layout(
        xaxis = list(title = "Tasa de Continuidad (%)"),
        yaxis = list(title = ""),
        margin = list(l = 100),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Tabla: Resumen de continuidad
  output$cont_tabla_resumen <- DT::renderDataTable({
    # Tabla combinada por dependencia y ruralidad
    tabla_depe <- continuidad_filtrada() %>%
      group_by(DEPENDENCIA_label) %>%
      summarise(
        total = n(),
        continua = sum(continua_es, na.rm = TRUE),
        tasa_pct = round((sum(continua_es, na.rm = TRUE) / n()) * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(tasa_pct)) %>%
      rename(
        "Dependencia" = DEPENDENCIA_label,
        "Total Egresados" = total,
        "Continúan ES" = continua,
        "Tasa (%)" = tasa_pct
      )
    
    datatable(
      tabla_depe,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bt',
        buttons = list('copy','csv', list(extend='excel', title='continuidad_por_dependencia')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Tasa (%)',
        backgroundColor = styleInterval(c(48, 50, 52), c('#ffcccc', '#fff3cd', '#d4edda', '#c3e6cb'))
      )
  })

  # ================================================================
  # TAB TITULADOS TP
  # ================================================================
  tit_dep_label <- function(cod) dplyr::case_when(
    as.character(cod)=="1"~"Municipal", as.character(cod)=="2"~"Particular Subvencionado",
    as.character(cod)=="4"~"Corporación de Administración Delegada",
    as.character(cod)=="5"~"Servicio Local de Educación", TRUE~NA_character_)

  titulados_filtrados <- reactive({
    d <- titulados %>% dplyr::mutate(DEP_LABEL = tit_dep_label(COD_DEPE2))
    if (!is.null(input$tit_region) && input$tit_region != "Todas")
      d <- d %>% dplyr::filter(NOM_REG_RBD_A == input$tit_region)
    if (!is.null(input$tit_dependencia) && input$tit_dependencia != "Todas")
      d <- d %>% dplyr::filter(DEP_LABEL == input$tit_dependencia)
    if (!is.null(input$tit_especialidad) && input$tit_especialidad != "Todas")
      d <- d %>% dplyr::filter(NOM_ESPE == input$tit_especialidad)
    d
  })

  output$tit_kpi_total <- renderText({ format(nrow(titulados_filtrados()), big.mark = ".") })
  output$tit_kpi_mujeres <- renderText({
    d <- titulados_filtrados(); if (nrow(d) == 0) return("s/i")
    paste0(round(100 * sum(d$GEN_ALU == 2, na.rm = TRUE) / nrow(d), 1), "%")
  })
  output$tit_kpi_tiempo <- renderText({
    d <- titulados_filtrados() %>%
      dplyr::mutate(t = suppressWarnings(as.integer(AGNO_TITULACION) - as.integer(AGNO_ESCOLAR))) %>%
      dplyr::filter(!is.na(t), t >= 0, t <= 15)
    if (nrow(d) == 0) return("s/i")
    paste0(round(mean(d$t), 1), " años")
  })
  output$tit_kpi_tasa <- renderText({
    ba <- base_apoyo
    if (!is.null(input$tit_region) && input$tit_region != "Todas")
      ba <- ba %>% dplyr::filter(NombreRegión == input$tit_region)
    egr <- sum(ba$egre_2023, na.rm = TRUE); tit <- sum(ba$titu_2024_egre_2023, na.rm = TRUE)
    if (egr == 0) return("s/i")
    paste0(round(100 * tit / egr, 1), "%")
  })

  output$tit_plot_especialidad <- renderPlotly({
    d <- titulados_filtrados() %>% dplyr::filter(!is.na(NOM_ESPE)) %>%
      dplyr::count(NOM_ESPE, sort = TRUE) %>% head(15) %>% dplyr::arrange(n)
    if (nrow(d) == 0) return(plotly_empty())
    d$NOM_ESPE <- factor(d$NOM_ESPE, levels = d$NOM_ESPE)
    apply_plotly_theme(plot_ly(d, x = ~n, y = ~NOM_ESPE, type = "bar", orientation = "h",
      marker = list(color = "#34536A"), text = ~format(n, big.mark="."), textposition = "outside") %>%
      layout(xaxis = list(title = "Titulados"), yaxis = list(title = ""), margin = list(l = 10)))
  })
  output$tit_plot_rubro <- renderPlotly({
    col <- intersect(c("GLOSA_RUBRO"), names(titulados_filtrados()))
    if (length(col) == 0) return(plotly_empty() %>% layout(title = list(text = "Sin datos de rubro")))
    d <- titulados_filtrados() %>% dplyr::filter(!is.na(GLOSA_RUBRO), GLOSA_RUBRO != "") %>%
      dplyr::count(GLOSA_RUBRO, sort = TRUE) %>% head(12) %>% dplyr::arrange(n)
    if (nrow(d) == 0) return(plotly_empty() %>% layout(title = list(text = "Sin datos de rubro")))
    d$GLOSA_RUBRO <- factor(d$GLOSA_RUBRO, levels = d$GLOSA_RUBRO)
    apply_plotly_theme(plot_ly(d, x = ~n, y = ~GLOSA_RUBRO, type = "bar", orientation = "h",
      marker = list(color = "#6E5F80"), text = ~format(n, big.mark="."), textposition = "outside") %>%
      layout(xaxis = list(title = "Titulados"), yaxis = list(title = ""), margin = list(l = 10)))
  })
  output$tit_plot_region <- renderPlotly({
    d <- titulados_filtrados() %>% dplyr::filter(!is.na(NOM_REG_RBD_A)) %>%
      dplyr::count(NOM_REG_RBD_A, sort = TRUE) %>% dplyr::arrange(n)
    if (nrow(d) == 0) return(plotly_empty())
    d$NOM_REG_RBD_A <- factor(d$NOM_REG_RBD_A, levels = d$NOM_REG_RBD_A)
    apply_plotly_theme(plot_ly(d, x = ~n, y = ~NOM_REG_RBD_A, type = "bar", orientation = "h",
      marker = list(color = "#3C7F6D"), text = ~format(n, big.mark="."), textposition = "outside") %>%
      layout(xaxis = list(title = "Titulados"), yaxis = list(title = ""), margin = list(l = 10)))
  })
  output$tit_plot_genero <- renderPlotly({
    d <- titulados_filtrados() %>% dplyr::filter(!is.na(DEP_LABEL), GEN_ALU %in% c(1, 2)) %>%
      dplyr::mutate(Género = ifelse(GEN_ALU == 1, "Hombres", "Mujeres")) %>%
      dplyr::count(DEP_LABEL, Género)
    if (nrow(d) == 0) return(plotly_empty())
    apply_plotly_theme(plot_ly(d, x = ~n, y = ~DEP_LABEL, color = ~Género, type = "bar", orientation = "h",
      colors = c("Hombres" = "#34536A", "Mujeres" = "#B35A5A")) %>%
      layout(barmode = "stack", xaxis = list(title = "Titulados"), yaxis = list(title = ""),
             margin = list(l = 10), legend = list(orientation = "h")))
  })

  # ================================================================
  # TAB ESTABLECIMIENTOS — Ficha experta (contexto + SIMCE + IDPS)
  # ================================================================

  # Diccionarios y helpers
  ee_ind_lab <- c("1" = "Autoestima Académica y Motivación Escolar",
                  "2" = "Clima de Convivencia Escolar",
                  "3" = "Participación y Formación Ciudadana",
                  "4" = "Hábitos de Vida Saludable")
  ee_ind_desc <- c(
    "1" = "Percepción de los estudiantes sobre sus capacidades y sus ganas de aprender.",
    "2" = "Ambiente de respeto, organizado y seguro para el aprendizaje.",
    "3" = "Participación, vida democrática y sentido de pertenencia al establecimiento.",
    "4" = "Hábitos de vida activa, alimentación y autocuidado.")
  ee_subdim_lab <- c("11"="Autovaloración académica","12"="Motivación escolar",
                     "21"="Ambiente de respeto","22"="Ambiente organizado","23"="Ambiente seguro",
                     "31"="Participación","32"="Vida democrática","33"="Sentido de pertenencia",
                     "41"="Vida activa","42"="Hábitos alimenticios","43"="Autocuidado")
  ee_num <- function(x) suppressWarnings(as.numeric(x))
  ee_band_idps <- function(v) ifelse(is.na(v), "#BBBBBB", ifelse(v < 60, "#C0392B", ifelse(v < 75, "#D4A017", "#1E8449")))
  # Tendencia para SIMCE (positivo = mejor). sig: -1 baja signif., 0 sin cambio signif., 1 sube signif.
  ee_trend_html <- function(dif, sig, label) {
    if (is.na(dif)) return(paste0("<span style='color:#999'>", label, ": s/i</span>"))
    sig <- ifelse(is.na(sig), 0, sig)
    arrow <- if (dif > 0) "▲" else if (dif < 0) "▼" else "▬"
    col <- if (sig > 0) "#1E8449" else if (sig < 0) "#C0392B" else "#7F8C8D"
    nota <- if (sig != 0) " <i>(signif.)</i>" else ""
    sprintf("<span style='color:%s'>%s %+d pts%s</span> <span style='color:#777;font-size:11px'>%s</span>",
            col, arrow, round(dif), nota, label)
  }

  # Selector de la ficha: inicialmente todos los EE; al buscar se restringe a los
  # resultados de "Filtros de Búsqueda" (una sola búsqueda alimenta tabla + ficha)
  observe({
    bo <- base_apoyo %>% dplyr::mutate(rbd_n = suppressWarnings(as.integer(rbd))) %>% dplyr::arrange(rbd_n)
    ch <- setNames(as.character(bo$rbd), paste0(bo$rbd, " — ", bo$Nombre))
    updateSelectizeInput(session, "ee_rbd", choices = ch, server = TRUE,
                         selected = as.character(bo$rbd[1]))
  })

  # Al ejecutar la búsqueda, la ficha pasa a listar SOLO los establecimientos filtrados
  observeEvent(resultado_busqueda(), {
    dat <- resultado_busqueda()
    if (is.null(dat) || nrow(dat) == 0) return(invisible())
    info <- base_apoyo %>%
      dplyr::select(rbd, Nombre) %>%
      dplyr::filter(as.character(rbd) %in% as.character(dat$rbd))
    info <- info[order(suppressWarnings(as.integer(info$rbd))), ]
    ch <- setNames(as.character(info$rbd), paste0(info$rbd, " — ", info$Nombre))
    updateSelectizeInput(session, "ee_rbd", choices = ch, server = TRUE,
                         selected = as.character(info$rbd[1]))
  })

  ee_dat <- reactive({
    req(input$ee_rbd)
    base_apoyo %>% dplyr::filter(as.character(rbd) == input$ee_rbd) %>% dplyr::slice(1)
  })

  output$ee_ficha <- renderUI({
    d <- ee_dat(); req(nrow(d) == 1)
    dep <- mapeo_dependencias$nom_depe[match(as.character(d$cod_depe2), mapeo_dependencias$cod_depe2)]
    rural <- if (isTRUE(ee_num(d$RuralidadRBD) == 1)) "Rural" else if (isTRUE(ee_num(d$RuralidadRBD) == 0)) "Urbano" else "s/i"
    fmt <- function(x) if (is.na(ee_num(x))) "s/i" else formatC(round(ee_num(x)), big.mark = ".", format = "d")
    ive <- ee_num(d$IVE)
    ive_lab <- if (is.na(ive)) "s/i" else if (ive >= 85) "Muy alta" else if (ive >= 70) "Alta" else if (ive >= 50) "Media" else "Baja"
    gse_map <- c("1"="Bajo","2"="Medio Bajo","3"="Medio","4"="Medio Alto","5"="Alto")
    gse <- gse_map[as.character(d$gse_grupo %||% d$cod_grupo)]
    tagList(
      div(class = "panel-custom",
        h4(icon("school"), " ", d$Nombre %||% "—",
           tags$small(style="color:#777;font-weight:normal", paste0("  · RBD ", d$rbd))),
        p(icon("map-marker-alt"), " ", d$NombreComuna %||% "—", " · ", d$NombreRegión %||% "—",
          tags$br(), icon("building"), " ", dep %||% "Sin información", " · ", rural),
        fluidRow(
          column(3, div(class = "metric-card",
            h5("Matrícula EMTP"), h2(fmt(d$MatriculaEMTP)),
            tags$small(style="color:#777", paste0("de ", fmt(d$MATRICULA_OFICIAL_2025), " total")))),
          column(3, div(class = "metric-card",
            h5("Vulnerabilidad (IVE)"),
            h2(if (is.na(ive)) "s/i" else paste0(round(ive), "%")),
            tags$small(style="color:#777", ive_lab))),
          column(3, div(class = "metric-card",
            h5("Grupo Socioecon."), h2(if (is.na(gse)) "s/i" else gse),
            tags$small(style="color:#777", "GSE SIMCE"))),
          column(3, div(class = "metric-card",
            h5("Docentes EMTP"), h2(fmt(d$DocentesEMTP_Total)),
            tags$small(style="color:#777",
              paste0(fmt(d$DocentesEspecialidad_Total), " de especialidad"))))
        )
      )
    )
  })

  # Enlace: al seleccionar una fila de la tabla del buscador, mostrar su ficha
  observeEvent(input$tabla_establecimientos_rows_selected, {
    dat <- tryCatch(resultado_busqueda(), error = function(e) NULL)
    idx <- input$tabla_establecimientos_rows_selected
    if (!is.null(dat) && length(idx) > 0) {
      rbd_sel <- as.character(dat$rbd[idx[length(idx)]])  # última fila clickeada
      if (!is.na(rbd_sel)) updateSelectizeInput(session, "ee_rbd", selected = rbd_sel)
    }
  }, ignoreNULL = TRUE)

  # ---- Mapa de georreferenciación del establecimiento ----
  output$ee_mapa <- renderLeaflet({
    d <- ee_dat(); req(nrow(d) == 1)
    lat <- ee_num(d$LATITUD); lon <- ee_num(d$LONGITUD)
    if (is.na(lat) || is.na(lon) || abs(lat) > 90 || abs(lon) > 180)
      return(leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(-71, -35, 4) %>%
               addControl("Sin coordenadas para este establecimiento", position = "topright"))
    dep <- mapeo_dependencias$nom_depe[match(as.character(d$cod_depe2), mapeo_dependencias$cod_depe2)]
    fmt <- function(x) if (is.na(ee_num(x))) "s/i" else formatC(round(ee_num(x)), big.mark = ".", format = "d")
    rural <- if (isTRUE(ee_num(d$RuralidadRBD) == 1)) "Rural" else "Urbano"
    pie <- function(v) if (is.na(ee_num(v))) "s/i" else paste0(round(ee_num(v)), "%")
    popup <- paste0(
      "<div style='font-size:12px;line-height:1.5;min-width:210px'>",
      "<b style='font-size:13px;color:#34536A'>", d$Nombre, "</b><br>",
      "<span style='color:#777'>RBD ", d$rbd, " · ", rural, "</span><hr style='margin:5px 0'>",
      "<b>Comuna:</b> ", d$NombreComuna %||% "s/i", "<br>",
      "<b>Dependencia:</b> ", dep %||% "s/i", "<br>",
      "<b>Matrícula EMTP:</b> ", fmt(d$MatriculaEMTP), " (de ", fmt(d$MATRICULA_OFICIAL_2025), ")<br>",
      "<b>Especialidades:</b> ", fmt(d$N_ESPECIALIDADES), " · <b>Docentes:</b> ", fmt(d$DocentesEMTP_Total), "<br>",
      "<b>IVE:</b> ", pie(d$IVE), " · <b>SIMCE L/M:</b> ", fmt(d$prom_lect2m_rbd), "/", fmt(d$prom_mate2m_rbd),
      "</div>")
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
      setView(lng = lon, lat = lat, zoom = 15) %>%
      addAwesomeMarkers(lng = lon, lat = lat,
        icon = awesomeIcons(icon = "graduation-cap", library = "fa", markerColor = "darkblue"),
        popup = popup, label = d$Nombre) %>%
      addCircleMarkers(lng = lon, lat = lat, radius = 22, color = "#34536A",
                       fillOpacity = 0.08, weight = 1)
  })

  # ---- Especialidades impartidas (matrícula por especialidad + % mujeres) ----
  output$ee_especialidades <- renderPlotly({
    req(input$ee_rbd)
    d <- matricula_raw %>%
      dplyr::filter(as.character(rbd) == input$ee_rbd, !is.na(nom_espe)) %>%
      dplyr::group_by(nom_espe) %>%
      dplyr::summarise(n = dplyr::n(),
                       pct_m = round(100 * sum(gen_alu == 2, na.rm = TRUE) / dplyr::n()), .groups = "drop") %>%
      dplyr::arrange(n)
    if (nrow(d) == 0)
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = list(text = "Sin especialidades registradas")))
    d$nom_espe <- factor(d$nom_espe, levels = d$nom_espe)
    p <- plot_ly(d, x = ~n, y = ~nom_espe, type = "bar", orientation = "h",
                 marker = list(color = "#34536A"),
                 text = ~paste0(n, " (", pct_m, "% M)"), textposition = "outside",
                 hovertext = ~paste0(nom_espe, "<br>", n, " estudiantes<br>", pct_m, "% mujeres"),
                 hoverinfo = "text") %>%
      layout(xaxis = list(title = "Matrícula EMTP"), yaxis = list(title = ""),
             margin = list(l = 10))
    apply_plotly_theme(p)
  })

  # ---- Asistencia anual del establecimiento (categoría) ----
  output$ee_asis_prom <- renderText({
    req(input$ee_rbd)
    d <- matricula_raw %>% dplyr::filter(as.character(rbd) == input$ee_rbd)
    m <- mean(suppressWarnings(as.numeric(d$tasa_asis_anual)), na.rm = TRUE)
    if (is.nan(m) || is.na(m)) "" else paste0("Promedio: ", gsub("\\.", ",", sprintf("%.1f", m)), "%")
  })

  output$ee_asistencia <- renderPlotly({
    req(input$ee_rbd)
    lab <- c("1"="Crítica (<50%)","2"="Grave (50-84%)","3"="Reiterada (85-89%)","4"="Esperada (>=90%)")
    col <- c("Crítica (<50%)"="#963A3A","Grave (50-84%)"="#C0392B",
             "Reiterada (85-89%)"="#D4A017","Esperada (>=90%)"="#1E8449")
    d <- matricula_raw %>%
      dplyr::filter(as.character(rbd) == input$ee_rbd, !is.na(categoria_asis_anual)) %>%
      dplyr::mutate(Cat = lab[as.character(categoria_asis_anual)]) %>%
      dplyr::filter(!is.na(Cat)) %>% dplyr::count(Cat)
    if (nrow(d) == 0)
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = list(text = "Sin datos de asistencia")))
    d <- d %>% dplyr::mutate(pct = round(100 * n / sum(n), 1),
                             Cat = factor(Cat, levels = rev(c("Crítica (<50%)","Grave (50-84%)","Reiterada (85-89%)","Esperada (>=90%)"))))
    p <- plot_ly(d, x = ~pct, y = ~Cat, type = "bar", orientation = "h",
                 marker = list(color = col[as.character(d$Cat)]),
                 text = ~paste0(pct, "%"), textposition = "outside",
                 hovertext = ~paste0(Cat, "<br>", n, " estudiantes (", pct, "%)"), hoverinfo = "text") %>%
      layout(xaxis = list(title = "% de estudiantes", range = c(0, 100), ticksuffix = "%"),
             yaxis = list(title = ""), margin = list(l = 10))
    apply_plotly_theme(p)
  })

  # ---- SIMCE: distribución por Estándar de Aprendizaje (100% apilado) ----
  output$ee_simce_dist <- renderPlotly({
    d <- ee_dat(); req(nrow(d) == 1)
    df <- data.frame(
      Prueba = factor(c("Matemática", "Lectura"), levels = c("Matemática", "Lectura")),
      Insuficiente = c(ee_num(d$palu_eda_ins_mate2m_rbd), ee_num(d$palu_eda_ins_lect2m_rbd)),
      Elemental    = c(ee_num(d$palu_eda_ele_mate2m_rbd), ee_num(d$palu_eda_ele_lect2m_rbd)),
      Adecuado     = c(ee_num(d$palu_eda_ade_mate2m_rbd), ee_num(d$palu_eda_ade_lect2m_rbd))
    )
    if (all(is.na(c(df$Insuficiente, df$Elemental, df$Adecuado))))
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = list(text = "Sin datos SIMCE para este establecimiento")))
    lbl <- function(v) ifelse(is.na(v) | v == 0, "", paste0(round(v), "%"))
    p <- plot_ly(df, y = ~Prueba, x = ~Insuficiente, type = "bar", orientation = "h",
                 name = "Insuficiente", marker = list(color = "#C0392B"),
                 text = ~lbl(Insuficiente), textposition = "inside",
                 insidetextfont = list(color = "white"), hoverinfo = "name+x") %>%
      add_trace(x = ~Elemental, name = "Elemental", marker = list(color = "#D4A017"),
                text = ~lbl(Elemental)) %>%
      add_trace(x = ~Adecuado, name = "Adecuado", marker = list(color = "#1E8449"),
                text = ~lbl(Adecuado)) %>%
      layout(barmode = "stack",
             xaxis = list(title = "% de estudiantes", range = c(0, 100), ticksuffix = "%"),
             yaxis = list(title = ""),
             legend = list(orientation = "h", x = 0, y = 1.15),
             margin = list(l = 80, t = 30))
    apply_plotly_theme(p) %>% layout(legend = list(orientation = "h", x = 0, y = 1.18))
  })

  # ---- SIMCE: puntaje + tendencias (año anterior y mismo GSE) ----
  output$ee_simce_cards <- renderUI({
    d <- ee_dat(); req(nrow(d) == 1)
    card <- function(titulo, prom, dif, sig, difg, sigg, nalu) {
      prom <- ee_num(prom); nalu <- ee_num(nalu)
      div(class = "metric-card", style = "margin-bottom:10px;",
        h5(titulo, if (!is.na(nalu)) tags$small(style="color:#999;font-weight:normal;font-size:12px",
                                                paste0(" · ", round(nalu), " evaluados"))),
        h2(if (is.na(prom)) "s/i" else round(prom),
           tags$small(style="color:#777;font-size:13px", " pts promedio")),
        div(style = "font-size:13px;line-height:1.7;",
          HTML(ee_trend_html(ee_num(dif), ee_num(sig), "vs. medición anterior")), tags$br(),
          HTML(ee_trend_html(ee_num(difg), ee_num(sigg), "vs. mismo grupo socioecon."))))
    }
    tagList(
      card("Lectura", d$prom_lect2m_rbd, d$dif_lect2m_rbd, d$sigdif_lect2m_rbd,
           d$difgru_lect2m_rbd, d$siggru_lect2m_rbd, d$nalu_lect2m_rbd),
      card("Matemática", d$prom_mate2m_rbd, d$dif_mate2m_rbd, d$sigdif_mate2m_rbd,
           d$difgru_mate2m_rbd, d$siggru_mate2m_rbd, d$nalu_mate2m_rbd)
    )
  })

  # ---- IDPS: 4 dimensiones (promedio de subdimensiones) con bandas ----
  ee_idps_dat <- reactive({
    req(input$ee_rbd)
    idps_dimensiones %>%
      dplyr::filter(as.character(rbd) == input$ee_rbd) %>%
      dplyr::mutate(Indicador = ee_ind_lab[as.character(id_indicador)],
                    prom = ee_num(prom), dif = ee_num(dif), sigdif = ee_num(sigdif)) %>%
      dplyr::filter(!is.na(Indicador))
  })

  output$ee_idps_plot <- renderPlotly({
    d <- ee_idps_dat()
    if (nrow(d) == 0 || all(is.na(d$prom)))
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = list(text = "Sin datos IDPS para este establecimiento")))
    g <- d %>% dplyr::group_by(Indicador) %>%
      dplyr::summarise(prom = round(mean(prom, na.rm = TRUE)), .groups = "drop") %>%
      dplyr::arrange(prom)
    g$Indicador <- factor(g$Indicador, levels = g$Indicador)
    p <- plot_ly(g, x = ~prom, y = ~Indicador, type = "bar", orientation = "h",
                 marker = list(color = ee_band_idps(g$prom)),
                 text = ~prom, textposition = "outside", hoverinfo = "x+y") %>%
      layout(xaxis = list(title = "Puntaje (0–100)", range = c(0, 110)),
             yaxis = list(title = ""), margin = list(l = 10),
             shapes = list(
               list(type = "line", x0 = 60, x1 = 60, y0 = -0.5, y1 = 3.5,
                    line = list(color = "#D4A017", dash = "dot", width = 1)),
               list(type = "line", x0 = 75, x1 = 75, y0 = -0.5, y1 = 3.5,
                    line = list(color = "#1E8449", dash = "dot", width = 1))))
    apply_plotly_theme(p)
  })

  output$ee_idps_interp <- renderUI({
    d <- ee_idps_dat()
    if (nrow(d) == 0 || all(is.na(d$prom)))
      return(div(class = "text-muted", "Sin datos IDPS para este establecimiento."))
    g <- d %>% dplyr::group_by(id_indicador) %>%
      dplyr::summarise(prom = round(mean(prom, na.rm = TRUE)),
                       dif = mean(dif, na.rm = TRUE),
                       n_sig = sum(sigdif != 0, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(id_indicador)
    filas <- lapply(seq_len(nrow(g)), function(i) {
      ind <- as.character(g$id_indicador[i]); v <- g$prom[i]
      col <- ee_band_idps(v)
      nivel <- if (is.na(v)) "s/i" else if (v < 60) "bajo" else if (v < 75) "medio" else "alto"
      tend <- if (is.na(g$dif[i])) "" else {
        ar <- if (g$dif[i] > 0.5) "▲" else if (g$dif[i] < -0.5) "▼" else "▬"
        cc <- if (g$dif[i] > 0.5) "#1E8449" else if (g$dif[i] < -0.5) "#C0392B" else "#7F8C8D"
        sprintf("<span style='color:%s'>%s%s</span>", cc, ar,
                if (g$n_sig[i] > 0) " signif." else "")
      }
      div(style = "margin-bottom:9px;padding-bottom:7px;border-bottom:1px solid #eee;",
        tags$span(style = sprintf("display:inline-block;min-width:34px;font-weight:700;color:%s", col),
                  if (is.na(v)) "s/i" else v),
        tags$b(ee_ind_lab[ind]),
        HTML(paste0(" &nbsp;<span style='font-size:11px;color:#777'>(", nivel, ") ", tend, "</span>")),
        div(style = "font-size:11px;color:#888;margin-left:34px;", ee_ind_desc[ind]))
    })
    tagList(div(style = "font-size:13px;", filas))
  })

  # ================================================================
  # CHATBOT RAG FLOTANTE — delega en R/chatbot_rag.R
  # ================================================================
  chatbot_server(
    input, output, session,
    matricula   = matricula_raw,
    docentes    = docentes_raw,
    egresados   = egresados_2024,
    continuidad = continuidad_es,
    titulados   = titulados,
    base_apoyo  = base_apoyo
  )

}

# Iniciar la aplicación
shinyApp(ui = ui, server = server)
