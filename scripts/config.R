# =============================================================================
# CONFIG — VISUALIZADOR EMTP v2
# =============================================================================
# Para actualizar datos de un nuevo año:
#   1. Ajustar ANIO_* y los paths de RUTA_*
#   2. Correr: Rscript scripts/preparar_datos.R
#   3. Verificar KPIs en local: shiny::runApp()
#   4. rsconnect::deployApp()
# =============================================================================

DATOS_BRUTOS <- "/Users/andreslazcano/00.ProyectosShiny/emtp_publica/datos brutos"

# --- Años activos -----------------------------------------------------------
ANIO_MATRICULA      <- 2025
ANIO_DOCENTES       <- 2025
ANIO_EGRESADOS      <- 2025   # matrícula vigente
ANIO_EGRESADOS_CONT <- 2024   # egresados usados para continuidad ES
ANIO_IVE            <- 2026
ANIO_SIMCE          <- 2025
ANIO_IDPS           <- 2025
ANIO_TITULADOS      <- 2024

# --- Rutas a archivos fuente ------------------------------------------------
# Se usa list.files() para evitar problemas con tildes en nombres de archivos

.buscar_archivo <- function(carpeta, patron) {
  archivos <- list.files(file.path(DATOS_BRUTOS, carpeta),
                         pattern = patron, ignore.case = TRUE, full.names = TRUE)
  if (length(archivos) == 0) stop("No se encontró archivo con patrón '", patron,
                                  "' en: ", file.path(DATOS_BRUTOS, carpeta))
  archivos[1]
}

RUTA_MATRICULA      <- .buscar_archivo("Matricula-por-estudiante-2025", "2025.*WEB\\.CSV$")
RUTA_MATRICULA_2024 <- .buscar_archivo("Matricula-por-estudiante-2024", "2024.*WEB\\.CSV$")
RUTA_ASISTENCIA     <- .buscar_archivo("Asistencia-anual-2025",          "ASISTENCIA.*ANUAL.*PUBL.*2025.*csv$")
RUTA_DIRECTORIO <- .buscar_archivo("Directorio-Oficial-EE-2025",   "Directorio.*WEB\\.csv$")
RUTA_DOCENTES   <- .buscar_archivo("Directorio-Docentes-2025",     "Docentes.*PUBL\\.csv$")
RUTA_EGRESADOS      <- .buscar_archivo("Egresados_EM_2025",        "2025.*PUBL\\.csv$")
RUTA_EGRESADOS_CONT <- .buscar_archivo("Egresados-EM-2024",        "2024.*PUBL\\.csv$")
RUTA_EGRESADOS_2023 <- .buscar_archivo("Egresados-EM-2023",        "2023.*PUBL\\.csv$")
RUTA_MAT_ES     <- .buscar_archivo("Matricula-Ed-Superior-2025",   "Ed_Superior.*MRUN\\.csv$")
RUTA_IVE        <- file.path(DATOS_BRUTOS, "IVE_2026.xlsx")
RUTA_SIMCE      <- file.path(DATOS_BRUTOS, "2M", "Archivos CSV (Planos)",
                              "simce2m2025_rbd_preliminar.csv")
RUTA_IDPS       <- file.path(DATOS_BRUTOS, "2M-1", "Archivos CSV (Planos)",
                              "idps2M2025_rbd_preliminar.csv")
RUTA_IDPS_DIM   <- file.path(DATOS_BRUTOS, "2M-1", "Archivos CSV (Planos)",
                              "idps2M2025_rbd_dim_preliminar.csv")
RUTA_IDPS_NIV   <- file.path(DATOS_BRUTOS, "2M-1", "Archivos CSV (Planos)",
                              "idps2m2025_rbd_subdim_niveles_preliminar.csv")
RUTA_TITULADOS  <- .buscar_archivo("Practicantes-y-Titulados-Tecnico-Profesional-2024",
                                   "Titulados.*MRUN\\.csv$")
RUTA_COMUNAS    <- "data/geographic/comunas_simplificado.rds"

# --- Filtro EMTP canónico ---------------------------------------------------
# COD_ENSE: jóvenes termina en 10 (410,510..810), adultos en 63 (463,563..863)
EMTP_ENSE_MIN <- 410
EMTP_ENSE_MAX <- 863

# --- Codificaciones ---------------------------------------------------------
ENC_MATRICULA  <- "latin1"
ENC_DIRECTORIO <- "latin1"
ENC_DOCENTES   <- "UTF-8"
ENC_EGRESADOS  <- "UTF-8"
ENC_MAT_ES     <- "UTF-8"
ENC_SIMCE      <- "latin1"
ENC_IDPS       <- "UTF-8"
ENC_TITULADOS  <- "UTF-8"

# --- Validaciones (actualizar con cada nuevo año) ---------------------------
VAL_MATRICULA_EMTP_MIN  <- 150000   # solo 3°-4° jóvenes + adultos
VAL_MATRICULA_EMTP_MAX  <- 250000
VAL_RBD_EMTP_MIN        <- 800
VAL_RBD_EMTP_MAX        <- 1100
VAL_DOCENTES_CARGOS_MIN <- 15000
VAL_DOCENTES_CARGOS_MAX <- 25000
VAL_EGRESADOS_EMTP_MIN  <- 50000
VAL_EGRESADOS_EMTP_MAX  <- 100000
