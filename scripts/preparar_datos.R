#!/usr/bin/env Rscript
# =============================================================================
# PREPARAR DATOS — VISUALIZADOR EMTP
# =============================================================================
# Lee CSVs/XLSX crudos desde "datos brutos/" y genera data/app/*.rds en el
# ESQUEMA que espera la app original (app.R):
#   matricula.rds       -> matricula_raw          (columnas en minúsculas)
#   base_apoyo.rds      -> base_apoyo             (reconstruido desde brutos)
#   docentes.rds        -> docentes_raw
#   docentes_idich.rds  -> docentes_idich
#   docentes_long.rds   -> docentes_especialidad_long
#   egresados.rds       -> egresados_2024         (esquema 22 cols con labels)
#   continuidad.rds     -> continuidad_es         (egresados2024 × mat_es2025)
#   indicadores_continuidad.rds
#   titulados.rds       -> titulados (ESTADO_PRACTICA==1)
#   idps_dimensiones.rds-> idps_dim (para tab Establecimientos)
#   comunas.rds         -> comunas (sf)
#   meta.rds            -> diccionarios, choices, colores
#
# Ejecutar desde la raíz del proyecto:  Rscript scripts/preparar_datos.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(colorspace)
  library(sf)
  library(openxlsx)
})

Sys.setlocale("LC_ALL", "en_US.UTF-8")
source("scripts/config.R")

t0 <- proc.time()
cat("=============================================================================\n")
cat(sprintf("PREPARAR DATOS EMTP | Matrícula %d | Docentes %d | Egresados %d | mat_ES %d\n",
            ANIO_MATRICULA, ANIO_DOCENTES, ANIO_EGRESADOS_CONT, ANIO_MATRICULA))
cat("=============================================================================\n\n")

dir.create("data/app", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# UTILIDADES
# =============================================================================
ok <- function(msg, n = NULL) {
  if (!is.null(n)) cat(sprintf("  ✓ %s: %s\n", msg, format(n, big.mark = ".")))
  else            cat(sprintf("  ✓ %s\n", msg))
}
validar <- function(val, min_v, max_v, label) {
  if (val < min_v || val > max_v)
    warning(sprintf("⚠  %s = %s fuera del rango esperado [%s, %s]",
                    label, format(val, big.mark="."),
                    format(min_v, big.mark="."), format(max_v, big.mark=".")), call. = FALSE)
  else ok(label, val)
}
guardar <- function(obj, nombre) {
  ruta <- file.path("data/app", nombre)
  saveRDS(obj, ruta)
  cat(sprintf("  → %s (%.0f KB)\n", nombre, file.info(ruta)$size / 1024))
  invisible(ruta)
}
# pipe("cat ...") evita problemas NFD/NFC en macOS con tildes en nombres de archivo
leer_csv <- function(ruta, enc = "UTF-8", delim = ";") {
  con <- pipe(paste("cat", shQuote(ruta)), open = "rb")
  on.exit(close(con), add = TRUE)
  read_delim(con, delim = delim, locale = locale(encoding = enc),
             show_col_types = FALSE, name_repair = "minimal")
}

# =============================================================================
# DICCIONARIOS FIJOS
# =============================================================================
mapeo_dependencias <- data.frame(
  cod_depe2 = c("1","2","4","5"),
  nom_depe  = c("Municipal","Particular Subvencionado",
                "Corporación de Administración Delegada",
                "Servicio Local de Educación Pública"),
  stringsAsFactors = FALSE
)
# Etiqueta de dependencia para egresados/continuidad (coincide con base procesada original)
dep_label <- function(cod) {
  dplyr::case_when(
    as.character(cod) == "1" ~ "Municipal",
    as.character(cod) == "2" ~ "Particular Subvencionado",
    as.character(cod) == "4" ~ "Corporación de Administración Delegada",
    as.character(cod) == "5" ~ "Servicio Local de Educación",
    TRUE ~ NA_character_
  )
}

# Nombres OFICIALES completos — ANEXO VII (ER Matrícula por alumno 2025)
dic_especialidades <- tribble(
  ~codigo, ~nombre,
  41001,"Administración",                       41002,"Contabilidad",
  41003,"Secretariado",                         41004,"Ventas",
  41005,"Administración (con mención)",
  51001,"Edificación",                          51002,"Terminaciones de Construcción",
  51003,"Montaje Industrial",                   51004,"Obras viales y de infraestructura",
  51005,"Instalaciones Sanitarias",            51006,"Refrigeración y climatización",
  51009,"Construcción (con mención)",
  52008,"Mecánica Industrial",                  52009,"Construcciones Metálicas",
  52010,"Mecánica Automotriz",                  52011,"Matricería",
  52012,"Mecánica de mantención de aeronaves",  52013,"Mecánica Industrial (con mención)",
  53014,"Electricidad",                         53015,"Electrónica",
  53016,"Telecomunicaciones",
  54018,"Explotación minera",                   54019,"Metalurgia Extractiva",
  54020,"Asistencia en geología",
  55022,"Gráfica",                              55023,"Dibujo Técnico",
  56025,"Operación de planta química",          56026,"Laboratorio químico",
  56027,"Química Industrial (con mención)",
  57028,"Tejido",                               57029,"Textil",
  57030,"Vestuario y Confección Textil",        57031,"Productos del cuero",
  58033,"Conectividad y Redes",                 58034,"Programación",
  58035,"Telecomunicaciones",
  61001,"Elaboración Industrial de Alimentos",  61002,"Servicio de Alimentación Colectiva",
  61003,"Gastronomía (con mención)",
  62004,"Atención de párvulos",                 62005,"Atención de adultos mayores",
  62006,"Atención de Enfermería",               62007,"Atención Social y Recreativa",
  62008,"Atención de Enfermería (con mención)",
  63009,"Servicio de turismo",                  63010,"Servicios Hoteleros",
  63011,"Servicio de hotelería",
  64001,"Atención de párvulos",                 64008,"Atención de Enfermería (con mención)",
  71001,"Forestal",                             71002,"Procesamiento de la madera",
  71003,"Productos de la madera",               71004,"Celulosa y Papel",
  71005,"Muebles y Terminaciones de la Madera",
  72006,"Agropecuaria",                         72007,"Agropecuaria (con mención)",
  81001,"Naves mercantes y especiales",         81002,"Pesquería",
  81003,"Acuicultura",                          81004,"Operación portuaria",
  81005,"Tripulación naves mercantes y especiales"
  # Nota: las especialidades artísticas (sectores 910/920/930) pertenecen a
  # Educación Media Artística (EMA), NO a EMTP, y quedan fuera (COD_ENSE 910/963
  # están fuera del rango EMTP 410–863).
) %>% mutate(codigo = as.integer(codigo))

# Sectores Económicos — ANEXO VII (COD_SEC = 3 primeros dígitos de COD_ESPE)
dic_sectores <- tribble(
  ~cod_sec, ~sector,
  0,  "Ciclo General / Sin información",
  410,"Administración y Comercio",  510,"Construcción",
  520,"Metalmecánico",              530,"Electricidad",
  540,"Minero",                     550,"Gráfica",
  560,"Químico",                    570,"Confección",
  580,"Tecnología y Telecomunicaciones", 610,"Alimentación",
  620,"Programas y Proyectos Sociales",  630,"Hotelería y Turismo",
  640,"Salud y Educación",          710,"Maderero",
  720,"Agropecuario",               810,"Marítimo"
  # Sectores 910/920/930 (artísticos) pertenecen a EMA, no a EMTP → excluidos.
) %>% mutate(cod_sec = as.integer(cod_sec))

asignar_rft <- function(deprov) {
  case_when(
    deprov %in% c("ARICA","IQUIQUE")                                        ~ "Norte 1",
    deprov %in% c("ANTOFAGASTA - TOCOPILLA","EL LOA","COPIAPÓ","HUASCO")    ~ "Norte 2",
    deprov %in% c("ELQUI","LIMARÍ","CHOAPA","QUILLOTA","SAN FELIPE",
                  "VALPARAÍSO","SAN ANTONIO")                               ~ "Centro Norte",
    deprov %in% c("SANTIAGO CENTRO","SANTIAGO NORTE","SANTIAGO PONIENTE",
                  "SANTIAGO ORIENTE")                                       ~ "Metropolitana Norte",
    deprov %in% c("CORDILLERA","SANTIAGO SUR","TALAGANTE")                  ~ "Metropolitana Sur",
    deprov %in% c("CACHAPOAL","COLCHAGUA","CARDENAL CARO","CURICÓ",
                  "TALCA","LINARES","CAUQUENES")                            ~ "Centro Sur",
    deprov %in% c("ÑUBLE","BIOBÍO","CONCEPCIÓN","ARAUCO")                   ~ "Sur 1",
    deprov %in% c("MALLECO","CAUTÍN NORTE","CAUTÍN SUR","VALDIVIA","RANCO") ~ "Sur 2",
    deprov %in% c("OSORNO","LLANQUIHUE","CHILOÉ","COYHAIQUE","MAGALLANES")  ~ "Sur 3",
    TRUE ~ "Sin Asignar"
  )
}

# =============================================================================
# 1. MATRÍCULA EMTP  (esquema en minúsculas, filtro 3°-4° global)
# =============================================================================
cat(">>> [1/9] MATRÍCULA EMTP <<<\n")
mat_raw <- leer_csv(RUTA_MATRICULA, enc = ENC_MATRICULA) %>% rename_with(tolower)

# tipo_ensenanza_emtp por RBD (calculado sobre TODO EMTP, antes del filtro de grado)
tipo_ense_rbd <- mat_raw %>%
  mutate(rbd = as.character(rbd), cod_ense = as.numeric(cod_ense)) %>%
  filter(!is.na(cod_ense), cod_ense >= EMTP_ENSE_MIN, cod_ense <= EMTP_ENSE_MAX) %>%
  group_by(rbd) %>%
  summarise(tiene_jovenes = any(cod_ense %% 100 == 10, na.rm = TRUE),
            tiene_adultos = any(cod_ense %% 100 == 63, na.rm = TRUE), .groups = "drop") %>%
  mutate(tipo_ensenanza_emtp = case_when(
    tiene_jovenes & tiene_adultos ~ "Ambos",
    tiene_jovenes ~ "Jóvenes",
    tiene_adultos ~ "Adultos",
    TRUE ~ NA_character_)) %>%
  select(rbd, tipo_ensenanza_emtp)

matricula_raw <- mat_raw %>%
  mutate(
    rbd          = as.character(rbd),
    mrun         = as.character(mrun),
    cod_depe2    = as.character(cod_depe2),
    cod_com_rbd  = as.character(cod_com_rbd),
    cod_ense     = as.numeric(cod_ense),
    cod_grado    = as.numeric(cod_grado),
    gen_alu      = as.numeric(gen_alu),
    cod_espe     = as.numeric(cod_espe),
    estado_estab = as.integer(estado_estab)
  ) %>%
  filter(
    estado_estab == 1,
    !is.na(cod_ense), cod_ense >= EMTP_ENSE_MIN, cod_ense <= EMTP_ENSE_MAX,
    # Jóvenes (termina en 10): 3° y 4° medio | Adultos (termina en 63): niveles 1 a 4
    (cod_ense %% 100 == 10 & cod_grado %in% c(3, 4)) |
    (cod_ense %% 100 == 63 & cod_grado %in% c(1, 2, 3, 4))
  ) %>%
  left_join(tipo_ense_rbd, by = "rbd") %>%
  mutate(
    nom_espe     = dic_especialidades$nombre[match(cod_espe, dic_especialidades$codigo)],
    nom_sector   = dic_sectores$sector[match(as.integer(cod_espe %/% 100L), dic_sectores$cod_sec)],
    rft          = asignar_rft(nom_deprov_rbd),
    RuralidadRBD = as.numeric(rural_rbd),
    nombre_sost  = NA_character_,         # no disponible en datos brutos
    datos_complementarios = FALSE,
    # Columnas demográficas (no están en la base WEB pública) — NA para no romper minutas
    cod_etnia_alu        = NA_character_,
    cod_nac_alu          = NA_character_,
    pais_origen_alu      = NA_character_,
    emb_alu              = NA,
    int_alu              = NA
  )

# Asistencia anual 2025 (cruce por MRUN+RBD) — archivo grande, se leen solo 3 columnas
cat("  Cargando asistencia anual 2025 (archivo grande)...\n")
asis_lookup <- {
  con <- pipe(paste("cat", shQuote(RUTA_ASISTENCIA)), open = "rb")
  on.exit(close(con), add = TRUE)
  readr::read_delim(con, delim = ";", locale = readr::locale(encoding = "latin1"),
                    show_col_types = FALSE, name_repair = "minimal",
                    col_select = c("MRUN","RBD","CATEGORIA_ASIS_ANUAL","TASA_ASISTENCIA_ANUAL")) %>%
    transmute(mrun = as.character(MRUN), rbd = as.character(RBD),
              categoria_asis_anual = suppressWarnings(as.integer(CATEGORIA_ASIS_ANUAL)),
              # TASA viene como fracción 0-1 con coma decimal -> a porcentaje 0-100
              tasa_asis_anual      = suppressWarnings(as.numeric(gsub(",", ".", TASA_ASISTENCIA_ANUAL))) * 100) %>%
    distinct(mrun, rbd, .keep_all = TRUE)
}
matricula_raw <- matricula_raw %>% left_join(asis_lookup, by = c("rbd","mrun"))
ok("Matrícula con asistencia (cat. no nula)", sum(!is.na(matricula_raw$categoria_asis_anual)))
rm(asis_lookup); gc(verbose = FALSE)

validar(nrow(matricula_raw), VAL_MATRICULA_EMTP_MIN, VAL_MATRICULA_EMTP_MAX, "Filas matrícula EMTP")
ok("RBDs únicos", n_distinct(matricula_raw$rbd))
ok("Especialidades", n_distinct(matricula_raw$nom_espe[!is.na(matricula_raw$nom_espe)]))
guardar(matricula_raw, "matricula.rds")
rbds_emtp <- unique(matricula_raw$rbd)

# Matrícula TOTAL del establecimiento (todas las enseñanzas) — para base_apoyo / minutas
mat_total_rbd <- mat_raw %>%
  mutate(rbd = as.character(rbd)) %>%
  group_by(rbd) %>%
  summarise(MATRICULA_OFICIAL_2025 = n(), .groups = "drop")

rm(mat_raw); gc(verbose = FALSE)

# =============================================================================
# 2. DOCENTES EMTP  (port de la lógica original)
# =============================================================================
cat("\n>>> [2/9] DOCENTES <<<\n")
docentes_csv <- leer_csv(RUTA_DOCENTES, enc = ENC_DOCENTES)

normalizar_cols_docentes <- function(df) {
  nm <- names(df)
  if (!"rbd" %in% nm && "RBD" %in% nm) df <- rename(df, rbd = RBD)
  if (!"SUBSECTOR1" %in% names(df)) df$SUBSECTOR1 <- NA_integer_
  if (!"SUBSECTOR2" %in% names(df)) df$SUBSECTOR2 <- NA_integer_
  df
}

docentes_raw <- docentes_csv %>%
  normalizar_cols_docentes() %>%
  mutate(
    rbd       = as.character(rbd),
    COD_ENS_1 = suppressWarnings(as.numeric(as.character(COD_ENS_1))),
    COD_ENS_2 = suppressWarnings(as.numeric(as.character(COD_ENS_2))),
    HORAS1    = suppressWarnings(as.numeric(HORAS1)),
    HORAS2    = suppressWarnings(as.numeric(HORAS2)),
    tiene_joven  = (!is.na(COD_ENS_1) & COD_ENS_1 %% 100 == 10) |
                   (!is.na(COD_ENS_2) & COD_ENS_2 %% 100 == 10),
    tiene_adulto = (!is.na(COD_ENS_1) & COD_ENS_1 %% 100 == 63) |
                   (!is.na(COD_ENS_2) & COD_ENS_2 %% 100 == 63),
    Poblacion = case_when(
      tiene_joven & tiene_adulto ~ "Ambas",
      tiene_joven  ~ "Jóvenes",
      tiene_adulto ~ "Adultos",
      TRUE ~ NA_character_),
    COD_DEPE2 = if_else(rbd == "25824", 2L, suppressWarnings(as.integer(COD_DEPE2)))  # corrección Mineduc 2025
  ) %>%
  select(-tiene_joven, -tiene_adulto) %>%
  filter(
    (COD_ENS_1 >= EMTP_ENSE_MIN & COD_ENS_1 <= EMTP_ENSE_MAX & !is.na(HORAS1) & HORAS1 > 0) |
    (COD_ENS_2 >= EMTP_ENSE_MIN & COD_ENS_2 <= EMTP_ENSE_MAX & !is.na(HORAS2) & HORAS2 > 0)
  )

validar(nrow(docentes_raw),            VAL_DOCENTES_CARGOS_MIN, VAL_DOCENTES_CARGOS_MAX, "Cargos docentes EMTP")
ok("Docentes únicos (MRUN)", n_distinct(docentes_raw$MRUN))
ok("RBDs con docentes",      n_distinct(docentes_raw$rbd))
guardar(docentes_raw, "docentes.rds")

# --- docentes_idich (ID_ICH + SUBSECTOR normalizado) ---
docentes_idich <- docentes_raw %>%
  mutate(
    ID_ICH    = as.numeric(coalesce(COD_ENS_1, COD_ENS_2)),
    Poblacion = case_when(
      ID_ICH %% 100 == 10 ~ "Jóvenes",
      ID_ICH %% 100 == 63 ~ "Adultos",
      TRUE ~ NA_character_),
    SUBSECTOR1 = suppressWarnings(as.numeric(as.character(SUBSECTOR1))),
    SUBSECTOR2 = suppressWarnings(as.numeric(as.character(SUBSECTOR2))),
    SUBSECTOR1 = ifelse(!is.na(SUBSECTOR1) & (is.na(HORAS1) | HORAS1 <= 0), NA, SUBSECTOR1),
    SUBSECTOR2 = ifelse(!is.na(SUBSECTOR2) & (is.na(HORAS2) | HORAS2 <= 0), NA, SUBSECTOR2),
    SUBSECTOR2 = ifelse(!is.na(SUBSECTOR1) & SUBSECTOR2 == SUBSECTOR1, NA, SUBSECTOR2)
  )
guardar(docentes_idich, "docentes_idich.rds")

# --- docentes_long (por subsector, con especialidad) ---
docentes_long <- docentes_idich %>%
  filter(rbd %in% rbds_emtp) %>%
  pivot_longer(cols = c(SUBSECTOR1, SUBSECTOR2), names_to = "col_sub", values_to = "SUBSECTOR") %>%
  mutate(SUBSECTOR = as.numeric(SUBSECTOR)) %>%
  filter(!is.na(SUBSECTOR), SUBSECTOR >= 31001, SUBSECTOR <= 81004) %>%
  mutate(Especialidad_grouped = if_else(startsWith(as.character(SUBSECTOR), "3"),
                                        "Formación General", as.character(SUBSECTOR))) %>%
  distinct(MRUN, rbd, Especialidad_grouped, .keep_all = TRUE) %>%
  mutate(
    es_especialidad = Especialidad_grouped != "Formación General",
    tipo_docente    = ifelse(es_especialidad, "Módulos de Especialidad", "Formación General")
  ) %>%
  left_join(dic_especialidades, by = c("SUBSECTOR" = "codigo")) %>%
  mutate(Especialidad = case_when(
    es_especialidad & !is.na(nombre) ~ nombre,
    es_especialidad ~ as.character(SUBSECTOR),
    TRUE ~ "Formación General"))
guardar(docentes_long, "docentes_long.rds")

# Conteo de docentes de módulos de especialidad por RBD (SUBSECTOR > 40000)
doc_especialidad_rbd <- docentes_long %>%
  dplyr::filter(es_especialidad) %>%
  dplyr::distinct(MRUN, rbd) %>%
  dplyr::count(rbd, name = "DocentesEspecialidad_Total")

rm(docentes_csv); gc(verbose = FALSE)

# =============================================================================
# 3. EGRESADOS EMTP 2024  (esquema 22 cols con labels)
# =============================================================================
cat("\n>>> [3/9] EGRESADOS EMTP 2024 <<<\n")
egresados_2024 <- leer_csv(RUTA_EGRESADOS_CONT, enc = ENC_EGRESADOS, delim = ";") %>%
  rename_with(toupper) %>%
  mutate(
    RBD      = as.character(RBD),
    MRUN     = as.character(MRUN),
    COD_ENSE = suppressWarnings(as.numeric(COD_ENSE)),
    COD_GRADO= suppressWarnings(as.numeric(COD_GRADO))
  ) %>%
  filter(!is.na(COD_ENSE), COD_ENSE >= EMTP_ENSE_MIN, COD_ENSE <= EMTP_ENSE_MAX,
         !is.na(MARCA_EGRESO), MARCA_EGRESO == 1) %>%
  mutate(
    AGNO_EGRESO      = if ("AGNO" %in% names(.)) AGNO else ANIO_EGRESADOS_CONT,
    TIPO_ENSE_label  = case_when(COD_ENSE %% 100 == 10 ~ "Jóvenes",
                                 COD_ENSE %% 100 == 63 ~ "Adultos", TRUE ~ NA_character_),
    DEPENDENCIA_label = dep_label(COD_DEPE2),
    RURALIDAD_label   = case_when(RURAL_RBD == 0 ~ "Urbano", RURAL_RBD == 1 ~ "Rural", TRUE ~ NA_character_)
  )
validar(nrow(egresados_2024), VAL_EGRESADOS_EMTP_MIN, VAL_EGRESADOS_EMTP_MAX, "Egresados EMTP 2024")
ok("RBDs con egresados", n_distinct(egresados_2024$RBD))
guardar(egresados_2024, "egresados.rds")

# =============================================================================
# 4. CONTINUIDAD ES  (egresados 2024 × matrícula ES 2025)
# =============================================================================
cat("\n>>> [4/9] CONTINUIDAD ES <<<\n")

# Género desde matrícula 2024 por MRUN (los egresados no traen GEN_ALU)
cat("  Cargando género desde matrícula 2024...\n")
gen_alu_2024 <- leer_csv(RUTA_MATRICULA_2024, enc = ENC_MATRICULA) %>%
  rename_with(toupper) %>%
  transmute(MRUN = as.character(MRUN), GEN_ALU = suppressWarnings(as.integer(GEN_ALU))) %>%
  filter(!is.na(MRUN), MRUN != "NA", !is.na(GEN_ALU)) %>%
  distinct(MRUN, .keep_all = TRUE)

cat("  Cargando matrícula ES 2025 (archivo grande)...\n")
mat_es <- leer_csv(RUTA_MAT_ES, enc = ENC_MAT_ES) %>%
  rename_with(tolower) %>%
  transmute(
    MRUN              = as.character(mrun),
    gen_alu_es        = suppressWarnings(as.integer(gen_alu)),
    tipo_inst_3       = as.character(tipo_inst_3),
    modalidad         = as.character(modalidad),
    nivel_carrera_2   = if ("nivel_carrera_2" %in% names(.)) as.character(nivel_carrera_2) else NA_character_,
    area_conocimiento = as.character(area_conocimiento),
    acreditada_carr   = if ("acreditada_carr" %in% names(.)) as.character(acreditada_carr) else NA_character_,
    acreditada_inst   = if ("acreditada_inst" %in% names(.)) as.character(acreditada_inst) else NA_character_,
    forma_ingreso     = if ("forma_ingreso" %in% names(.)) as.character(forma_ingreso) else NA_character_
  ) %>%
  distinct(MRUN, .keep_all = TRUE)

continuidad_es <- egresados_2024 %>%
  transmute(
    MRUN, RBD, COD_DEPE2, DEPENDENCIA_label, RURAL_RBD, RURALIDAD_label,
    TIPO_ENSE_label, PROM_NOTAS_ALU, NOM_REG_RBD_A, NOM_COM_RBD
  ) %>%
  left_join(gen_alu_2024, by = "MRUN") %>%
  left_join(mat_es,       by = "MRUN") %>%
  mutate(
    continua_es        = !is.na(tipo_inst_3) | !is.na(area_conocimiento) | !is.na(gen_alu_es),
    forma_ingreso_label = forma_ingreso
  )

n_total <- nrow(continuidad_es)
n_cont  <- sum(continuidad_es$continua_es, na.rm = TRUE)
muj     <- continuidad_es %>% filter(GEN_ALU == 2)
hom     <- continuidad_es %>% filter(GEN_ALU == 1)
pct     <- function(a, b) if (b > 0) round(100 * a / b, 1) else NA_real_
indicadores_continuidad <- list(
  pct_continuidad          = pct(n_cont, n_total),
  pct_continuidad_mujeres  = pct(sum(muj$continua_es, na.rm = TRUE), nrow(muj)),
  pct_continuidad_hombres  = pct(sum(hom$continua_es, na.rm = TRUE), nrow(hom)),
  n_egresados              = n_total,
  n_continuan              = n_cont,
  n_mujeres                = nrow(muj),
  n_mujeres_continuan      = sum(muj$continua_es, na.rm = TRUE),
  n_hombres                = nrow(hom),
  n_hombres_continuan      = sum(hom$continua_es, na.rm = TRUE),
  anio_egresados           = ANIO_EGRESADOS_CONT,
  anio_mat_es              = ANIO_MATRICULA
)
indicadores_continuidad$brecha_genero <-
  round(abs(indicadores_continuidad$pct_continuidad_mujeres -
            indicadores_continuidad$pct_continuidad_hombres), 1)

cat(sprintf("  Tasa continuidad: %.1f%% (%s de %s)\n",
            indicadores_continuidad$pct_continuidad,
            format(n_cont, big.mark="."), format(n_total, big.mark=".")))
guardar(continuidad_es,          "continuidad.rds")
guardar(indicadores_continuidad, "indicadores_continuidad.rds")
rm(mat_es, gen_alu_2024); gc(verbose = FALSE)

# =============================================================================
# 5. BASE APOYO  (reconstruida desde directorio + IVE + SIMCE; resto = NA)
# =============================================================================
cat("\n>>> [5/9] BASE APOYO (establecimientos) <<<\n")

# 5a. Directorio EE
dir_ee <- leer_csv(RUTA_DIRECTORIO, enc = ENC_DIRECTORIO) %>%
  rename_with(toupper) %>%
  mutate(RBD = as.character(RBD)) %>%
  filter(RBD %in% rbds_emtp) %>%
  transmute(
    rbd          = RBD,
    Nombre       = NOM_RBD,
    NombreRegión = NOM_REG_RBD_A,
    Provincia    = if ("NOM_DEPROV_RBD" %in% names(.)) NOM_DEPROV_RBD else NA_character_,
    NombreComuna = NOM_COM_RBD,
    cod_depe2    = suppressWarnings(as.integer(COD_DEPE2)),
    RutSostenedor= if ("RUT_SOSTENEDOR" %in% names(.)) as.character(RUT_SOSTENEDOR) else NA_character_,
    RuralidadRBD = suppressWarnings(as.numeric(RURAL_RBD)),
    CONVENIO_PIE_2025 = if ("CONVENIO_PIE" %in% names(.)) CONVENIO_PIE else NA,
    PACE_2025         = if ("PACE" %in% names(.)) PACE else NA,
    COD_REG_RBD  = suppressWarnings(as.integer(COD_REG_RBD)),
    # Coordenadas (formato Mineduc con coma decimal) para el mapa
    LATITUD  = suppressWarnings(as.numeric(str_replace(as.character(LATITUD),  ",", "."))),
    LONGITUD = suppressWarnings(as.numeric(str_replace(as.character(LONGITUD), ",", ".")))
  ) %>%
  distinct(rbd, .keep_all = TRUE)

# 5b. IVE (xlsx con fila de encabezado variable — se busca "ID_RBD")
cat("  Cargando IVE...\n")
ive_all <- read.xlsx(RUTA_IVE, startRow = 1, colNames = FALSE)
header_row <- which(apply(ive_all, 1, function(r) any(grepl("ID_RBD", r, fixed = TRUE))))[1]
ive_cols  <- trimws(as.character(unlist(ive_all[header_row, ])))
ive_datos <- ive_all[(header_row + 1):nrow(ive_all), ]; names(ive_datos) <- ive_cols
col_ive   <- grep("PRIORITARI", names(ive_datos), ignore.case = TRUE, value = TRUE)[1]
ive <- ive_datos %>%
  transmute(rbd = as.character(ID_RBD), IVE = suppressWarnings(as.numeric(.data[[col_ive]]))) %>%
  filter(!is.na(rbd), rbd != "NA") %>% distinct(rbd, .keep_all = TRUE)
ok("RBDs con IVE", nrow(ive))

# 5c. SIMCE (todas las columnas que usa la minuta)
cat("  Cargando SIMCE...\n")
simce_cols <- c("nalu_lect2m_rbd","nalu_mate2m_rbd",
                "prom_lect2m_rbd","prom_mate2m_rbd","dif_lect2m_rbd","dif_mate2m_rbd",
                "difgru_lect2m_rbd","difgru_mate2m_rbd","sigdif_lect2m_rbd","sigdif_mate2m_rbd",
                "siggru_lect2m_rbd","siggru_mate2m_rbd",
                "palu_eda_ins_lect2m_rbd","palu_eda_ele_lect2m_rbd","palu_eda_ade_lect2m_rbd",
                "palu_eda_ins_mate2m_rbd","palu_eda_ele_mate2m_rbd","palu_eda_ade_mate2m_rbd")
simce <- leer_csv(RUTA_SIMCE, enc = ENC_SIMCE) %>%
  rename_with(tolower) %>%
  mutate(rbd = as.character(rbd)) %>%
  select(rbd, any_of(c("cod_grupo", simce_cols))) %>%
  distinct(rbd, .keep_all = TRUE) %>%
  # Conteos de estudiantes por estándar (nº alumnos × % en cada nivel) — para agregación territorial
  mutate(
    n_palu_eda_ins_lect2m_rbd = round(nalu_lect2m_rbd * palu_eda_ins_lect2m_rbd / 100),
    n_palu_eda_ele_lect2m_rbd = round(nalu_lect2m_rbd * palu_eda_ele_lect2m_rbd / 100),
    n_palu_eda_ade_lect2m_rbd = round(nalu_lect2m_rbd * palu_eda_ade_lect2m_rbd / 100),
    n_palu_eda_ins_mate2m_rbd = round(nalu_mate2m_rbd * palu_eda_ins_mate2m_rbd / 100),
    n_palu_eda_ele_mate2m_rbd = round(nalu_mate2m_rbd * palu_eda_ele_mate2m_rbd / 100),
    n_palu_eda_ade_mate2m_rbd = round(nalu_mate2m_rbd * palu_eda_ade_mate2m_rbd / 100)
  )
ok("RBDs con SIMCE", nrow(simce))

# 5c-bis. IDPS por indicador (puntaje + distribución Bajo/Medio/Alto) — para minutas y ficha
cat("  Cargando IDPS niveles...\n")
idps_ind <- tryCatch({
  niv <- leer_csv(RUTA_IDPS_NIV, enc = ENC_IDPS) %>% rename_with(tolower) %>%
    mutate(rbd = as.character(rbd)) %>%
    group_by(rbd, id_indicador) %>%
    summarise(Bajo  = mean(suppressWarnings(as.numeric(niv_bajo_por)),  na.rm = TRUE),
              Medio = mean(suppressWarnings(as.numeric(niv_medio_por)), na.rm = TRUE),
              Alto  = mean(suppressWarnings(as.numeric(niv_alto_por)),  na.rm = TRUE), .groups = "drop")
  # Puntaje promedio por indicador (desde idps_dim, ya cargado abajo como idps_dimensiones)
  prom <- leer_csv(RUTA_IDPS_DIM, enc = ENC_IDPS) %>% rename_with(tolower) %>%
    mutate(rbd = as.character(rbd)) %>%
    group_by(rbd, id_indicador) %>%
    summarise(Puntaje = mean(suppressWarnings(as.numeric(prom)), na.rm = TRUE), .groups = "drop")
  niv %>% left_join(prom, by = c("rbd","id_indicador")) %>%
    mutate(across(c(Bajo,Medio,Alto,Puntaje), ~ round(.x))) %>%
    tidyr::pivot_wider(id_cols = rbd, names_from = id_indicador,
                       values_from = c(Puntaje, Bajo, Medio, Alto),
                       names_glue = "IDPS{id_indicador}_{.value}")
}, error = function(e) { cat("  (IDPS niveles no disponible:", conditionMessage(e), ")\n"); NULL })

# 5d. Agregados de matrícula y docentes por RBD
mat_agg <- matricula_raw %>%
  group_by(rbd) %>%
  summarise(
    MatriculaEMTP        = n(),
    MatriculaMujeresEMTP = sum(gen_alu == 2, na.rm = TRUE),
    MatriculaHombresEMTP = sum(gen_alu == 1, na.rm = TRUE),
    N_ESPECIALIDADES     = n_distinct(cod_espe[!is.na(cod_espe) & cod_espe > 0]),
    matricula_3medio          = sum(cod_ense %% 100 == 10 & cod_grado == 3, na.rm = TRUE),
    matricula_4medio          = sum(cod_ense %% 100 == 10 & cod_grado == 4, na.rm = TRUE),
    matricula_jovenes         = sum(cod_ense %% 100 == 10, na.rm = TRUE),
    matricula_adultos         = sum(cod_ense %% 100 == 63, na.rm = TRUE),
    matricula_adultos_1nivel  = sum(cod_ense %% 100 == 63 & cod_grado == 1, na.rm = TRUE),
    `EMTP para Jóvenes ciclo diferenciado` = ifelse(any(cod_ense %% 100 == 10), "Sí", "No"),
    `EMTP para Adultos ciclo diferenciado` = ifelse(any(cod_ense %% 100 == 63), "Sí", "No"),
    .groups = "drop"
  ) %>%
  rename(`MatrículaTotal del Establecimiento` = MatriculaEMTP) %>%
  mutate(MatriculaEMTP = `MatrículaTotal del Establecimiento`)

doc_agg <- docentes_raw %>%
  group_by(rbd) %>%
  summarise(
    DocentesEMTP_Total   = n(),
    DocentesEMTP_Hombres = sum(DOC_GENERO == 1, na.rm = TRUE),
    DocentesEMTP_Mujeres = sum(DOC_GENERO == 2, na.rm = TRUE),
    .groups = "drop"
  )

# 5e-bis. Cohorte titulación al año de egreso (para minuta):
#   egre_2023 = egresados EMTP 2023 por RBD ; titu_2024_egre_2023 = titulados 2024 con egreso 2023
egre_2023_agg <- leer_csv(RUTA_EGRESADOS_2023, enc = ENC_EGRESADOS, delim = ";") %>%
  rename_with(toupper) %>%
  mutate(RBD = as.character(RBD), COD_ENSE = suppressWarnings(as.numeric(COD_ENSE))) %>%
  filter(!is.na(COD_ENSE), COD_ENSE >= EMTP_ENSE_MIN, COD_ENSE <= EMTP_ENSE_MAX,
         !is.na(MARCA_EGRESO), MARCA_EGRESO == 1) %>%
  group_by(rbd = RBD) %>% summarise(egre_2023 = n(), .groups = "drop")

titu_cohorte_agg <- leer_csv(RUTA_TITULADOS, enc = ENC_TITULADOS) %>%
  rename_with(toupper) %>%
  { if ("RBD_EGRESO" %in% names(.)) rename(., RBD = RBD_EGRESO) else . } %>%
  mutate(RBD = as.character(RBD),
         COD_ENSE = suppressWarnings(as.numeric(COD_ENSE)),
         ESTADO_PRACTICA = suppressWarnings(as.integer(ESTADO_PRACTICA)),
         AGNO_ESCOLAR = suppressWarnings(as.integer(AGNO_ESCOLAR))) %>%
  filter(!is.na(COD_ENSE), COD_ENSE >= EMTP_ENSE_MIN, COD_ENSE <= EMTP_ENSE_MAX,
         !is.na(ESTADO_PRACTICA), ESTADO_PRACTICA == 1, AGNO_ESCOLAR == 2023) %>%
  group_by(rbd = RBD) %>% summarise(titu_2024_egre_2023 = n(), .groups = "drop")

# 5e. Ensamblar base_apoyo + columnas no reconstruibles (NA) que la minuta referencia
base_apoyo <- dir_ee %>%
  left_join(ive,            by = "rbd") %>%
  left_join(simce,          by = "rbd") %>%
  left_join(mat_agg,        by = "rbd") %>%
  left_join(doc_agg,        by = "rbd") %>%
  left_join(egre_2023_agg,  by = "rbd") %>%
  left_join(titu_cohorte_agg, by = "rbd") %>%
  left_join(mat_total_rbd,  by = "rbd") %>%
  left_join(doc_especialidad_rbd, by = "rbd") %>%
  { if (!is.null(idps_ind)) left_join(., idps_ind, by = "rbd") else . } %>%
  mutate(
    DocentesEspecialidad_Total = coalesce(DocentesEspecialidad_Total, 0L),
    # Matrícula total del establecimiento (todas las enseñanzas)
    `MatrículaTotal del Establecimiento` = MATRICULA_OFICIAL_2025,
    # Cohorte titulación (0 si el RBD no aparece en egresados 2023 / titulados)
    egre_2023           = coalesce(egre_2023, 0L),
    titu_2024_egre_2023 = coalesce(titu_2024_egre_2023, 0L),
    # No disponibles en datos brutos (quedan NA; las minutas degradan esas secciones)
    nombre_sost     = NA_character_,
    direccion       = NA_character_,
    nombre_director = NA_character_,
    NOMBRE_SLEP     = NA_character_,
    Bicentenario    = NA_character_,
    gse_agencia     = if ("cod_grupo" %in% names(.)) as.character(cod_grupo) else NA_character_,
    RuralidadRBD_2025 = RuralidadRBD
  )

# Columnas referenciadas por las minutas pero NO reconstruibles desde datos brutos → NA
# (SIMCE no trae los conteos n_palu_eda_*; equipamiento proviene de fuentes externas)
cols_na_num <- c("n_palu_eda_ins_lect2m_rbd","n_palu_eda_ele_lect2m_rbd","n_palu_eda_ade_lect2m_rbd",
                 "n_palu_eda_ins_mate2m_rbd","n_palu_eda_ele_mate2m_rbd","n_palu_eda_ade_mate2m_rbd",
                 "EquipamientoRegular_2020","EquipamientoRegular_2021","EquipamientoRegular_2022",
                 "EquipamientoRegular_2023","EquipamientoRegular_2024","EquipamientoRegular_TOTAL",
                 "EquipamientoSLEP_2023","EquipamientoSLEP_2024","EquipamientoSLEP_TOTAL")
cols_na_chr <- c("EquipamientoRegular_2020_espe","EquipamientoRegular_2021_espe","EquipamientoRegular_2022_espe",
                 "EquipamientoRegular_2023_espe","EquipamientoRegular_2024_espe")
for (cc in cols_na_num) if (!cc %in% names(base_apoyo)) base_apoyo[[cc]] <- NA_real_
for (cc in cols_na_chr) if (!cc %in% names(base_apoyo)) base_apoyo[[cc]] <- NA_character_
# Adjudica: 0 (no NA) — la minuta hace `if (Adjudica == 1)`; con 0 imprime "no adjudicado" sin crashear
base_apoyo$EquipamientoRegular_Adjudica <- 0L
ok("Establecimientos en base_apoyo", nrow(base_apoyo))
guardar(base_apoyo, "base_apoyo.rds")

# 5f. IDPS dimensiones (para tab Establecimientos)
cat("  Cargando IDPS dimensiones...\n")
idps_dim <- tryCatch(
  leer_csv(RUTA_IDPS_DIM, enc = ENC_IDPS) %>% rename_with(tolower) %>%
    mutate(rbd = as.character(rbd)),
  error = function(e) { cat("  (IDPS dim no disponible)\n"); NULL }
)
if (!is.null(idps_dim)) guardar(idps_dim, "idps_dimensiones.rds")
rm(dir_ee, ive, simce, mat_agg, doc_agg); gc(verbose = FALSE)

# =============================================================================
# 6. TITULADOS TP 2024  (titulados reales = ESTADO_PRACTICA == 1)
# =============================================================================
cat("\n>>> [6/9] TITULADOS TP <<<\n")
titulados <- leer_csv(RUTA_TITULADOS, enc = ENC_TITULADOS) %>%
  rename_with(toupper) %>%
  { if ("RBD_EGRESO" %in% names(.)) rename(., RBD = RBD_EGRESO) else . } %>%
  mutate(
    RBD      = as.character(RBD),
    MRUN     = as.character(MRUN),
    COD_ENSE = suppressWarnings(as.numeric(COD_ENSE)),
    COD_ESPE = suppressWarnings(as.numeric(COD_ESPE)),
    GEN_ALU  = suppressWarnings(as.numeric(GEN_ALU)),
    ESTADO_PRACTICA = suppressWarnings(as.integer(ESTADO_PRACTICA))
  ) %>%
  filter(!is.na(COD_ENSE), COD_ENSE >= EMTP_ENSE_MIN, COD_ENSE <= EMTP_ENSE_MAX,
         !is.na(ESTADO_PRACTICA), ESTADO_PRACTICA == 1) %>%   # solo titulados
  mutate(NOM_ESPE   = dic_especialidades$nombre[match(COD_ESPE, dic_especialidades$codigo)],
         NOM_SECTOR = dic_sectores$sector[match(as.integer(COD_ESPE %/% 100L), dic_sectores$cod_sec)])
ok("Titulados TP (ESTADO_PRACTICA==1)", nrow(titulados))
guardar(titulados, "titulados.rds")

# =============================================================================
# 7. COMUNAS (sf + agregados de matrícula)  — port de la lógica original
# =============================================================================
cat("\n>>> [7/9] COMUNAS <<<\n")
comunas <- readRDS(RUTA_COMUNAS) %>% mutate(cod_comuna = as.character(cod_comuna))
comunas$Region <- str_trim(comunas$Region)
comunas$Region <- recode(comunas$Region,
  "Región del Libertador Bernardo O'Higgins" = "Región del Libertador General Bernardo O'Higgins",
  "Región de Magallanes y Antártica Chilena" = "Región de Magallanes y de la Antártica Chilena",
  "Región del Bío-Bío" = "Región del Biobío",
  "Región de Aysén del Gral.Ibañez del Campo" = "Región de Aysén del General Carlos Ibáñez del Campo")

mat_comunas_attr <- matricula_raw %>%
  select(cod_com_rbd, nom_reg_rbd_a, nombre_sost, cod_pro_rbd,
         nom_deprov_rbd, nom_com_rbd, nom_espe, cod_depe2) %>%
  distinct() %>% rename(cod_comuna = cod_com_rbd)

mat_comuna <- matricula_raw %>% filter(gen_alu %in% c(1, 2)) %>%
  group_by(cod_com_rbd) %>% summarise(matricula = n(), .groups = "drop") %>%
  rename(cod_comuna = cod_com_rbd)
mat_genero_wide <- matricula_raw %>% filter(gen_alu %in% c(1, 2)) %>%
  group_by(cod_com_rbd, gen_alu) %>% summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = gen_alu, values_from = n, values_fill = 0) %>%
  rename(cod_comuna = cod_com_rbd) %>%
  rename_with(~ c("matricula_hombres", "matricula_mujeres"), .cols = c(`1`, `2`))
estab_comuna <- matricula_raw %>%
  group_by(cod_com_rbd) %>% summarise(n_establecimientos = n_distinct(rbd), .groups = "drop") %>%
  rename(cod_comuna = cod_com_rbd)

comunas <- comunas %>%
  left_join(mat_comunas_attr, by = "cod_comuna") %>%
  mutate(rft = asignar_rft(nom_deprov_rbd)) %>%
  left_join(mat_comuna,      by = "cod_comuna") %>%
  left_join(mat_genero_wide, by = "cod_comuna") %>%
  left_join(estab_comuna,    by = "cod_comuna")

orden_norte_a_sur <- c(
  "Región de Arica y Parinacota","Región de Tarapacá","Región de Antofagasta",
  "Región de Atacama","Región de Coquimbo","Región de Valparaíso",
  "Región Metropolitana de Santiago","Región del Libertador General Bernardo O'Higgins",
  "Región del Maule","Región de Ñuble","Región del Biobío","Región de La Araucanía",
  "Región de Los Ríos","Región de Los Lagos",
  "Región de Aysén del General Carlos Ibáñez del Campo",
  "Región de Magallanes y de la Antártica Chilena")
regiones_presentes <- intersect(orden_norte_a_sur, unique(comunas$Region))
n_reg       <- length(regiones_presentes)
colores_raw <- qualitative_hcl(n_reg, palette = "Dark 3")
mitad <- ceiling(n_reg / 2)
idx   <- as.vector(rbind(seq_len(mitad), seq(mitad + 1, n_reg))); idx <- idx[!is.na(idx)]
colores_regiones <- setNames(colores_raw[idx], regiones_presentes)
comunas$fill_color_base <- colores_regiones[comunas$Region]

mat_min <- min(comunas$matricula, na.rm = TRUE); mat_max <- max(comunas$matricula, na.rm = TRUE)
comunas$fill_opacity <- ((comunas$matricula - mat_min) / (mat_max - mat_min)) * 0.7 + 0.3
comunas$fill_opacity[is.na(comunas$fill_opacity)] <- 0.3
comunas$fill_opacity <- pmin(pmax(comunas$fill_opacity, 0.3), 1)
comunas$fill_color_final <- ifelse(is.na(comunas$matricula) | comunas$matricula == 0,
                                   "#BBBBBB", comunas$fill_color_base)
guardar(comunas, "comunas.rds")
rm(mat_comuna, mat_genero_wide, estab_comuna, mat_comunas_attr); gc(verbose = FALSE)

# =============================================================================
# 8. META (diccionarios, choices, colores)
# =============================================================================
cat("\n>>> [8/9] META <<<\n")
choices_especialidades_doc <- {
  tmp <- docentes_long %>% filter(es_especialidad) %>%
    distinct(SUBSECTOR, Especialidad) %>% arrange(SUBSECTOR)
  setNames(tmp$SUBSECTOR, paste(tmp$SUBSECTOR, tmp$Especialidad))
}
codigos_presentes <- sort(unique(
  docentes_raw$SUBSECTOR1[docentes_raw$SUBSECTOR1 %in% dic_especialidades$codigo]))
choices_especialidades <- setNames(
  codigos_presentes,
  paste(codigos_presentes,
        dic_especialidades$nombre[match(codigos_presentes, dic_especialidades$codigo)], sep = " - "))

meta <- list(
  indicadores_continuidad    = indicadores_continuidad,
  dic_especialidades         = dic_especialidades,
  dic_sectores               = dic_sectores,
  mapeo_dependencias         = mapeo_dependencias,
  colores_regiones           = colores_regiones,
  choices_especialidades_doc = choices_especialidades_doc,
  choices_especialidades     = choices_especialidades,
  anio_matricula             = ANIO_MATRICULA,
  anio_docentes              = ANIO_DOCENTES,
  anio_egresados             = ANIO_EGRESADOS_CONT
)
guardar(meta, "meta.rds")

# =============================================================================
# 9. RESUMEN
# =============================================================================
elapsed   <- round((proc.time() - t0)[["elapsed"]])
archivos  <- list.files("data/app", pattern = "\\.rds$", full.names = TRUE)
tam_total <- sum(file.info(archivos)$size) / 1024^2
cat("\n=============================================================================\n")
cat(sprintf("✅ COMPLETADO en %d s | Total: %.1f MB en data/app/\n", elapsed, tam_total))
cat("=============================================================================\n")
for (f in archivos) cat(sprintf("  %-32s %6.0f KB\n", basename(f), file.info(f)$size / 1024))
cat("\nPróximos pasos:\n  1. shiny::runApp()\n  2. rsconnect::deployApp()\n")
cat("=============================================================================\n")
