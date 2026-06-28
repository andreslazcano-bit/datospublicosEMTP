# =============================================================================
# R/filtros_docentes.R
# Filtros reutilizables para la base de CARGOS DOCENTES EMTP (docentes_raw)
# -----------------------------------------------------------------------------
# Basado en la estructura real de la base y en los filtros de la pestaña
# "Docentes" de la app. Pensado en BASE R (sin dependencias extra), tolerante a
# nombres de columna en mayúsculas o minúsculas.
#
# IMPORTANTE — dos unidades de conteo:
#   * CARGOS  = filas (un docente puede tener varios contratos/establecimientos)
#   * PERSONAS = MRUN únicos
# Por eso el resumen entrega ambos (en la app: ~18.957 cargos / 18.766 personas).
#
# Nota: se asume que `df` ya viene filtrado a EMTP-TP como en app.R
# (COD_ENS_1/2 entre 410 y 863 con HORAS > 0). Igual se incluye `solo_tp` por si
# se parte de la base cruda.
# =============================================================================

# --- Diccionarios / constantes ---------------------------------------------
DEP_LABELS_DOC <- c("1" = "Municipal",
                    "2" = "Particular Subvencionado",
                    "3" = "Particular Pagado",
                    "4" = "Corporación de Administración Delegada",
                    "5" = "SLEP")
GENERO_LABELS_DOC <- c("1" = "Hombre", "2" = "Mujer")

# Subsector que define ESPECIALIDAD EMTP (módulos TP): 41001–81004
# (31001–39999 es Formación General y queda fuera)
SUBSECTOR_ESP_MIN <- 41001L
SUBSECTOR_ESP_MAX <- 81004L

# --- Helper: resolver una columna tolerando mayúsculas/minúsculas -----------
.col_doc <- function(df, ...) {
  cand <- c(...)
  hit  <- cand[cand %in% names(df)]
  if (length(hit)) hit[1] else NA_character_
}

# --- Helper: edad a partir de DOC_FEC_NAC (formato YYYYMMDD) -----------------
edad_docente <- function(fec_nac, anio_ref = as.integer(format(Sys.Date(), "%Y"))) {
  s   <- as.character(fec_nac)
  yy  <- suppressWarnings(as.integer(substr(s, 1, 4)))
  mm  <- suppressWarnings(as.integer(substr(s, 5, 6)))
  mes_hoy <- as.integer(format(Sys.Date(), "%m"))
  edad <- anio_ref - yy - ifelse(!is.na(mm) & mm > mes_hoy, 1L, 0L)
  edad[is.na(edad) | edad < 15 | edad > 90] <- NA_integer_
  edad
}

# --- Helper: normalizar dependencia a códigos 1..5 --------------------------
.norm_dep_doc <- function(x) {
  if (is.null(x)) return(NULL)
  x <- x[!(tolower(as.character(x)) %in% c("todas", "todos", ""))]
  if (length(x) == 0) return(NULL)
  out <- integer(0)
  for (v in x) {
    v <- tolower(trimws(as.character(v)))
    code <- suppressWarnings(as.integer(v))
    if (!is.na(code)) { out <- c(out, code); next }
    code <- if (grepl("slep|servicio local", v)) 5L
            else if (grepl("municipal|daem", v)) 1L
            else if (grepl("subvencion", v))     2L
            else if (grepl("pagado|privado", v)) 3L
            else if (grepl("corpora|corem", v))  4L
            else NA_integer_
    out <- c(out, code)
  }
  unique(out[!is.na(out)])
}

# =============================================================================
# FUNCIÓN PRINCIPAL DE FILTRADO
# Devuelve el data.frame filtrado. Cada filtro acepta "Todas"/"Todos"/NULL = sin
# filtro. La geografía, dependencia, especialidad, etc. admiten VECTORES (unión).
# =============================================================================
filtrar_docentes <- function(
    df,
    region          = "Todas",   # NOM_REG_RBD_A (abreviatura interna: "RM","BBIO"...)
    comuna          = "Todas",   # NOM_COM_RBD
    deprov          = "Todas",   # NOM_DEPROV_RBD
    dependencia     = "Todas",   # 1..5 o etiqueta ("Municipal"...); admite vector
    ruralidad       = "Todas",   # "Rural" / "Urbano" / "Todas"
    genero          = "Todos",   # "Hombre"/"Mujer" o 1/2; "Todos" = ambos
    especialidad_tp = FALSE,     # TRUE = solo docentes de módulos de ESPECIALIDAD (SUBSECTOR 41001-81004)
    subsector       = NULL,      # vector de códigos de subsector concretos (ej. 53014 = Electricidad)
    poblacion       = "Todas",   # "Jóvenes" / "Adultos" / "Ambas"
    tramo           = "Todos",   # TRAMO_CARR_DOCENTE (texto)
    solo_pedagogia  = FALSE,     # TRUE = solo con título en Educación (TIT_ID 1)
    edad_min        = NULL,      # edad mínima (usa DOC_FEC_NAC)
    edad_max        = NULL,      # edad máxima
    horas_min       = NULL,      # HORAS_CONTRATO mínimo
    rbd             = NULL,      # uno o varios RBD
    solo_tp         = FALSE      # TRUE = restringe a cargos EMTP-TP (COD_ENS 410-863, HORAS>0)
) {
  stopifnot(is.data.frame(df))
  d <- df

  es_todas <- function(x) is.null(x) || (length(x) == 1 && tolower(as.character(x)) %in% c("todas", "todos", ""))

  # Resolver columnas (mayúsculas o minúsculas)
  c_reg   <- .col_doc(d, "NOM_REG_RBD_A", "nom_reg_rbd_a")
  c_com   <- .col_doc(d, "NOM_COM_RBD", "nom_com_rbd")
  c_dprov <- .col_doc(d, "NOM_DEPROV_RBD", "nom_deprov_rbd")
  c_dep   <- .col_doc(d, "COD_DEPE2", "cod_depe2")
  c_rural <- .col_doc(d, "RURAL_RBD", "rural_rbd")
  c_gen   <- .col_doc(d, "DOC_GENERO", "doc_genero")
  c_pob   <- .col_doc(d, "Poblacion", "POBLACION", "poblacion")
  c_tramo <- .col_doc(d, "TRAMO_CARR_DOCENTE", "tramo_carr_docente")
  c_fec   <- .col_doc(d, "DOC_FEC_NAC", "doc_fec_nac")
  c_horas <- .col_doc(d, "HORAS_CONTRATO", "horas_contrato")
  c_rbd   <- .col_doc(d, "RBD", "rbd")
  c_s1    <- .col_doc(d, "SUBSECTOR1", "subsector1")
  c_s2    <- .col_doc(d, "SUBSECTOR2", "subsector2")
  c_e1    <- .col_doc(d, "COD_ENS_1", "cod_ens_1")
  c_e2    <- .col_doc(d, "COD_ENS_2", "cod_ens_2")
  c_t1    <- .col_doc(d, "TIT_ID_1", "tit_id_1")
  c_t2    <- .col_doc(d, "TIT_ID_2", "tit_id_2")
  c_h1    <- .col_doc(d, "HORAS1", "horas1")
  c_h2    <- .col_doc(d, "HORAS2", "horas2")

  # 0) Restringir a EMTP-TP (opcional) ---------------------------------------
  if (solo_tp && !is.na(c_e1) && !is.na(c_e2)) {
    e1 <- suppressWarnings(as.numeric(as.character(d[[c_e1]])))
    e2 <- suppressWarnings(as.numeric(as.character(d[[c_e2]])))
    h1 <- if (!is.na(c_h1)) suppressWarnings(as.numeric(as.character(d[[c_h1]]))) else rep(1, nrow(d))
    h2 <- if (!is.na(c_h2)) suppressWarnings(as.numeric(as.character(d[[c_h2]]))) else rep(0, nrow(d))
    keep <- ((!is.na(e1) & e1 >= 410 & e1 <= 863 & !is.na(h1) & h1 > 0) |
             (!is.na(e2) & e2 >= 410 & e2 <= 863 & !is.na(h2) & h2 > 0))
    d <- d[keep, , drop = FALSE]
  }

  # 1) Geografía -------------------------------------------------------------
  if (!es_todas(region) && !is.na(c_reg))
    d <- d[!is.na(d[[c_reg]]) & d[[c_reg]] %in% region, , drop = FALSE]
  if (!es_todas(comuna) && !is.na(c_com))
    d <- d[!is.na(d[[c_com]]) & d[[c_com]] %in% comuna, , drop = FALSE]
  if (!es_todas(deprov) && !is.na(c_dprov))
    d <- d[!is.na(d[[c_dprov]]) & d[[c_dprov]] %in% deprov, , drop = FALSE]

  # 2) Dependencia (admite vector / etiquetas) -------------------------------
  dep_codes <- .norm_dep_doc(dependencia)
  if (!is.null(dep_codes) && !is.na(c_dep)) {
    dd <- suppressWarnings(as.integer(as.character(d[[c_dep]])))
    d  <- d[!is.na(dd) & dd %in% dep_codes, , drop = FALSE]
  }

  # 3) Ruralidad (RURAL_RBD: 0=Urbano, 1=Rural) ------------------------------
  if (!es_todas(ruralidad) && !is.na(c_rural)) {
    val <- if (grepl("urb", tolower(as.character(ruralidad)[1]))) 0L else 1L
    rr  <- suppressWarnings(as.integer(as.character(d[[c_rural]])))
    d   <- d[!is.na(rr) & rr == val, , drop = FALSE]
  }

  # 4) Género (DOC_GENERO: 1=Hombre, 2=Mujer) --------------------------------
  if (!es_todas(genero) && !is.na(c_gen)) {
    g <- tolower(as.character(genero)[1])
    cod_g <- if (g %in% c("2", "mujer", "mujeres", "femenino", "f")) 2L
             else if (g %in% c("1", "hombre", "hombres", "masculino", "m")) 1L else NA_integer_
    if (!is.na(cod_g)) {
      gg <- suppressWarnings(as.integer(as.character(d[[c_gen]])))
      d  <- d[!is.na(gg) & gg == cod_g, , drop = FALSE]
    }
  }

  # 5) Especialidad EMTP --------------------------------------------------
  #    (a) especialidad_tp = TRUE → cualquier módulo de especialidad (41001–81004)
  #    (b) subsector concreto(s) → solo esos códigos
  if (!is.na(c_s1) && !is.na(c_s2)) {
    s1 <- suppressWarnings(as.integer(as.character(d[[c_s1]])))
    s2 <- suppressWarnings(as.integer(as.character(d[[c_s2]])))
    if (!is.null(subsector) && length(subsector) > 0) {
      cods <- as.integer(subsector)
      d <- d[(!is.na(s1) & s1 %in% cods) | (!is.na(s2) & s2 %in% cods), , drop = FALSE]
    } else if (isTRUE(especialidad_tp)) {
      d <- d[(!is.na(s1) & s1 >= SUBSECTOR_ESP_MIN & s1 <= SUBSECTOR_ESP_MAX) |
             (!is.na(s2) & s2 >= SUBSECTOR_ESP_MIN & s2 <= SUBSECTOR_ESP_MAX), , drop = FALSE]
    }
  }

  # 6) Población (Jóvenes / Adultos / Ambas) ---------------------------------
  if (!es_todas(poblacion) && !is.na(c_pob)) {
    pob <- tolower(as.character(poblacion)[1])
    d <- d[!is.na(d[[c_pob]]) & grepl(substr(pob, 1, 5), tolower(d[[c_pob]]), fixed = TRUE), , drop = FALSE]
  }

  # 7) Tramo de carrera docente ----------------------------------------------
  if (!es_todas(tramo) && !is.na(c_tramo))
    d <- d[!is.na(d[[c_tramo]]) & d[[c_tramo]] %in% tramo, , drop = FALSE]

  # 8) Solo con título en Educación / Pedagogía (TIT_ID 1) -------------------
  if (isTRUE(solo_pedagogia) && !is.na(c_t1)) {
    t1 <- suppressWarnings(as.integer(as.character(d[[c_t1]])))
    t2 <- if (!is.na(c_t2)) suppressWarnings(as.integer(as.character(d[[c_t2]]))) else rep(NA_integer_, nrow(d))
    d  <- d[(!is.na(t1) & t1 == 1L) | (!is.na(t2) & t2 == 1L), , drop = FALSE]
  }

  # 9) Edad (rango) ----------------------------------------------------------
  if ((!is.null(edad_min) || !is.null(edad_max)) && !is.na(c_fec)) {
    ed <- edad_docente(d[[c_fec]])
    if (!is.null(edad_min)) d <- d[!is.na(ed) & ed >= edad_min, , drop = FALSE]
    if (!is.null(edad_max)) { ed <- edad_docente(d[[c_fec]]); d <- d[!is.na(ed) & ed <= edad_max, , drop = FALSE] }
  }

  # 10) Horas de contrato mínimas --------------------------------------------
  if (!is.null(horas_min) && !is.na(c_horas)) {
    hc <- suppressWarnings(as.numeric(as.character(d[[c_horas]])))
    d  <- d[!is.na(hc) & hc >= horas_min, , drop = FALSE]
  }

  # 11) RBD específico(s) -----------------------------------------------------
  if (!is.null(rbd) && !is.na(c_rbd))
    d <- d[as.character(d[[c_rbd]]) %in% as.character(rbd), , drop = FALSE]

  d
}

# =============================================================================
# RESUMEN: cargos, personas únicas, % pedagogía y desglose de género
# =============================================================================
resumen_docentes <- function(df) {
  c_mrun <- .col_doc(df, "MRUN", "mrun")
  c_rbd  <- .col_doc(df, "RBD", "rbd")
  c_gen  <- .col_doc(df, "DOC_GENERO", "doc_genero")
  c_t1   <- .col_doc(df, "TIT_ID_1", "tit_id_1")
  c_t2   <- .col_doc(df, "TIT_ID_2", "tit_id_2")

  n_cargos   <- nrow(df)
  n_personas <- if (!is.na(c_mrun)) length(unique(df[[c_mrun]])) else NA_integer_
  n_ee       <- if (!is.na(c_rbd))  length(unique(df[[c_rbd]]))  else NA_integer_

  # Sobre PERSONAS únicas
  pct_ped <- NA_real_; n_h <- NA_integer_; n_m <- NA_integer_
  if (!is.na(c_mrun)) {
    du <- df[!duplicated(df[[c_mrun]]), , drop = FALSE]
    if (!is.na(c_t1)) {
      t1 <- suppressWarnings(as.integer(as.character(du[[c_t1]])))
      t2 <- if (!is.na(c_t2)) suppressWarnings(as.integer(as.character(du[[c_t2]]))) else rep(NA_integer_, nrow(du))
      n_ped   <- sum((!is.na(t1) & t1 == 1L) | (!is.na(t2) & t2 == 1L))
      pct_ped <- if (nrow(du) > 0) round(100 * n_ped / nrow(du), 1) else NA_real_
    }
    if (!is.na(c_gen)) {
      gg  <- suppressWarnings(as.integer(as.character(du[[c_gen]])))
      n_h <- sum(gg == 1L, na.rm = TRUE); n_m <- sum(gg == 2L, na.rm = TRUE)
    }
  }

  list(
    cargos              = n_cargos,
    personas            = n_personas,
    establecimientos    = n_ee,
    pct_pedagogia       = pct_ped,
    hombres             = n_h,
    mujeres             = n_m,
    pct_mujeres         = if (!is.na(n_h) && (n_h + n_m) > 0) round(100 * n_m / (n_h + n_m), 1) else NA_real_
  )
}

# =============================================================================
# OPCIONES PARA LOS selectInput (choices de la UI)
# =============================================================================
opciones_filtros_docentes <- function(df) {
  uval <- function(cols) {
    c <- .col_doc(df, cols[1], cols[2])
    if (is.na(c)) return(character(0))
    sort(unique(as.character(df[[c]][!is.na(df[[c]])])))
  }
  list(
    region      = c("Todas", uval(c("NOM_REG_RBD_A", "nom_reg_rbd_a"))),
    comuna      = c("Todas", uval(c("NOM_COM_RBD", "nom_com_rbd"))),
    deprov      = c("Todas", uval(c("NOM_DEPROV_RBD", "nom_deprov_rbd"))),
    poblacion   = c("Todas", uval(c("Poblacion", "poblacion"))),
    tramo       = c("Todos", uval(c("TRAMO_CARR_DOCENTE", "tramo_carr_docente"))),
    dependencia = c("Todas" = "Todas", setNames(names(DEP_LABELS_DOC), DEP_LABELS_DOC)),
    genero      = c("Todos", "Hombre", "Mujer"),
    ruralidad   = c("Todas", "Rural", "Urbano")
  )
}

# =============================================================================
# EJEMPLOS DE USO
# -----------------------------------------------------------------------------
# library(dplyr)  # no es necesario; todo es base R
#
# # 1) Docentes de ESPECIALIDAD (módulos TP) en la Araucanía, mujeres:
# d1 <- filtrar_docentes(docentes_raw, region = "ARAUC",
#                        especialidad_tp = TRUE, genero = "Mujer")
# resumen_docentes(d1)
#
# # 2) Docentes de Electricidad (subsector 53014) en liceos rurales, con pedagogía:
# d2 <- filtrar_docentes(docentes_raw, subsector = 53014,
#                        ruralidad = "Rural", solo_pedagogia = TRUE)
#
# # 3) Municipales + SLEP (unión), tramo "Experto", mayores de 60:
# d3 <- filtrar_docentes(docentes_raw, dependencia = c("Municipal", "SLEP"),
#                        tramo = "Experto", edad_min = 60)
#
# # 4) Para poblar los selectInput de la UI:
# opts <- opciones_filtros_docentes(docentes_raw)
# # selectInput("doc_f_region", "Región:", choices = opts$region)
# =============================================================================
