# =============================================================================
# R/chatbot_rag.R  — Chatbot RAG flotante para Explorador EMTP
# KB estática + dinámica (datos reales) | TF-IDF | Ollama local
# =============================================================================

library(httr2)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# =============================================================================
# 1. KB ESTÁTICA
# =============================================================================
rag_kb_estatica <- list(

  sistema_rft = list(
    id = "sistema_rft",
    titulo = "Sistema Red Futuro Técnico (RFT)",
    cuerpo = paste(
      "La Red Futuro Técnico (RFT) es una política del Ministerio de Educación de Chile",
      "que articula establecimientos EMTP con instituciones de educación superior y el mundo del trabajo.",
      "Opera mediante convenios entre un ejecutor (universidad, CFT o empresa) y establecimientos.",
      "La variable 'rft' en la base de matrícula indica si el estudiante pertenece a la red.",
      "'rft_ejecutor' identifica la institución ejecutora.",
      "Los establecimientos RFT reciben apoyo técnico, equipamiento y vinculación laboral.",
      "El análisis RFT compara matrícula, género, dependencia y continuidad entre establecimientos RFT y no-RFT."
    )
  ),

  matricula_estructura = list(
    id = "matricula_estructura",
    titulo = "Estructura y Variables de la Base de Matrícula EMTP",
    cuerpo = paste(
      "La base de matrícula tiene una fila por estudiante-especialidad en 2025.",
      "Variables clave: rbd (identificador del establecimiento), mrun (identificador único del estudiante),",
      "cod_ense (tipo de enseñanza: 410-510-610-710-810 jóvenes, 463-563-663-763-863 adultos),",
      "cod_espe (código de especialidad), nom_espe (nombre de especialidad),",
      "cod_grado (grado: 1=primero medio, 3=tercero medio equivalente),",
      "gen_alu (género del alumno: 1=hombre, 2=mujer),",
      "cod_depe2 (dependencia: 1=municipal, 2=particular subvencionado, 3=particular pagado, 4=corporación, 5=SLEP),",
      "RuralidadRBD (ruralidad del establecimiento: Rural/Urbano),",
      "nom_reg_rbd_a (nombre de la región del establecimiento),",
      "nom_com_rbd (nombre de la comuna del establecimiento),",
      "rft (si pertenece a Red Futuro Técnico: 'PERTENECE' o NA),",
      "categoria_asis_anual (asistencia anual del estudiante).",
      "Solo se incluyen estudiantes con cod_ense entre 410 y 863 (educación TP).",
      "Los sectores productivos agrupan especialidades por área: Agropecuario, Construcción, Industria, etc."
    )
  ),

  docentes_estructura = list(
    id = "docentes_estructura",
    titulo = "Estructura y Variables de la Base de Docentes EMTP",
    cuerpo = paste(
      "La base de docentes tiene una fila por docente-contrato-establecimiento.",
      "Variables clave: MRUN (identificador único docente), RBD (establecimiento),",
      "COD_ENS_1/COD_ENS_2 (tipo de enseñanza que imparte, igual escala que matrícula),",
      "HORAS1/HORAS2 (horas contratadas por tipo de enseñanza),",
      "SUBSECTOR1/SUBSECTOR2 (subsector curricular, define si es especialidad o formación general),",
      "TIT_ID_1/TIT_ID_2 (tipo de título: 1=Educación/pedagogía, 2=otro título universitario, 3=sin título universitario),",
      "DOC_FEC_NAC (fecha de nacimiento del docente, formato YYYYMMDD),",
      "COD_DEPE2 (dependencia del establecimiento), COD_REG_RBD (región del establecimiento).",
      "Para identificar docentes de ESPECIALIDAD EMTP se usa: SUBSECTOR entre 41001 y 81004",
      "(excluyendo 31001-39999 que es Formación General).",
      "Para formación pedagógica: TIT_ID=1 indica título en Educación (pedagogía).",
      "Los docentes están en 969-970 establecimientos con matrícula TP activa en 2025."
    )
  ),

  codigos_mineduc = list(
    id = "codigos_mineduc",
    titulo = "Códigos y Clasificaciones MINEDUC",
    cuerpo = paste(
      "DEPENDENCIA (cod_depe2): 1=Municipal/DAEM, 2=Particular Subvencionado, 3=Particular Pagado,",
      "4=Corporación Municipal (COREM/CORVI), 5=SLEP (Servicio Local de Educación Pública).",
      "Los SLEP reemplazaron progresivamente a los DAEM municipales desde 2018.",
      "TIPO DE ENSEÑANZA (cod_ense): 410=Industrial Jóv, 510=Técnica Jóv, 610=Agrícola Jóv,",
      "710=Marítima Jóv, 810=Comercial Jóv; +53 en el código = versión Adultos.",
      "GÉNERO: 1=Masculino, 2=Femenino. GRADO: 1=primero, 2=segundo, 3=tercero, 4=cuarto.",
      "SUBSECTOR: primeros 3 dígitos identifican el sector productivo.",
      "RURALIDAD: según definición MINEDUC basada en población comunal.",
      "Los sectores productivos EMTP son: Agropecuario, Acuicultura, Alimentación, Arte y Artesanía,",
      "Comunicación, Construcción, Electricidad, Gráfica, Hotelería, Industrial, Marítimo,",
      "Metalmeccánica, Minería, Química, Salud, Telecomunicaciones, Textil, Comercial/Administración."
    )
  ),

  metodologia_rotacion = list(
    id = "metodologia_rotacion",
    titulo = "Metodología de Análisis de Rotación Docente",
    cuerpo = paste(
      "La rotación docente se analiza comparando el conjunto de MRUN entre años consecutivos.",
      "Un docente 'sale' si aparece en el año t pero no en el año t+1.",
      "Un docente 'ingresa' si aparece en t+1 pero no en t. El balance neto = ingresos - salidas.",
      "Para salida por edad: edad = año_salida - año_nacimiento (de DOC_FEC_NAC).",
      "Si edad >= 65 al año de salida, se clasifica como probable retiro por edad.",
      "La salida acumulada sin retorno identifica docentes que salieron y no volvieron hasta 2025.",
      "Un docente 'rota de establecimiento' si cambia su RBD pero sigue activo en el sistema."
    )
  ),

  egresados_continuidad = list(
    id = "egresados_continuidad",
    titulo = "Egresados EMTP y Continuidad en Educación Superior",
    cuerpo = paste(
      "Los egresados EMTP son estudiantes que completaron el 4° año de enseñanza media técnico-profesional.",
      "La base de egresados 2024 tiene 73.931 registros.",
      "La tasa de continuidad general ES 2024-2025 es 50.3%.",
      "Se distingue continuidad en: CFT, IP y Universidades.",
      "Los CFT son la principal vía de continuidad para egresados EMTP.",
      "La información de continuidad proviene del cruce entre base MINEDUC y base SIES."
    )
  ),

  indicadores_calidad = list(
    id = "indicadores_calidad",
    titulo = "Indicadores de Calidad y Contexto: SIMCE, IDPS, IVE y GSE",
    cuerpo = paste(
      "SIMCE: Sistema de Medición de la Calidad de la Educación. Para EMTP se usa el SIMCE de 2° medio",
      "(II medio), en Comprensión de Lectura y Matemática. Cada establecimiento tiene un puntaje promedio",
      "y la distribución de estudiantes en tres Estándares de Aprendizaje: Insuficiente, Elemental y Adecuado.",
      "IDPS: Indicadores de Desarrollo Personal y Social, medidos por la Agencia de Calidad en cuatro dimensiones:",
      "(1) Autoestima académica y motivación escolar, (2) Clima de convivencia escolar,",
      "(3) Participación y formación ciudadana, (4) Hábitos de vida saludable. Se reportan en puntaje (0-100)",
      "y en porcentaje de estudiantes en nivel bajo/medio/alto.",
      "IVE: Índice de Vulnerabilidad Escolar (JUNAEB); a mayor IVE, mayor vulnerabilidad socioeconómica del alumnado.",
      "GSE: Grupo Socioeconómico asignado por la Agencia de Calidad; niveles Bajo, Medio Bajo, Medio, Medio Alto y Alto.",
      "Estos indicadores se consultan por establecimiento (RBD) en la pestaña Establecimientos y agregados por",
      "territorio (región/comuna/dependencia/sector) en la pestaña Análisis Territorial.",
      "Para un establecimiento puntual, pregunta por su RBD (ej. 'indicadores del RBD 8492')."
    )
  ),

  asistencia_anual = list(
    id = "asistencia_anual",
    titulo = "Asistencia Anual de los Estudiantes EMTP",
    cuerpo = paste(
      "La asistencia anual 2025 se mide por estudiante (cruce MRUN + RBD) y se clasifica en cuatro categorías:",
      "Categoría 1 = Inasistencia crítica (menos de 50% de asistencia);",
      "Categoría 2 = Inasistencia grave (entre 50% y 84%);",
      "Categoría 3 = Asistencia reiterada o en riesgo (entre 85% y 89%);",
      "Categoría 4 = Asistencia esperada o normal (90% o más).",
      "La columna categoria_asis_anual guarda la categoría (1-4) y tasa_asis_anual el porcentaje de asistencia.",
      "Se puede preguntar la asistencia promedio o la distribución por categorías, filtrando por región, comuna,",
      "dependencia, especialidad o sector económico (ej. 'asistencia de Electricidad en el Biobío').",
      "La asistencia promedio y su distribución también aparecen en las minutas descargables por establecimiento y territorio."
    )
  ),

  titulados_practica = list(
    id = "titulados_practica",
    titulo = "Titulados EMTP y Práctica Profesional",
    cuerpo = paste(
      "Para titularse de la EMTP el estudiante debe aprobar una práctica profesional tras egresar de 4° medio.",
      "La base de Practicantes y Titulados TP 2024 registra la práctica: especialidad (NOM_ESPE), sector (NOM_SECTOR),",
      "región/comuna del establecimiento, dependencia, género del estudiante, empresa y rubro económico de la práctica (GLOSA_RUBRO).",
      "El total de titulados EMTP 2024 (prácticas aprobadas, ESTADO_PRACTICA aprobado) es 57.655 personas.",
      "La tasa de titulación al año de egreso (cohorte de egresados 2023) es 76,7% y el tiempo medio a la titulación ~1,1 años.",
      "El rubro económico de la práctica (GLOSA_RUBRO) describe el sector productivo de la empresa donde se realizó la práctica.",
      "Se puede consultar titulados por especialidad, región, dependencia, género o rubro de la práctica.",
      "Para profundizar, ve a la pestaña Egresados y Titulados (sub-pestaña Titulados)."
    )
  ),

  sectores_economicos = list(
    id = "sectores_economicos",
    titulo = "Sectores Económicos y Especialidades EMTP (ANEXO VII)",
    cuerpo = paste(
      "Las especialidades EMTP se agrupan en sectores económicos según el ANEXO VII del Reporte de Matrícula por alumno.",
      "Cada especialidad (nom_espe, código cod_espe) pertenece a un sector económico (nom_sector, código cod_sec = cod_espe sin",
      "los dos últimos dígitos). Sectores: Agropecuario, Alimentación, Administración y Comercio, Construcción, Electricidad,",
      "Confección, Gráfica, Hotelería y Turismo, Maderero, Metalmecánico, Minero, Marítimo, Químico, Salud y Educación,",
      "Tecnología y Telecomunicaciones, entre otros.",
      "Las especialidades artísticas (Artes Visuales, Teatro, Danza; códigos 91xxx/92xxx/93xxx) NO son EMTP sino Educación",
      "Media Artística (EMA) y quedan EXCLUIDAS de todos los conteos.",
      "Se puede pedir la matrícula por sector económico (ej. 'matrícula por sector' o 'estudiantes del sector minero')."
    )
  )
)

# =============================================================================
# 2. KB DINÁMICA (estadísticas reales de los datos cargados)
# =============================================================================
build_data_stats_kb <- function(matricula=NULL, docentes=NULL, egresados=NULL, continuidad=NULL,
                                titulados=NULL, base_apoyo=NULL) {
  docs <- list()

  if (!is.null(matricula) && nrow(matricula) > 0) {
    tryCatch({
      n_total  <- nrow(matricula)
      n_ee     <- length(unique(matricula$rbd))
      n_est    <- length(unique(matricula$mrun))
      pct_f    <- round(100 * mean(matricula$gen_alu == 2, na.rm=TRUE), 1)
      pct_m    <- round(100 * mean(matricula$gen_alu == 1, na.rm=TRUE), 1)
      dep_tab  <- sort(table(matricula$cod_depe2), decreasing=TRUE)
      dep_lbl  <- c("1"="Municipal","2"="Part. Subv.","3"="Part. Pagado","4"="Corp. Municipal","5"="SLEP")
      dep_txt  <- paste(sapply(names(dep_tab), function(k)
        paste0(dep_lbl[k],": ",format(dep_tab[k],big.mark=".")," (",round(100*dep_tab[k]/n_total,1),"%)")), collapse="; ")
      reg_tab  <- sort(table(matricula$nom_reg_rbd_a), decreasing=TRUE)
      reg_top5 <- paste(names(head(reg_tab,5)),"(",format(head(reg_tab,5),big.mark="."),")", collapse="; ")
      n_rft    <- sum(matricula$rft == "PERTENECE", na.rm=TRUE)
      pct_rft  <- round(100*n_rft/n_total,1)
      n_ee_rft <- length(unique(matricula$rbd[!is.na(matricula$rft) & matricula$rft=="PERTENECE"]))
      col_rural_kb <- intersect(c("rural_rbd","RURAL_RBD"), names(matricula))[1]
      n_rural  <- if (!is.na(col_rural_kb)) sum(!is.na(matricula[[col_rural_kb]]) & as.integer(matricula[[col_rural_kb]]) == 1, na.rm=TRUE) else 0L
      pct_rural<- round(100*n_rural/n_total,1)
      esp_tab  <- sort(table(matricula$nom_espe), decreasing=TRUE)
      esp_top5 <- paste(names(head(esp_tab,5)),"(",format(head(esp_tab,5),big.mark="."),")", collapse="; ")
      # Sector económico (top 5)
      sec_txt <- ""
      if ("nom_sector" %in% names(matricula)) {
        sec_tab  <- sort(table(matricula$nom_sector[!is.na(matricula$nom_sector)]), decreasing=TRUE)
        sec_top5 <- paste(names(head(sec_tab,5)),"(",format(head(sec_tab,5),big.mark="."),")", collapse="; ")
        sec_txt  <- paste0("Top 5 sectores económicos por matrícula: ", sec_top5, ".")
      }
      # Asistencia anual (promedio y % en cada categoría)
      asis_txt <- ""
      if ("tasa_asis_anual" %in% names(matricula)) {
        prom_asis <- round(mean(matricula$tasa_asis_anual, na.rm=TRUE), 1)
        cat_tab   <- table(factor(as.character(matricula$categoria_asis_anual), levels=c("1","2","3","4")))
        nv        <- sum(cat_tab)
        pct_esp   <- if (nv > 0) round(100*cat_tab["4"]/nv, 1) else NA
        pct_crit  <- if (nv > 0) round(100*cat_tab["1"]/nv, 1) else NA
        asis_txt  <- paste0("Asistencia anual promedio de los estudiantes EMTP: ", prom_asis,
                            "%. Con asistencia esperada (>=90%): ", pct_esp,
                            "%. Con inasistencia crítica (<50%): ", pct_crit, "%.")
      }

      docs$stats_matricula <- list(
        id = "stats_matricula",
        titulo = "Estadísticas Reales de Matrícula EMTP 2025",
        cuerpo = paste(
          paste0("Total registros matrícula EMTP 2025: ", format(n_total, big.mark="."), "."),
          paste0("Número de establecimientos con matrícula TP: ", format(n_ee, big.mark="."), "."),
          paste0("Número de estudiantes únicos (MRUN): ", format(n_est, big.mark="."), "."),
          paste0("Distribución por género: ", pct_m, "% hombres, ", pct_f, "% mujeres."),
          paste0("Distribución por dependencia: ", dep_txt, "."),
          paste0("Top 5 regiones por matrícula: ", reg_top5, "."),
          paste0("Estudiantes en RFT: ", format(n_rft, big.mark="."), " (", pct_rft,
                 "% del total); ", format(n_ee_rft, big.mark="."), " establecimientos RFT."),
          paste0("Matrícula en establecimientos rurales: ", format(n_rural, big.mark="."),
                 " estudiantes (", pct_rural, "%)."),
          paste0("Top 5 especialidades por matrícula: ", esp_top5, "."),
          sec_txt,
          asis_txt
        )
      )
    }, error=function(e) NULL)
  }

  if (!is.null(docentes) && nrow(docentes) > 0) {
    tryCatch({
      n_doc    <- length(unique(docentes$MRUN))
      n_ee_doc <- length(unique(docentes$rbd))
      doc_esp  <- docentes
      if ("SUBSECTOR" %in% names(doc_esp))
        doc_esp <- doc_esp[!is.na(doc_esp$SUBSECTOR) & doc_esp$SUBSECTOR >= 41001 & doc_esp$SUBSECTOR <= 81004, ]
      n_esp <- length(unique(doc_esp$MRUN))
      ped_pct <- NA_real_
      if ("TIT_ID_1" %in% names(doc_esp) && n_esp > 0) {
        doc_u   <- doc_esp[!duplicated(doc_esp$MRUN), ]
        n_ped   <- sum(doc_u$TIT_ID_1 == 1 | (!is.na(doc_u$TIT_ID_2) & doc_u$TIT_ID_2 == 1), na.rm=TRUE)
        ped_pct <- round(100 * n_ped / nrow(doc_u), 1)
      }
      horas_prom <- round(mean(docentes$HORAS1[!is.na(docentes$HORAS1) & docentes$HORAS1 > 0], na.rm=TRUE), 1)

      docs$stats_docentes <- list(
        id = "stats_docentes",
        titulo = "Estadísticas Reales de Docentes EMTP 2025",
        cuerpo = paste(
          paste0("Total de docentes únicos EMTP en 2025: ", format(n_doc, big.mark="."), "."),
          paste0("Establecimientos con docentes EMTP: ", format(n_ee_doc, big.mark="."), "."),
          paste0("Docentes de módulos de ESPECIALIDAD (SUBSECTOR 41001-81004): ", format(n_esp, big.mark="."), "."),
          if (!is.na(ped_pct)) paste0("Docentes de especialidad con título en Educación/pedagogía: ", ped_pct, "%.") else "",
          paste0("Promedio de horas contratadas: ", horas_prom, " horas."),
          "Fuente: Directorio de Docentes MINEDUC, publicado julio 2025."
        )
      )
    }, error=function(e) NULL)
  }

  if (!is.null(egresados) && nrow(egresados) > 0) {
    tryCatch({
      n_egr <- nrow(egresados)
      cont_txt <- ""
      if (!is.null(continuidad) && nrow(continuidad) > 0 && "continuidad_es" %in% names(continuidad)) {
        tasa <- round(100 * mean(continuidad$continuidad_es == 1, na.rm=TRUE), 1)
        cont_txt <- paste0("Tasa de continuidad en educación superior (2024→2025): ", tasa, "%. ")
      }
      docs$stats_egresados <- list(
        id = "stats_egresados",
        titulo = "Estadísticas Reales de Egresados EMTP 2024",
        cuerpo = paste(
          paste0("Total de egresados EMTP en 2024: ", format(n_egr, big.mark="."), " estudiantes."),
          cont_txt,
          "El CFT es la vía más frecuente de continuidad para egresados EMTP."
        )
      )
    }, error=function(e) NULL)
  }

  # --- Titulados (práctica profesional) ---
  if (!is.null(titulados) && nrow(titulados) > 0) {
    tryCatch({
      n_tit  <- nrow(titulados)
      pct_fm <- round(100 * mean(titulados$GEN_ALU == 2, na.rm=TRUE), 1)
      esp_t  <- sort(table(titulados$NOM_ESPE), decreasing=TRUE)
      esp_t5 <- paste(names(head(esp_t,5)),"(",format(head(esp_t,5),big.mark="."),")", collapse="; ")
      rub_t  <- if ("GLOSA_RUBRO" %in% names(titulados))
                  sort(table(titulados$GLOSA_RUBRO[!is.na(titulados$GLOSA_RUBRO)]), decreasing=TRUE) else table(character(0))
      rub_t5 <- if (length(rub_t)) paste(names(head(rub_t,5)),"(",format(head(rub_t,5),big.mark="."),")", collapse="; ") else "N/D"
      docs$stats_titulados <- list(
        id = "stats_titulados",
        titulo = "Estadísticas Reales de Titulados EMTP 2024 (práctica profesional)",
        cuerpo = paste(
          paste0("Total de titulados EMTP en 2024 (con práctica aprobada): ", format(n_tit, big.mark="."), "."),
          paste0("Porcentaje de mujeres entre titulados: ", pct_fm, "%."),
          "Tasa de titulación al año de egreso (cohorte egresados 2023): 76,7%.",
          "Tiempo promedio a la titulación: ~1,1 años después del egreso.",
          paste0("Top 5 especialidades por titulados: ", esp_t5, "."),
          paste0("Top 5 rubros económicos de la práctica profesional: ", rub_t5, "."),
          "Solo se cuentan prácticas con ESTADO_PRACTICA aprobado (no el total de registros)."
        )
      )
    }, error=function(e) NULL)
  }

  # --- Indicadores de establecimientos: SIMCE / IDPS / IVE / GSE ---
  if (!is.null(base_apoyo) && nrow(base_apoyo) > 0) {
    tryCatch({
      ba <- base_apoyo
      prom_lect <- round(mean(suppressWarnings(as.numeric(ba$prom_lect2m_rbd)), na.rm=TRUE))
      prom_mate <- round(mean(suppressWarnings(as.numeric(ba$prom_mate2m_rbd)), na.rm=TRUE))
      n_simce   <- sum(!is.na(suppressWarnings(as.numeric(ba$prom_lect2m_rbd))))
      ive_prom  <- round(mean(suppressWarnings(as.numeric(ba$IVE)), na.rm=TRUE), 1)
      idps_lbl  <- c("Autoestima académica y motivación", "Clima de convivencia",
                     "Participación y formación ciudadana", "Hábitos de vida saludable")
      idps_prom <- sapply(1:4, function(i) round(mean(suppressWarnings(as.numeric(ba[[paste0("IDPS",i,"_Puntaje")]])), na.rm=TRUE)))
      idps_txt  <- paste(paste0(idps_lbl, ": ", idps_prom), collapse="; ")
      docs$stats_indicadores <- list(
        id = "stats_indicadores",
        titulo = "Indicadores de Establecimientos EMTP: SIMCE, IDPS, IVE y GSE 2025",
        cuerpo = paste(
          paste0("SIMCE 2° medio 2025 (promedio entre ", format(n_simce, big.mark="."),
                 " establecimientos EMTP con dato): Lectura ", prom_lect, " puntos, Matemática ", prom_mate, " puntos."),
          "El SIMCE clasifica a los estudiantes en tres estándares de aprendizaje: Insuficiente, Elemental y Adecuado.",
          paste0("IVE promedio (Índice de Vulnerabilidad Escolar) de los establecimientos EMTP: ", ive_prom, "%."),
          "El GSE (grupo socioeconómico de la Agencia de Calidad) clasifica a los establecimientos en: Bajo, Medio Bajo, Medio, Medio Alto y Alto.",
          paste0("IDPS — Indicadores de Desarrollo Personal y Social, puntaje promedio por dimensión: ", idps_txt, "."),
          "Estos indicadores están disponibles por establecimiento (RBD) y agregados por territorio en las pestañas Establecimientos y Análisis Territorial."
        )
      )
    }, error=function(e) NULL)
  }

  docs$stats_rotacion <- list(
    id = "stats_rotacion",
    titulo = "Estadísticas Reales de Rotación Docente EMTP 2021-2025",
    cuerpo = paste(
      "Número de docentes de especialidad EMTP en 2025: 6.066 personas únicas.",
      "Número de establecimientos analizados: 969-970.",
      "Tasa de salida anual del sistema: entre 14.5% y 18.5% cada año entre 2021 y 2025.",
      "Salidas en el último período (2024→2025): 938 docentes.",
      "De las salidas 2024→2025: 841 personas (90%) tienen menos de 65 años.",
      "Balance neto negativo en los dos últimos períodos: salieron más de los que ingresaron.",
      "Salida acumulada sin retorno (2021-2024): 3.705 docentes no aparecen en 2025.",
      "Del total de salidas definitivas: 88% son menores de 65 años.",
      "Solo el 32% de docentes de especialidad tiene título en Educación en 2025.",
      "Esa proporción bajó desde 36.2% en 2021 (descenso sostenido año a año).",
      "Rotación de establecimiento (movilidad): 2-3% de docentes activos cambia de establecimiento por año."
    )
  )

  docs
}

# =============================================================================
# 2.5. CARGA DE DOCUMENTOS DESDE CARPETA data/rag_docs/
# =============================================================================
load_rag_docs_folder <- function(path="data/rag_docs") {
  if (!dir.exists(path)) return(list())
  files <- list.files(path, pattern="\\.(txt|md)$", full.names=TRUE)
  files <- files[!grepl("README", basename(files), ignore.case=TRUE)]
  # Documentos excluidos temporalmente (pendiente de revisión)
  files <- files[!grepl("rex.?1080|REX.?1080|alternancia|Alternancia|orientaciones.?rex|Orientaciones.?rex",
                         basename(files), ignore.case=TRUE)]
  if (length(files) == 0) {
    cat("[RAG] Carpeta", path, "sin documentos .txt/.md (solo README)\n")
    return(list())
  }
  docs <- list()
  for (f in files) {
    tryCatch({
      id    <- gsub("[^a-zA-Z0-9_]", "_", tools::file_path_sans_ext(basename(f)))
      lines <- readLines(f, encoding="UTF-8", warn=FALSE)
      text  <- paste(lines, collapse="\n")
      if (nchar(trimws(text)) < 30) next
      # Título: primera línea no vacía
      titulo <- trimws(lines[which(nchar(trimws(lines)) > 0)[1]])
      titulo <- gsub("^#+\\s*", "", titulo)   # quitar marcas Markdown
      if (nchar(titulo) > 100) titulo <- substr(titulo, 1, 100)
      # Dividir en chunks de ~2000 chars
      chunks <- character(0)
      remaining <- text
      while (nchar(remaining) > 0) {
        if (nchar(remaining) <= 2200) { chunks <- c(chunks, remaining); break }
        pos <- regexpr("\\n\\n", substr(remaining, 1800, 2200))
        cut_at <- if (pos > 0) 1800 + pos - 1 else 2000
        chunks    <- c(chunks, substr(remaining, 1, cut_at))
        remaining <- trimws(substr(remaining, cut_at + 1, nchar(remaining)))
      }
      for (i in seq_along(chunks)) {
        cid <- if (length(chunks) > 1) paste0(id, "_p", i) else id
        docs[[cid]] <- list(
          id     = cid,
          titulo = if (length(chunks) > 1) paste0(titulo, " (parte ", i, ")") else titulo,
          cuerpo = chunks[[i]]
        )
      }
      cat("[RAG] Doc cargado:", basename(f), "-", length(chunks), "chunk(s)\n")
    }, error=function(e) cat("[RAG] Error en", basename(f), ":", conditionMessage(e), "\n"))
  }
  cat("[RAG] Total docs desde carpeta:", length(docs), "\n")
  docs
}

combine_knowledge_bases <- function(kb_dinamica=list(), docs_folder="data/rag_docs") {
  kb_folder <- load_rag_docs_folder(docs_folder)
  c(rag_kb_estatica, kb_dinamica, kb_folder)
}

# =============================================================================
# 3. TF-IDF + RETRIEVAL
# =============================================================================
tokenize_text <- function(text) {
  text <- tolower(gsub("[^a-záéíóúüñ0-9%.]", " ", text))
  tokens <- unlist(strsplit(text, "\\s+"))
  tokens <- tokens[nchar(tokens) > 2]
  sw <- c("que","con","para","por","los","las","del","una","son","sus","este",
          "esta","hay","como","pero","sin","sobre","entre","cuando","tiene",
          "puede","cada","desde","hasta","solo","ser","fue","han","sido")
  tokens[!tokens %in% sw]
}

build_tfidf_index <- function(kb) {
  docs_text  <- lapply(kb, function(d) paste(d$titulo, d$cuerpo))
  all_tokens <- lapply(docs_text, tokenize_text)
  all_terms  <- sort(unique(unlist(all_tokens)))
  N <- length(docs_text)
  tf_matrix  <- matrix(0, nrow=length(all_terms), ncol=N,
                       dimnames=list(all_terms, names(kb)))
  for (j in seq_along(all_tokens)) {
    tab     <- table(all_tokens[[j]])
    matched <- intersect(names(tab), all_terms)
    if (length(matched) > 0)
      tf_matrix[matched, j] <- as.numeric(tab[matched]) / length(all_tokens[[j]])
  }
  df_counts <- rowSums(tf_matrix > 0)
  idf       <- log((N+1)/(df_counts+1)) + 1
  list(tfidf=tf_matrix*idf, terms=all_terms, idf=idf,
       doc_ids=names(kb), doc_meta=lapply(kb, function(d) list(id=d$id, titulo=d$titulo)))
}

retrieve_docs_with_kb <- function(query, index, kb, top_k=3) {
  qtokens <- tokenize_text(query)
  qvec    <- setNames(numeric(length(index$terms)), index$terms)
  qtab    <- table(qtokens)
  matched <- intersect(names(qtab), index$terms)
  if (length(matched) > 0) {
    qvec[matched] <- as.numeric(qtab[matched]) / length(qtokens)
    qvec          <- qvec * index$idf
  }
  norms_docs <- sqrt(colSums(index$tfidf^2))
  norm_q     <- sqrt(sum(qvec^2))
  if (norm_q == 0) return(list(docs=list(), scores=numeric(0)))
  scores  <- colSums(index$tfidf * qvec) / (norms_docs * norm_q + 1e-10)
  ranked  <- order(scores, decreasing=TRUE)
  top_idx <- ranked[seq_len(min(top_k, length(ranked)))]
  # Umbral absoluto (0.05) Y umbral relativo (≥40% del score del primer resultado)
  # Esto descarta chunks que solo coinciden de forma marginal
  abs_thresh <- 0.05
  top_score  <- scores[top_idx[1]]
  rel_thresh <- 0.40 * top_score
  min_thresh <- max(abs_thresh, rel_thresh)
  top_idx <- top_idx[scores[top_idx] >= min_thresh]
  cat("[RAG] Scores recuperados:",
      paste0(round(scores[top_idx], 3), collapse=", "),
      "| umbral:", round(min_thresh, 3), "\n")
  docs_retrieved <- lapply(top_idx, function(i) {
    did <- index$doc_ids[i]
    list(id=did, titulo=kb[[did]]$titulo, cuerpo=kb[[did]]$cuerpo, score=round(scores[i],4))
  })
  list(docs=docs_retrieved, scores=scores[top_idx])
}

# =============================================================================
# 3.5. MOTOR DE CONSULTA DIRECTA SOBRE DATOS
# =============================================================================

# Normalizar texto a ASCII sin acentos para comparaciones
.norm <- function(x) {
  s <- tolower(iconv(as.character(x), to="ASCII//TRANSLIT"))
  # macOS iconv translitea los acentos como signos sueltos ANTES de la vocal:
  # "í"→"'i", "ó"→"'o", "ñ"→"~n", etc. Hay que eliminar esos artefactos para que
  # "matrícula"→"matricula", "ÑUÑOA"→"nunoa", "geología"→"geologia".
  gsub("[~^`'\"]", "", s)
}

# ¿La pregunta es sobre NORMATIVA (decretos/leyes)? → preferir RAG documental,
# nunca el motor de datos (evita que "¿qué dice el Decreto 452?" devuelva matrícula).
.is_normativa_query <- function(q) {
  grepl(paste0("decreto|\\bley\\b|\\blge\\b|articulo|art\\.|normativ|reglament|",
               "bases curricular|curricul|\\bdfl\\b|circular"),
        q, ignore.case=FALSE)
}

# Preguntas FUERA DEL ALCANCE de los datos del chatbot. Devuelve un mensaje
# honesto (sin inventar) o NULL. Cubre: (1) tendencias/serie histórica, que el
# chatbot no tiene (solo 2025 / egresados 2024); (2) opiniones y causalidad.
.scope_note <- function(qn) {
  if (grepl(paste0("como ha (cambiado|evolucionado|variado)|evolucion|tendencia|historic|",
                   "a lo largo|ultimos? anos|ultimos? años|serie de tiempo|de 20\\d\\d a 20\\d\\d|",
                   "20\\d\\d-20\\d\\d|crecimiento de la matricula|proyeccion|hace \\d+ anos|",
                   "ano anterior|anos anteriores"), qn))
    return(paste0(
      "Trabajo con datos de matrícula y docentes 2025 (y egresados 2024); no tengo la ",
      "serie histórica para calcular tendencias. Para la evolución 2018-2025, descarga los ",
      "Reportes Históricos en la pestaña Inicio."))
  if (grepl(paste0("\\bpor que\\b|mejor especialidad|peor especialidad|cual es la mejor|",
                   "cual es el mejor|me recomiendas|recomienda|deberia|conviene|tu opinion|",
                   "que opinas|es bueno estudiar|vale la pena"), qn))
    return(paste0(
      "Entrego datos descriptivos del sistema EMTP, no opiniones ni explicaciones causales. ",
      "Puedo darte cifras concretas: por ejemplo, matrícula o docentes por región, ",
      "dependencia, especialidad o establecimiento."))
  NULL
}

# ¿Es la pregunta una consulta cuantitativa / sobre datos?
# MEJORA: (1) excluye explícitamente normativa; (2) amplía verbos coloquiales
# ("cuéntame", "muéstrame", "info sobre", "compara"...); (3) añade una señal
# ESTRUCTURAL: si la pregunta nombra una entidad de datos (docente, estudiante,
# liceo, egresado, especialidad) la tratamos como consulta de datos aunque no
# use un verbo cuantitativo. Esto reduce falsos negativos del dispatcher.
.is_data_query <- function(q) {
  if (.is_normativa_query(q)) return(FALSE)
  quant <- grepl(paste0(
    "cuanto|cuantos|cuanta|cuantas|numero|total|cantidad|hay |tiene |tienen |",
    "cuales|cual |porcentaje|porcent|%|ofrecen|ofrecidos|oferta|\\bdame|\\bdime\\b|",
    "cuentame|cuenta |muestra|muestrame|listame|lista |distribucion|desglose|",
    "promedio|compara|comparar|comparacion|diferencia|versus|\\bvs\\b|que pasa con"),
    q, ignore.case=FALSE)
  ent <- grepl(paste0(
    "docente|profesor|maestro|educador|estudiante|alumno|alumna|matricula|",
    "establecimiento|colegio|liceo|escuela|egresado|titulado|titulaci|graduado|",
    "practicante|especialidad|asistencia|inasistencia|sector economic|sector productiv|rubro"),
    q, ignore.case=FALSE)
  quant || ent
}

# Detectar intención de género:
# "solo_m" = solo mujeres, "solo_h" = solo hombres,
# "breakdown" = ambos (desglose), NULL = sin filtro
.detect_genero <- function(q) {
  # Límite de palabra en nino/nina: "femenino" contiene "nino" y "femenina"
  # contiene "nina" → sin \\b daban falso "breakdown" en preguntas de género.
  has_m <- grepl("mujer|femenin|\\bnina|chica", q)
  has_h <- grepl("hombre|mascul|varon|\\bnino|chico", q)
  if (has_m && has_h) return("breakdown")
  if (has_m)          return("solo_m")
  if (has_h)          return("solo_h")
  NULL
}

# Intención sobre RURALIDAD. Distingue tres casos cuando aparecen rural y urbano:
#   "both"    = UNIÓN ("tanto rurales como urbanos", "considerando ambos") → SIN filtro
#   "compare" = COMPARACIÓN ("rurales o urbanos?", "compara", "más/menos")
#   "rural"/"urbano" = filtro simple ; "none" = no se menciona
# Antes, mencionar ambos disparaba siempre la comparación (bug: "considerando
# rurales y urbanos" se interpretaba como comparar, no como total).
.ruralidad_intent <- function(q) {
  qn <- .norm(q)
  has_r <- grepl("\\brural", qn); has_u <- grepl("\\burbano", qn)
  if (!has_r && !has_u) return("none")
  if (has_r && has_u) {
    # 1) Verbo de comparación EXPLÍCITO gana siempre ("compara rurales y urbanos")
    if (grepl("compar|versus|\\bvs\\b|diferencia", qn)) return("compare")
    # 2) Frase de UNIÓN → total, sin filtro ("tanto rurales como urbanos")
    union_kw <- grepl(paste0(
      "tanto .* como|\\bambos\\b|\\bambas\\b|considerando|incluyendo|sumando|",
      "juntos|juntas|en total|en conjunto|rurales y urbanos|urbanos y rurales|",
      "rurales como urbanos|urbanos como rurales|y los urbanos|y los rurales|",
      "tanto rurales|tanto urbanos|sin distincion|sin distinguir"), qn)
    if (union_kw) return("both")
    # 3) Cue débil de comparación ("rurales o urbanos?", "más/menos")
    if (grepl("\\bo\\b|\\bmas\\b|\\bmenos\\b|mayor|menor", qn)) return("compare")
    return("both")   # por defecto, mencionar ambos = unión (no comparación)
  }
  if (has_r) "rural" else "urbano"
}

# Intención GENÉRICA cuando se mencionan VARIOS valores de una misma dimensión
# (regiones, dependencias, especialidades...). Devuelve:
#   "compare" = comparar entre sí ("A vs B", "más en A o B", "compara")
#   "union"   = sumarlos / tratarlos juntos ("A y B", "tanto A como B", "ambos")
# Por defecto, mencionar dos valores SIN señal de comparación = unión.
.combo_intent <- function(qn) {
  if (grepl("compar|versus|\\bvs\\b|diferencia|\\bcontra\\b|frente a", qn)) return("compare")
  union_kw <- grepl(paste0(
    "tanto .* como|\\bambos\\b|\\bambas\\b|considerando|incluyendo|sumando|sumad|",
    "juntos|juntas|en total|en conjunto|combinad|en su conjunto|",
    "sin distincion|sin distinguir|todas las|todos los|ya sea"), qn)
  if (union_kw) return("union")
  if (grepl("\\bo\\b|\\bmas\\b|\\bmenos\\b|mayor|menor|cual tiene|quien tiene|cual de", qn))
    return("compare")
  "union"
}

# Detecta los GRADOS mencionados (unión). cod_grado: 3=1°,4=2°,5=3°,6=4° medio.
# Solo actúa si la pregunta tiene contexto de grado/medio (evita números sueltos).
.match_grados <- function(q) {
  qn <- .norm(q)
  if (!grepl("medi[oa]|grado|\\bem\\b|\\bnivel\\b", qn)) return(integer(0))
  codes <- integer(0)
  if (grepl("primer|\\b1 ?(medio|ro|er|°|º)", qn)) codes <- c(codes, 3L)
  if (grepl("segundo|\\b2 ?(medio|do|°|º)", qn))   codes <- c(codes, 4L)
  if (grepl("tercer|\\b3 ?(medio|ro|er|°|º)", qn)) codes <- c(codes, 5L)
  if (grepl("cuarto|\\b4 ?(medio|to|°|º)", qn))    codes <- c(codes, 6L)
  unique(codes)
}

# Detecta tipo de enseñanza / población. CONSERVADOR: "jóvenes" a secas suele
# significar "estudiantes" (no la modalidad), así que solo filtra "jóvenes"
# cuando se dice regular/diurna; "adultos/EPJA/vespertina" sí es claro.
#   "adultos" | "jovenes" | "both" | NULL
.match_poblacion <- function(q) {
  qn <- .norm(q)
  has_a <- grepl("adulto|\\bepja\\b|vespertin|nocturn", qn)
  has_j <- grepl("jovenes? regular|ensenanza regular|modalidad regular|diurn|\\bregular(es)?\\b", qn)
  if (has_a && has_j) return("both")
  if (has_a) return("adultos")
  if (has_j) return("jovenes")
  NULL
}

# Buscar coincidencia de especialidad en la pregunta (fuzzy sobre nom_espe)
.match_specialty <- function(q, esp_names) {
  if (is.null(esp_names) || length(esp_names) == 0) return(NULL)
  esps      <- unique(esp_names[!is.na(esp_names)])
  esps_norm <- .norm(esps)
  words     <- strsplit(q, "\\s+")[[1]]
  words     <- words[nchar(words) >= 4]
  # Priorizar coincidencias exactas de palabras más largas primero
  words     <- words[order(-nchar(words))]
  for (w in words) {
    hits <- which(grepl(w, esps_norm, fixed=TRUE))
    if (length(hits) > 0) return(esps[hits[1]])
  }
  NULL
}

# Palabras genéricas que NO deben matchear nombres de especialidad (no incluye
# términos que sí son parte de especialidades: construccion, administracion, etc.)
.esp_stopwords <- c(
  "cuantos","cuantas","cuanta","cuanto","estudiantes","estudiante","alumnos","alumno","alumnas",
  "matricula","matriculados","docentes","docente","profesores","profesor","liceos","liceo",
  "colegios","colegio","escuelas","escuela","establecimientos","establecimiento","plantel",
  "region","regiones","comuna","comunas","deprov","rural","rurales","urbano","urbanos","ruralidad",
  "dependencia","municipal","municipales","particular","particulares","subvencionado","subvencionados",
  "pagado","pagados","privado","privados","corporacion","slep","chile","pais","nacional","total",
  "mayor","menor","mayores","menores","sostenedor","sostenedores","genero","sexo","mujeres","hombres",
  "cuales","tienen","tiene","ofrecen","ofrecidos","oferta","ofrece","especialidad","especialidades",
  "considerando","ambos","ambas","tanto","como","juntos","juntas","medio","grado","grados",
  "jovenes","adultos","primero","segundo","tercero","cuarto","quinto","dame","dime","muestrame",
  "puedes","decirme","nombrame","listame","nivel","niveles","ensenanza","cantidad","numero",
  "porcentaje","promedio","mas","menos","entre","sobre","bajo","tecnico","tecnicos","tecnica",
  "profesional","profesionales","quienes","donde","entre","para","esos","esas","estos","estas"
)

# NUEVO: devuelve TODAS las especialidades mencionadas (unión de 2 o más).
# Ej: "matrícula de Electricidad y Mecánica" → c("Electricidad", "Mecánica ...").
# Una palabra puede mapear a varias especialidades (ej. "mecanica" → todas las
# de mecánica); se incluyen todas (intención de unión).
.match_all_specialties <- function(q, esp_names) {
  if (is.null(esp_names) || length(esp_names) == 0) return(character(0))
  esps      <- unique(esp_names[!is.na(esp_names)])
  esps_norm <- .norm(esps)
  qn        <- .norm(q)
  words     <- strsplit(gsub("[^a-z ]", " ", qn), "\\s+")[[1]]
  words     <- words[nchar(words) >= 4 & !(words %in% .esp_stopwords)]
  words     <- words[order(-nchar(words))]
  hits <- character(0)
  for (w in words) {
    idx <- which(grepl(w, esps_norm, fixed=TRUE))
    if (length(idx)) hits <- c(hits, esps[idx])
  }
  unique(hits)
}

# Buscar coincidencia de comuna (fuzzy sobre nom_com_rbd)
.match_comuna <- function(q, com_names) {
  if (is.null(com_names) || length(com_names) == 0) return(NULL)
  coms      <- unique(com_names[!is.na(com_names)])
  coms_norm <- .norm(coms)
  q_norm    <- .norm(q)   # normalizar query a ASCII minúsculas

  # --- Estrategia 1: patrón explícito "(en la |la )?comuna de XXXX" ---
  m <- regmatches(q_norm, regexpr("(?:en |la |del? )?comuna de ([a-z][a-z ]{2,30}?)(?:\\s*[\\?.,;]|$)",
                                  q_norm, perl=TRUE))
  if (length(m) > 0) {
    com_candidate <- trimws(sub(".*comuna de ", "", m[[1]]))
    com_candidate <- gsub("[^a-z ]", "", com_candidate)
    # intentar palabra completa, luego solo primera palabra
    for (candidate in c(com_candidate, strsplit(com_candidate, " ")[[1]][1])) {
      if (nchar(candidate) >= 3) {
        hits <- which(grepl(candidate, coms_norm, fixed=TRUE))
        if (length(hits) > 0) return(coms[hits[1]])
      }
    }
  }

  # --- Estrategia 2: búsqueda por palabras (normalizadas, sin stopwords) ---
  q_clean <- gsub("[^a-z ]", " ", q_norm)    # eliminar puntuación
  words   <- strsplit(q_clean, "\\s+")[[1]]
  words   <- words[nchar(words) >= 4]
  words   <- words[order(-nchar(words))]
  # Stopwords: genéricas + nombres de especialidades EMTP más comunes
  stopwords_com <- c(
    # Referencia NACIONAL: "en chile", "del país"... = SIN filtro de comuna.
    # (Evita que "chile" matchee la comuna real "Chile Chico".)
    "chile","pais","nacion","nacionalmente","nacionales",
    # Macro-geografía (no son comunas): evita p.ej. "central" → "Estacion Central"
    "central","centro","norte","sur","oriente","poniente","cordillera","costa","valle",
    # Entidades y filtros (no son comunas)
    "liceo","liceos","colegio","colegios","escuela","escuelas",
    "establecimiento","establecimientos","plantel","planteles","recinto",
    "rural","rurales","urbano","urbanos","ruralidad",
    "egresado","egresados","titulado","titulados","matricula","matriculados",
    "puedes","podrias","decirme","dime","cuentame","muestrame","nombra","lista",
    "existen","cantidad","aproximadamente","actualmente","ofrecen","oferta",
    "cuantos","cuantas","cuanta","cuanto","total","numero",
    "hombres","mujeres","hombre","mujer","genero","sexo",
    "estudiante","estudiantes","alumno","alumnos","alumna",
    "docente","docentes","profesor","profesores",
    "especialidad","educacion","tecnico","tecnicos",
    "tiene","tienen","como","cual","donde","cuales",
    "para","esta","este","esos","esas","todo","todos",
    "nivel","sector","zona","area","barrio","distrito",
    "region","regional","provincial","nacional","local",
    "administracion","contabilidad","electronica","mecanica",
    "construccion","programacion","gastronomia","acuicultura",
    "enfermeria","deportes","turismo","agricultura","mineria",
    "automotriz","telecomunicaciones","vestuario","madera",
    "quimica","pesca","soldadura","electricidad","logistica",
    "computacion","informatica","sistemas","redes","seguridad",
    "maritimo","aeronautica","forestal","industrial","comercio"
  )
  words <- words[!words %in% stopwords_com]
  for (w in words) {
    hits <- which(grepl(w, coms_norm, fixed=TRUE))
    if (length(hits) > 0) return(coms[hits[1]])
  }
  NULL
}

# Mapear keywords a patrón de búsqueda en nombre de región
# Buscar DEPROV en la query contra los valores reales de nom_deprov_rbd
.match_deprov <- function(q, deprov_names) {
  if (is.null(deprov_names) || length(deprov_names) == 0) return(NULL)
  # Activa solo si la query menciona indicador de DEPROV o provincia
  if (!grepl("deprov|departamento provincial|provinc", q)) return(NULL)
  deprovs      <- unique(deprov_names[!is.na(deprov_names) & nzchar(deprov_names)])
  deprovs_norm <- .norm(deprovs)
  q_norm       <- .norm(q)
  # Subprovinciales de Santiago: "santiago centro", "santiago norte", etc.
  for (suf in c("centro","norte","sur","oriente","poniente")) {
    if (grepl(paste0("santiago.{0,5}",suf), q_norm, perl=TRUE)) {
      idx <- which(grepl(paste0("SANTIAGO.*",toupper(suf)), deprovs, ignore.case=TRUE))
      if (length(idx) > 0) return(deprovs[idx[1]])
    }
  }
  # Match general: cada deprov puede tener nombre compuesto "A - B"; buscar cada parte
  for (i in seq_along(deprovs_norm)) {
    parts <- trimws(strsplit(deprovs_norm[i], " - ")[[1]])
    if (any(sapply(parts, function(p) nchar(p) >= 4 && grepl(p, q_norm, fixed=TRUE)))) {
      return(deprovs[i])
    }
  }
  NULL
}

.match_region <- function(q) {
  # pat = abreviatura real en nom_reg_rbd_a/NOM_REG_RBD_A
  # lbl = nombre completo para mostrar
  # Solo se incluyen nombres de REGIONES — no nombres de comunas.
  # Los nombres de comunas (valdivia, temuco, concepcion, talca, etc.)
  # deben caer al nivel de búsqueda de comuna, no de región.
  map <- list(
    list(keys=c("tarapaca","region de tarapaca"),                    pat="TPCA",  lbl="Tarapac\u00e1"),
    list(keys=c("antofagasta","region de antofagasta"),              pat="ANTOF", lbl="Antofagasta"),
    list(keys=c("atacama","region de atacama"),                      pat="ATCMA", lbl="Atacama"),
    list(keys=c("coquimbo","region de coquimbo"),                    pat="COQ",   lbl="Coquimbo"),
    list(keys=c("valparaiso","region de valparaiso"),                pat="VALPO", lbl="Valpara\u00edso"),
    list(keys=c("metropolitana","region metropolitana","\\brm\\b"), pat="RM",    lbl="Metropolitana"),
    list(keys=c("ohiggins","higgins","region de ohiggins"),          pat="LGBO",  lbl="O'Higgins"),
    list(keys=c("maule","region del maule"),                         pat="MAULE", lbl="Maule"),
    list(keys=c("nuble","region de nuble"),                          pat="NUBLE", lbl="\u00d1uble"),
    list(keys=c("biobio","region del biobio","region de biobio"),    pat="BBIO",  lbl="Biob\u00edo"),
    list(keys=c("araucania","la araucania","region de la araucania"),pat="ARAUC", lbl="La Araucan\u00eda"),
    list(keys=c("los rios","region de los rios"),                    pat="RIOS",  lbl="Los R\u00edos"),
    list(keys=c("los lagos","region de los lagos"),                  pat="LAGOS", lbl="Los Lagos"),
    list(keys=c("aysen","region de aysen"),                          pat="AYSEN", lbl="Ays\u00e9n"),
    list(keys=c("magallanes","region de magallanes"),                pat="MAG",   lbl="Magallanes"),
    list(keys=c("arica","arica y parinacota"),                       pat="AYP",   lbl="Arica y Parinacota")
  )
  for (m in map) {
    if (any(sapply(m$keys, function(k) grepl(k, q, perl=TRUE)))) return(m)
  }
  NULL
}

# =============================================================================
# NUEVO (mejora): helpers para multi-region, contexto persistente y comparacion.
# Se agregan SIN tocar .match_region original para no romper logica existente.
# =============================================================================

# Mapa de regiones (mismas keys/pat que .match_region) accesible a nivel superior,
# para poder detectar TODAS las regiones citadas (comparacion region-vs-region).
.region_map <- list(
  list(keys=c("tarapaca","region de tarapaca"),                    pat="TPCA",  lbl="Tarapaca"),
  list(keys=c("antofagasta","region de antofagasta"),              pat="ANTOF", lbl="Antofagasta"),
  list(keys=c("atacama","region de atacama"),                      pat="ATCMA", lbl="Atacama"),
  list(keys=c("coquimbo","region de coquimbo"),                    pat="COQ",   lbl="Coquimbo"),
  list(keys=c("valparaiso","region de valparaiso"),                pat="VALPO", lbl="Valparaiso"),
  list(keys=c("metropolitana","region metropolitana","\\brm\\b"),  pat="RM",    lbl="Metropolitana"),
  list(keys=c("ohiggins","higgins","region de ohiggins"),          pat="LGBO",  lbl="O'Higgins"),
  list(keys=c("maule","region del maule"),                         pat="MAULE", lbl="Maule"),
  list(keys=c("nuble","region de nuble"),                          pat="NUBLE", lbl="Nuble"),
  list(keys=c("biobio","region del biobio","region de biobio"),    pat="BBIO",  lbl="Biobio"),
  list(keys=c("araucania","la araucania","region de la araucania"),pat="ARAUC", lbl="La Araucania"),
  list(keys=c("los rios","region de los rios"),                    pat="RIOS",  lbl="Los Rios"),
  list(keys=c("los lagos","region de los lagos"),                  pat="LAGOS", lbl="Los Lagos"),
  list(keys=c("aysen","region de aysen"),                          pat="AYSEN", lbl="Aysen"),
  list(keys=c("magallanes","region de magallanes"),                pat="MAG",   lbl="Magallanes"),
  list(keys=c("arica","arica y parinacota"),                       pat="AYP",   lbl="Arica y Parinacota")
)

# Devuelve TODAS las regiones citadas, ordenadas por posicion en el texto.
.match_all_regions <- function(q) {
  hits <- list()
  for (m in .region_map) {
    pos <- vapply(m$keys, function(k) {
      r <- regexpr(k, q, perl=TRUE); if (r > 0) as.integer(r) else NA_integer_
    }, integer(1))
    if (any(!is.na(pos))) { m$pos <- min(pos, na.rm=TRUE); hits[[length(hits)+1]] <- m }
  }
  if (length(hits) == 0) return(list())
  hits[order(vapply(hits, function(m) m$pos, integer(1)))]
}

# Aplica el filtro de región soportando UNIÓN (varias regiones con %in%).
# Detecta la columna (minúscula o mayúscula). Devuelve list(df, filters, matched).
# Si la consulta es una COMPARACIÓN de regiones, eso ya se resolvió antes en el
# dispatcher, así que aquí siempre se filtra por la unión de las mencionadas.
.apply_region_filter <- function(q, df, filters) {
  col <- intersect(c("nom_reg_rbd_a","NOM_REG_RBD_A"), names(df))[1]
  if (is.na(col)) return(list(df=df, filters=filters, matched=FALSE))
  regs <- .match_all_regions(q)
  if (length(regs) == 0) return(list(df=df, filters=filters, matched=FALSE))
  pats <- vapply(regs, function(m) m$pat, character(1))
  lbls <- paste(vapply(regs, function(m) m$lbl, character(1)), collapse=", ")
  df <- df[!is.na(df[[col]]) & df[[col]] %in% pats, ]
  list(df=df, filters=c(filters, paste0("región: ", lbls)), matched=TRUE)
}

.match_dependency <- function(q) {
  # UNIÓN: acumula TODAS las dependencias mencionadas (antes devolvía solo la
  # primera, perdiendo "municipal Y particular subvencionado"). El filtro de
  # las .query_* usa %in%, así que un vector con varios códigos funciona directo.
  codes <- integer(0)
  if (grepl("slep|servicio local", q))                               codes <- c(codes, 5L)
  if (grepl("municipal|daem|\\bpublic[oa]s?\\b|estatal", q))         codes <- c(codes, 1L, 4L, 5L)
  if (grepl("particular subvencionado|part.*subv|subvencionado", q)) codes <- c(codes, 2L)
  if (grepl("particular pagado|part.*pag|privado|\\bpagados?\\b", q)) codes <- c(codes, 3L)
  if (grepl("corporacion|corem", q))                                 codes <- c(codes, 4L)
  # "particular" solo o "particulares" → Part. Subvencionado (el más común),
  # solo si no se detectó ya una dependencia particular más específica.
  if (!length(codes) && grepl("\\bparticulares?\\b", q))             codes <- c(codes, 2L)
  if (!length(codes)) return(NULL)
  unique(codes)
}

# Buscar nombre de SLEP específico en la query (sobre columna nombre_slep)
.match_slep_nombre <- function(q, slep_names) {
  if (is.null(slep_names) || length(slep_names) == 0) return(NULL)
  sleps      <- unique(slep_names[!is.na(slep_names) & nzchar(slep_names)])
  sleps_norm <- .norm(sleps)
  q_norm     <- .norm(q)
  # Solo buscar si la query menciona "slep" o "servicio local"
  if (!grepl("slep|servicio local", q_norm)) return(NULL)
  # Quitar stopwords de la query, buscar coincidencia
  q_clean <- gsub("slep|servicio local de educacion|servicio local|cuantos|cuantas|
    estudiantes|hay|tiene|del|los|las|en|de|el|la|establecimientos", " ", q_norm)
  words <- strsplit(trimws(gsub("  +"," ",q_clean)), " ")[[1]]
  words <- words[nchar(words) >= 4]
  for (w in words) {
    hits <- which(grepl(w, sleps_norm, fixed=TRUE))
    if (length(hits) > 0) return(sleps[hits[1]])
  }
  NULL
}

# Línea de resumen de género siempre presente (con desglose)
.gender_summary <- function(df) {
  nh <- sum(!is.na(df$gen_alu) & df$gen_alu == 1)
  nm <- sum(!is.na(df$gen_alu) & df$gen_alu == 2)
  n  <- nh + nm
  if (n == 0) return("")
  paste0(
    "  Hombres: ", format(nh, big.mark="."),
    " (", round(100*nh/n, 1), "%)\n",
    "  Mujeres: ", format(nm, big.mark="."),
    " (", round(100*nm/n, 1), "%)\n"
  )
}

# ── Consulta sobre matrícula ─────────────────────────────────────────────────
.query_matricula <- function(q, df) {
  filters <- character(0)
  genero  <- .detect_genero(q)

  # Aplicar filtro de género SOLO si es específico (no breakdown)
  if (!is.null(genero) && genero == "solo_m") {
    df <- df[!is.na(df$gen_alu) & df$gen_alu == 2, ]
    filters <- c(filters, "mujeres")
  } else if (!is.null(genero) && genero == "solo_h") {
    df <- df[!is.na(df$gen_alu) & df$gen_alu == 1, ]
    filters <- c(filters, "hombres")
  }
  # Si genero=="breakdown" NO filtramos → mostramos desglose completo

  # Especialidad (UNIÓN de 2 o más: "Electricidad y Mecánica") — defensivo.
  # Se quita "asistencia/inasistencia" del texto para no chocar con la especialidad
  # "Asistencia en Geología" cuando la pregunta es sobre asistencia escolar.
  if ("nom_espe" %in% names(df)) {
    q_esp <- gsub("asistencia|inasistencia", " ", q)
    esps_match <- tryCatch(.match_all_specialties(q_esp, df$nom_espe), error=function(e) character(0))
    if (length(esps_match) > 0) {
      sel <- tryCatch(!is.na(df$nom_espe) & .norm(df$nom_espe) %in% .norm(esps_match),
                      error=function(e) NULL)
      if (!is.null(sel)) {
        df <- df[sel, ]
        filters <- c(filters, paste0("especialidad: ", paste(esps_match, collapse=", ")))
      }
    }
  }

  # Grado (unión: "3° y 4° medio") — cod_grado 3=1°,4=2°,5=3°,6=4° — defensivo
  grados <- tryCatch(.match_grados(q), error=function(e) integer(0))
  if (length(grados) > 0 && "cod_grado" %in% names(df)) {
    sel <- tryCatch({ cg <- suppressWarnings(as.numeric(as.character(df$cod_grado))); !is.na(cg) & cg %in% grados },
                    error=function(e) NULL)
    if (!is.null(sel)) {
      df <- df[sel, ]
      grd_lbl <- c("3"="1°","4"="2°","5"="3°","6"="4°")
      filters <- c(filters, paste0("grado: ", paste(grd_lbl[as.character(grados)], collapse=", "), " medio"))
    }
  }

  # Tipo de enseñanza / población (jóvenes regular vs adultos/EPJA) — defensivo
  pob <- tryCatch(.match_poblacion(q), error=function(e) NULL)
  if (!is.null(pob) && pob %in% c("jovenes","adultos") && "cod_ense" %in% names(df)) {
    sel <- tryCatch({
      ce <- suppressWarnings(as.numeric(as.character(df$cod_ense)))
      if (pob == "adultos") (!is.na(ce) & ce %% 100 == 63) else (!is.na(ce) & ce %% 100 == 10)
    }, error=function(e) NULL)
    if (!is.null(sel)) {
      df <- df[sel, ]
      filters <- c(filters, if (pob == "adultos") "EPJA/adultos" else "enseñanza regular (jóvenes)")
    }
  }

  # Región (tiene prioridad sobre comuna; soporta unión de varias regiones) — defensivo
  rf <- tryCatch(.apply_region_filter(q, df, filters),
                 error=function(e) list(df=df, filters=filters, matched=FALSE))
  df <- rf$df; filters <- rf$filters
  if (!rf$matched && "nom_com_rbd" %in% names(df)) {
    # Solo buscar comuna si no encontramos región
    com <- tryCatch(.match_comuna(q, df$nom_com_rbd), error=function(e) NULL)
    if (!is.null(com)) {
      com_norm <- .norm(com)
      df <- df[!is.na(df$nom_com_rbd) & grepl(com_norm, .norm(df$nom_com_rbd), fixed=TRUE), ]
      # Etiqueta explícita: el filtro es por la UBICACIÓN DEL ESTABLECIMIENTO
      filters <- c(filters, paste0("establecimientos en la comuna de ", com))
    } else {
      # El usuario mencionó una comuna pero no está en los datos →
      # extraer el nombre del patrón "comarca de XXXX" y avisar
      q_nc <- .norm(q)
      mc   <- regmatches(q_nc, regexpr("(?:en (?:la |el )?)?(?:la )?comuna de ([a-z][a-z ]{1,29}?)(?:\\s*[\\?.,;\\n]|$)",
                                       q_nc, perl=TRUE))
      if (length(mc) > 0) {
        com_mencionada <- trimws(gsub("[^a-z ]", "", sub(".*comuna de ", "", mc[[1]])))
        if (nchar(com_mencionada) >= 3) {
          coms_disponibles <- sort(unique(df$nom_com_rbd[!is.na(df$nom_com_rbd)]))
          muestra_coms <- paste(head(coms_disponibles, 8), collapse=", ")
          return(list(
            found=TRUE, entity="matricula", n=0,
            filters=c(paste0("establecimientos en la comuna de ", toupper(com_mencionada))),
            raw_answer=paste0(
              "Búsqueda en matrícula EMTP 2025 [establecimientos en la comuna de ",
              toupper(com_mencionada), "]:\n",
              "  No se encontraron establecimientos EMTP ubicados en esta comuna.\n",
              "  La comuna '", toupper(com_mencionada), "' no tiene establecimientos de\n",
              "  educación técnico-profesional media en la base de datos 2025.\n",
              "  Algunas comunas con establecimientos EMTP: ", muestra_coms, ", ...\n"
            ),
            source="Matrícula EMTP 2025 (calculado en tiempo real)"
          ))
        }
      }
    }
  }

  # Dependencia
  dep     <- .match_dependency(q)
  dep_lbl <- c("1"="Municipal","2"="Part. Subv.","3"="Part. Pagado","4"="Corp.","5"="SLEP")
  if (!is.null(dep) && "cod_depe2" %in% names(df)) {
    df <- df[!is.na(df$cod_depe2) & df$cod_depe2 %in% dep, ]
    filters <- c(filters, paste0("dependencia: ", paste(dep_lbl[as.character(dep)], collapse="/")))
  }

  # Nombre de SLEP específico (ej: "SLEP Chinchorro"). Precedencia corregida y
  # uso de %in% para soportar 'dep' como vector (unión de dependencias).
  if (("nombre_slep" %in% names(df)) && (is.null(dep) || identical(as.integer(dep), 5L))) {
    slep_match <- .match_slep_nombre(q, df$nombre_slep)
    if (!is.null(slep_match)) {
      df <- df[!is.na(df$nombre_slep) & .norm(df$nombre_slep) == .norm(slep_match), ]
      # Reemplazar etiqueta genérica SLEP por nombre específico
      filters <- filters[!grepl("^dependencia:", filters)]
      filters <- c(filters, paste0("SLEP: ", slep_match))
    }
  }

  # Ruralidad (matrícula usa RURAL_RBD 0=urbano 1=rural, puede estar en minúsculas)
  col_rural_mat <- intersect(c("RURAL_RBD","rural_rbd"), names(df))[1]
  ri_mat <- .ruralidad_intent(q)
  if (ri_mat %in% c("rural","urbano") && !is.na(col_rural_mat)) {
    val <- if (ri_mat == "urbano") 0L else 1L
    df <- df[!is.na(df[[col_rural_mat]]) & as.integer(df[[col_rural_mat]]) == val, ]
    filters <- c(filters, if (val == 1L) "establecimientos rurales" else "establecimientos urbanos")
  }

  # RFT
  if (grepl("rft|red futuro", q) && "rft" %in% names(df)) {
    df <- df[!is.na(df$rft) & df$rft == "PERTENECE", ]
    filters <- c(filters, "RFT")
  }

  n    <- nrow(df)
  n_ee <- if ("rbd"      %in% names(df)) length(unique(df$rbd))      else NA_integer_
  lbl  <- if (length(filters) == 0) "total" else paste(filters, collapse=" | ")

  # ── Bloque ASISTENCIA (cuando la pregunta la menciona) ──────────────────────
  asis_txt <- ""
  if (grepl("asistenc|inasistenc|ausent", q) && "categoria_asis_anual" %in% names(df)) {
    catlbl <- c("1"="Inasistencia crítica (<50%)", "2"="Inasistencia grave (50-84%)",
                "3"="Asistencia reiterada (85-89%)", "4"="Asistencia esperada (≥90%)")
    ca  <- as.character(df$categoria_asis_anual)
    tt  <- table(factor(ca, levels=c("1","2","3","4")))
    nv  <- sum(tt)
    prom_a <- if ("tasa_asis_anual" %in% names(df)) mean(df$tasa_asis_anual, na.rm=TRUE) else NA_real_
    if (nv > 0) {
      partes <- sapply(names(tt), function(k)
        paste0("    ", catlbl[k], ": ", format(as.integer(tt[k]), big.mark="."),
               " (", round(100*tt[k]/nv, 1), "%)"))
      asis_txt <- paste0(
        "  Asistencia anual de los estudiantes (con dato: ", format(nv, big.mark="."), "):\n",
        paste(partes, collapse="\n"), "\n",
        if (is.finite(prom_a)) paste0("    Asistencia promedio: ",
                                      gsub("\\.", ",", sprintf("%.1f", prom_a)), "%\n") else "")
    }
  }

  # ── Bloque SECTOR ECONÓMICO (cuando la pregunta lo menciona) ────────────────
  sec_txt <- ""
  if (grepl("sector econom|sector produc|por sector|\\brubro\\b|rama economic", q) &&
      "nom_sector" %in% names(df)) {
    st <- sort(table(df$nom_sector[!is.na(df$nom_sector) & nzchar(df$nom_sector)]), decreasing=TRUE)
    if (length(st) > 0) {
      ns <- sum(st)
      partes <- sapply(head(names(st), 14), function(k)
        paste0("    ", k, ": ", format(as.integer(st[k]), big.mark="."),
               " (", round(100*st[k]/ns, 1), "%)"))
      sec_txt <- paste0("  Matrícula por sector económico:\n", paste(partes, collapse="\n"), "\n")
    }
  }

  # Cuando se pidió solo mujeres o solo hombres: respuesta directa explícita
  intro <- if (!is.null(genero) && genero == "solo_m") {
    paste0("  Estudiantes mujeres: ", format(n, big.mark="."), "\n")
  } else if (!is.null(genero) && genero == "solo_h") {
    paste0("  Estudiantes hombres: ", format(n, big.mark="."), "\n")
  } else {
    paste0("  Total estudiantes: ", format(n, big.mark="."), "\n")
  }

  tiene_filtro_comuna <- any(grepl("^establecimientos en la comuna de", filters))
  raw <- paste0(
    "Matrícula EMTP 2025 [", lbl, "]:\n",
    # Aclaración metodológica solo cuando hay filtro de comuna
    if (tiene_filtro_comuna)
      "  ACLARACIÓN: Este conteo incluye estudiantes matriculados en establecimientos\n  UBICADOS en la comuna indicada. El domicilio del estudiante puede ser distinto.\n"
    else "",
    intro,
    if ("gen_alu" %in% names(df)) .gender_summary(df) else "",
    if (!is.na(n_ee)) paste0(
      if (tiene_filtro_comuna) "  Establecimientos ubicados en la comuna: "
      else "  Establecimientos con esta matrícula: ",
      format(n_ee, big.mark="."), "\n") else "",
    asis_txt,
    sec_txt
  )
  # Si hay un desglose (asistencia o sector), mostrar la tabla verbatim
  # (el resumen del LLM a 2 oraciones perdería el detalle).
  hay_desglose <- nzchar(asis_txt) || nzchar(sec_txt)
  list(found=TRUE, entity="matricula", raw_answer=raw, n=n, filters=filters,
       render_raw=hay_desglose,
       source="Matrícula EMTP 2025 (calculado en tiempo real)")
}

# ── Diccionario subsectores TP y matcher ────────────────────────────────────
.dic_subsectores <- list(
  list(cod=c(41001L),        nom="Administración"),
  list(cod=c(41002L),        nom="Contabilidad"),
  list(cod=c(41003L),        nom="Secretariado"),
  list(cod=c(41004L),        nom="Ventas"),
  list(cod=c(51001L),        nom="Edificación"),
  list(cod=c(51002L),        nom="Terminaciones de Construcción"),
  list(cod=c(51003L),        nom="Montaje Industrial"),
  list(cod=c(51004L),        nom="Obras Viales"),
  list(cod=c(51005L),        nom="Instalaciones Sanitarias"),
  list(cod=c(51006L),        nom="Refrigeración y Climatización"),
  list(cod=c(52008L),        nom="Mecánica Industrial"),
  list(cod=c(52009L),        nom="Construcciones Metálicas"),
  list(cod=c(52010L),        nom="Mecánica"),
  list(cod=c(52012L),        nom="Mecánica de Aeronaves"),
  list(cod=c(53014L),        nom="Electricidad"),
  list(cod=c(53015L),        nom="Electrónica"),
  list(cod=c(53016L),        nom="Telecomunicaciones"),
  list(cod=c(54018L),        nom="Explotación Minera"),
  list(cod=c(54019L),        nom="Metalurgia Extractiva"),
  list(cod=c(54020L),        nom="Asistencia en Geología"),
  list(cod=c(55022L),        nom="Gráfica"),
  list(cod=c(55023L),        nom="Dibujo Técnico"),
  list(cod=c(56025L),        nom="Operación de Planta Química"),
  list(cod=c(56026L),        nom="Laboratorio Químico"),
  list(cod=c(58033L),        nom="Conectividad y Redes"),
  list(cod=c(58034L),        nom="Programación"),
  list(cod=c(61001L),        nom="Elaboración Industrial de Alimentos"),
  list(cod=c(61002L),        nom="Servicio de Alimentación Colectiva"),
  list(cod=c(62004L),        nom="Atención de Párvulos"),
  list(cod=c(62005L),        nom="Atención de Adultos Mayores"),
  list(cod=c(62006L),        nom="Atención de Enfermos"),
  list(cod=c(62007L),        nom="Atención Social y Recreativa"),
  list(cod=c(63009L),        nom="Servicio de Turismo"),
  list(cod=c(63010L),        nom="Servicio de Hotelería"),
  list(cod=c(71001L),        nom="Forestal"),
  list(cod=c(71002L,71003L), nom="Madera"),
  list(cod=c(72006L),        nom="Agropecuaria"),
  list(cod=c(81001L),        nom="Naves Mercantes"),
  list(cod=c(81002L),        nom="Pesquería"),
  list(cod=c(81003L),        nom="Acuicultura"),
  list(cod=c(81004L),        nom="Operación Portuaria")
)

# Tabla de keywords → subsector(s), reutilizable (versión única y versión unión).
.subsector_kw <- list(
  list(k="administracion|administrativ",            s=41001L),
  list(k="contabilidad|contador|contable",          s=41002L),
  list(k="secretariado|secretaria",                 s=41003L),
  list(k="ventas|comercial",                        s=41004L),
  list(k="edificacion|construccion|albanil",        s=c(51001L,51002L,51003L,51004L,51005L,51006L)),
  list(k="refriger|climatiz|aire acondicionado",    s=51006L),
  list(k="mecanica industrial",                     s=52008L),
  list(k="metalic|soldadura|construccion metalic",  s=52009L),
  list(k="mecanica de aeronave|aeronautic",         s=52012L),
  list(k="\\bmecanica\\b",                          s=c(52008L,52010L)),
  list(k="electricidad|electrico|electrica",        s=53014L),
  list(k="electronica|electronico",                 s=53015L),
  list(k="telecomunicacion",                        s=c(53016L,58035L)),
  list(k="mineria|\\bminero|explotacion minera",    s=54018L),
  list(k="metalurgia",                              s=54019L),
  list(k="geologia",                                s=54020L),
  list(k="grafica|imprenta",                        s=55022L),
  list(k="dibujo",                                  s=55023L),
  list(k="quimica|laboratorio quimico",             s=c(56025L,56026L)),
  list(k="\\bredes\\b|conectividad",                s=58033L),
  list(k="programacion|informatica|software",       s=58034L),
  list(k="alimentos|alimentacion",                  s=c(61001L,61002L)),
  list(k="gastronomia|cocina|chef|culinari",        s=c(61001L,61002L)),
  list(k="parvulos|educacion parvularia|jardin",    s=62004L),
  list(k="adultos mayores|geriatri",                s=62005L),
  list(k="enfermeria|enfermos|\\bsalud\\b|tecnico salud", s=62006L),
  list(k="social|recreativ|sociocomunit",           s=62007L),
  list(k="turismo",                                 s=63009L),
  list(k="hoteleria|hotel|hospitalidad",            s=63010L),
  list(k="forestal|silvicultura",                   s=71001L),
  list(k="madera|mueble|carpinteria",               s=c(71002L,71003L)),
  list(k="agropecuaria|agricola|veterinaria|pecuari",s=72006L),
  list(k="nautica|naves|maritimo|marina mercante",  s=81001L),
  list(k="pesca|pesqueria",                         s=81002L),
  list(k="acuicultura|acuicola",                    s=81003L),
  list(k="portuario|puerto",                        s=81004L)
)

.subsector_nom <- function(s) {
  idx <- which(sapply(.dic_subsectores, function(d) any(d$cod %in% s)))[1]
  if (is.na(idx)) paste(s, collapse="/") else .dic_subsectores[[idx]]$nom
}

# Devuelve list(cods=integer vector, nom=label) del PRIMER subsector que matchea, o NULL
.match_subsector_doc <- function(q) {
  q_n <- .norm(q)
  for (e in .subsector_kw) {
    if (grepl(e$k, q_n, perl=TRUE))
      return(list(cods=as.integer(e$s), nom=.subsector_nom(e$s)))
  }
  NULL
}

# UNIÓN: todos los subsectores mencionados (2 o más). list(cods, nom) o NULL.
# Ej: "docentes de Electricidad y Mecánica" → cods de ambas.
.match_all_subsectors_doc <- function(q) {
  q_n <- .norm(q); cods <- integer(0); noms <- character(0)
  for (e in .subsector_kw) {
    if (grepl(e$k, q_n, perl=TRUE)) {
      cods <- c(cods, as.integer(e$s)); noms <- c(noms, .subsector_nom(e$s))
    }
  }
  if (!length(cods)) return(NULL)
  list(cods=unique(cods), nom=paste(unique(noms), collapse=", "))
}

# ── Consulta sobre docentes ──────────────────────────────────────────────────
.query_docentes <- function(q, df) {
  filters  <- character(0)
  col_mrun <- intersect(c("MRUN","mrun"), names(df))[1]
  col_rbd  <- intersect(c("RBD","rbd"),  names(df))[1]
  col_dep  <- intersect(c("COD_DEPE2","cod_depe2"), names(df))[1]
  col_reg  <- intersect(c("NOM_REG_RBD_A","nom_reg_rbd_a","COD_REG_RBD","cod_reg_rbd"), names(df))[1]
  is_esp   <- "SUBSECTOR1" %in% names(df)

  # Totales ANTES de filtrar por especialidad (para referencia contextual)
  n_total_doc <- if (!is.na(col_mrun)) length(unique(df[[col_mrun]])) else nrow(df)

  # ¿El usuario pide formación general o el total incluyendo FG?
  pide_fg <- grepl("formacion general|fg\\b|incluyendo|incluso|sumando|todos los docentes|total.*docentes|docentes.*total", q)

  if (is_esp && !pide_fg) {
    # Filtrar solo docentes de módulos de especialidad EMTP (Subsector 41001–81004)
    df <- df[
      (!is.na(df$SUBSECTOR1) & df$SUBSECTOR1 >= 41001 & df$SUBSECTOR1 <= 81004) |
      (!is.na(df$SUBSECTOR2) & df$SUBSECTOR2 >= 41001 & df$SUBSECTOR2 <= 81004), ]
    filters <- c(filters, "docentes de módulos de especialidad EMTP")
  } else if (is_esp && pide_fg) {
    filters <- c(filters, "todos los docentes en establecimientos EMTP")
  }

  # Especialidad(es) específica(s) (subsector TP) — soporta unión de 2 o más
  sub_match <- if (is_esp && !pide_fg) .match_all_subsectors_doc(q) else NULL
  if (!is.null(sub_match)) {
    df <- df[
      (!is.na(df$SUBSECTOR1) & df$SUBSECTOR1 %in% sub_match$cods) |
      (!is.na(df$SUBSECTOR2) & df$SUBSECTOR2 %in% sub_match$cods), ]
    # Reemplazar etiqueta genérica
    filters <- filters[!grepl("^docentes de m", filters)]
    filters <- c(filters, paste0("especialidad: ", sub_match$nom))
  }

  # Género
  genero_doc <- .detect_genero(q)
  if (!is.null(genero_doc) && genero_doc %in% c("solo_h","solo_m") && "DOC_GENERO" %in% names(df)) {
    cod_gen <- if (genero_doc == "solo_h") 1L else 2L
    df <- df[!is.na(df$DOC_GENERO) & df$DOC_GENERO == cod_gen, ]
    filters <- c(filters, if (genero_doc == "solo_h") "hombres" else "mujeres")
  }

  # Región (soporta unión de varias regiones)
  rf <- .apply_region_filter(q, df, filters); df <- rf$df; filters <- rf$filters

  # Dependencia
  dep     <- .match_dependency(q)
  dep_lbl <- c("1"="Municipal","2"="Part. Subv.","3"="Part. Pagado","4"="Corp.","5"="SLEP")
  if (!is.null(dep) && !is.na(col_dep)) {
    df <- df[!is.na(df[[col_dep]]) & df[[col_dep]] %in% dep, ]
    filters <- c(filters, paste0("dependencia: ", paste(dep_lbl[as.character(dep)], collapse="/")))
  }

  # Ruralidad (docentes usa RURAL_RBD 0/1)
  ri_doc <- .ruralidad_intent(q)
  if (ri_doc %in% c("rural","urbano") && "RURAL_RBD" %in% names(df)) {
    val <- if (ri_doc == "urbano") 0L else 1L
    df <- df[!is.na(df$RURAL_RBD) & df$RURAL_RBD == val, ]
    filters <- c(filters, if (val == 1L) "liceos rurales" else "liceos urbanos")
  }

  n_filas <- nrow(df)
  n_uniq  <- if (!is.na(col_mrun)) length(unique(df[[col_mrun]])) else n_filas
  n_ee    <- if (!is.na(col_rbd))  length(unique(df[[col_rbd]]))  else NA_integer_
  lbl     <- if (length(filters) == 0) "total" else paste(filters, collapse=" | ")

  # % con título en Educación (pedagogía) sobre docentes únicos de especialidad
  ped_txt <- ""
  if (grepl("pedagog|titulo|formacion|titulados", q) && is_esp &&
      "TIT_ID_1" %in% names(df) && n_uniq > 0) {
    doc_u   <- df[!duplicated(df[[col_mrun]]), ]
    n_ped   <- sum(doc_u$TIT_ID_1 == 1 | (!is.na(doc_u$TIT_ID_2) & doc_u$TIT_ID_2 == 1), na.rm=TRUE)
    pct_ped <- round(100 * n_ped / nrow(doc_u), 1)
    ped_txt <- paste0(
      "  Con título en Educación/Pedagogía: ", format(n_ped, big.mark="."),
      " (", pct_ped, "% de los docentes de especialidad)\n",
      "  NOTA: Este porcentaje corresponde al universo de docentes que imparten\n",
      "  módulos de especialidad EMTP (no al total de docentes del establecimiento).\n"
    )
  } else if (is_esp && "TIT_ID_1" %in% names(df) && n_uniq > 0) {
    # Siempre mostrar % pedagogía aunque no se preguntó directamente
    doc_u   <- df[!duplicated(df[[col_mrun]]), ]
    n_ped   <- sum(doc_u$TIT_ID_1 == 1 | (!is.na(doc_u$TIT_ID_2) & doc_u$TIT_ID_2 == 1), na.rm=TRUE)
    pct_ped <- round(100 * n_ped / nrow(doc_u), 1)
    ped_txt <- paste0("  Con título en Pedagogía: ", pct_ped, "% (", format(n_ped, big.mark="."), " docentes)\n")
  }

  # Desglose de género (sobre docentes únicos)
  gen_txt <- ""
  if ("DOC_GENERO" %in% names(df) && n_uniq > 0 && is.null(genero_doc)) {
    doc_u2 <- df[!duplicated(df[[col_mrun]]), ]
    n_h <- sum(!is.na(doc_u2$DOC_GENERO) & doc_u2$DOC_GENERO == 1)
    n_m <- sum(!is.na(doc_u2$DOC_GENERO) & doc_u2$DOC_GENERO == 2)
    n_g <- n_h + n_m
    if (n_g > 0)
      gen_txt <- paste0(
        "  Hombres: ", format(n_h, big.mark="."), " (", round(100*n_h/n_g,1), "%)",
        " | Mujeres: ", format(n_m, big.mark="."), " (", round(100*n_m/n_g,1), "%)\n"
      )
  }

  raw <- paste0(
    "Docentes EMTP 2025 [", lbl, "]:\n",
    "  Docentes únicos (personas): ", format(n_uniq, big.mark="."), "\n",
    gen_txt,
    if (!is.na(n_ee)) paste0("  Establecimientos: ", format(n_ee, big.mark="."), "\n") else "",
    if (is_esp && !pide_fg && is.null(sub_match))
      paste0("  NOTA: Este universo corresponde solo a docentes que imparten módulos\n",
             "  de especialidad TP. El total de docentes en establecimientos EMTP\n",
             "  (incluyendo formación general) es ", format(n_total_doc, big.mark="."), " personas.\n")
    else "",
    ped_txt
  )
  list(found=TRUE, entity="docentes", raw_answer=raw, n=n_uniq, filters=filters,
       source="Directorio Docentes EMTP 2025 (calculado en tiempo real)")
}

# ── Consulta sobre establecimientos ─────────────────────────────────────────
.query_establecimientos <- function(q, df) {
  filters <- character(0)
  dep_lbl <- c("1"="Municipal","2"="Part. Subv.","3"="Part. Pagado","4"="Corp.","5"="SLEP")

  # --- Geografía: cascada región (unión) → DEPROV → comuna ---
  rf <- .apply_region_filter(q, df, filters); df <- rf$df; filters <- rf$filters
  if (!rf$matched) {
    # ¿DEPROV?
    dprov <- if ("nom_deprov_rbd" %in% names(df)) .match_deprov(q, df$nom_deprov_rbd) else NULL
    if (!is.null(dprov)) {
      df <- df[!is.na(df$nom_deprov_rbd) & .norm(df$nom_deprov_rbd) == .norm(dprov), ]
      filters <- c(filters, paste0("DEPROV: ", dprov))
    } else if ("nom_com_rbd" %in% names(df)) {
      # ¿Comuna?
      com <- .match_comuna(q, df$nom_com_rbd)
      if (!is.null(com)) {
        com_norm <- .norm(com)
        n_antes  <- nrow(df)
        df <- df[!is.na(df$nom_com_rbd) & grepl(com_norm, .norm(df$nom_com_rbd), fixed=TRUE), ]
        if (nrow(df) == 0 && n_antes > 0) {
          coms <- sort(unique(df$nom_com_rbd[!is.na(df$nom_com_rbd)]))
          muestra <- paste(head(coms, 6), collapse=", ")
          return(list(found=TRUE, entity="establecimientos", n=0,
            filters=c(paste0("comuna: ", toupper(com))),
            raw_answer=paste0(
              "Establecimientos EMTP 2025 [comuna: ", toupper(com), "]:\n",
              "  No se encontraron establecimientos EMTP en esa comuna.\n",
              "  Comunas con establecimientos EMTP: ", muestra, ", ...\n"
            ),
            source="Matrícula EMTP 2025 (calculado en tiempo real)"))
        }
        filters <- c(filters, paste0("comuna: ", com))
      }
    }
  }
  dep <- .match_dependency(q)
  if (!is.null(dep) && "cod_depe2" %in% names(df)) {
    df <- df[!is.na(df$cod_depe2) & df$cod_depe2 %in% dep, ]
    filters <- c(filters, paste0("dependencia: ", paste(dep_lbl[as.character(dep)], collapse="/")))
  }
  ri_ee <- .ruralidad_intent(q)
  if (ri_ee %in% c("rural","urbano")) {
    col_rural_ee <- intersect(c("RuralidadRBD","rural_rbd","RURAL_RBD"), names(df))[1]
    if (!is.na(col_rural_ee)) {
      if (col_rural_ee == "RuralidadRBD") {
        val_lbl <- if (ri_ee == "urbano") "URBANO" else "RURAL"
        df <- df[!is.na(df[[col_rural_ee]]) & toupper(df[[col_rural_ee]]) == val_lbl, ]
        filters <- c(filters, if (val_lbl == "RURAL") "rurales" else "urbanos")
      } else {
        val <- if (ri_ee == "urbano") 0L else 1L
        df <- df[!is.na(df[[col_rural_ee]]) & as.integer(df[[col_rural_ee]]) == val, ]
        filters <- c(filters, if (val == 1L) "rurales" else "urbanos")
      }
    }
  }
  if (grepl("rft|red futuro", q) && "rft" %in% names(df)) {
    df <- df[!is.na(df$rft) & df$rft == "PERTENECE", ]
    filters <- c(filters, "RFT")
  }
  # NUEVO: filtro por especialidad (unión de 2 o más) → "¿cuáles son los
  # establecimientos de Electricidad y Mecánica en Antofagasta?".
  if ("nom_espe" %in% names(df)) {
    esps_match <- .match_all_specialties(q, df$nom_espe)
    if (length(esps_match) > 0) {
      df <- df[!is.na(df$nom_espe) & .norm(df$nom_espe) %in% .norm(esps_match), ]
      filters <- c(filters, paste0("especialidad: ", paste(esps_match, collapse=", ")))
    }
  }
  n_ee <- if ("rbd" %in% names(df)) length(unique(df$rbd)) else nrow(df)
  lbl  <- if (length(filters) == 0) "total" else paste(filters, collapse=" | ")

  # ¿Pregunta de listado? ("cuáles son", "dime cuáles", etc.)
  pide_lista <- grepl("cuales|nombra|lista|dime|muestrame|cuál|cual ", q)

  lista_txt <- ""
  if (pide_lista && n_ee > 0 && n_ee <= 30 && "nom_rbd" %in% names(df) && "rbd" %in% names(df)) {
    ee_uniq <- unique(df[, c("rbd", "nom_rbd")])
    ee_uniq <- ee_uniq[order(ee_uniq$nom_rbd), ]
    lista_txt <- paste0(
      "  Lista de establecimientos:\n",
      paste0("    • ", ee_uniq$nom_rbd, " (RBD ", ee_uniq$rbd, ")", collapse="\n"), "\n"
    )
  } else if (pide_lista && n_ee > 30) {
    lista_txt <- paste0("  (Son ", n_ee, " establecimientos; usa los filtros de la plataforma para verlos todos)\n")
  }

  raw  <- paste0(
    "Establecimientos EMTP 2025 [", lbl, "]:\n",
    "  Establecimientos (RBD únicos): ", format(n_ee, big.mark="."), "\n",
    lista_txt
  )
  list(found=TRUE, entity="establecimientos", raw_answer=raw, n=n_ee, filters=filters,
       source="Matrícula EMTP 2025 (calculado en tiempo real)")
}

# ── Especialidades ofrecidas según tipo de establecimiento ──────────────────
.query_especialidades_breakdown <- function(q, df) {
  filters      <- character(0)
  tiene_geo    <- FALSE  # TRUE cuando hay filtro región o comuna

  # Ruralidad
  col_rural <- intersect(c("RURAL_RBD","rural_rbd"), names(df))[1]
  ri_esp <- .ruralidad_intent(q)
  if (ri_esp %in% c("rural","urbano") && !is.na(col_rural)) {
    val <- if (ri_esp == "urbano") 0L else 1L
    df  <- df[!is.na(df[[col_rural]]) & as.integer(df[[col_rural]]) == val, ]
    filters <- c(filters, if (val == 1L) "liceos rurales" else "liceos urbanos")
  }

  # Región (soporta unión de varias regiones)
  rf <- .apply_region_filter(q, df, filters); df <- rf$df; filters <- rf$filters
  if (rf$matched) tiene_geo <- TRUE

  # Comuna
  col_com <- intersect(c("nom_com_rbd","NOM_COM_RBD"), names(df))[1]
  if (!tiene_geo && !is.na(col_com)) {
    com <- .match_comuna(q, df[[col_com]])
    if (!is.null(com)) {
      df <- df[!is.na(df[[col_com]]) & grepl(.norm(com), .norm(df[[col_com]]), fixed=TRUE), ]
      filters   <- c(filters, paste0("comuna: ", toupper(com)))
      tiene_geo <- TRUE
    }
  }

  # Dependencia
  dep <- .match_dependency(q)
  col_dep <- intersect(c("cod_depe2","COD_DEPE2"), names(df))[1]
  dep_lbl <- c("1"="Municipal","2"="Part. Subv.","3"="Part. Pagado","4"="Corp.","5"="SLEP")
  if (!is.null(dep) && !is.na(col_dep)) {
    df <- df[!is.na(df[[col_dep]]) & df[[col_dep]] %in% as.character(dep), ]
    filters <- c(filters, paste0("dependencia: ", paste(dep_lbl[as.character(dep)], collapse="/")))
  }

  col_esp  <- intersect(c("nom_espe","NOM_ESPE"),   names(df))[1]
  col_rbd  <- intersect(c("rbd","RBD"),              names(df))[1]
  col_nom  <- intersect(c("nom_rbd","NOM_RBD"),      names(df))[1]
  col_com2 <- intersect(c("nom_com_rbd","NOM_COM_RBD"), names(df))[1]

  if (is.na(col_esp) || !col_esp %in% names(df)) {
    return(list(found=FALSE, entity="especialidades",
      raw_answer="No hay datos de especialidades disponibles.",
      n=0, filters=filters, source=NULL))
  }

  df_esp     <- df[!is.na(df[[col_esp]]), ]
  n_ee_total <- if (!is.na(col_rbd)) length(unique(df[[col_rbd]])) else NA_integer_

  esp_tab <- sort(table(df_esp[[col_esp]]), decreasing=TRUE)
  n_esp   <- length(esp_tab)
  n_rbd_por_esp <- if (!is.na(col_rbd)) {
    sapply(names(esp_tab), function(e)
      length(unique(df_esp[[col_rbd]][df_esp[[col_esp]] == e])))
  } else rep(NA_integer_, n_esp)

  lbl   <- if (length(filters) == 0) "total" else paste(filters, collapse=" | ")
  top_n <- min(n_esp, 15)
  top_esp <- names(esp_tab)[seq_len(top_n)]

  esp_lines <- paste0(
    "    ", seq_len(top_n), ". ", top_esp,
    ": ", format(as.integer(esp_tab[top_esp]), big.mark="."),
    " est.",
    if (!is.na(col_rbd)) paste0(" (", n_rbd_por_esp[seq_len(top_n)], " liceos)") else "",
    collapse="\n"
  )

  # Listado por establecimiento — solo cuando hay filtro geográfico y ≤ 40 liceos
  lista_ee_txt <- ""
  if (tiene_geo && !is.na(col_rbd) && !is.na(col_esp) && n_ee_total <= 40 &&
      !is.na(col_nom)) {
    ee_ids <- unique(df[[col_rbd]])
    ee_lines <- sapply(ee_ids, function(id) {
      sub_df  <- df_esp[df_esp[[col_rbd]] == id, ]
      nom     <- if (!is.na(col_nom))  unique(df[[col_nom]] [df[[col_rbd]] == id])[1] else paste("RBD", id)
      com_ee  <- if (!is.na(col_com2)) unique(df[[col_com2]][df[[col_rbd]] == id])[1] else ""
      esps    <- paste(sort(unique(sub_df[[col_esp]])), collapse="; ")
      n_est   <- nrow(sub_df)
      paste0("    • ", nom, " (", com_ee, ", RBD ", id, ") — ", n_est, " est.\n",
             "      Especialidades: ", esps)
    })
    # Ordenar por nombre
    noms_ord <- sapply(ee_ids, function(id) unique(df[[col_nom]][df[[col_rbd]] == id])[1])
    ee_lines <- ee_lines[order(noms_ord)]
    lista_ee_txt <- paste0(
      "  Detalle por establecimiento:\n",
      paste(ee_lines, collapse="\n"), "\n"
    )
  } else if (tiene_geo && n_ee_total > 40) {
    lista_ee_txt <- paste0("  (", n_ee_total, " establecimientos — se muestra resumen por especialidad)\n")
  }

  raw <- paste0(
    "Especialidades EMTP 2025 [", lbl, "]:\n",
    "  Establecimientos (RBD únicos): ", format(n_ee_total, big.mark="."), "\n",
    "  Especialidades distintas: ", n_esp, "\n",
    if (nzchar(lista_ee_txt)) lista_ee_txt else paste0(
      "  Top ", top_n, " especialidades por matrícula:\n",
      esp_lines, "\n",
      if (n_esp > top_n) paste0("  ...y ", n_esp - top_n, " especialidades más.\n") else ""
    )
  )

  list(found=TRUE, entity="especialidades", raw_answer=raw,
       n=n_esp, filters=filters,
       source="Matrícula EMTP 2025 (calculado en tiempo real)")
}

# ── Ficha completa de un establecimiento por RBD ────────────────────────────
# Extrae el número de RBD de la query (1-6 dígitos)
.extract_rbd <- function(q) {
  # Estrategia 1: "rbd 1234", "rbd: 1234", "rbd numero 1234", "rbd n 1234",
  # "rbd codigo 1234" — admite un conector ("numero/nro/codigo/n/#") opcional.
  g1 <- regmatches(q, regexec("rbd[^0-9a-z]{0,3}(?:numero|nro\\.?|cod(?:igo)?|n\\.?|#)?[^0-9]{0,4}([0-9]{1,6})", q, perl=TRUE))[[1]]
  if (length(g1) >= 2) return(g1[2])
  # Estrategia 2: número de 4-6 dígitos suelto — excluye años (20xx) y porcentajes
  m2 <- regmatches(q, regexpr("\\b([0-9]{4,6})\\b", q, perl=TRUE))
  if (length(m2) > 0) {
    num <- as.integer(m2)
    if (num >= 2000 && num <= 2099) return(NULL)  # es un año, no un RBD
    return(m2)
  }
  NULL
}

.query_rbd <- function(rbd_num, mat, doc=NULL, ba=NULL) {
  rbd_str <- as.character(rbd_num)
  dep_lbl <- c("1"="Municipal","2"="Part. Subvencionado","3"="Part. Pagado",
               "4"="Corp. Adm. Delegada","5"="SLEP")

  # --- Matrícula ---
  sub_mat <- mat[!is.na(mat$rbd) & as.character(mat$rbd) == rbd_str, ]
  if (nrow(sub_mat) == 0) {
    return(list(
      found=TRUE, entity="rbd", n=0, filters=paste0("RBD ", rbd_str),
      raw_answer=paste0(
        "Consulta RBD ", rbd_str, ":\n",
        "  No se encontró este RBD en la base de matrícula EMTP 2025.\n",
        "  Posibles causas: el establecimiento no imparte EMTP, el código es incorrecto\n",
        "  o no está activo en el período reportado.\n"
      ),
      source="Matrícula EMTP 2025 (calculado en tiempo real)"
    ))
  }

  nom     <- sub_mat$nom_rbd[1]
  comuna  <- sub_mat$nom_com_rbd[1]  %||% "N/D"
  # nom_reg_rbd_a puede traer código abreviado ("AYP") o nombre completo
  region_raw <- sub_mat$nom_reg_rbd_a[1] %||% ""
  reg_map <- c(
    "AYP"="Arica y Parinacota","TAR"="Tarapacá","ANT"="Antofagasta",
    "ATA"="Atacama","COQ"="Coquimbo","VAL"="Valparaíso","RME"="Metropolitana",
    "OHI"="O'Higgins","MAU"="Maule","NUB"="Ñuble","BIO"="Biobío",
    "LAR"="La Araucanía","LRI"="Los Ríos","LLA"="Los Lagos",
    "AYS"="Aysén","MAG"="Magallanes"
  )
  region  <- if (nchar(region_raw) <= 3 && region_raw %in% names(reg_map))
               reg_map[region_raw] else region_raw
  if (!nzchar(region)) region <- "N/D"
  deprov  <- sub_mat$nom_deprov_rbd[1] %||% "N/D"
  depe2   <- as.character(sub_mat$cod_depe2[1])
  depe_lbl <- dep_lbl[depe2] %||% paste0("Cód. ", depe2)
  rural   <- if (!is.null(sub_mat$rural_rbd) && !is.na(sub_mat$rural_rbd[1]))
               ifelse(sub_mat$rural_rbd[1] == 1, "Rural", "Urbano") else "N/D"
  slep    <- if (!is.null(sub_mat$nombre_slep) && !is.na(sub_mat$nombre_slep[1]) &&
                 nzchar(sub_mat$nombre_slep[1]))
               paste0(" (", sub_mat$nombre_slep[1], ")") else ""
  rft_val <- if ("rft" %in% names(sub_mat) && !is.na(sub_mat$rft[1]))
               sub_mat$rft[1] else "N/D"

  # Matrícula total y género
  n_tot <- nrow(sub_mat)
  n_h   <- sum(!is.na(sub_mat$gen_alu) & sub_mat$gen_alu == 1)
  n_m   <- sum(!is.na(sub_mat$gen_alu) & sub_mat$gen_alu == 2)
  pct_m <- if (n_tot > 0) round(100*n_m/n_tot, 1) else 0
  pct_h <- if (n_tot > 0) round(100*n_h/n_tot, 1) else 0

  # Grados (1° a 4° EM)
  grados_txt <- ""
  if ("cod_grado" %in% names(sub_mat)) {
    grd_tbl <- sort(table(sub_mat$cod_grado[!is.na(sub_mat$cod_grado)]))
    grd_lbl <- c("3"="1° EM","4"="2° EM","5"="3° EM","6"="4° EM")
    grd_parts <- sapply(names(grd_tbl), function(g)
      paste0(grd_lbl[g] %||% paste0("Grado ",g), ": ", grd_tbl[g]))
    grados_txt <- paste0("  Matrícula por grado: ", paste(grd_parts, collapse=" | "), "\n")
  }

  # Especialidades
  esps <- character(0)
  if ("nom_espe" %in% names(sub_mat))
    esps <- sort(unique(sub_mat$nom_espe[!is.na(sub_mat$nom_espe) & nzchar(sub_mat$nom_espe)]))
  esps_txt <- if (length(esps) > 0)
    paste0("  Especialidades EMTP (matrícula 2025):\n",
           paste0("    • ", esps, collapse="\n"), "\n")
  else "  Especialidades: no disponibles en la base de matrícula\n"

  # --- Docentes (si disponible) ---
  doc_txt <- ""
  if (!is.null(doc) && nrow(doc) > 0) {
    col_rbd_doc <- intersect(c("RBD","rbd"), names(doc))[1]
    sub_doc <- doc[!is.na(doc[[col_rbd_doc]]) &
                   as.character(doc[[col_rbd_doc]]) == rbd_str, ]
    # Filtrar solo especialidad EMTP (SUBSECTOR 41001-81004)
    if ("SUBSECTOR1" %in% names(sub_doc)) {
      sub_doc_esp <- sub_doc[
        (!is.na(sub_doc$SUBSECTOR1) & sub_doc$SUBSECTOR1 >= 41001 & sub_doc$SUBSECTOR1 <= 81004) |
        (!is.na(sub_doc$SUBSECTOR2) & sub_doc$SUBSECTOR2 >= 41001 & sub_doc$SUBSECTOR2 <= 81004), ]
    } else {
      sub_doc_esp <- sub_doc
    }
    col_mrun_doc <- intersect(c("MRUN","mrun"), names(sub_doc_esp))[1]
    n_doc  <- if (!is.na(col_mrun_doc)) length(unique(sub_doc_esp[[col_mrun_doc]])) else nrow(sub_doc_esp)
    # % pedagogia
    ped_txt_rbd <- ""
    if (n_doc > 0 && "TIT_ID_1" %in% names(sub_doc_esp)) {
      doc_u  <- sub_doc_esp[!duplicated(sub_doc_esp[[col_mrun_doc]]), ]
      n_ped  <- sum(doc_u$TIT_ID_1 == 1 | (!is.na(doc_u$TIT_ID_2) & doc_u$TIT_ID_2 == 1), na.rm=TRUE)
      pct_ped_rbd <- round(100*n_ped/nrow(doc_u), 1)
      ped_txt_rbd <- paste0("    Con título en Educación/Pedagogía: ",
                             n_ped, " (", pct_ped_rbd, "%)\n")
    }
    doc_txt <- paste0(
      "  Docentes de módulos de especialidad EMTP: ", n_doc, "\n",
      ped_txt_rbd
    )
  }

  # --- Asistencia (desde matrícula del propio RBD) ---
  asis_txt <- ""
  if ("tasa_asis_anual" %in% names(sub_mat)) {
    prom_a <- mean(sub_mat$tasa_asis_anual, na.rm=TRUE)
    if (is.finite(prom_a))
      asis_txt <- paste0("  Asistencia promedio 2025: ",
                         gsub("\\.", ",", sprintf("%.1f", prom_a)), "%\n")
  }

  # --- Indicadores del establecimiento (SIMCE / IDPS / IVE / GSE) desde base_apoyo ---
  ind_txt <- ""
  if (!is.null(ba) && nrow(ba) > 0) {
    col_rbd_ba <- intersect(c("rbd","RBD"), names(ba))[1]
    brow <- ba[!is.na(ba[[col_rbd_ba]]) & as.character(ba[[col_rbd_ba]]) == rbd_str, ]
    if (nrow(brow) > 0) {
      brow <- brow[1, ]
      .num <- function(x) { v <- suppressWarnings(as.numeric(x)); if (length(v) && is.finite(v)) v else NA_real_ }
      partes <- character(0)

      # IVE
      ive <- .num(brow$IVE)
      if (!is.na(ive)) partes <- c(partes, paste0("    IVE (vulnerabilidad): ",
                                                  gsub("\\.", ",", sprintf("%.1f", ive)), "%"))
      # GSE
      gse_map <- c("1"="Bajo","2"="Medio Bajo","3"="Medio","4"="Medio Alto","5"="Alto")
      gse_v <- as.character(brow$gse_agencia %||% NA)
      if (!is.na(gse_v) && gse_v %in% names(gse_map))
        partes <- c(partes, paste0("    Grupo socioeconómico (GSE): ", gse_map[gse_v]))

      # SIMCE 2° medio (puntajes promedio)
      pl <- .num(brow$prom_lect2m_rbd); pm <- .num(brow$prom_mate2m_rbd)
      if (!is.na(pl) || !is.na(pm)) {
        sl <- if (!is.na(pl)) paste0("Lectura ", round(pl)) else NULL
        sm <- if (!is.na(pm)) paste0("Matemática ", round(pm)) else NULL
        partes <- c(partes, paste0("    SIMCE 2° medio (puntaje prom.): ",
                                   paste(c(sl, sm), collapse=" · ")))
      }
      # SIMCE distribución por estándar (Lectura)
      ins <- .num(brow$palu_eda_ins_lect2m_rbd); ele <- .num(brow$palu_eda_ele_lect2m_rbd); ade <- .num(brow$palu_eda_ade_lect2m_rbd)
      if (!is.na(ins) || !is.na(ele) || !is.na(ade))
        partes <- c(partes, paste0("    SIMCE Lectura por estándar: Insuficiente ",
                                   ifelse(is.na(ins),"s/d",paste0(round(ins),"%")), " · Elemental ",
                                   ifelse(is.na(ele),"s/d",paste0(round(ele),"%")), " · Adecuado ",
                                   ifelse(is.na(ade),"s/d",paste0(round(ade),"%"))))
      # IDPS (4 dimensiones)
      idps_lbl <- c("Autoestima académica y motivación", "Clima de convivencia",
                    "Participación y formación ciudadana", "Hábitos de vida saludable")
      idps_vals <- sapply(1:4, function(i) .num(brow[[paste0("IDPS", i, "_Puntaje")]]))
      if (any(!is.na(idps_vals))) {
        idps_parts <- sapply(which(!is.na(idps_vals)), function(i)
          paste0(idps_lbl[i], " ", round(idps_vals[i])))
        partes <- c(partes, paste0("    IDPS por dimensión: ", paste(idps_parts, collapse=" · ")))
      }

      if (length(partes) > 0)
        ind_txt <- paste0("  Indicadores del establecimiento:\n", paste(partes, collapse="\n"), "\n")
    }
  }

  raw <- paste0(
    "Ficha establecimiento RBD ", rbd_str, ":\n",
    "  Nombre: ", nom, "\n",
    "  RBD: ", rbd_str, "\n",
    "  Región: ", region, "\n",
    "  Provincia/DEPROV: ", deprov, "\n",
    "  Comuna: ", comuna, "\n",
    "  Dependencia: ", depe_lbl, slep, "\n",
    "  Ruralidad: ", rural, "\n",
    "  Red de Futa (RFT): ", rft_val, "\n",
    "  Matrícula EMTP total 2025: ", format(n_tot, big.mark="."), "\n",
    "  Hombres: ", n_h, " (", pct_h, "%) | Mujeres: ", n_m, " (", pct_m, "%)\n",
    asis_txt,
    grados_txt,
    esps_txt,
    ind_txt,
    if (nzchar(doc_txt)) paste0(
      "  ---\n",
      "  NOTA: los docentes corresponden a quienes imparten módulos de especialidad EMTP\n",
      "  en este establecimiento (no al total de docentes del colegio).\n",
      doc_txt
    ) else ""
  )
  list(found=TRUE, entity="rbd", n=n_tot, filters=paste0("RBD ", rbd_str),
       raw_answer=raw, render_raw=TRUE,
       source="Matrícula + Docentes EMTP 2025 (calculado en tiempo real)")
}

# ─── Extracción de filtros activos (para contexto persistente) ──────────────
# Devuelve, para una query normalizada, los filtros DETECTABLES como términos
# canónicos re-inyectables en otra query. Sirve para heredar contexto entre
# turnos. NOTA: no incluye género (suele cambiar entre turnos) ni especialidad
# (difícil de re-inyectar de forma fiable contra nom_espe).
.extract_filter_terms <- function(qn) {
  out <- list(region=NULL, rural=NULL, dep=NULL, entity=NULL)
  reg <- .match_region(qn)
  if (!is.null(reg)) out$region <- reg$keys[[1]]          # key inyectable (ej. "araucania")
  ri <- .ruralidad_intent(qn)            # solo hereda un filtro simple, no "both"/"compare"
  if (ri %in% c("rural","urbano"))      out$rural  <- ri
  dep <- .detect_dep_mentions(qn)
  if (length(dep) >= 1)                 out$dep    <- dep[[1]]$term
  if (grepl("docente|profesor", qn))                                 out$entity <- "docentes"
  else if (grepl("establecimiento|colegio|liceo|escuela", qn))       out$entity <- "establecimientos"
  else if (grepl("estudiante|alumno|alumna|matricula", qn))          out$entity <- "estudiantes"
  out
}

# ─── Resolución de seguimiento conversacional (REESCRITA) ────────────────────
# Antes: solo añadía "(contexto: pregunta anterior)" sin mantener filtros, y
# tomaba como contexto el turno actual (bug: el mensaje del usuario ya estaba
# en `msgs` al llamarse). Ahora:
#   1) Detecta si la query actual es un FRAGMENTO de seguimiento.
#   2) Toma el turno de usuario ANTERIOR (no el actual).
#   3) Hereda SOLO las dimensiones (región/ruralidad/dependencia/entidad) que
#      estaban en el turno previo y FALTAN en el actual, inyectando términos
#      reales que los matchers existentes ya saben interpretar.
.resolve_followup <- function(query, msgs) {
  qn <- .norm(query)

  cur <- .extract_filter_terms(qn)
  # Fragmento de seguimiento: empieza con conector/deíctico, o es corto y sin entidad propia
  is_fragment <-
    grepl("^(y |e |ahora|entonces|tambien|de esos|de esas|esos|esas|solo )", qn) ||
    (grepl(paste0("^(cuanto|cuantos|cuanta|cuantas|cuales|cual|dime|muestrame|",
                  "nombra|lista|listame)"), qn) &&
     nchar(trimws(query)) < 45 && is.null(cur$entity))
  if (!is_fragment || length(msgs) == 0) return(query)

  # Turno de usuario ANTERIOR (el último de la lista es la pregunta actual)
  user_msgs <- Filter(function(m) m$role == "user", msgs)
  if (length(user_msgs) < 2) return(query)
  prev <- .norm(user_msgs[[length(user_msgs) - 1]]$text)
  ctx  <- .extract_filter_terms(prev)

  # Heredar dimensiones presentes en el contexto y ausentes en la query actual
  inject <- character(0)
  for (dim in names(ctx)) {
    if (!is.null(ctx[[dim]]) && is.null(cur[[dim]])) inject <- c(inject, ctx[[dim]])
  }
  if (length(inject) == 0) return(query)
  augmented <- paste0(query, " ", paste(unique(inject), collapse=" "))
  cat("[Followup] heredado:", paste(inject, collapse=", "), "=>", augmented, "\n")
  augmented
}

# ─── Normalización de texto del usuario (lenguaje natural / Chile) ──────────
# Tolera puntuación libre ("??", "¿¿", "!!"), espacios extra, etc. NO cambia el
# significado; solo limpia la forma para que el enrutamiento sea más robusto.
.clean_user_text <- function(q) {
  q <- trimws(as.character(q))
  q <- gsub("[¿?]{2,}", "?", q)     # ??  ¿¿  ¿?  → ?
  q <- gsub("[¡!]{2,}", "!", q)     # !!  ¡¡        → !
  q <- gsub("\\s+", " ", q)
  q
}

# ¿El mensaje parece DEPENDER del turno anterior? (deícticos, conectores, etc.)
.looks_like_followup <- function(qn) {
  grepl(paste0(
    "\\b(esos|esas|ese|esa|estos|estas|dichos|dichas|mismos|mismas|aquellos|aquellas|",
    "anterior|anteriores|ellos|ellas|ahi|alli|de esos|de esas)\\b|",
    "^(y |e |ahora|entonces|tambien|ademas|pero |puedes|me puedes|podrias|",
    "y que|y los|y las|y el|y la|y en|y cuant|dame|dime|muestrame|nombra|lista|",
    "cuales son|y cuales|de ahi)"),
    qn)
}

# ─── Contextualización conversacional (reescritura con historial) ───────────
# Convierte un mensaje de seguimiento en una PREGUNTA AUTOCONTENIDA usando el
# historial, mediante el LLM. Así "¿puedes decirme cuáles son esos
# establecimientos?" tras "matrícula de Electricidad en Antofagasta" se reescribe
# como "¿Cuáles son los establecimientos de Electricidad en Antofagasta?".
# Si no hay LLM o no parece seguimiento, cae a la heurística .resolve_followup.
.contextualize_query <- function(query, msgs, model = "llama3.2") {
  q_clean   <- .clean_user_text(query)
  user_msgs <- Filter(function(m) m$role == "user", msgs)
  asst_msgs <- Filter(function(m) isTRUE(m$role == "assistant"), msgs)
  if (length(user_msgs) < 2) return(list(q = q_clean, pretty = NULL))  # sin historial → tal cual
  # Una consulta con RBD explícito es autocontenida: no reformular (evita que el
  # LLM la reescriba y se pierda el código del establecimiento).
  if (!is.null(.extract_rbd(.norm(q_clean)))) return(list(q = q_clean, pretty = NULL))

  if (!.use_groq() || !.looks_like_followup(.norm(q_clean))) {
    # Sin LLM o mensaje autocontenido → heurística previa (ligera)
    return(list(q = .resolve_followup(q_clean, msgs), pretty = NULL))
  }

  prev_user <- user_msgs[[length(user_msgs) - 1]]$text          # pregunta anterior
  last_asst <- if (length(asst_msgs)) tail(asst_msgs, 1)[[1]]$text else ""
  hist_txt <- paste0(
    "Pregunta anterior del usuario: ", substr(prev_user, 1, 300), "\n",
    if (nzchar(last_asst)) paste0("Respuesta anterior (resumen): ", substr(last_asst, 1, 300), "\n") else ""
  )
  sys <- paste0(
    "Eres un reformulador de preguntas para un asistente de datos de educacion tecnico-profesional ",
    "de Chile (EMTP). Conviertes el NUEVO MENSAJE en UNA sola pregunta autocontenida en espanol de Chile, ",
    "comprensible sin leer la conversacion. Reglas:\n",
    "- Si el nuevo mensaje usa referencias ('esos', 'esas', 'ahi', 'y en...?') u omite el sujeto, ",
    "completalo con el contexto (region, comuna, especialidad, dependencia, entidad) del turno anterior.\n",
    "- Conserva la intencion real del nuevo mensaje (si pide nombres o un listado, mantenlo).\n",
    "- Entiende lenguaje natural y modismos chilenos. NO inventes datos ni cifras.\n",
    "- Responde SOLO con la pregunta reformulada, sin comillas ni explicacion."
  )
  user <- paste0(hist_txt, "Nuevo mensaje del usuario: ", q_clean, "\n\nPregunta autocontenida:")
  res  <- tryCatch(ask_llm(user, model = model, system = sys, max_tokens = 80),
                   error = function(e) list(ok = FALSE))
  if (!isTRUE(res$ok)) return(list(q = .resolve_followup(q_clean, msgs), pretty = NULL))

  out <- trimws(res$text)
  out <- gsub('^["“”«]|["“”»]$', "", out)  # quitar comillas
  out <- strsplit(out, "\n")[[1]][1]                                    # una sola linea
  if (is.null(out) || is.na(out) || !nzchar(out) || nchar(out) > 240)
    return(list(q = .resolve_followup(q_clean, msgs), pretty = NULL))
  cat("[CTX] reformulada:", out, "\n")
  list(q = out, pretty = out)
}

# ─── Heurísticas de contexto ─────────────────────────────────────────────────
# Palabras que FUERZAN contexto docente aunque no digan "docente"/"profesor"
.keywords_docente_forzado <- function(q)
  grepl(paste0("pedagog|habilitacion|titulo docente|formacion docente|",
               "horas contrat|anos servicio|subsector|rotacion docente|",
               "salida docente|directorio docente|docentes.*especial|especial.*docente"),
        q)

# Palabras que FUERZAN contexto matrícula (incluye modismos chilenos)
.keywords_matricula_forzado <- function(q)
  grepl(paste0("matricula|matriculad|alumno|alumna|estudiante|estudiantado|escolar|",
               "jovene|chico|chica|\\bcabro|chiquill|\\bnino|\\bnina|pupilo"), q)

# ¿La pregunta no tiene ningún indicador de entidad? → ambigua
.is_ambiguous <- function(q) {
  has_ent <- grepl(paste0("docente|profesor|\\bprofes?\\b|maestro|educador|matricula|alumno|alumna|",
                          "estudiante|estudiantado|escolar|cabro|chiquill|establecimiento|colegio|",
                          "liceo|escuela|plantel|recinto|egresado|titulado|graduado|",
                          "asistencia|inasistencia|sector economic|sector productiv"), q)
  has_force <- .keywords_docente_forzado(q) || .keywords_matricula_forzado(q)
  !has_ent && !has_force
}

# ── Dispatcher principal ─────────────────────────────────────────────────────
# =============================================================================
# NUEVO (mejora): MOTOR DE PREGUNTAS COMPARATIVAS
# Reutiliza las funciones .query_* existentes ejecutándolas dos (o más) veces,
# una por cada lado de la comparación, modificando la query para forzar cada
# subconjunto. Así no se duplica la lógica de filtros ya probada.
# =============================================================================

# Definiciones de dependencia para detección/comparación (k=patrón, term=texto
# canónico re-inyectable, lbl=etiqueta legible).
.dep_defs <- list(
  list(k="slep|servicio local",                       term="slep",                     lbl="SLEP"),
  list(k="municipal|daem",                            term="municipal",                lbl="Municipal"),
  list(k="particular subvencionado|subvencionado",    term="particular subvencionado", lbl="Part. Subvencionado"),
  list(k="particular pagado|\\bpagados?\\b|privado",   term="particular pagado",        lbl="Part. Pagado"),
  list(k="corporacion|corem",                         term="corporacion",              lbl="Corporación")
)

# Dependencias mencionadas en la query, ordenadas por aparición (lista de defs).
.detect_dep_mentions <- function(qn) {
  found <- list()
  for (d in .dep_defs) {
    r <- regexpr(d$k, qn, perl=TRUE)
    if (r > 0) { d$pos <- as.integer(r); found[[length(found)+1]] <- d }
  }
  if (length(found) == 0) return(list())
  found[order(vapply(found, function(d) d$pos, integer(1)))]
}

# Quita todas las menciones de dependencia (para luego forzar una sola).
.strip_dep_keywords <- function(qn) {
  for (d in .dep_defs) qn <- gsub(d$k, " ", qn, perl=TRUE)
  trimws(gsub("\\s+", " ", qn))
}

# Heurística de entidad reutilizable. IMPORTANTE: una entidad de datos explícita
# (estudiante/docente) gana sobre la palabra de LUGAR "liceo/colegio". Así
# "estudiantes en liceos rurales o urbanos" se cuenta como ESTUDIANTES, no liceos.
.entity_of <- function(qn) {
  if (.keywords_docente_forzado(qn) || grepl("docente|profesor|\\bprofes?\\b|maestro|educador", qn)) return("docentes")
  if (.keywords_matricula_forzado(qn))                                                       return("matricula")
  if (grepl("establecimiento|colegio|liceo|escuela|plantel|recinto|\\bee\\b", qn))           return("establecimientos")
  "matricula"
}

# Ejecuta la consulta de la entidad indicada con una query (ya normalizada).
.run_entity_query <- function(qn, entity, mat, doc) {
  switch(entity,
    docentes         = if (!is.null(doc) && nrow(doc) > 0) .query_docentes(qn, doc) else NULL,
    establecimientos = if (!is.null(mat) && nrow(mat) > 0) .query_establecimientos(qn, mat) else NULL,
    if (!is.null(mat) && nrow(mat) > 0) .query_matricula(qn, mat) else NULL)
}

# Detecta el eje de comparación. Devuelve list(axis=..., values=...) o NULL.
# Región y dependencia comparan SOLO si la intención es comparar; si se
# mencionan dos como UNIÓN ("Maule y Biobío", "municipal y subvencionado"),
# NO se compara: las .query_* las filtran juntas.
.detect_comparison <- function(qn) {
  if (.ruralidad_intent(qn) == "compare") return(list(axis="rural"))
  es_compare <- .combo_intent(qn) == "compare"
  regs <- .match_all_regions(qn)
  if (length(regs) >= 2 && es_compare) return(list(axis="region", values=regs))
  deps <- .detect_dep_mentions(qn)
  if (length(deps) >= 2 && es_compare) return(list(axis="dep", values=deps))
  NULL
}

# Construye y ejecuta la comparación. qn = query NORMALIZADA.
.query_comparative <- function(qn, mat, doc=NULL) {
  cmp <- .detect_comparison(qn)
  if (is.null(cmp)) return(NULL)
  entity <- .entity_of(qn)

  # Construir los "lados" como queries que fuerzan cada subconjunto -------------
  axis_lbl <- cmp$axis
  if (cmp$axis == "rural") {
    q_rural <- gsub("urbano[s]?", " ", qn)        # deja solo rural
    q_urb   <- gsub("rural(es|idad)?", " ", qn)   # deja solo urbano
    if (!grepl("rural",  q_rural)) q_rural <- paste(q_rural, "rural")
    if (!grepl("\\burbano", q_urb)) q_urb  <- paste(q_urb,   "urbano")
    sides <- list(list(lbl="Rurales", q=q_rural), list(lbl="Urbanos", q=q_urb))
    axis_lbl <- "ruralidad"
  } else if (cmp$axis == "region") {
    regs     <- cmp$values
    all_keys <- unique(unlist(lapply(regs, function(m) m$keys)))
    sides <- lapply(regs, function(m) {
      q_one <- qn
      for (k in setdiff(all_keys, m$keys)) q_one <- gsub(k, " ", q_one, perl=TRUE)
      list(lbl=m$lbl, q=q_one)
    })
    axis_lbl <- "región"
  } else if (cmp$axis == "dep") {
    base <- .strip_dep_keywords(qn)
    sides <- lapply(cmp$values, function(d) list(lbl=d$lbl, q=paste(base, d$term)))
    axis_lbl <- "dependencia"
  } else return(NULL)

  # Ejecutar cada lado y recolectar n -----------------------------------------
  unidad <- switch(entity, docentes="docentes", establecimientos="establecimientos", "estudiantes")
  res <- lapply(sides, function(s) {
    r <- tryCatch(.run_entity_query(.norm(s$q), entity, mat, doc), error=function(e) NULL)
    list(lbl=s$lbl, n=if (!is.null(r)) r$n else NA_integer_)
  })
  ns <- vapply(res, function(r) as.numeric(r$n %||% NA_real_), numeric(1))
  if (all(is.na(ns))) return(NULL)

  lines <- vapply(res, function(r)
    paste0("  ", r$lbl, ": ", format(r$n, big.mark="."), " ", unidad), character(1))

  # Conclusión: cuál es mayor y por cuánto
  concl <- ""
  if (sum(!is.na(ns)) >= 2) {
    idx_max <- which.max(ns); idx_min <- which.min(ns)
    d   <- ns[idx_max] - ns[idx_min]
    pct <- if (ns[idx_min] > 0) round(100 * d / ns[idx_min], 1) else NA_real_
    concl <- paste0(
      "  → Hay más en ", res[[idx_max]]$lbl, ": ", format(d, big.mark="."),
      " ", unidad, " más que en ", res[[idx_min]]$lbl,
      if (!is.na(pct)) paste0(" (+", pct, "%)") else "", ".\n")
  }

  raw <- paste0(
    "Comparación por ", axis_lbl, " — ", unidad, " EMTP 2025:\n",
    paste(lines, collapse="\n"), "\n", concl)
  list(found=TRUE, entity="comparativa", raw_answer=raw,
       n=sum(ns, na.rm=TRUE), filters=paste0("comparación por ", axis_lbl),
       source="EMTP 2025 (calculado en tiempo real)")
}

# =============================================================================
# NUEVO: MULTI-ENTIDAD — "¿cuántos estudiantes y docentes hay en Los Ríos?"
# Detecta 2+ entidades y corre la consulta de cada una con los MISMOS filtros
# (región, comuna, dependencia...), concatenando las respuestas.
# =============================================================================
.detect_entities <- function(qn) {
  ents <- character(0)
  if (grepl("docente|profesor|\\bprofes?\\b|maestro|educador", qn))        ents <- c(ents, "docentes")
  if (.keywords_matricula_forzado(qn))                             ents <- c(ents, "matricula")
  if (grepl("egresado|titulado|graduado", qn))                     ents <- c(ents, "egresados")
  # "liceo/colegio" es ambiguo (lugar); solo cuenta como entidad si se pregunta
  # explícitamente por su cantidad.
  if (grepl("establecimiento|plantel|\\bee\\b", qn) ||
      grepl("cuant[oa]s? (liceos|colegios|escuelas)|numero de (liceos|colegios|establecimientos)", qn))
    ents <- c(ents, "establecimientos")
  unique(ents)
}

.query_multi_entity <- function(q, mat, doc, egr) {
  ents <- .detect_entities(q)
  if (length(ents) < 2) return(NULL)
  parts <- character(0); srcs <- character(0)
  for (e in ents) {
    r <- switch(e,
      docentes         = if (!is.null(doc) && nrow(doc) > 0) .query_docentes(q, doc) else NULL,
      matricula        = if (!is.null(mat) && nrow(mat) > 0) .query_matricula(q, mat) else NULL,
      establecimientos = if (!is.null(mat) && nrow(mat) > 0) .query_establecimientos(q, mat) else NULL,
      egresados        = if (!is.null(egr) && nrow(egr) > 0)
        list(raw_answer = paste0("Egresados EMTP 2024: ", format(nrow(egr), big.mark="."), " registros.\n"),
             source = "Base egresados EMTP 2024 (tiempo real)") else NULL)
    if (!is.null(r)) { parts <- c(parts, r$raw_answer); srcs <- c(srcs, r$source) }
  }
  if (length(parts) < 2) return(NULL)
  list(found = TRUE, entity = "multi", raw_answer = paste(parts, collapse = "\n"),
       render_raw = TRUE, n = 0, filters = character(0),
       source = paste(unique(srcs), collapse = " · "))
}

# ── Consulta sobre titulados (práctica profesional) ──────────────────────────
.query_titulados <- function(q, df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  filters <- character(0)

  # Género
  genero <- tryCatch(.detect_genero(q), error=function(e) NULL)
  if (!is.null(genero) && genero == "solo_m") {
    df <- df[!is.na(df$GEN_ALU) & df$GEN_ALU == 2, ]; filters <- c(filters, "mujeres")
  } else if (!is.null(genero) && genero == "solo_h") {
    df <- df[!is.na(df$GEN_ALU) & df$GEN_ALU == 1, ]; filters <- c(filters, "hombres")
  }

  # Especialidad (unión)
  if ("NOM_ESPE" %in% names(df)) {
    esps <- tryCatch(.match_all_specialties(q, df$NOM_ESPE), error=function(e) character(0))
    if (length(esps) > 0) {
      df <- df[!is.na(df$NOM_ESPE) & .norm(df$NOM_ESPE) %in% .norm(esps), ]
      filters <- c(filters, paste0("especialidad: ", paste(esps, collapse=", ")))
    }
  }

  # Región (reusa el matcher; detecta NOM_REG_RBD_A automáticamente)
  rf <- tryCatch(.apply_region_filter(q, df, filters),
                 error=function(e) list(df=df, filters=filters, matched=FALSE))
  df <- rf$df; filters <- rf$filters

  # Dependencia
  dep     <- tryCatch(.match_dependency(q), error=function(e) NULL)
  dep_lbl <- c("1"="Municipal","2"="Part. Subv.","3"="Part. Pagado","4"="Corp.","5"="SLEP")
  if (!is.null(dep) && "COD_DEPE2" %in% names(df)) {
    df <- df[!is.na(df$COD_DEPE2) & df$COD_DEPE2 %in% dep, ]
    filters <- c(filters, paste0("dependencia: ", paste(dep_lbl[as.character(dep)], collapse="/")))
  }

  n   <- nrow(df)
  lbl <- if (length(filters) == 0) "total" else paste(filters, collapse=" | ")
  nh  <- sum(!is.na(df$GEN_ALU) & df$GEN_ALU == 1)
  nm  <- sum(!is.na(df$GEN_ALU) & df$GEN_ALU == 2)
  gen_txt <- if ((nh + nm) > 0)
    paste0("  Hombres: ", format(nh, big.mark="."), " (", round(100*nh/(nh+nm),1), "%)",
           " | Mujeres: ", format(nm, big.mark="."), " (", round(100*nm/(nh+nm),1), "%)\n") else ""

  # Desglose opcional (top): por especialidad / sector económico de la práctica / región
  .top_tbl <- function(col, titulo, top = 12) {
    if (!(col %in% names(df))) return("")
    tt <- sort(table(df[[col]][!is.na(df[[col]]) & nzchar(as.character(df[[col]]))]), decreasing=TRUE)
    if (length(tt) == 0) return("")
    tot <- sum(tt)
    partes <- sapply(head(names(tt), top), function(k)
      paste0("    ", k, ": ", format(as.integer(tt[k]), big.mark="."),
             " (", round(100*tt[k]/tot, 1), "%)"))
    paste0("  ", titulo, ":\n", paste(partes, collapse="\n"), "\n")
  }
  extra <- ""
  if (grepl("rubro|empresa|sector economic|sector productiv|donde hacen|donde realiz|practica en", q))
    extra <- .top_tbl("GLOSA_RUBRO", "Titulados por rubro económico de la práctica")
  else if (grepl("especialidad", q) && !grepl("especialidad:", lbl))
    extra <- .top_tbl("NOM_ESPE", "Titulados por especialidad")
  else if (grepl("\\bsector\\b", q))
    extra <- .top_tbl("NOM_SECTOR", "Titulados por sector económico")
  else if (grepl("region|regiones", q) && !rf$matched)
    extra <- .top_tbl("NOM_REG_RBD_A", "Titulados por región")

  hay_desglose <- nzchar(extra)
  raw <- paste0(
    "Titulados de la EMTP (práctica profesional) [", lbl, "]:\n",
    "  Total titulados: ", format(n, big.mark="."), "\n",
    gen_txt,
    extra
  )
  list(found=TRUE, entity="egresados", raw_answer=raw, n=n, filters=filters,
       render_raw=hay_desglose,
       source="Base Practicantes y Titulados TP 2024 (calculado en tiempo real)")
}

query_data_direct <- function(query, matricula=NULL, docentes=NULL, egresados=NULL,
                              titulados=NULL, base_apoyo=NULL) {
  q <- .norm(query)
  cat("[QDD] q:", substr(q,1,80), "\n")
  if (!.is_data_query(q)) {
    # Aunque no sea cuantitativa, un RBD específico sí es una consulta de datos
    rbd_chk <- .extract_rbd(q)
    if (!is.null(rbd_chk)) {
      cat("[QDD] RBD detectado (sin keyword cuantitativo):", rbd_chk, "\n")
      return(.query_rbd(rbd_chk, matricula, docentes, base_apoyo))
    }
    return(NULL)
  }

  # Prioridad: RBD específico
  rbd_num <- .extract_rbd(q)
  if (!is.null(rbd_num) && !is.null(matricula) && nrow(matricula) > 0) {
    cat("[QDD] Consulta por RBD:", rbd_num, "\n")
    return(.query_rbd(rbd_num, matricula, docentes, base_apoyo))
  }

  # Prioridad: TITULADOS (práctica profesional) — antes del enrutamiento egresado
  if (grepl("titulad|titulaci|practica profesional|\\bpractica\\b|practicante", q) &&
      !is.null(titulados) && nrow(titulados) > 0) {
    cat("[QDD] Consulta de titulados\n")
    tit <- tryCatch(.query_titulados(q, titulados), error=function(e) {
      cat("[QDD] titulados error:", conditionMessage(e), "\n"); NULL })
    if (!is.null(tit)) return(tit)
  }

  # NUEVO — Prioridad: pregunta COMPARATIVA (rural/urbano, región-región, dep-dep)
  # Se intenta antes del enrutamiento normal; si no hay eje comparable, devuelve NULL.
  cmp <- tryCatch(.query_comparative(q, matricula, docentes), error=function(e) {
    cat("[QDD] comparativa error:", conditionMessage(e), "\n"); NULL })
  if (!is.null(cmp)) { cat("[QDD] Comparativa:", cmp$filters, "\n"); return(cmp) }

  # NUEVO — Prioridad: MULTI-ENTIDAD ("estudiantes y docentes en Los Ríos")
  multi <- tryCatch(.query_multi_entity(q, matricula, docentes, egresados),
                    error=function(e) { cat("[QDD] multi error:", conditionMessage(e), "\n"); NULL })
  if (!is.null(multi)) { cat("[QDD] Multi-entidad\n"); return(multi) }

  # Prioridad: keywords de contexto forzado
  force_doc <- .keywords_docente_forzado(q)
  force_mat <- .keywords_matricula_forzado(q)

  # NUEVO: señal geográfica reutilizable (región o comuna detectada) para mejorar
  # el enrutamiento de preguntas de "oferta" sin la palabra literal "region".
  has_geo <- !is.null(.match_region(q)) ||
             (!is.null(matricula) && "nom_com_rbd" %in% names(matricula) &&
              !is.null(.match_comuna(q, matricula$nom_com_rbd)))

  is_doc  <- force_doc || grepl("docente|profesor|\\bprofes?\\b|maestro|educador", q)
  # MEJORA: "liceo/colegio" NO enruta a establecimientos si ya hay entidad
  # estudiante/matrícula (force_mat). "estudiantes en los liceos de Valdivia"
  # cuenta estudiantes; "cuántos liceos hay en Valdivia" cuenta establecimientos.
  is_est  <- !force_doc && !force_mat && grepl("establecimiento|colegio|liceo|escuela|plantel|recinto|\\bee\\b", q)
  is_egr  <- !force_doc && grepl("egresado|titulado|graduado", q)
  # Pregunta sobre qué especialidades OFRECEN ciertos liceos.
  # MEJORA: ahora también enruta aquí cuando hay señal geográfica detectada
  # (ej. "cuéntame sobre la oferta en el Maule" → especialidades del Maule).
  is_esp_breakdown <- !force_doc &&
    grepl("especialidad|especialidades|ofrecen|ofrecidos|oferta", q) &&
    (grepl("rural|urbano|slep|municipal|particular|dependencia|region|liceo|establecimiento", q) ||
     has_geo)
  is_mat  <- !is_doc && !is_est && !is_egr && !is_esp_breakdown
  cat("[QDD] is_doc:", is_doc, "| force_doc:", force_doc, "| is_mat:", is_mat, "\n")

  # Detectar ambigüedad (no hay entidad ni keywords de fuerza, pregunta corta)
  if (.is_ambiguous(q) && nchar(q) < 50) {
    return(list(found=FALSE, entity="ambiguous", clarification=TRUE,
      options=list(
        a=list(label="Estudiantes / matrícula",  q=paste0(query, " (estudiantes)")),
        b=list(label="Docentes de especialidad", q=paste0(query, " (docentes)")),
        c=list(label="Establecimientos EMTP",    q=paste0(query, " (establecimientos)"))
      ), source=NULL))
  }

  if (is_doc && !is.null(docentes)  && nrow(docentes)  > 0) return(.query_docentes(q, docentes))
  if (is_esp_breakdown && !is.null(matricula) && nrow(matricula) > 0) return(.query_especialidades_breakdown(q, matricula))
  if (is_est && !is.null(matricula) && nrow(matricula) > 0) return(.query_establecimientos(q, matricula))
  if (is_mat && !is.null(matricula) && nrow(matricula) > 0) return(.query_matricula(q, matricula))
  if (is_egr && !is.null(egresados) && nrow(egresados) > 0) {
    return(list(found=TRUE, entity="egresados",
      raw_answer=paste0("Egresados EMTP 2024: ", format(nrow(egresados), big.mark="."), " registros."),
      n=nrow(egresados), filters=character(0), source="Base egresados EMTP 2024 (tiempo real)"))
  }
  NULL
}

# =============================================================================
# 4. OLLAMA
# =============================================================================
# =============================================================================
# 4. LLM BACKEND: Groq (primario) con fallback a Ollama local
# =============================================================================
# Groq usa la API compatible OpenAI. Modelo recomendado: llama-3.3-70b-versatile
# API key se lee desde variable de entorno GROQ_API_KEY (nunca hardcodeada).
# Si no hay clave, intenta Ollama local en localhost:11434.

.use_groq <- function() nzchar(Sys.getenv("GROQ_API_KEY"))

check_llm_available <- function(model="llama3.2") {
  if (.use_groq()) {
    # Verificar Groq con un ping mínimo
    key <- Sys.getenv("GROQ_API_KEY")
    tryCatch({
      resp <- request("https://api.groq.com/openai/v1/models") |>
        req_headers(Authorization = paste("Bearer", key)) |>
        req_timeout(5) |> req_error(is_error=function(r) FALSE) |> req_perform()
      list(available = resp_status(resp) == 200,
           model_found = TRUE,
           models = "groq/llama-3.3-70b-versatile")
    }, error=function(e) list(available=FALSE, model_found=FALSE, models=character(0)))
  } else {
    # Fallback: Ollama local
    tryCatch({
      resp <- request("http://localhost:11434/api/tags") |>
        req_timeout(3) |> req_error(is_error=function(r) FALSE) |> req_perform()
      if (resp_status(resp) == 200) {
        nms <- sapply(resp_body_json(resp)$models, function(m) m$name)
        list(available=TRUE, model_found=any(startsWith(nms, model)), models=nms)
      } else list(available=FALSE, model_found=FALSE, models=character(0))
    }, error=function(e) list(available=FALSE, model_found=FALSE, models=character(0)))
  }
}

# Mantener alias para compatibilidad interna
check_ollama_available <- check_llm_available

# MEJORA: ask_llm acepta ahora un `system` prompt (rol "system" en Groq) y un
# `max_tokens` configurable. Un mensaje de sistema fuerte ancla mucho mejor la
# concisión que pedirla solo en el texto del usuario, y bajar max_tokens evita
# respuestas largas. temperature=0.1 reduce la "creatividad" (menos invención).
ask_llm <- function(prompt, model="llama3.2", timeout_sec=120,
                    system=NULL, max_tokens=320) {
  if (.use_groq()) {
    key <- Sys.getenv("GROQ_API_KEY")
    tryCatch({
      msgs <- if (!is.null(system) && nzchar(system))
        list(list(role="system", content=system), list(role="user", content=prompt))
      else
        list(list(role="user", content=prompt))
      resp <- request("https://api.groq.com/openai/v1/chat/completions") |>
        req_headers(
          Authorization = paste("Bearer", key),
          `Content-Type` = "application/json"
        ) |>
        req_body_json(list(
          model       = "llama-3.3-70b-versatile",
          messages    = msgs,
          temperature = 0.1,
          max_tokens  = max_tokens
        )) |>
        req_timeout(timeout_sec) |>
        req_error(is_error=function(r) FALSE) |>
        req_perform()
      if (resp_status(resp) == 200) {
        body <- resp_body_json(resp)
        list(ok=TRUE, text=trimws(body$choices[[1]]$message$content),
             model="groq/llama-3.3-70b-versatile")
      } else {
        msg <- tryCatch(resp_body_json(resp)$error$message, error=function(e) paste("HTTP", resp_status(resp)))
        list(ok=FALSE, text=paste("Error Groq:", msg), model="groq")
      }
    }, error=function(e)
      list(ok=FALSE, text=paste0("Groq no disponible: ", conditionMessage(e)), model="groq"))
  } else {
    # Fallback: Ollama local. El system se antepone al prompt (API /generate).
    full_prompt <- if (!is.null(system) && nzchar(system))
      paste0(system, "\n\n", prompt) else prompt
    tryCatch({
      resp <- request("http://localhost:11434/api/generate") |>
        req_body_json(list(model=model, prompt=full_prompt, stream=FALSE,
                           options=list(temperature=0.1, num_predict=max_tokens, top_p=0.9))) |>
        req_timeout(timeout_sec) |> req_error(is_error=function(r) FALSE) |> req_perform()
      if (resp_status(resp) == 200) {
        body <- resp_body_json(resp)
        list(ok=TRUE, text=trimws(body$response), model=body$model)
      } else list(ok=FALSE, text=paste("Error HTTP", resp_status(resp)), model=model)
    }, error=function(e)
      list(ok=FALSE, text=paste0("LLM no disponible: ", conditionMessage(e)), model=model))
  }
}

# Alias para compatibilidad con llamadas existentes
ask_ollama <- ask_llm


# =============================================================================
# CAPA SEMÁNTICA: texto → plan estructurado (JSON) → cálculo real en R
# -----------------------------------------------------------------------------
# El LLM SOLO traduce la pregunta a un plan de consulta sobre columnas del
# esquema. TODOS los números los calcula R sobre los dataframes reales, por lo
# que es imposible que invente cifras. Si el plan es inválido o no hay LLM,
# rag_answer cae al motor determinista (.query_*) que ya existía.
# =============================================================================

# Esquema documentado: lo único que el LLM puede usar (nombres + códigos).
# Se mantiene en ASCII a propósito (el matcher .norm ya ignora tildes/mayúsculas).
.semantic_schema_text <- function() {
'BASES Y COLUMNAS DISPONIBLES (usa EXACTAMENTE estos nombres de columna):

base "matricula" (1 fila por estudiante-especialidad, matricula EMTP 2025):
- nom_reg_rbd_a: region del establecimiento (texto; ej: Maule, Biobio, Metropolitana, La Araucania)
- nom_deprov_rbd: departamento provincial de educacion (DEPROV)
- nom_com_rbd: comuna del establecimiento
- nom_com_alu: comuna de DOMICILIO del estudiante (distinta a la del liceo)
- cod_depe2: dependencia (1=Municipal, 2=Particular Subvencionado, 3=Particular Pagado, 4=Corporacion, 5=SLEP)
- nombre_sost: nombre del sostenedor
- nombre_slep: nombre del SLEP
- RuralidadRBD: ruralidad del establecimiento (valores: Rural, Urbano)
- nom_espe: especialidad
- nom_sector: sector economico de la especialidad (texto; ej: Minero, Metalmecanico, Salud y Educacion, Construccion)
- cod_sec: codigo del sector economico
- cod_grado: grado (3=1ro medio, 4=2do medio, 5=3ro medio, 6=4to medio)
- cod_jor: jornada
- gen_alu: genero del estudiante (1=Hombre, 2=Mujer)
- edad_alu: edad del estudiante (numero)
- categoria_asis_anual: categoria de asistencia anual del estudiante (1=Inasistencia critica <50%, 2=grave 50-84%, 3=reiterada 85-89%, 4=esperada >=90%)
- tasa_asis_anual: porcentaje de asistencia anual del estudiante (numero 0-100; usar con metric=mean)
- rft: pertenece a Red Futuro Tecnico (valor de texto: PERTENECE)
- cod_etnia_alu: etnia (numero; mayor a 0 = declara pueblo originario)
- cod_nac_alu: nacionalidad (numero)
- tipo_ensenanza_emtp: Jovenes / Adultos / Ambos
- rbd: id establecimiento ; mrun: id estudiante

base "docentes" (1 fila por docente-contrato, EMTP 2025):
- NOM_REG_RBD_A, NOM_DEPROV_RBD, NOM_COM_RBD: geografia (mismos valores que matricula)
- COD_DEPE2: dependencia (mismos codigos que matricula)
- RURAL_RBD: ruralidad (0=Urbano, 1=Rural)
- DOC_GENERO: genero docente (1=Hombre, 2=Mujer)
- TRAMO_CARR_DOCENTE: tramo de la carrera docente (texto)
- TIT_ID_1, TIT_ID_2: tipo de titulo (1=Educacion/pedagogia, 2=otro universitario, 3=sin titulo universitario)
- HORAS_CONTRATO, HORAS_AULA: horas (numero)
- SUBSECTOR1, SUBSECTOR2: subsector/especialidad (codigo 41001-81004 = modulos de especialidad TP)
- Poblacion: Jovenes / Adultos / Ambas
- MRUN: id docente (persona) ; RBD: id establecimiento

base "egresados" (egresados EMTP 2024):
- NOM_REG_RBD_A, NOM_COM_RBD, NOM_DEPROV_RBD: geografia
- DEPENDENCIA_label: dependencia (texto)
- RURALIDAD_label: Rural / Urbano
- TIPO_ENSE_label: tipo de ensenanza
- PROM_NOTAS_ALU: promedio de notas del egresado (numero)
- RBD, MRUN

base "continuidad" (continuidad a educacion superior 2024 a 2025):
- continua_es: continua en educacion superior (1=Si, 0=No)
- tipo_inst_3: tipo de institucion (CFT, IP, Universidad)
- nivel_carrera_2: nivel de la carrera
- acreditada_inst: institucion acreditada
- DEPENDENCIA_label, RURALIDAD_label, TIPO_ENSE_label: cruces
- GEN_ALU: genero (1=Hombre, 2=Mujer) ; MRUN: id'
}

# Prompt que obliga al LLM a devolver SOLO un plan JSON.
.semantic_plan_prompt <- function(query) {
  sys <- paste0(
    "Eres un traductor de preguntas a CONSULTAS estructuradas sobre bases EMTP de Chile. ",
    "NO respondes con texto ni con numeros: devuelves UNICAMENTE un objeto JSON valido.\n\n",
    .semantic_schema_text(), "\n\n",
    "FORMATO JSON (exactamente estas claves):\n",
    '{ "dataset":"matricula|docentes|egresados|continuidad", "metric":"count_rows|count_distinct|mean|rate|sum|share|avg_per_rbd", "distinct_col":"col", "value_col":"col", "filters":[{"col":"col","op":"==|!=|>|>=|<|<=|in|contains","val":"valor"}], "group_by":["col"], "sort":"desc|asc", "limit":20 }',
    "\n\nREGLAS:\n",
    "- Usa SOLO columnas del esquema. Si la pregunta NO se puede responder con estas bases, devuelve {\"dataset\":null}.\n",
    "- Estudiantes unicos: metric=count_distinct, distinct_col=mrun. 'Matricula'/'registros': metric=count_rows.\n",
    "- Docentes (personas): metric=count_distinct, distinct_col=MRUN. Establecimientos: count_distinct con distinct_col=rbd o RBD.\n",
    "- Promedios (notas, edad, horas): metric=mean con value_col. Porcentaje/tasa sobre variable 0/1 (ej. continua_es): metric=rate con value_col.\n",
    "- 'por X' / 'segun X' / 'comparar X' => X va en group_by. Maximo 2 columnas en group_by.\n",
    "- SUPERLATIVOS/RANKING ('cual tiene mas/menos X', 'que region tiene mas...', 'top', 'ranking'): pon esa dimension en group_by, sort=desc (o asc para 'menos'), y limit pequeno (ej. 5 para ranking, 1 para 'el que mas').\n",
    "- PORCENTAJE DEL TOTAL ('que porcentaje del total nacional...'): metric=share con los filtros del subconjunto y SIN group_by (calcula el subconjunto sobre el total de la base).\n",
    "- PROMEDIO POR ESTABLECIMIENTO ('cuantos estudiantes/docentes por liceo en promedio'): metric=avg_per_rbd (divide registros entre establecimientos unicos).\n",
    "- Para texto (region, comuna, especialidad) escribe el nombre tal cual; se compara sin distinguir mayusculas ni tildes (usa op contains si es parcial).\n",
    "- Devuelve SOLO el JSON, sin explicacion ni comentarios."
  )
  list(sys = sys, user = paste0("PREGUNTA: ", query, "\n\nJSON:"))
}

# Extrae y parsea el primer objeto JSON del texto del LLM.
.semantic_parse_plan <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  txt <- gsub("```json|```", "", txt)
  st <- regexpr("\\{", txt)
  ends <- gregexpr("\\}", txt)[[1]]
  if (st < 1 || ends[1] < 1) return(NULL)
  js <- substr(txt, st, max(ends))
  plan <- tryCatch(
    jsonlite::fromJSON(js, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE),
    error = function(e) NULL)
  if (is.null(plan) || is.null(plan$dataset) || identical(plan$dataset, "null") ||
      (is.list(plan$dataset) && length(plan$dataset) == 0)) return(NULL)
  # Normalizar filters a lista de listas
  f <- plan$filters
  if (!is.null(f) && !is.null(names(f)) && "col" %in% names(f)) plan$filters <- list(f)
  plan
}

.sem_pick_df <- function(dataset, mat, doc, egr, cont) {
  switch(dataset %||% "", matricula = mat, docentes = doc,
         egresados = egr, continuidad = cont, NULL)
}

# Resuelve un nombre de columna sin distinguir mayusculas/minusculas.
.sem_col <- function(df, name) {
  if (is.null(name) || !nzchar(as.character(name)[1])) return(NA_character_)
  nm <- names(df); hit <- nm[tolower(nm) == tolower(as.character(name)[1])]
  if (length(hit)) hit[1] else NA_character_
}

# Etiqueta legible para valores codificados (en agrupaciones).
.sem_relabel <- function(col, val) {
  cl <- tolower(col); v <- as.character(val)
  m <- switch(cl,
    gen_alu      = c("1"="Hombres","2"="Mujeres"),
    doc_genero   = c("1"="Hombres","2"="Mujeres"),
    gen_alu_es   = c("1"="Hombres","2"="Mujeres"),
    cod_depe2    = c("1"="Municipal","2"="Part. Subvencionado","3"="Part. Pagado","4"="Corporacion","5"="SLEP"),
    rural_rbd    = c("0"="Urbano","1"="Rural"),
    continua_es  = c("0"="No continua","1"="Continua"),
    cod_grado    = c("3"="1ro medio","4"="2do medio","5"="3ro medio","6"="4to medio"),
    tit_id_1     = c("1"="Pedagogia","2"="Otro titulo univ.","3"="Sin titulo univ."),
    NULL)
  if (is.null(m)) return(v)
  out <- m[v]; ifelse(is.na(out), v, unname(out))
}

.sem_fmt <- function(v, metric) {
  if (length(v) == 0 || is.na(v)) return("sin dato")
  if (metric == "mean") return(format(round(v, 2), nsmall = 2, big.mark = "."))
  if (metric == "avg_per_rbd") return(format(round(v, 1), nsmall = 1, big.mark = "."))
  if (metric == "rate") return(paste0(format(round(v, 1), nsmall = 1), "%"))
  format(round(v), big.mark = ".")
}

# Ejecuta el plan sobre los datos REALES. Devuelve lista tipo "direct" o NULL.
.execute_plan <- function(plan, mat, doc, egr, cont) {
  df <- .sem_pick_df(plan$dataset, mat, doc, egr, cont)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df_all <- df  # copia completa (denominador para metric=share)

  # --- Filtros (con validación de columnas: si no existe, se ignora) ---
  labels <- character(0)
  for (f in (plan$filters %||% list())) {
    col <- .sem_col(df, f$col); if (is.na(col)) next
    op  <- f$op %||% "=="; val <- f$val
    x   <- df[[col]]
    if (op == "contains") {
      pat  <- .norm(as.character(val)[1])
      keep <- !is.na(x) & grepl(pat, .norm(as.character(x)), fixed = TRUE)
    } else if (op == "in") {
      vals <- .norm(as.character(unlist(val)))
      keep <- !is.na(x) & .norm(as.character(x)) %in% vals
    } else if (op %in% c("==", "!=")) {
      num_ok <- is.numeric(x) && !is.na(suppressWarnings(as.numeric(as.character(val)[1])))
      if (num_ok) {
        v <- as.numeric(as.character(val)[1])
        keep <- if (op == "==") (!is.na(x) & x == v) else (!is.na(x) & x != v)
      } else {
        v <- .norm(as.character(val)[1]); xn <- .norm(as.character(x))
        keep <- if (op == "==") (!is.na(x) & xn == v) else (!is.na(x) & xn != v)
      }
    } else if (op %in% c(">", ">=", "<", "<=")) {
      xn <- suppressWarnings(as.numeric(as.character(x)))
      v  <- suppressWarnings(as.numeric(as.character(val)[1]))
      keep <- !is.na(xn) & switch(op, ">" = xn > v, ">=" = xn >= v, "<" = xn < v, "<=" = xn <= v)
    } else next
    df <- df[keep, , drop = FALSE]
    labels <- c(labels, paste0(f$col, " ", op, " ", paste(unlist(val), collapse = "/")))
  }

  metric <- plan$metric %||% "count_rows"
  metric_lbl <- switch(metric,
    count_distinct = "conteo unico", mean = "promedio", rate = "tasa (%)",
    sum = "suma", share = "% del total", avg_per_rbd = "promedio por establecimiento",
    "registros")

  metric_fun <- function(sub) {
    if (nrow(sub) == 0) return(if (metric %in% c("mean","rate","avg_per_rbd")) NA_real_ else 0)
    if (metric == "count_rows") return(nrow(sub))
    if (metric == "count_distinct") {
      dc <- .sem_col(sub, plan$distinct_col %||% ""); if (is.na(dc)) return(nrow(sub))
      return(length(unique(sub[[dc]])))
    }
    if (metric == "avg_per_rbd") {
      rc <- .sem_col(sub, "rbd"); if (is.na(rc)) rc <- .sem_col(sub, "RBD")
      if (is.na(rc)) return(NA_real_)
      nd <- length(unique(sub[[rc]])); if (nd == 0) return(NA_real_)
      return(round(nrow(sub) / nd, 1))
    }
    vc <- .sem_col(sub, plan$value_col %||% ""); if (is.na(vc)) return(NA_real_)
    v  <- suppressWarnings(as.numeric(as.character(sub[[vc]])))
    if (metric == "mean") return(mean(v, na.rm = TRUE))
    if (metric == "rate") return(100 * mean(v, na.rm = TRUE))
    if (metric == "sum")  return(sum(v, na.rm = TRUE))
    nrow(sub)
  }

  # --- group_by (máx 2 columnas válidas) ---
  gcols <- character(0)
  for (g in (plan$group_by %||% character(0))) { cc <- .sem_col(df, g); if (!is.na(cc)) gcols <- c(gcols, cc) }
  gcols <- head(gcols, 2)

  flt_txt <- if (length(labels)) paste(labels, collapse = " | ") else "sin filtros"

  # --- metric=share: porcentaje del subconjunto respecto del TOTAL de la base ---
  if (metric == "share") {
    base_count <- function(sub) {
      dc <- .sem_col(sub, plan$distinct_col %||% "")
      if (!is.na(dc)) length(unique(sub[[dc]])) else nrow(sub)
    }
    denom <- base_count(df_all)
    if (length(gcols) == 0) {
      num <- base_count(df)
      val <- if (denom > 0) round(100 * num / denom, 1) else NA_real_
      raw <- paste0("Base ", plan$dataset, " — % del total [", flt_txt, "]:\n",
                    "  Resultado: ", ifelse(is.na(val), "sin dato", paste0(format(val, nsmall = 1), "%")),
                    "  (", format(num, big.mark = "."), " de ", format(denom, big.mark = "."), ")\n")
      return(list(found = TRUE, entity = plan$dataset, raw_answer = raw, n = val,
                  filters = labels, source = paste0("Base ", plan$dataset, " EMTP (calculado en tiempo real)")))
    }
    # share por grupo (% de cada grupo respecto del total de la base)
    g1 <- gcols[1]
    parts <- split(seq_len(nrow(df)), as.character(df[[g1]]))
    rows <- lapply(names(parts), function(k) {
      pc <- base_count(df[parts[[k]], , drop = FALSE])
      list(lbl = .sem_relabel(g1, k), val = if (denom > 0) round(100 * pc / denom, 1) else NA_real_)
    })
    rows <- rows[order(-vapply(rows, function(r) as.numeric(r$val), numeric(1)))]
    rows <- head(rows, 40)
    lines <- vapply(rows, function(r) paste0("  ", r$lbl, ": ",
                    ifelse(is.na(r$val), "sin dato", paste0(format(r$val, nsmall = 1), "%"))), character(1))
    raw <- paste0("Base ", plan$dataset, " — % del total por ", g1, " [", flt_txt, "]:\n",
                  paste(lines, collapse = "\n"), "\n")
    return(list(found = TRUE, entity = plan$dataset, raw_answer = raw, render_raw = TRUE,
                n = length(rows), filters = labels,
                source = paste0("Base ", plan$dataset, " EMTP (calculado en tiempo real)")))
  }

  if (length(gcols) == 0) {
    val <- metric_fun(df)
    raw <- paste0("Base ", plan$dataset, " — ", metric_lbl,
                  " [", flt_txt, "]:\n  Resultado: ", .sem_fmt(val, metric),
                  "  (filas consideradas: ", format(nrow(df), big.mark="."), ")\n")
    return(list(found = TRUE, entity = plan$dataset, raw_answer = raw,
                n = val, filters = labels, source = paste0("Base ", plan$dataset, " EMTP (calculado en tiempo real)")))
  }

  # Agrupar
  keyvals <- lapply(gcols, function(c) as.character(df[[c]]))
  keystr  <- do.call(paste, c(keyvals, list(sep = "")))
  idxs    <- split(seq_len(nrow(df)), keystr)
  rows <- lapply(names(idxs), function(k) {
    parts <- strsplit(k, "", fixed = TRUE)[[1]]
    list(parts = parts, val = metric_fun(df[idxs[[k]], , drop = FALSE]))
  })
  vals <- vapply(rows, function(r) as.numeric(r$val), numeric(1))
  ord  <- if ((plan$sort %||% "desc") == "asc") order(vals) else order(-vals)
  rows <- rows[ord]
  lim  <- suppressWarnings(as.integer(plan$limit %||% 20)); if (is.na(lim) || lim < 1) lim <- 20
  rows <- head(rows, min(lim, 40))

  lines <- vapply(rows, function(r) {
    etiqueta <- paste(mapply(function(col, v) .sem_relabel(col, v), gcols, r$parts), collapse = " | ")
    paste0("  ", etiqueta, ": ", .sem_fmt(r$val, metric))
  }, character(1))

  raw <- paste0("Base ", plan$dataset, " — ", metric_lbl, " por ",
                paste(gcols, collapse = " + "), " [", flt_txt, "]:\n",
                paste(lines, collapse = "\n"), "\n")
  # render_raw=TRUE: el desglose se muestra tal cual (no se colapsa a 2 oraciones)
  list(found = TRUE, entity = plan$dataset, raw_answer = raw, render_raw = TRUE,
       n = length(rows), filters = labels,
       source = paste0("Base ", plan$dataset, " EMTP (calculado en tiempo real)"))
}

# Orquesta: pregunta -> plan (LLM) -> ejecucion (R). Devuelve lista "direct" o NULL.
.answer_semantic <- function(query, mat, doc, egr, cont, model = "llama3.2") {
  pr   <- .semantic_plan_prompt(query)
  resp <- tryCatch(ask_llm(pr$user, model = model, system = pr$sys, max_tokens = 320),
                   error = function(e) list(ok = FALSE))
  if (!isTRUE(resp$ok)) return(NULL)
  plan <- .semantic_parse_plan(resp$text)
  if (is.null(plan)) { cat("[SEM] plan no parseable\n"); return(NULL) }
  cat("[SEM] plan:", plan$dataset, "| metric:", plan$metric %||% "?",
      "| group_by:", paste(plan$group_by, collapse=","), "\n")
  out <- tryCatch(.execute_plan(plan, mat, doc, egr, cont),
                  error = function(e) { cat("[SEM] exec error:", conditionMessage(e), "\n"); NULL })
  out
}

# ¿La pregunta requiere la CAPA SEMÁNTICA? (modo híbrido)
# TRUE solo cuando pide un cruce/medida que el motor determinista NO cubre bien:
#   (1) agrupaciones "por X" / "según" / "distribución";
#   (2) métricas que el motor no calcula (promedio, notas, edad, horas, tasas);
#   (3) dimensiones no cubiertas (tramo docente, etnia, nacionalidad, domicilio,
#       jornada, tipo de institución de continuidad, acreditación, sostenedor...).
# Las preguntas comunes (conteos por región/comuna/dependencia/ruralidad/género,
# y las comparativas rural/urbano o región-región) NO la activan: las atiende el
# motor determinista, que da respuestas más ricas (desglose de género, EE, etc.).
.needs_semantic <- function(qn) {
  grupo <- grepl(paste0(
    "\\bpor (region|regiones|comuna|comunas|dependencia|dependencias|especialidad|",
    "especialidades|sexo|genero|grado|sostenedor|slep|ruralidad|tramo|institucion|",
    "instituciones|nivel|titulo|poblacion|jornada|etnia|nacionalidad|edad)\\b|",
    "\\bsegun\\b|desglos|distribu|ranking|\\bpor cada\\b|\\ben cada\\b|",
    "\\bcada (region|comuna|dependencia|especialidad)\\b"), qn)
  metrica <- grepl(
    "promedio|\\bmedia\\b|\\bnotas?\\b|\\bedad\\b|\\bhoras\\b|tasa de|porcentaje de continu", qn)
  dimension <- grepl(paste0(
    "tramo|carrera docente|etnia|indigena|originario|mapuche|nacionalidad|extranjer|",
    "migrant|domicilio|jornada|acreditad|\\bcft\\b|instituto profesional|tipo de institucion|",
    "nivel de carrera|sostenedor"), qn)
  # Superlativos / rankings: "cual es la especialidad con MAS matricula",
  # "que region tiene MAS docentes", "ranking de comunas", "top 5..."
  superlativo <- grepl(paste0(
    "\\b(con mas|con menos|que mas|que menos|el que mas|la que mas|los que mas|",
    "mayor numero|menor numero|mas alta|mas baja|mas alto|mas bajo|ranking|\\btop\\b)\\b|",
    "\\b(cual|cuales|que|donde|quien)\\b[^.?!]{0,45}\\b(mas|menos|mayor|menor)\\b"), qn)
  # Porcentaje respecto del total (benchmark): "que porcentaje del total ..."
  share <- grepl("porcentaje del total|del total nacional|proporcion del total|que parte del total", qn)
  # Umbrales numéricos: "liceos con mas de 500 estudiantes", "docentes mayores de 60",
  # "especialidades con menos de 100 alumnos", "entre 200 y 500" → operadores >/<.
  umbral <- grepl(paste0(
    "\\bmas de \\d|\\bmenos de \\d|mayor(es)? (a|de|que) \\d|menor(es)? (a|de|que) \\d|",
    "entre \\d+ y \\d+|al menos \\d|por lo menos \\d|\\bsobre \\d|\\bbajo \\d"), qn)
  grupo || metrica || dimension || superlativo || share || umbral
}

rag_answer <- function(query, index, kb, model="llama3.2", top_k=3,
                       matricula=NULL, docentes=NULL, egresados=NULL, continuidad=NULL,
                       titulados=NULL, base_apoyo=NULL) {

  # 1. Resolución de la consulta de datos — modo HÍBRIDO:
  #    (a) RBD específico        → ficha determinista (precisa, offline)
  #    (b) Motor determinista    → preguntas comunes (respuestas ricas)
  #    (c) Capa semántica        → SOLO cruces avanzados (.needs_semantic)
  #    (d) Capa semántica        → último recurso si (b) no resolvió y hay LLM
  qn_rag  <- .norm(query)
  es_norm <- .is_normativa_query(qn_rag)
  direct  <- NULL

  # 0. Fuera de alcance (tendencias / opinión): responder con honestidad, sin
  #    enrutar a datos (evita conteos engañosos). No aplica a normativa.
  if (!es_norm) {
    oos <- .scope_note(qn_rag)
    if (!is.null(oos))
      return(list(answer = oos, sources = character(0), scores = numeric(0), ok = TRUE))
  }

  if (!es_norm) {
    .det <- function() tryCatch(
      query_data_direct(query, matricula=matricula, docentes=docentes, egresados=egresados,
                        titulados=titulados, base_apoyo=base_apoyo),
      error=function(e) { cat("[RAG] det error:", conditionMessage(e), "\n"); NULL })
    .sem <- function() .answer_semantic(query, matricula, docentes, egresados, continuidad, model = model)

    if (!is.null(.extract_rbd(qn_rag))) {
      # (a) Ficha por RBD
      direct <- .det()
    } else {
      # (b) Determinista primero (más rico para lo común)
      det <- .det()
      es_aclaracion <- !is.null(det) && isTRUE(det$clarification)
      # Titulados se resuelven SOLO con el motor determinista: la capa semántica
      # no conoce esa base y los forzaría sobre 'egresados' (cifra errónea).
      es_titulados <- grepl("titulad|titulaci|practicante|practica profesional", qn_rag) &&
                      !is.null(det) && isTRUE(det$found)
      # Si el motor determinista ya produjo un DESGLOSE rico (asistencia, sector,
      # ficha RBD, etc. → render_raw), ese resultado es autoritativo y más completo
      # que el número único de la capa semántica: no lo sobrescribimos.
      det_rico <- !is.null(det) && isTRUE(det$found) && isTRUE(det$render_raw)
      # (c) Cruce avanzado → preferir capa semántica (si resuelve)
      if (!es_aclaracion && !es_titulados && !det_rico &&
          .use_groq() && .is_data_query(qn_rag) && .needs_semantic(qn_rag)) {
        cat("[RAG] cruce avanzado → capa semántica\n")
        sem <- .sem()
        direct <- if (!is.null(sem)) sem else det
      } else {
        direct <- det
      }
      # (d) Último recurso: determinista no resolvió y hay LLM
      if (is.null(direct) && .use_groq() && .is_data_query(qn_rag)) direct <- .sem()
    }
  }

  # Mapeo entidad → pestaña de la app donde profundizar
  .tab_hint <- function(entity) {
    switch(entity,
      matricula        = "\U0001F4CA Para explorar visualmente estos datos (incluida asistencia, SIMCE/IDPS y sector económico por territorio), ve a la pestaña **Análisis Territorial** o **Visualizaciones Matrícula** en la app.",
      docentes         = "\U0001F4CA Para ver detalles y gráficos, ve a la pestaña **Docentes** en la app (sub-pestañas: Género, Títulos y Función, Detalle).",
      establecimientos = "\U0001F4CA Para filtrar establecimientos y descargar sus minutas (PDF/Excel), usa la pestaña **Establecimientos** en la app.",
      rbd              = "\U0001F4CA Para ver la ficha completa con SIMCE, IDPS, IVE/GSE y asistencia, y descargar su minuta (PDF/Excel), busca este establecimiento en la pestaña **Establecimientos** de la app.",
      egresados        = "\U0001F4CA Para explorar continuidad y titulación, ve a la pestaña **Egresados y Titulados** en la app.",
      continuidad      = "\U0001F4CA Para explorar la continuidad en educación superior, ve a la pestaña **Egresados y Titulados** (sub-pestaña Continuidad de Estudios).",
      ""
    )
  }

  if (!is.null(direct) && isTRUE(direct$found)) {
    cat("[RAG] Consulta directa:", direct$entity, "| n=", direct$n, "\n")
    hint <- .tab_hint(direct$entity)
    # Resultados con desglose (tabla): mostrar tal cual, sin colapsar con el LLM
    if (isTRUE(direct$render_raw)) {
      ans <- paste0(direct$raw_answer, if (nzchar(hint)) paste0("\n\n", hint) else "")
      return(list(answer=ans, sources=c(direct$source), scores=numeric(0), ok=TRUE))
    }
    # MEJORA: mensaje de SISTEMA con reglas duras de concisión y anti-invención.
    # El system prompt ancla el comportamiento mucho mejor que pedirlo en el user.
    sys_direct <- paste0(
      "Eres el asistente de datos del Explorador EMTP (educación técnico-profesional ",
      "de Chile). Reglas INVIOLABLES:\n",
      "1) Responde SOLO con las cifras del bloque DATOS; está prohibido inventar, ",
      "estimar, redondear o añadir números que no aparezcan ahí.\n",
      "2) Si los DATOS no contienen la respuesta, dilo en una frase; no rellenes.\n",
      "3) Máximo 2 oraciones. Sin listas, sin encabezados, sin preámbulos como ",
      "'Según los datos'. Ve directo a la cifra.\n",
      "4) Si la pregunta es de existencia ('¿hay...?', '¿existe...?', '¿tiene...?'), ",
      "empieza con 'Sí' o 'No' según la cifra y luego dala.\n",
      "5) Español de Chile, tono claro y profesional."
    )
    prompt <- paste0(
      "DATOS (calculados en tiempo real desde la base oficial):\n",
      direct$raw_answer, "\n\n",
      "PREGUNTA DEL USUARIO: ", query, "\n\n",
      "Redacta la respuesta (máx. 2 oraciones, solo con esas cifras):"
    )
    # max_tokens bajo (180) refuerza la brevedad a nivel de API.
    result <- ask_ollama(prompt, model=model, system=sys_direct, max_tokens=180)
    # Si Ollama no está disponible, devolver raw_answer directamente
    if (!result$ok) {
      answer_final <- paste0(direct$raw_answer, if (nzchar(hint)) paste0("\n\n", hint) else "")
      return(list(answer=answer_final, sources=c(direct$source), scores=numeric(0), ok=TRUE))
    }
    answer_final <- if (nzchar(hint)) paste0(result$text, "\n\n", hint) else result$text
    return(list(answer=answer_final, sources=c(direct$source),
                scores=numeric(0), ok=result$ok))
  }

  # 1b. Pregunta ambigua — pedir aclaración sin llamar a Ollama
  if (!is.null(direct) && isTRUE(direct$clarification)) {
    cat("[RAG] Aclaración solicitada\n")
    opts <- direct$options
    return(list(
      answer   = NULL,
      clarification = TRUE,
      options  = opts,
      sources  = character(0), scores=numeric(0), ok=TRUE
    ))
  }

  # 1c. Si es una consulta de DATOS que el motor no pudo resolver, NO usar el RAG
  #     documental (decretos/leyes): produce respuestas confusas y fuera de tema.
  #     Mejor un mensaje honesto que orienta a reformular.
  if (!es_norm && .is_data_query(qn_rag)) {
    cat("[RAG] data query sin resolver → mensaje honesto (no RAG documental)\n")
    return(list(
      answer = paste0(
        "No pude calcular ese dato puntual con seguridad. ¿Puedes reformularlo de forma ",
        "más simple? Por ejemplo: *\"matrícula de Electricidad en Valdivia\"*, ",
        "*\"liceos rurales en Los Ríos\"* o *\"docentes de Mecánica en el Biobío\"*. ",
        "También puedes explorar ese cruce en las pestañas de la app."),
      sources = character(0), scores = numeric(0), ok = TRUE))
  }

  # 2. Fallback: TF-IDF + Ollama (solo preguntas normativas/conceptuales)
  retrieved <- retrieve_docs_with_kb(query, index, kb, top_k=top_k)
  if (length(retrieved$docs) == 0)
    return(list(answer="No encontré información relevante para esa pregunta.", sources=character(0), scores=numeric(0), ok=FALSE))

  context_text <- paste(lapply(retrieved$docs, function(d)
    paste0("[FUENTE: ", d$titulo, " | relevancia: ", d$score, "]\n", d$cuerpo)),
    collapse="\n\n---\n\n")

  # MEJORA: reglas de uso de fragmentos movidas a un system prompt; el user solo
  # lleva fragmentos + pregunta. max_tokens 380 ≈ 2 párrafos.
  sys_rag <- paste0(
    "Eres el asistente normativo del Explorador EMTP (Chile). Respondes usando ",
    "EXCLUSIVAMENTE los fragmentos de documentos oficiales entregados. Reglas:\n",
    "1) Usa solo los fragmentos directamente relevantes; ignora los que tratan ",
    "otro tema aunque compartan palabras.\n",
    "2) Prohibido inventar. Si los fragmentos no responden, dilo en una frase.\n",
    "3) Máximo 2 párrafos cortos, en español de Chile.\n",
    "4) Cita el decreto o ley de origen cuando afirmes algo normativo."
  )
  prompt <- paste0(
    "FRAGMENTOS:\n", context_text, "\n\n",
    "PREGUNTA DEL USUARIO: ", query, "\n\n",
    "Responde de forma concisa (máx. 2 párrafos):"
  )

  result <- ask_ollama(prompt, model=model, system=sys_rag, max_tokens=380)
  # Si Ollama no está disponible, devolver los fragmentos RAG directamente
  if (!result$ok) {
    raw_rag <- paste0(
      "Información encontrada en documentos oficiales:\n\n",
      paste(lapply(seq_along(retrieved$docs), function(i)
        paste0("**", retrieved$docs[[i]]$titulo, "**\n", retrieved$docs[[i]]$cuerpo)
      ), collapse="\n\n---\n\n")
    )
    return(list(answer=raw_rag,
                sources=sapply(retrieved$docs, function(d) d$titulo),
                scores=retrieved$scores, ok=TRUE))
  }
  list(answer=result$text, sources=sapply(retrieved$docs, function(d) d$titulo),
       scores=retrieved$scores, ok=result$ok)
}

# =============================================================================
# 5. UI FLOTANTE
# =============================================================================
chatbot_floating_ui <- function() {
  tagList(
    tags$style(HTML("
      #chatbot-bubble-btn {
        position:fixed; bottom:28px; right:28px; width:56px; height:56px;
        border-radius:50%; background:#34536A; border:none; cursor:pointer;
        box-shadow:0 4px 16px rgba(0,0,0,0.28); z-index:9998;
        display:flex; align-items:center; justify-content:center;
        color:white; font-size:22px; transition:transform 0.2s,background 0.2s;
      }
      #chatbot-bubble-btn:hover { background:#2a4357; transform:scale(1.08); }
      #chatbot-panel {
        position:fixed; bottom:96px; right:24px; width:380px; max-height:560px;
        background:white; border-radius:16px; box-shadow:0 8px 32px rgba(0,0,0,0.22);
        z-index:9997; display:none; flex-direction:column; overflow:hidden;
        border:1px solid #e0e0e0;
        font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;
      }
      #chatbot-panel.open { display:flex; }
      #chatbot-panel-header {
        background:linear-gradient(135deg,#34536A 0%,#2a4357 100%);
        color:white; padding:14px 16px 12px;
        display:flex; align-items:center; justify-content:space-between; flex-shrink:0;
      }
      .chatbot-header-title { font-weight:600; font-size:15px; }
      .chatbot-header-sub   { font-size:11px; opacity:0.8; margin-top:2px; }
      #chatbot-close-btn {
        background:none; border:none; color:white; cursor:pointer;
        font-size:20px; line-height:1; padding:0 4px; opacity:0.8;
      }
      #chatbot-close-btn:hover { opacity:1; }
      #chatbot-history {
        flex:1; overflow-y:auto; padding:14px 12px;
        min-height:200px; max-height:340px; background:#f8f9fa;
      }
      .chat-msg-user { display:flex; justify-content:flex-end; margin-bottom:8px; }
      .chat-bubble-user {
        background:#34536A; color:white; padding:8px 13px;
        border-radius:16px 16px 4px 16px; max-width:80%; font-size:13px; line-height:1.45;
      }
      .chat-msg-bot  { display:flex; flex-direction:column; align-items:flex-start; margin-bottom:8px; }
      .chat-bubble-bot {
        background:white; color:#2c3e50; padding:8px 13px;
        border-radius:16px 16px 16px 4px; max-width:85%; font-size:13px; line-height:1.5;
        border:1px solid #e0e0e0; box-shadow:0 1px 3px rgba(0,0,0,0.06);
      }
      .chat-bubble-thinking {
        background:#f0f0f0; color:#888; font-style:italic; font-size:12px;
        padding:7px 12px; border-radius:16px 16px 16px 4px; display:inline-block;
      }
      .chat-sources {
        margin-top:4px; padding:4px 8px; background:#EBF3FB; border-radius:6px;
        font-size:11px; color:#1F4E79; border-left:3px solid #34536A; max-width:85%;
      }
      .chat-empty { text-align:center; color:#aaa; padding:40px 20px; font-size:13px; }
      #chatbot-input-area {
        padding:10px 12px; background:white; border-top:1px solid #eee; flex-shrink:0;
      }
      .chat-chips { display:flex; flex-wrap:wrap; gap:5px; margin-bottom:8px; }
      .chat-chip {
        background:#f0f4f7; border:1px solid #d0dde6; color:#34536A;
        border-radius:12px; padding:3px 10px; font-size:11px; cursor:pointer;
      }
      .chat-chip:hover { background:#d6e8f5; }
      #chatbot-input-row { display:flex; gap:8px; align-items:flex-end; }
      #chatbot-raw-input {
        flex:1; border:1px solid #ddd; border-radius:10px; padding:8px 12px;
        font-size:13px; resize:none; outline:none; font-family:inherit; line-height:1.4;
        box-sizing:border-box;
      }
      #chatbot-raw-input:focus { border-color:#34536A; }
      #chatbot-raw-send {
        background:#34536A; color:white; border:none; border-radius:10px;
        padding:0 16px; font-size:16px; cursor:pointer; flex-shrink:0;
        height:56px; transition:background 0.2s;
      }
      #chatbot-raw-send:hover { background:#2a4357; }
      #chatbot-status-bar { font-size:10px; color:#aaa; margin-top:5px; text-align:right; }
    ")),

    # Botón burbuja
    tags$button(
      id = "chatbot-bubble-btn",
      title = "Asistente EMTP",
      HTML('<i class="fa fa-robot"></i>'),
      onclick = "chatbotToggle()"
    ),

    # Panel flotante
    div(id = "chatbot-panel",
      # Cabecera
      div(id = "chatbot-panel-header",
        div(
          div(class="chatbot-header-title", HTML('<i class="fa fa-robot"></i> Asistente EMTP')),
          div(style="display:flex;align-items:center;gap:6px;margin-top:2px;",
            tags$span(style=paste0(
              "background:rgba(255,255,255,0.18);color:#fff;font-size:9px;",
              "font-weight:600;letter-spacing:.5px;padding:2px 6px;",
              "border-radius:4px;text-transform:uppercase;"
            ), "Prototipo"),
            div(class="chatbot-header-sub", style="margin-top:0;", "prototipo en desarrollo")
          )
        ),
        div(style="display:flex;align-items:center;gap:8px;",
          uiOutput("chatbot_status_dot", inline=TRUE),
          tags$button(id="chatbot-close-btn", HTML("&times;"), onclick="chatbotToggle()")
        )
      ),

      # Historial de mensajes
      div(id = "chatbot-history",
        uiOutput("chat_history_ui")
      ),

      # Input
      div(id = "chatbot-input-area",
        div(class = "chat-chips",
          lapply(list(
            "¿Cuántos estudiantes tiene el sistema EMTP?",
            "Matrícula por sector económico",
            "¿Cuál es la asistencia promedio de los estudiantes?",
            "Titulados por especialidad",
            "Dame la ficha del RBD 1",
            "¿Qué dice el Decreto 452?"
          ), function(q) {
            tags$span(
              class = "chat-chip",
              q,
              onclick = sprintf(
                "document.getElementById('chatbot-raw-input').value='%s';chatbotSend();",
                gsub("'", "\\'", q)
              )
            )
          })
        ),
        div(id = "chatbot-input-row",
          tags$textarea(
            id          = "chatbot-raw-input",
            placeholder = "Pregunta sobre datos EMTP...",
            rows        = 2
          ),
          tags$button(
            id      = "chatbot-raw-send",
            HTML('<i class="fa fa-paper-plane"></i>'),
            onclick = "chatbotSend()"
          )
        ),
        div(id = "chatbot-status-bar", uiOutput("chatbot_model_label", inline=TRUE))
      )
    ),

    # === JAVASCRIPT ===
    tags$script(HTML("
      function chatbotToggle() {
        var p = document.getElementById('chatbot-panel');
        if (p.classList.contains('open')) {
          p.classList.remove('open');
        } else {
          p.classList.add('open');
          chatbotScrollBottom();
          setTimeout(function(){
            var t = document.getElementById('chatbot-raw-input');
            if (t) t.focus();
          }, 150);
        }
      }

      function chatbotSend() {
        var ta   = document.getElementById('chatbot-raw-input');
        var text = ta ? ta.value.trim() : '';
        if (!text) return;
        // Enviar texto+nonce a Shiny — nonce fuerza un nuevo evento aunque el texto sea igual
        Shiny.setInputValue('chatbot_query', {t: text, n: Date.now()}, {priority: 'event'});
        ta.value = '';
        ta.focus();
      }

      // Enter = enviar, Shift+Enter = nueva línea
      document.addEventListener('keydown', function(e) {
        if (e.target && e.target.id === 'chatbot-raw-input') {
          if (e.key === 'Enter' && !e.shiftKey) {
            e.preventDefault();
            chatbotSend();
          }
        }
      });

      function chatbotScrollBottom() {
        var h = document.getElementById('chatbot-history');
        if (h) h.scrollTop = h.scrollHeight;
      }

      Shiny.addCustomMessageHandler('chat_scroll', function(m) {
        setTimeout(chatbotScrollBottom, 80);
      });

      Shiny.addCustomMessageHandler('chat_online_status', function(m) {
        var dot = document.getElementById('chatbot-online-dot');
        if (dot) dot.style.display = m.online ? 'block' : 'none';
      });
    "))
  )
}

# =============================================================================
# 6. SERVER
# =============================================================================
chatbot_server <- function(input, output, session,
                           matricula=NULL, docentes=NULL,
                           egresados=NULL, continuidad=NULL,
                           titulados=NULL, base_apoyo=NULL) {

  kb_dinamica <- build_data_stats_kb(matricula, docentes, egresados, continuidad,
                                     titulados = titulados, base_apoyo = base_apoyo)
  kb_completa <- combine_knowledge_bases(kb_dinamica)
  tfidf_index <- build_tfidf_index(kb_completa)
  cat("[Chatbot] KB:", length(kb_completa), "docs | TF-IDF:", length(tfidf_index$terms), "términos\n")

  chat_messages <- reactiveVal(list())
  ollama_ok     <- reactiveVal(FALSE)

  observe({
    st <- check_llm_available("llama3.2")
    ollama_ok(st$available && st$model_found)
    session$sendCustomMessage("chat_online_status", list(online=st$available && st$model_found))
  })

  output$chatbot_status_dot <- renderUI({
    col <- if (ollama_ok()) "#2ecc71" else "#e0a800"
    tags$span(id="chatbot-online-dot",
              style=paste0("width:8px;height:8px;background:",col,
                           ";border-radius:50%;display:inline-block;"))
  })

  output$chatbot_model_label <- renderUI({
    if (ollama_ok()) {
      lbl <- if (.use_groq()) "groq / llama-3.3-70b" else "llama3.2 local"
      tags$span(style="color:#2ecc71;", HTML(paste0('<i class="fa fa-circle"></i> ', lbl, ' listo')))
    } else
      tags$span(style="color:#e0a800;", HTML('<i class="fa fa-circle"></i> modo datos &mdash; respuestas directas'))
  })

  output$chat_history_ui <- renderUI({
    msgs <- chat_messages()
    if (length(msgs) == 0) {
      return(div(class="chat-empty",
        HTML('<i class="fa fa-comments" style="font-size:32px;display:block;margin-bottom:10px;"></i>'),
        "Pregunta sobre el sistema EMTP:",
        tags$br(), tags$small("matrícula, docentes, egresados, RFT, rotación...")))
    }
    tagList(lapply(msgs, function(m) {
      if (m$role == "user") {
        div(class="chat-msg-user", div(class="chat-bubble-user", m$text))
      } else if (m$role == "thinking") {
        div(class="chat-msg-bot",
          div(class="chat-bubble-thinking",
            HTML('<i class="fa fa-spinner fa-spin"></i> Buscando en datos...')))
      } else if (m$role == "clarification") {
        # Mensaje de aclaración: pregunta + chips de opciones clicables
        tagList(
          div(class="chat-msg-bot",
            div(class="chat-bubble-bot",
              HTML('<i class="fa fa-question-circle"></i> <strong>¿A qué te refieres?</strong>'),
              tags$br(),
              HTML(m$text)
            )
          ),
          div(class="chat-clarif-chips", style="margin:4px 0 8px 0; display:flex; flex-wrap:wrap; gap:5px;",
            lapply(m$options, function(opt) {
              tags$span(
                class="chat-chip",
                style="background:#EBF3FB; border-color:#34536A; font-size:12px;",
                opt$label,
                onclick=sprintf(
                  "document.getElementById('chatbot-raw-input').value=%s;chatbotSend();",
                  paste0("'", gsub("'","\\\\'", opt$q), "'")
                )
              )
            })
          )
        )
      } else {
        answer_html <- gsub("\n", "<br>", htmltools::htmlEscape(m$text %||% ""))
        tagList(
          div(class="chat-msg-bot", div(class="chat-bubble-bot", HTML(answer_html))),
          if (!is.null(m$sources) && length(m$sources) > 0)
            div(class="chat-sources",
              HTML(paste0('<i class="fa fa-book"></i> <strong>Fuentes:</strong> ',
                          paste(m$sources, collapse=" · "))))
        )
      }
    }))
  })

  # === OBSERVAR chatbot_query (viene desde JS via Shiny.setInputValue) ===
  observeEvent(input$chatbot_query, {
    req(input$chatbot_query)
    query <- trimws(input$chatbot_query$t %||% "")
    cat("[Chatbot] Pregunta recibida:", query, "\n")
    if (nchar(query) == 0) return()

    # Detectar saludos / mensajes off-topic → respuesta instantánea sin Ollama
    q_low <- tolower(query)
    saludo_resp <- NULL
    if (grepl("^(hola|buenas|hey|hi|hello|buenos días|buenas tardes|buenas noches|saludos|qué tal|como estás|cómo estás|buen día)[!\\?\\. ]*$", q_low, perl=TRUE)) {
      saludo_resp <- paste0("¡Hola! Soy el asistente de datos EMTP. Puedo responder ",
        "preguntas sobre matrícula, docentes, establecimientos, titulados, asistencia, ",
        "SIMCE/IDPS, sectores económicos y normativa de la Educación Media Técnico Profesional.\n\n",
        "Ejemplos: *¿Cuántos estudiantes hay en Valparaíso?*, ",
        "*matrícula por sector económico*, *asistencia promedio en el Biobío*, ",
        "*titulados por especialidad*, *dame la ficha del RBD 1234*, *¿qué dice el Decreto 452?*")
    } else if (grepl("^(gracias|muchas gracias|ok|oka?y|perfecto|listo|genial|excelente)[!\\., ]*$", q_low, perl=TRUE)) {
      saludo_resp <- "¡Con gusto! Si tienes otra pregunta sobre EMTP, estoy aquí."
    } else if (grepl("^(adiós|adios|hasta luego|chao|bye|hasta pronto)[!\\., ]*$", q_low, perl=TRUE)) {
      saludo_resp <- "¡Hasta luego! Recuerda que puedes volver cuando necesites datos EMTP."
    } else if (nchar(query) < 8 && !grepl("[0-9]", query) &&
               !grepl("hay|tiene|cual|cuant|que|quien|como|donde", q_low)) {
      saludo_resp <- paste0("No entendí bien tu pregunta. Puedes consultarme sobre ",
        "matrícula, docentes, establecimientos o normativa EMTP. ",
        "Por ejemplo: *¿Cuántos docentes hay en la RM?*")
    }

    if (!is.null(saludo_resp)) {
      msgs_s <- isolate(chat_messages())
      msgs_s <- c(msgs_s,
        list(list(role="user",      text=query,       ts=Sys.time())),
        list(list(role="assistant", text=saludo_resp,
                  sources=character(0), scores=numeric(0), ok=TRUE, ts=Sys.time()))
      )
      chat_messages(msgs_s)
      session$sendCustomMessage("chat_scroll", list())
      return()
    }

    # Agregar mensaje usuario + pensando
    msgs <- isolate(chat_messages())
    msgs <- c(msgs,
      list(list(role="user",     text=query, ts=Sys.time())),
      list(list(role="thinking", text="",    ts=Sys.time()))
    )
    chat_messages(msgs)
    session$sendCustomMessage("chat_scroll", list())

    # Capturar locales para el closure. La CONTEXTUALIZACIÓN (reescritura del
    # seguimiento con el historial) se hace DENTRO de onFlushed para que el
    # spinner se muestre antes de la llamada de red.
    prev_msgs <- isolate(chat_messages())
    lqraw <- query
    lprev <- prev_msgs
    lkb  <- kb_completa
    lid  <- tfidf_index
    lmat <- matricula
    ldoc <- docentes
    legr <- egresados
    lcont <- continuidad
    ltit <- titulados
    lba  <- base_apoyo

    # onFlushed: primero se renderiza el spinner, luego se llama al LLM
    session$onFlushed(function() {
      cx <- tryCatch(.contextualize_query(lqraw, lprev),
                     error=function(e) list(q=lqraw, pretty=NULL))
      lq      <- cx$q
      lpretty <- cx$pretty   # pregunta reformulada (solo si el LLM reescribió)
      cat("[Chatbot] onFlushed INICIO, query:", lq, "\n")
      result <- tryCatch({
        r <- rag_answer(lq, lid, lkb, model="llama3.2", top_k=3,
                        matricula=lmat, docentes=ldoc, egresados=legr, continuidad=lcont,
                        titulados=ltit, base_apoyo=lba)
        cat("[Chatbot] rag_answer OK, chars:", nchar(r$answer %||% "[clarif]"), "\n")
        r
      }, error=function(e) {
        cat("[Chatbot] rag_answer ERROR:", conditionMessage(e), "\n")
        list(answer=paste0("Error interno: ", conditionMessage(e)),
             sources=character(0), scores=numeric(0), ok=FALSE)
      })
      # Transparencia: si reformulamos un seguimiento, anteponer "Entendí:" para
      # que el usuario vea cómo se interpretó su mensaje.
      if (!is.null(lpretty) && !isTRUE(result$clarification) && !is.null(result$answer)) {
        result$answer <- paste0("↪ Entendí: ", lpretty, "\n\n", result$answer)
      }
      msgs2 <- isolate(chat_messages())
      cat("[Chatbot] actualizando msgs2, length:", length(msgs2), "\n")

      # Caso aclaración: mostrar opciones clicables en lugar de respuesta
      if (isTRUE(result$clarification)) {
        msgs2[[length(msgs2)]] <- list(
          role="clarification",
          text="Por favor, selecciona una opción o reformula tu pregunta:",
          options=result$options,
          ts=Sys.time()
        )
      } else {
        msgs2[[length(msgs2)]] <- list(
          role="assistant", text=result$answer %||% "",
          sources=result$sources, scores=result$scores,
          ok=result$ok, ts=Sys.time()
        )
      }
      chat_messages(msgs2)
      cat("[Chatbot] chat_messages actualizado OK\n")
      session$sendCustomMessage("chat_scroll", list())
    }, once=TRUE)
  })

  # CRÍTICO: panel empieza con display:none → Shiny suspende los outputs.
  # Debe estar DESPUÉS de definir todos los output$...
  outputOptions(output, "chat_history_ui",    suspendWhenHidden=FALSE)
  outputOptions(output, "chatbot_status_dot", suspendWhenHidden=FALSE)
  outputOptions(output, "chatbot_model_label",suspendWhenHidden=FALSE)
}
