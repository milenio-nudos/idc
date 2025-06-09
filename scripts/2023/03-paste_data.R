# 0. Preparation ---------------------------------------------------------------
`%!in%` = Negate(`%in%`) #Function to negate filter

options(scipen = 0)

source("scripts/2023/01-proc_estudiante_data.R")
source("scripts/2023/02-proc_docente_data.R")

pacman::p_load(
  #Data manipulation
  tidyverse, haven, labelled, sjlabelled, magrittr, responsePatterns, scales,
  googlesheets4, readxl,
  #Data exploration
  summarytools, Hmisc, psych,
  #Plot
  sjPlot, ggplot2, corrplot, ggrepel, scales,
  #Markdown
  knitr,kableExtra,
  #Multilevel models
  lme4, performance, reghelper)

#Load data
nudos_4b <- nudos_simce|>
  filter(s_course == 2)
nudos_2m <- nudos_simce|>
  filter(s_course == 1)

rm(nudos_simce)

nudos_prof <- simce_prof

rm(simce_prof)

# Import political Data
nudos_politico <- readxl::read_xlsx("data/raw_data/2023_nudos_politico.xlsx")

# Import mother data (Conectivity)
nudos_mother <- readxl::read_xlsx("data/raw_data/2023_nudos_mother_data.xlsx")

nudos_mother <- nudos_mother|>
  mutate(
    id_comuna = sub("^0+", "", id_comuna)
  )

# Import pobreza multidimensional index
i_pobreza <- readxl::read_xlsx("data/raw_data/2022_pobreza_multi_comunal.xlsx")

i_pobreza <- i_pobreza|>
  select(id_comuna = `Código`,
         ipm_comunal = indice)

# 1. Merge political data ------------------------------------------------------
names(nudos_politico)[4:37] <- paste0("b_item", 1:34)

nudos_politico <- nudos_politico|>
  select(-url, -comuna)|>
  rename(b_indice_tramites_t = "mean tramites",
         b_indice_info_t = "mean info",
         b_indice_ponderado_t = "mean pond",
         id_comuna = cod)

nudos_politico <- nudos_politico|>
  slice(-346)|>
  mutate(b_indice_sumado_t = rowMeans(across(c(b_item1:b_item34)), na.rm = TRUE))


nudos_data <- merge(nudos_mother, nudos_politico, by = "id_comuna")

# 2. Merge social data ---------------------------------------------------------

#4b data
nudos_4b <- nudos_4b|>
  group_by(m_id)|>
  mutate(
    c_4b_item1 = mean(s_item1, na.rm = TRUE),
    c_4b_item2 = mean(s_item2, na.rm = TRUE),
    c_4b_item3 = mean(s_item3, na.rm = TRUE),
    c_4b_item4 = mean(s_item4, na.rm = TRUE),
    c_4b_item5 = mean(s_item5, na.rm = TRUE),
    c_4b_item6 = mean(s_item6, na.rm = TRUE),
    c_4b_n_students = n_distinct(s_id)
  )|>
  ungroup()|>
  mutate(c_indice_4b = rowMeans(across(c(c_4b_item1:c_4b_item6)), na.rm = TRUE))|>
  mutate(c_indice_4b_e = scales::rescale(c_indice_4b),
         c_indice_4b_t = scales::rescale(c_indice_4b,
                                         to = c(0, 1), 
                                         from = c(1, 4)))|>
  select(
    id_comuna = m_id,
    c_indice_4b_e, c_indice_4b_t,
    c_4b_item1, c_4b_item2, c_4b_item3,
    c_4b_item4, c_4b_item5, c_4b_item6,
    c_4b_n_students)|>
  distinct(id_comuna, .keep_all = TRUE)

#2m data
nudos_2m <- nudos_2m|>
  group_by(m_id)|>
  mutate(
    #Create items by commune
    c_2m_item1 = mean(s_item1, na.rm = TRUE),
    c_2m_item2 = mean(s_item2, na.rm = TRUE),
    c_2m_item3 = mean(s_item3, na.rm = TRUE),
    c_2m_item4 = mean(s_item4, na.rm = TRUE),
    c_2m_item5 = mean(s_item5, na.rm = TRUE),
    c_2m_item6 = mean(s_item6, na.rm = TRUE),
    c_2m_n_students = n_distinct(s_id)
  )|>
  ungroup()|>
  mutate(c_indice_2m = rowMeans(across(c(c_2m_item1:c_2m_item6)), na.rm = TRUE))|>
  mutate(c_indice_2m_e = scales::rescale(c_indice_2m),
         c_indice_2m_t = scales::rescale(c_indice_2m,
                                         to = c(0, 1), 
                                         from = c(1, 4)))|>
  select(
    id_comuna = m_id,
    c_indice_2m_e, c_indice_2m_t,
    c_2m_item1, c_2m_item2, c_2m_item3,
    c_2m_item4, c_2m_item5, c_2m_item6,
    c_2m_n_students)|>
  distinct(id_comuna, .keep_all = TRUE)

#Professors data
nudos_prof <- nudos_prof|>
  group_by(m_id)|>
  mutate(
    c_prof_item1 = mean(p_item1, na.rm = TRUE),
    c_prof_item2 = mean(p_item2, na.rm = TRUE),
    c_prof_item3 = mean(p_item3, na.rm = TRUE),
    c_prof_item4 = mean(p_item4, na.rm = TRUE),
    c_prof_item5 = mean(p_item5, na.rm = TRUE),
    c_prof_item6 = mean(p_item6, na.rm = TRUE),
    c_prof_item7 = mean(p_item7, na.rm = TRUE),
    c_prof_n_prof = n_distinct(p_id)
  )|>
  ungroup()|>
  mutate(c_indice_prof = rowMeans(across(c(c_prof_item1:c_prof_item6)), na.rm = TRUE))|>
  mutate(c_indice_prof_e = scales::rescale(c_indice_prof),
         c_indice_prof_t = scales::rescale(c_indice_prof,
                                           to = c(0, 1), 
                                           from = c(1, 4)))|>
  select(
    id_comuna = m_id,
    c_indice_prof_e, c_indice_prof_t,
    c_prof_item1, c_prof_item2, c_prof_item3,
    c_prof_item4, c_prof_item5, c_prof_item6, c_prof_item7,
    c_prof_n_prof)

#Merge the three data
nudos_data <- merge(nudos_data, nudos_4b, by = "id_comuna", all.x = TRUE)
nudos_data <- merge(nudos_data, nudos_2m, by = "id_comuna", all.x = TRUE)
nudos_data <- merge(nudos_data, nudos_prof, by = "id_comuna", all.x = TRUE)

# 3. Order the data ------------------------------------------------------------

#Merge pobreza multidimensional
nudos_data <- merge(nudos_data, i_pobreza, by = "id_comuna", all.x = TRUE)

#Index aggregated simce
nudos_data <- nudos_data|>
  mutate(c_indice_total = rowMeans(across(c(c_indice_4b_t,c_indice_2m_t,c_indice_prof_t))))

#Sort Column Order
nudos_data <- nudos_data|>
  select(
    #Info comunal
    id_comuna, region, nombre_comuna, habitantes,
    #Indices externos
    idh_comunal, ipm_comunal,
    #Indices propios
    a_indice_t,
    b_indice_ponderado_t, b_indice_sumado_t, b_indice_tramites_t, b_indice_info_t, 
    c_indice_total, c_indice_4b_t, c_indice_2m_t, c_indice_prof_t,
    #Items A
    a_total, starts_with("a_item"),
    #Items B
    starts_with("b_item"),
    #Items C
    starts_with("c_4b_item"), c_4b_n_students, 
    starts_with("c_2m_item"), c_2m_n_students, 
    starts_with("c_prof_item"), c_prof_n_prof, 
  )|>
  rename_with(~ gsub("_t$", "", .), ends_with("_t")) #No "t" in column names

nudos_data[nudos_data == "NaN"] <- NA

mother_2024 <- nudos_data

nudos_data <- nudos_data|>
  distinct(id_comuna, .keep_all = TRUE)
# 4. Label data ----------------------------------------------------------------

var_label(nudos_data) <- list(
  id_comuna = "Código Gubernamental de la Comuna",
  region = "Región",
  nombre_comuna = "Nombre de la comuna",
  habitantes = "Cantidad de población comunal",
  idh_comunal = "Índice de desarrolo humano comunal",
  ipm_comunal = "Índice de Pobreza Multidimensional",
  a_indice = "Índice NUDOS 1: Proporción de la población conectada a internet en la comuna",
  b_indice_ponderado = "Índice NUDOS 2: Nivel de Digitalización del municipio Ponderado (Tramites 70% + Información 30%)",
  b_indice_sumado = "Índice NUDOS 2: Nivel de Digitalización del municio no ponderado",
  b_indice_tramites = "Índice NUDOS 2: Nivel de Digitalización del municipio (Sólo Trámites)",
  b_indice_info = "Índice NUDOS 2: Nivel de Digitalización del municipio (Sólo Información de Trasparencia)",
  c_indice_total = "Índice NUDOS 3: Evaluación de implementación de tecnologías para el aprendizaje en la escuela a nivel comunal (Profesores y Estudiantes)",
  c_indice_4b = "Índice NUDOS 3: Evaluación de implementación de tecnologías para el aprendizaje en la escuela a nivel comunal (Sólo Cuarto Básico)",
  c_indice_2m = "Índice NUDOS 3: Evaluación de implementación de tecnologías para el aprendizaje en la escuela a nivel comunal (Sólo Segundo Medio)",
  c_indice_prof = "Índice NUDOS 3: Evaluación de implementación de tecnologías para el aprendizaje en la escuela a nivel comunal (Sólo Profesores)",
  a_total = "Total de conexiones fijas residenciales",
  a_item1 = "Cantidad de conexiones fijas residenciales con tecnología ADSL",
  a_item2 = "Cantidad de conexiones fijas residenciales con tecnología FTTX",
  a_item3 = "Cantidad de conexiones fijas residenciales con tecnología MODEM",
  a_item4 = "Cantidad de conexiones fijas residenciales con tecnología OTA",
  a_item5 = "Cantidad de conexiones fijas residenciales con tecnología OTI",
  a_item6 = "Cantidad de conexiones fijas residenciales con tecnología W",
  b_item1 = "pago_circulacion",
  b_item2 = "pago_multas",
  b_item3 = "pago_patentes",
  b_item4 = "pago_aseo",
  b_item5 = "pago_varios",
  b_item6 = "claveunica",
  b_item7 = "dom_dig",
  b_item8 = "tvecino",
  b_item9 = "omil_dig",
  b_item10 = "trans_dig",
  b_item11 = "patentes_dig",
  b_item12 = "oirs_dig",
  b_item13 = "ornato_dig",
  b_item14 = "app",
  b_item15 = "plan_regulador",
  b_item16 = "concursos_publicos",
  b_item17 = "actas_concejo",
  b_item18 = "sesiones_concejo",
  b_item19 = "ordenan",
  b_item20 = "cod_etica",
  b_item21 = "pladeco",
  b_item22 = "sol_audiencia",
  b_item23 = "trans_activa",
  b_item24 = "acceso_info",
  b_item25 = "datos_ab",
  b_item26 = "org_comunitarias",
  b_item27 = "gestion_publica",
  b_item28 = "lobby",
  b_item29 = "compras",
  b_item30 = "redes",
  b_item31 = "sub_becas",
  b_item32 = "denuncias",
  b_item33 = "org_municipal",
  b_item34 = "noticias",
  c_4b_item1 = "En el colegio nos motivan a utilizar herramientas tecnológicas [4to básico]",
  c_4b_item2 = "Ocupamos la tecnología frecuentemente en el colegio para aprender [4to básico]",
  c_4b_item3 = "Para mis compañeros(as) es importante saber usar los computadores para aprender [4to básico]",
  c_4b_item4 = "Los profesores y profesoras nos enseñan a utilizar herramientas tecnológicas [4to básico]",
  c_4b_item5 = "En el colegio se preocupan de tener los computadores en buen estado [4to básico]",
  c_4b_item6 = "El colegio cuenta con internet de buena calidad [4to básico]",
  c_4b_n_students = "Cantidad de estudiantes de segundo medio en la comuna",
  c_2m_item1 = "En el colegio nos motivan a utilizar herramientas tecnológicas [IIº medio]",
  c_2m_item2 = "Ocupamos la tecnología frecuentemente en el colegio para aprender [IIº medio]",
  c_2m_item3 = "Para mis compañeros(as) es importante saber usar los computadores para aprender [IIº medio]",
  c_2m_item4 = "Los profesores y profesoras nos enseñan a utilizar herramientas tecnológicas [IIº medio]",
  c_2m_item5 = "En el colegio se preocupan de tener los computadores en buen estado [IIº medio]",
  c_2m_item6 = "El colegio cuenta con internet de buena calidad [IIº medio]",
  c_2m_n_students = "Cantidad de estudiantes de segundo medio en la comuna",
  c_prof_item1 = "El establecimiento entrega capacitaciones a sus funcionarios(as) para mejorar habilidades computacionales [Profesores]",
  c_prof_item2 = "Las y los profesionales del establecimiento se apoyan entre sí en caso de tener problemas asociados a la tecnología [Profesores]",
  c_prof_item3 = "El establecimiento se preocupa de que haya alguien a cargo de las herramientas tecnológicas [Profesores]",
  c_prof_item4 = "El establecimiento se preocupa constantemente de mejorar la infraestructura tecnológica [Profesores]",
  c_prof_item5 = "El establecimiento cuenta con internet rápido [Profesores]",
  c_prof_item6 = "El establecimiento cuenta con sala de tecnología equipada con computadores [Profesores]",
  c_prof_item7 = "Los computadores pueden ser utilizados por cualquiera que requiera información [Profesores]",
  c_prof_n_prof = "Cantidad de profesores de Lenguaje y Matemáticas en la comuna"
)

# 5. Crear nuevos índices ------------------------------------------------------

nudos_data <- nudos_data |>
  # Renombrar acceso como subíndice, aprovechar de renombrar índices definitivos
  rename(a_acceso = a_indice,
         c_indice = c_indice_total) |>
  group_by(id_comuna) |>
  mutate(
    # Índice de calidad de conectividad
    a_conectividad = a_item2 / a_total,
    
    # Nuevo índice informativo
    a_indice_alt = rowMeans(cbind(a_acceso, a_conectividad), na.rm = TRUE),
    
    # Índice Trámites y Transparencia usando across
    b_tramites = rowMeans(across(b_item1:b_item14), na.rm = TRUE),
    b_transparencia = rowMeans(across(b_item15:b_item34), na.rm = TRUE),
    
    # Nuevo índice político
    b_indice = rowMeans(cbind(b_tramites, b_transparencia), na.rm = TRUE)
  ) |>
  ungroup()

#Revisar distribución de los tres índices
summary(nudos_data$a_indice_alt)
summary(nudos_data$b_indice)
summary(nudos_data$c_indice)

#Revisar top 10 de nuevo acceso
nudos_data|>
  arrange(desc(a_indice_alt))|>
  select(nombre_comuna,a_indice_alt)|>
  print(n = 30)

# Se descarta la idea de agregar conectividad

# 6. Estandarización de Subíndices ---------------------------------------------

nudos_data <- nudos_data|>
  mutate(
    #Valores Z
    a_indice_z = 0.5 + 0.1 * (a_acceso - mean(a_acceso, na.rm = TRUE)) / sd(a_acceso, na.rm = TRUE),
    b_indice_z = 0.5 + 0.1 * (b_indice - mean(b_indice, na.rm = TRUE)) / sd(b_indice, na.rm = TRUE),
    c_indice_z = 0.5 + 0.1 * (c_indice - mean(c_indice, na.rm = TRUE)) / sd(c_indice, na.rm = TRUE),
    #Valores empíricos
    a_indice_e = scales::rescale(a_acceso),
    b_indice_e = scales::rescale(b_indice),
    c_indice_e = scales::rescale(c_indice),
    #Indicar decil de cada dato
    a_indice_decil = ntile(a_acceso, 10),
    b_indice_decil = ntile(b_indice, 10),
    c_indice_decil = ntile(c_indice, 10)
  )

# 7. Crear nuevo índice final --------------------------------------------------

nudos_data <- nudos_data|>
  
  mutate(
    #Estimar dos tipos de medias
    nudos = rowMeans(cbind(a_acceso, b_indice, c_indice), na.rm = FALSE),
    nudos_geo = exp(rowMeans(log(cbind(a_acceso, b_indice,c_indice)), na.rm = FALSE)),
    #Estandarizar índices
    nudos_z = rowMeans(cbind(a_indice_z, b_indice_z, c_indice_z), na.rm = FALSE),
    nudos_e = rowMeans(cbind(a_indice_e, b_indice_e, c_indice_e), na.rm = FALSE),
    nudos_z_geo = exp(rowMeans(log(cbind(a_indice_z, b_indice_z,c_indice_z)), na.rm = FALSE)),
    nudos_e_geo = exp(rowMeans(log(cbind(a_indice_e, b_indice_e,c_indice_e)), na.rm = FALSE)),
    #Indicar decil de cada dato
    nudos_decil = ntile(nudos, 10),
    nudos_z_decil = ntile(nudos_z, 10),
    nudos_e_decil = ntile(nudos_e, 10),
    nudos_geo_decil = ntile(nudos_geo, 10),
    nudos_z_geo_decil = ntile(nudos_z_geo, 10),
    nudos_e_geo_decil = ntile(nudos_e_geo, 10)
  )

# 8. Etiquetar -----------------------------------------------------------------

var_label(nudos_data$a_acceso) = "NUDOS 1: Indice de acceso"
var_label(nudos_data$b_indice) = "NUDOS 2: Indice político"

# 9. Últimos procesamientos -----------------------------------------------------
base_final <- nudos_data|>
  select(
    id_comuna, nombre_comuna, habitantes,
    idh_comunal, ipm_comunal,
    indice_nudos = nudos_z,
    a_indice = a_indice_z,
    b_indice = b_indice_z,
    c_indice = c_indice_z,
    a_total, starts_with("a_item"),
    b_tramites, b_transparencia, starts_with("b_item"),
    starts_with("c_4b_item"), c_4b_n_students,
    starts_with("c_2m_item"), c_2m_n_students,
    starts_with("c_prof_item"), c_prof_n_prof
  )

base_final <- base_final|>
  mutate(indice_nudos = round(indice_nudos, 3),
         a_indice = round(a_indice, 3),
         b_indice = round(b_indice, 3),
         c_indice = round(c_indice, 3))|>
  mutate(quartil_a = ntile(a_indice, 4))|>
  mutate(quartil_a = set_value_labels(quartil_a,
                                      "Alto" = 4,
                                      "Medio Alto" = 3,
                                      "Medio bajo" = 2,
                                      "Bajo" = 1))|>
  mutate(quartil_b = ntile(b_indice, 4))|>
  mutate(quartil_b = set_value_labels(quartil_b,
                                      "Alto" = 4,
                                      "Medio Alto" = 3,
                                      "Medio bajo" = 2,
                                      "Bajo" = 1))|>
  mutate(quartil_c = ntile(c_indice, 4))|>
  mutate(quartil_c = set_value_labels(quartil_c,
                                      "Alto" = 4,
                                      "Medio Alto" = 3,
                                      "Medio bajo" = 2,
                                      "Bajo" = 1))

# 5. Save data -----------------------------------------------------------------
saveRDS(mother_2024, file ="data/proc_data/private_data/2024_mother_data.rds")
writexl::write_xlsx(base_final, "data/proc_data/public_data/2024_idc_v1.xlsx")