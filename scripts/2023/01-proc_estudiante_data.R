# 0. Preparation ---------------------------------------------------------------
`%!in%` = Negate(`%in%`) #Function to negate filter

options(scipen = 0)

pacman::p_load(
  tidyverse,
  dplyr,
  readxl,
  janitor,
  matrixStats,
  lme4,
  labelled,
  sjPlot)

load("data/raw_data/2023_4b_estudiante.RData")
simce_4b <- data 

load("data/raw_data/2023_2m_estudiante.RData")
simce_2m <- data

rm(data)

simce_rbd <- read.csv("data/raw_data/2023_rbd.csv", sep = ";")

# 1. Paste rbd data ------------------------------------------------------------

# Paste school info
simce_rbd <- simce_rbd |> 
  clean_names()|>
  dplyr::select(rbd, nom_rbd, 
                cod_reg_rbd, nom_reg_rbd_a, cod_pro_rbd,cod_com_rbd, nom_com_rbd,
                cod_depe_rbd=cod_depe2, rural_rbd, 
                latitud_rbd = latitud, longitud_rbd = longitud)

simce_4b_rbd <- merge(simce_4b, simce_rbd, by="rbd", all.x=TRUE)

simce_2m_rbd <- merge(simce_2m, simce_rbd, by="rbd", all.x=TRUE)

# 2. Index estimation ----------------------------------------------------------

# Replace NA's in a vectorized manner
simce_2m_rbd <- simce_2m_rbd|>
  mutate(across(c(cest_p27_01:cest_p27_06), ~ replace(., . == 99 | . == 0, NA)))

simce_4b_rbd <- simce_4b_rbd|>
  mutate(across(c(cest_p26_01:cest_p26_06), ~ replace(., . == 99 | . == 0, NA)))

# Calculate mean and variance in a vectorized manner
simce_2m_rbd <- simce_2m_rbd|>
  mutate(
    indice_nudos_mean = rowMeans(across(c(cest_p27_01:cest_p27_06)), na.rm = TRUE))

simce_4b_rbd <- simce_4b_rbd|>
  mutate(
    indice_nudos_mean = rowMeans(across(c(cest_p26_01:cest_p26_06)), na.rm = TRUE))

# 3. School type of the student ------------------------------------------------

simce_2m_rbd <- simce_2m_rbd|>
  #Check if there's primary school
  mutate(
    nivel_enseñanza = dplyr::case_when(
      rbd %in% simce_4b_rbd$rbd ~ 1, #Primary and secondary
      rbd %!in% simce_4b_rbd$rbd ~ 2 #Only secondary
    )
  )

simce_4b_rbd <- simce_4b_rbd|>
  #Check if there's secondary school
  mutate(
    nivel_enseñanza = dplyr::case_when(
      rbd %in% simce_2m_rbd$rbd ~ 1, #Primary and secondary
      rbd %!in% simce_2m_rbd$rbd ~ 3 #Only primary
    )
  )

# 4. School level Index estimation ---------------------------------------------

simce_2m_rbd <- simce_2m_rbd|>
  group_by(rbd)|>
  mutate(
    #School Mean estimation
    school_indice_nudos_mean = mean(indice_nudos_mean, na.rm = TRUE))|>
  ungroup()

simce_4b_rbd <- simce_4b_rbd|>
  group_by(rbd)|>
  mutate(
    #School Mean estimation
    school_indice_nudos_mean = mean(indice_nudos_mean, na.rm = TRUE))|>
  ungroup()

# 5. N of students by school ---------------------------------------------------

simce_2m_rbd <- simce_2m_rbd|>
  group_by(rbd)|>
  #Get n of students by school
  mutate(n_students_by_school = length(unique(idAlumno)))|>
  ungroup()

simce_4b_rbd <- simce_4b_rbd|>
  group_by(rbd)|>
  #Get n of students by school
  mutate(n_students_by_school = length(unique(idAlumno)))|>
  ungroup()

# 6. N of schools by commune ---------------------------------------------------

simce_2m_rbd <- simce_2m_rbd|>
  group_by(nom_com_rbd)|>
  #Get n of schools by comunne
  mutate(n_school_by_commune = length(unique(rbd)))|>
  ungroup()

simce_4b_rbd <- simce_4b_rbd|>
  group_by(nom_com_rbd)|>
  #Get n of schools by comunne
  mutate(n_school_by_commune = length(unique(rbd)))|>
  ungroup()

# 7. Comunne Index score (Considering type of school) --------------------------

simce_2m_rbd <- simce_2m_rbd|>
  group_by(cod_com_rbd)|>
  # Create mean indexes by type of school
  mutate(
    commune_total_indice_nudos_mean = mean(indice_nudos_mean, na.rm = TRUE))|>
  ungroup()

simce_4b_rbd <- simce_4b_rbd|>
  group_by(cod_com_rbd)|>
  # Create mean indexes by type of school
  mutate(
    commune_total_indice_nudos_mean = mean(indice_nudos_mean, na.rm = TRUE))|>
  ungroup()

# 8. Add course level ----------------------------------------------------------

simce_2m_rbd <- simce_2m_rbd|>
  mutate(student_course = 1)|>
  mutate_all(~ifelse(is.nan(.), NA, .))

simce_4b_rbd <- simce_4b_rbd|>
  mutate(student_course = 2)|>
  mutate_all(~ifelse(is.nan(.), NA, .))

# 10. Select and rename data ---------------------------------------------------

nudos_2m <- simce_2m_rbd|>
  select(
    #Student level
    #id
    s_id = idAlumno,
    #course
    s_course = student_course,
    #index items
    s_item1 = cest_p27_01,
    s_item2 = cest_p27_02,
    s_item3 = cest_p27_03,
    s_item4 = cest_p27_04,
    s_item5= cest_p27_05,
    s_item6 = cest_p27_06,
    #Digital Opportunities and inequality index
    s_nudos_opp = indice_nudos_mean,
    
    #School level
    #id
    c_id = rbd,
    #name of school
    c_name = nom_rbd,
    #School dependence (Private/Public)
    c_dependence = cod_depe_rbd,
    #School level courses available
    c_levels = nivel_enseñanza,
    #Rural condition
    c_rural = rural_rbd,
    #GPS
    c_coord_x = latitud_rbd,
    c_coord_y = longitud_rbd,
    #Digital Opportunities and inequality index
    c_nudos_opp = school_indice_nudos_mean,
    c_nstudents = n_students_by_school,

    
    #Municipality level
    #id
    m_id = cod_com_rbd,
    #Name of commune
    m_name = nom_com_rbd,
    #n of schools by commune
    m_nschools = n_school_by_commune,
    #Digital Opportunities index
    m_nudos_opp_tot = commune_total_indice_nudos_mean,
    #It's Missing SIMCE, IDH, Desarrollo comunal
    r_id = cod_reg_rbd,
    r_name = nom_reg_rbd_a
  )

nudos_4b <- simce_4b_rbd|>
  select(
    #Student level
    #id
    s_id = idAlumno,
    #course
    s_course = student_course,
    #index items
    s_item1 = cest_p26_01,
    s_item2 = cest_p26_02,
    s_item3 = cest_p26_03,
    s_item4 = cest_p26_04,
    s_item5= cest_p26_05,
    s_item6 = cest_p26_06,
    #Digital Opportunities and inequality index
    s_nudos_opp = indice_nudos_mean,
    
    #School level
    #id
    c_id = rbd,
    #name of school
    c_name = nom_rbd,
    #School dependence (Private/Public)
    c_dependence = cod_depe_rbd,
    #School level courses available
    c_levels = nivel_enseñanza,
    #Rural condition
    c_rural = rural_rbd,
    #GPS
    c_coord_x = latitud_rbd,
    c_coord_y = longitud_rbd,
    #Digital Opportunities and inequality index
    c_nudos_opp = school_indice_nudos_mean,
    #Amount of students
    c_nstudents = n_students_by_school,
    
    #Municipality level
    #id
    m_id = cod_com_rbd,
    #Name of commune
    m_name = nom_com_rbd,
    #n of schools by commune
    m_nschools = n_school_by_commune,
    #Digital Opportunities index
    m_nudos_opp_tot = commune_total_indice_nudos_mean,
    #It's Missing SIMCE, IDH, Desarrollo comunal
    r_id = cod_reg_rbd,
    r_name = nom_reg_rbd_a
  )

# 11. Bind data and label columns ----------------------------------------------

#Bind both databases to obtain a mother data
nudos_simce <- rbind(nudos_4b,nudos_2m)

#Label columns
var_label(nudos_simce) <- list(
  s_id = "ID Estudiante",
  s_course = "Nivel de enseñanza del estudiante",
  s_item1 = "En el colegio nos motivan a utilizar herramientas tecnológicas",
  s_item2 = "Ocupamos la tecnología frecuentemente en el colegio para aprender",
  s_item3 = "Para mis compañeros(as) es importante saber usar los computadores para aprender",
  s_item4 = "Los profesores y profesoras nos enseñan a utilizar herramientas tecnológicas",
  s_item5 = "En el colegio se preocupan de tener los computadores en buen estado",
  s_item6 = "El colegio cuenta con internet de buena calidad",
  s_nudos_opp = "Estudiante: Índice de Oportunidades Digitales",
  c_id = "ID Escuela (RBD)",
  c_name = "Nombre de la escuela",
  c_dependence = "Dependencia administrativa de la escuela",
  c_levels = "Niveles de enseñaza que imparte la escuela",
  c_rural = "Condición rural de la escuela",
  c_coord_x = "Latitud de la escuela",
  c_coord_y = "Longitud de la escuela",
  c_nudos_opp = "Escuela: Índice de Oportunidades Digitales",
  c_nstudents = "Cantidad de estudiantes en la escuela",
  m_id = "ID Comuna",
  m_name = "Nombre de la comuna",
  m_nschools = "Cantidad de escuelas en la comuna",
  m_nudos_opp_tot = "Comuna: Índice de Oportunidades Digitales (Total de escuelas)",
  r_id = "ID regional",
  r_name = "Nombre de la región"
)

# 12. Label categorical values -------------------------------------------------

nudos_simce <- nudos_simce|>
  #Items
  mutate(across(c(s_item1:s_item6), ~ labelled::set_value_labels(., c(
    "No lo describe" = 1, 
    "Lo describe poco" = 2, 
    "Lo describe bastante" = 3,
    "Lo describe completamente" = 4)
  )
  )
  )|>
  mutate(
    #Nivel de enseñanza del estudiante
    s_course = labelled::set_value_labels(s_course, c(
      "Segundo Medio" = 1,
      "Cuarto básico" = 2)),
    #Dependencia Administrativa escuela
    c_dependence = labelled::set_value_labels(c_dependence, c(
      "Municipal" = 1,
      "Particular subvencionado" = 2,
      "Particular pagado" = 3,
      "Corp. de Administración Delegada (DL 3166)" = 4,
      "Servicio Local de Educación" = 5)),
    #Niveles de enseñanza escuelas
    c_levels = labelled::set_value_labels(c_levels, c(
      "Primario y secundario" = 1,
      "Solo secundario" = 2,
      "Solo primario" = 3)),
    #Escuela en zona rural
    c_rural = labelled::set_value_labels(c_rural, c(
      "Rural" = 1,
      "Urbana" = 0))
  )

# Definir el mapeo de las regiones originales a las recodificadas y numeradas
region_map <- c("TPCA" = "02. Tarapacá", "ANTOF" = "03. Antofagasta", "ATCMA" = "04. Atacama", 
                "COQ" = "05. Coquimbo", "VALPO" = "06. Valparaíso", "LGBO" = "11. Biobío", 
                "MAULE" = "09. Maule", "BBIO" = "11. Biobío", "ARAUC" = "12. La Araucanía", 
                "LAGOS" = "14. Los Lagos", "AYSEN" = "15. Aysén", "MAG" = "16. Magallanes y Antártica",  
                "RM" = "07. Metropolitana", "RIOS" = "13. Los Ríos", "AYP" = "01. Arica y Parinacota", 
                "NUBLE" = "10. Ñuble")

# Recodificar la columna r_name en la base de datos nudos_simce
nudos_simce$r_name <- recode(nudos_simce$r_name, !!!region_map)

# Definir el orden de los factores basado en los números
region_levels <- c("01. Arica y Parinacota", "02. Tarapacá", "03. Antofagasta", "04. Atacama", 
                   "05. Coquimbo", "06. Valparaíso", "07. Metropolitana", "08. O'Higgins", 
                   "09. Maule", "10. Ñuble", "11. Biobío", "12. La Araucanía", 
                   "13. Los Ríos", "14. Los Lagos", "15. Aysén", "16. Magallanes y Antártica")

# Convertir la columna r_name en factor con los niveles ordenados
nudos_simce$r_name <- factor(nudos_simce$r_name, levels = region_levels)
