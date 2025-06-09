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

# Import SIMCE teacher data
prof_4b <- readxl::read_excel("data/raw_data/2023_4b_docente.xlsx")
prof_2m_mate <- readxl::read_excel("data/raw_data/2023_2m_docente_mate.xlsx")
prof_2m_lect <- readxl::read_excel("data/raw_data/2023_2m_docente_lect.xlsx")

# Import RBD SIMCE
simce_rbd <- read.csv("data/raw_data/2023_rbd.csv", sep = ";")

# 1. Paste teachers data ---------------------------------------------------

# Check if 2m data is identical
identical(names(prof_2m_mate), names(prof_2m_lect))

# No identical. Check why:
setdiff(names(prof_2m_mate), names(prof_2m_lect))
setdiff(names(prof_2m_lect), names(prof_2m_mate))

# There's a column named "noAplica" and other named "noaplica", one have to be recoded:
prof_2m_lect <- rename(prof_2m_lect,
                       noAplica = noaplica)

# Now are identical:
identical(names(prof_2m_mate), names(prof_2m_lect))

# There's no ID of teacher, so 2m data is pasted without it
prof_2m <- rbind(prof_2m_mate,prof_2m_lect)

# Check differences 2m with 4b (Have different rows)
setdiff(names(prof_2m), names(prof_4b))

# The not contained variables are subjects which are not in basic education. Dropped them

# Paste teacher data
simce_prof <- rbind(
  prof_4b,
  prof_2m |> 
    select(-c(cprof_p02_11, cprof_p02_12))
)|>
  clean_names()

# 2. Paste RBD data ------------------------------------------------------------

# Paste school info
simce_rbd <- simce_rbd |> 
  clean_names()|>
  dplyr::select(rbd, nom_rbd, 
                cod_reg_rbd, nom_reg_rbd_a, cod_pro_rbd,cod_com_rbd, nom_com_rbd,
                cod_depe_rbd=cod_depe2, rural_rbd, 
                latitud_rbd = latitud, longitud_rbd = longitud)

simce_prof <- merge(simce_prof, simce_rbd, by="rbd", all.x=TRUE)

# 3. Index estimation ----------------------------------------------------------

simce_prof <- simce_prof|>
  # Replace NA's in a vectorized manner
  mutate(across(c(cprof_p19_01:cprof_p19_07), ~ replace(., . == 99 | . == 0, NA)))|>
  # Calculate student level inndex in a vectorized manner
  mutate(
    indice_nudos_mean = rowMeans(across(c(cprof_p19_01:cprof_p19_07)), na.rm = TRUE)
  )|>
  # Calculate school level mean
  group_by(rbd)|>
  mutate(
    school_indice_nudos_mean = mean(indice_nudos_mean, na.rm = TRUE)
  )|>
  ungroup()|>
  # Calculate commune level mean
  group_by(cod_com_rbd)|>
  mutate(
    commune_indice_nudos_mean = mean(school_indice_nudos_mean, na.rm = TRUE)
  )|>
  ungroup()

# 4. Count nschools and teachers -----------------------------------------------

simce_prof <- simce_prof|>
  mutate(p_id = 1:24095)|>
  mutate_all(~ifelse(is.nan(.), NA, .))

# 6. Select and rename columns -------------------------------------------------

simce_prof <- simce_prof|>
  select(
    #Teacher level
    p_id,
    #index items
    p_item1 = cprof_p19_01,
    p_item2 = cprof_p19_02,
    p_item3 = cprof_p19_03,
    p_item4 = cprof_p19_04,
    p_item5 = cprof_p19_05,
    p_item6 = cprof_p19_06,
    p_item7 = cprof_p19_07,
    #Digital Opportunities and inequality index
    p_nudos_opp = indice_nudos_mean,
    
    #School level
    #id
    c_id = rbd,
    #name of school
    c_name = nom_rbd,
    #School dependence (Private/Public)
    c_dependence = cod_depe_rbd,
    #Rural condition
    c_rural = rural_rbd,
    #Digital Opportunities index
    c_nudos_opp = school_indice_nudos_mean,
    
    #Municipality level
    #id
    m_id = cod_com_rbd,
    #Name of commune
    m_name = nom_com_rbd,
    #Digital Opportunities index
    m_nudos_opp = commune_indice_nudos_mean,
    
    #Regional level
    r_id = cod_reg_rbd,
    r_name = nom_reg_rbd_a
  )

# 5. Label data ----------------------------------------------------------------

#Label columns
var_label(simce_prof) <- list(
  p_id = "ID profesor",
  p_item1 = "El establecimiento entrega capacitaciones a sus funcionarios(as) para mejorar habilidades computacionales.",
  p_item2 = "Las y los profesionales del establecimiento se apoyan entre sí en caso de tener problemas asociados a la tecnología.",
  p_item3 = "El establecimiento se preocupa de que haya alguien a cargo de las herramientas tecnológicas.",
  p_item4 = "El establecimiento se preocupa constantemente de mejorar la infraestructura tecnológica.",
  p_item5 = "El establecimiento cuenta con internet rápido.",
  p_item6 = "El establecimiento cuenta con sala de tecnología equipada con computadores.",
  p_item7 = "Los computadores pueden ser utilizados por cualquiera que requiera información.",
  p_nudos_opp = "Profesor: Índice de Oportunidades Digitales",
  c_id = "ID Escuela (RBD)",
  c_name = "Nombre de la escuela",
  c_dependence = "Dependencia administrativa de la escuela",
  c_rural = "Condición rural de la escuela",
  c_nudos_opp = "Escuela: Índice de Oportunidades Digitales",
  m_id = "ID Comuna",
  m_name = "Nombre de la comuna",
  m_nudos_opp = "Comuna: Índice de Oportunidades Digitales",
  r_id = "ID regional",
  r_name = "Nombre de la región"
)

# Label values

simce_prof <- simce_prof|>
  #Items
  mutate(across(c(p_item1:p_item7), ~ labelled::set_value_labels(., c(
    "No lo describe" = 1, 
    "Lo describe poco" = 2, 
    "Lo describe bastante" = 3,
    "Lo describe completamente" = 4)
  )
  ),
  #Dependencia Administrativa escuela
  c_dependence = labelled::set_value_labels(c_dependence, c(
    "Municipal" = 1,
    "Particular subvencionado" = 2,
    "Particular pagado" = 3,
    "Corp. de Administración Delegada (DL 3166)" = 4,
    "Servicio Local de Educación" = 5)),
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

# Recodificar la columna r_name en la base de datos simce_prof
simce_prof$r_name <- recode(simce_prof$r_name, !!!region_map)

# Definir el orden de los factores basado en los números
region_levels <- c("01. Arica y Parinacota", "02. Tarapacá", "03. Antofagasta", "04. Atacama", 
                   "05. Coquimbo", "06. Valparaíso", "07. Metropolitana", "08. O'Higgins", 
                   "09. Maule", "10. Ñuble", "11. Biobío", "12. La Araucanía", 
                   "13. Los Ríos", "14. Los Lagos", "15. Aysén", "16. Magallanes y Antártica")

# Convertir la columna r_name en factor con los niveles ordenados
simce_prof$r_name <- factor(simce_prof$r_name, levels = region_levels)

