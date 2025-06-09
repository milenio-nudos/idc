# 0. Preparation ---------------------------------------------------------------
`%!in%` = Negate(`%in%`) #Function to negate filter

options(scipen = 0,
        encoding = "UTF-8")

pacman::p_load(
  tidyverse,
  dplyr,
  readxl,
  janitor,
  matrixStats,
  lme4,
  labelled,
  sjPlot,
  corrplot,
  Hmisc,
  ggplot2,
  ggrepel,
  psych,
  scales,
  tidyr,
  sjlabelled,
  stringr,
  here,
  knitr
  )

# Import SIMCE teacher data
docente_lect_2m <- read.csv(here("data", "raw_data", "2024_2m_docente_lect.csv"), sep = ";", fileEncoding = "UTF-8")
docente_mate_2m <- read.csv(here("data", "raw_data", "2024_2m_docente_mate.csv"), sep = ";", fileEncoding = "UTF-8")
docente_6b      <- read.csv(here("data", "raw_data", "2024_6b_docente.csv"), sep = ";", fileEncoding = "UTF-8")
docente_4b      <- read.csv(here("data", "raw_data", "2024_4b_docente.csv"), sep = ";", fileEncoding = "UTF-8")

# Import RBD SIMCE
simce_rbd_2m <- read.csv(here("data", "raw_data", "2024_2m_rbd.csv"), sep = ";", fileEncoding = "UTF-8")
simce_rbd_6b <- read.csv(here("data", "raw_data", "2024_6b_rbd.csv"), sep = ";", fileEncoding = "UTF-8")
simce_rbd_4b <- read.csv(here("data", "raw_data", "2024_4b_rbd.csv"), sep = ";", fileEncoding = "UTF-8")


# Paste docente data -------------------------------------------------------------------

# Check if 2m data is identical
identical(names(docente_lect_2m), names(docente_mate_2m))

# They're identical, so we bind them
docente_2m <- rbind(docente_lect_2m, docente_mate_2m)

#Check differences 4b data
setdiff(names(docente_2m), names(docente_4b))

# Different columns are "Inglés" y "Orientación". Better strategy is selecting the columns we need
# Select wanted columns
docente_2m <- docente_2m |>
  select(rbd,grado,
         starts_with("cdoce_p15"))|>
  mutate(grado = "2m")
docente_6b <- docente_6b |>
  select(rbd,grado,
         starts_with("cdoce_p15"))|>
  mutate(grado = "6b")
docente_4b <- docente_4b |>
  select(rbd,grado,
         starts_with("cdoce_p15"))|>
  mutate(grado = "4b")

# Paste RBD
# Paste school info
simce_rbd_2m <- simce_rbd_2m |> 
  clean_names()|>
  dplyr::select(rbd, nom_rbd, 
                cod_reg_rbd, nom_reg_rbd, cod_pro_rbd, cod_com_rbd, nom_com_rbd,
                cod_depe_rbd=cod_depe2, cod_rural_rbd, prom_lect2m_rbd, 
                prom_mate2m_rbd,grado) |>
  mutate(prom_lect6b_rbd = NA,
         prom_mate6b_rbd = NA,
         prom_lect4b_rbd = NA,
         prom_mate4b_rbd = NA)

docente_2m <- merge(docente_2m, simce_rbd_2m, by="rbd", all.x=TRUE)

simce_rbd_6b <- simce_rbd_6b |> 
  clean_names()|>
  dplyr::select(rbd, nom_rbd, 
                cod_reg_rbd, nom_reg_rbd, cod_pro_rbd, cod_com_rbd, nom_com_rbd,
                cod_depe_rbd=cod_depe2, cod_rural_rbd, prom_lect6b_rbd, 
                prom_mate6b_rbd,grado) |>
  mutate(prom_lect2m_rbd = NA,
         prom_mate2m_rbd = NA,
         prom_lect4b_rbd = NA,
         prom_mate4b_rbd = NA)


docente_6b <- merge(docente_6b, simce_rbd_6b, by="rbd", all.x=TRUE)

simce_rbd_4b <- simce_rbd_4b |> 
  clean_names()|>
  dplyr::select(rbd, nom_rbd, 
                cod_reg_rbd, nom_reg_rbd, cod_pro_rbd, cod_com_rbd, nom_com_rbd,
                cod_depe_rbd=cod_depe2, cod_rural_rbd, prom_lect4b_rbd, 
                prom_mate4b_rbd,grado) |>
  mutate(prom_lect6b_rbd = NA,
         prom_mate6b_rbd = NA,
         prom_lect2m_rbd = NA,
         prom_mate2m_rbd = NA)


docente_4b <- merge(docente_4b, simce_rbd_4b, by="rbd", all.x=TRUE)

# Merge all levels
docente <- rbind(docente_2m, docente_6b, docente_4b)

# 3. IDC estimation ----------------------------------------------------------
docente <- docente|>
  # Replace NA's in a vectorized manner
  mutate(across(c(cdoce_p15_01:cdoce_p15_07), ~ replace(., . == 99 | . == 0, NA)))|>
  # Calculate docente index in a vectorized manner
  mutate(idc = rowMeans(across(c(cdoce_p15_01:cdoce_p15_07)), na.rm = TRUE))|>
  # Calculate commune level mean
  group_by(cod_com_rbd)|>
  mutate(idc_comunal = mean(idc, na.rm = TRUE))|>
  ungroup()

# 4. Coun N --------------------------------------------------------------------

#Create ID variable and nan to NA
docente <- docente|>
  mutate(p_id = 1:nrow(docente))|>
  mutate_all(~ifelse(is.nan(.), NA, .))

#Create N variables
docente <- docente|>
  group_by(cod_com_rbd)|>
  mutate(n_docentes = n(),
         n_escuelas = n_distinct(rbd))|>
  ungroup()

# 4. Select and rename columns -------------------------------------------------

docente <- docente|>
  select(
    #Teacher level
    p_id = p_id,
    p_grado = grado.x,
    #index items
    p_item1 = cdoce_p15_01,
    p_item2 = cdoce_p15_02,
    p_item3 = cdoce_p15_03,
    p_item4 = cdoce_p15_04,
    p_item5 = cdoce_p15_05,
    p_item6 = cdoce_p15_06,
    p_item7 = cdoce_p15_07,
    #Digital Opportunities and inequality index
    p_idc = idc,
    #School level
    #id
    c_id = rbd,
    #name of school
    c_name = nom_rbd,
    #School dependence (Private/Public)
    c_dependence = cod_depe_rbd,
    #Rural condition
    c_rural = cod_rural_rbd,
    
    #Municipality level
    #id
    m_id = cod_com_rbd,
    #Name of commune
    m_name = nom_com_rbd,
    #Digital Opportunities index
    m_idc = idc_comunal,
    #N de profes
    m_ndocentes = n_docentes,
    #N de escuelas
    m_nescuelas = n_escuelas,
    
    #Regional level
    r_id = cod_reg_rbd,
    r_name = nom_reg_rbd
  )

# 6. Label data ----------------------------------------------------------------

#Label columns
var_label(docente) <- list(
  p_id = "ID profesor",
  p_grado = "Grado del profesor",
  p_item1 = "El establecimiento entrega capacitaciones a sus funcionarios(as) para mejorar habilidades computacionales.",
  p_item2 = "Las y los profesionales del establecimiento se apoyan entre sí en caso de tener problemas asociados a la tecnología.",
  p_item3 = "El establecimiento se preocupa de que haya alguien a cargo de las herramientas tecnológicas.",
  p_item4 = "El establecimiento se preocupa constantemente de mejorar la infraestructura tecnológica.",
  p_item5 = "El establecimiento cuenta con internet rápido.",
  p_item6 = "El establecimiento cuenta con sala de tecnología equipada con computadores.",
  p_item7 = "Los computadores pueden ser utilizados por cualquiera que requiera información.",
  p_idc = "Profesor: Índice de Oportunidades Digitales",
  c_id = "ID Escuela (RBD)",
  c_name = "Nombre de la escuela",
  c_dependence = "Dependencia administrativa de la escuela",
  c_rural = "Condición rural de la escuela",
  m_id = "ID Comuna",
  m_name = "Nombre de la comuna",
  m_ndocentes = "Cantidad de profesores",
  m_nescuelas = "Cantidad de escuelas",
  m_idc = "Comuna: Índice de Oportunidades Digitales",
  r_id = "ID regional",
  r_name = "Nombre de la región"
)

# Label values

docente <- docente|>
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
region_map <- c("DE TARAPAC?" = "01. Tarapacá", "DE ANTOFAGASTA" = "02. Antofagasta", "DE ATACAMA" = "03. Atacama", 
                "DE COQUIMBO" = "04. Coquimbo", "DE VALPARA?SO" = "05. Valparaíso", "DEL LIBERTADOR BERNARDO O" = "06. O'higgins", 
                "DEL MAULE" = "07. Maule", "DEL BIOB?O" = "08. Biobío", "DE LA ARAUCAN?A" = "09. La Araucanía", 
                "DE LOS LAGOS" = "10. Los Lagos", "DE AYS?N DEL GENERAL CARL" = "11. Aysén", "DE MAGALLANES Y DE LA ANT" = "12. Magallanes y Antártica",  
                "METROPOLITANA DE SANTIAGO" = "13. Metropolitana", "DE LOS R?OS" = "14. Los Ríos", "DE ARICA Y PARINACOTA" = "15. Arica y Parinacota", 
                "DE ?UBLE" = "16. Ñuble")

# Recodificar la columna r_name en la base de datos simce_prof
docente$r_name <- recode(docente$r_name, !!!region_map)

# Definir el orden de los factores basado en los números
region_levels <- c("01. Tarapacá", "02. Antofagasta", "03. Atacama", "04. Coquimbo", 
                   "05. Valparaíso", "06. O'higgins", "07. Maule", "08. Biobío", 
                   "09. La Araucanía", "10. Los Lagos", "11. Aysén", "12. Magallanes y Antártica", 
                   "13. Metropolitana", "14. Los Ríos", "15. Arica y Parinacota", "16. Ñuble")

# Convertir la columna r_name en factor con los niveles ordenados
docente$r_name <- factor(docente$r_name, levels = region_levels)



# 7. Create idc data -----------------------------------------------------------

idc <- docente|>
  select(m_id, m_name, m_ndocentes, m_nescuelas, r_id, r_name, m_idc)|>
  arrange(desc(m_idc))|>
  distinct(m_id, .keep_all = TRUE)|>
  mutate(ranking = 1:345)

# 8. Guardar base --------------------------------------------------------------

writexl::write_xlsx(idc, here("data", "proc_data", "idc_v1.xlsx"))

