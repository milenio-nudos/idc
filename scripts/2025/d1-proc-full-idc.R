# Preparation ----

pacman::p_load(tidyverse, haven)

#Bases IDC procesadas
a_conectividad <- readRDS("data/proc_data/private_data/2025_a_conectividad.rds")
b_politico <- readRDS("data/proc_data/private_data/2025_b_municipal.rds")
c_educacion <- readRDS("data/proc_data/private_data/2025_c_educativo.rds")

#Otros índices comunales
idh_comunal <- read_excel("data/raw_data/communal_index/2023_idh_comunal.xlsx")%>%
  select(id_comuna = CODIGO, idh_2023 = IDH)
pobreza_multi_comunal <- read_excel("data/raw_data/communal_index/2022_pobreza_multi_comunal.xlsx")%>%
  clean_names()%>%
  select(id_comuna = codigo, pobreza_2022 = indice)

# Standarize column labels ----

# Conectividad
a_conectividad <- a_conectividad %>%
  rename(id_comuna = codigo_comuna_n) %>%
  select(
    id_comuna, comuna, codigo_region, region, codigo_region, idc_2024, tipo_comuna,
    # Info 2024
    poblacion_censo_2024,
    # Índice conectividad 2024
    conectividad_2024_estandarizada_3,
    # Ranking 2024
    posicion_dimension_2024,
    # Ítems versión 2024
    conexiones_fijas_residenciales_2023, adsl_tecnologia_2023, 
    fttx_tecnologia_2023, modem_tecnologia_2023, ota_tecnologia_2023,
    oti_tecnologia_2023, w_tecnologia_2023,
    # Info 2025
    poblacion_proyeccion_2023_base2017,
    # Índice conectividad 2025
    c25v_z_3,
    # Ítems versión 2025
    conexiones_fijas_residenciales_2024, adsl_tecnologia_2024, 
    fttx_tecnologia_2024, modem_tecnologia_2024, ota_tecnologia_2024, 
    oti_tecnologia_2024, w_tecnologia_2024,
    # Ranking 2025
    posicion_dimension_2025
  ) %>%
  rename(
    a_indice_2024 = conectividad_2024_estandarizada_3,
    a_ranking_2024 = posicion_dimension_2024,
    a_item1_2024 = conexiones_fijas_residenciales_2023,
    a_item2_2024 = adsl_tecnologia_2023,
    a_item3_2024 = fttx_tecnologia_2023,
    a_item4_2024 = modem_tecnologia_2023,
    a_item5_2024 = ota_tecnologia_2023,
    a_item6_2024 = oti_tecnologia_2023,
    a_item7_2024 = w_tecnologia_2023,
    # ---
    a_indice_2025 = c25v_z_3,
    a_ranking_2025 = posicion_dimension_2025,
    a_item1_2025 = conexiones_fijas_residenciales_2024,
    a_item2_2025 = adsl_tecnologia_2024,
    a_item3_2025 = fttx_tecnologia_2024,
    a_item4_2025 = modem_tecnologia_2024,
    a_item5_2025 = ota_tecnologia_2024,
    a_item6_2025 = oti_tecnologia_2024,
    a_item7_2025 = w_tecnologia_2024,
    # ---
    id_region = codigo_region
  )

# Político
b_politico <- b_politico %>%
  rename(id_comuna = cod)

  # Variables fijas (sin año)
  fijas <- c("cod", "region", "comuna")

  # Patrones que NO son ítems
  no_items_pat <- c(
  "^puntaje_md_",
  "^ranking_",
  "^poblacion_",
  "^mean_tramites_",
  "^mean_info_",
  "^mean_pond_"
  )

  # Detectar columnas ítems 2024 y 2025
  items_2024 <- grep("_2024$", names(b_politico), value = TRUE)
  items_2024 <- items_2024[!grepl(paste(no_items_pat, collapse = "|"), items_2024)]

  items_2025 <- grep("_2025$", names(b_politico), value = TRUE)
  items_2025 <- items_2025[!grepl(paste(no_items_pat, collapse = "|"), items_2025)]

  # Comprobar que están en el mismo orden
  stopifnot(
  sub("_2024$", "", items_2024) == sub("_2025$", "", items_2025)
  )

  # Renombrar las columnas de ítems
  b_politico <- b_politico %>%
  rename_with(
    .cols = all_of(items_2024),
    .fn = ~paste0("b_item", seq_along(items_2024), "_2024")
  ) %>%
  rename_with(
    .cols = all_of(items_2025),
    .fn = ~paste0("b_item", seq_along(items_2025), "_2025")
  )

  # Asignar labels y convertir a double
  for (i in seq_along(items_2024)) {
  var_2024 <- paste0("b_item", i, "_2024")
  var_2025 <- paste0("b_item", i, "_2025")
  
  label_2024 <- items_2024[i]
  label_2025 <- items_2025[i]
  
  b_politico[[var_2024]] <- labelled(
    as.double(b_politico[[var_2024]]),
    label = label_2024
  )
  b_politico[[var_2025]] <- labelled(
    as.double(b_politico[[var_2025]]),
    label = label_2025
    )
  }

  names(b_politico)
  
#Renombrar y Eliminar columnas no utilizadas
  b_politico <- b_politico%>%
    rename(
      b_indice_2024 = puntaje_md_2024,
      b_ranking_2024 = ranking_2024,
      b_indice_2025 = puntaje_md_2025,
      b_ranking_2025 = ranking_2025
    )%>%
    select(-starts_with("mean"),-starts_with("pobla"),
           -comuna, -region)

# Educativo
  
names(c_educacion)

#Eliminar columnas no utilizadas
  c_educacion <- c_educacion %>%
    select(-nombre_comuna, -region, -region_code, -c_indice_2025)%>%
    rename(c_indice_2025 = c_indice_2025_z)%>%
    mutate(id_comuna = as.integer(id_comuna))
    

# Merge data ----
  
# Unir las tres bases por id_comuna
idc_full <- a_conectividad %>%
  left_join(b_politico, by = "id_comuna") %>%
  left_join(c_educacion, by = "id_comuna") %>%
  left_join(idh_comunal, by = "id_comuna") %>%
  left_join(pobreza_multi_comunal, by = "id_comuna")
  
# Verificar que no se pierdan filas
stopifnot(nrow(idc_full) == 345)

idc_full <- idc_full %>%
  mutate(
    # Promedio de los tres índices
    idc_2025 = rowMeans(select(., a_indice_2025, b_indice_2025, c_indice_2025), na.rm = TRUE),
    
    # Ranking (1 = mejor puntaje, orden descendente), NA si faltan datos
    idc_ranking_2024 = if_else(!is.na(idc_2024),
                               min_rank(desc(idc_2024)),
                               NA_integer_),
    idc_ranking_2025 = if_else(!is.na(idc_2025),
                               min_rank(desc(idc_2025)),
                               NA_integer_)
  ) %>%
  mutate(
    idc_tramo_2024 = ntile(idc_2024, 4),
    idc_tramo_2025 = ntile(idc_2025, 4)
  ) %>%
  mutate(
    idc_tramo_2024 = factor(idc_tramo_2024,
                            levels = c(4, 3, 2, 1),
                            labels = c("Alto", "Medio alto", "Medio bajo", "Bajo")),
    idc_tramo_2025 = factor(idc_tramo_2025,
                            levels = c(4, 3, 2, 1),
                            labels = c("Alto", "Medio alto", "Medio bajo", "Bajo"))
  )%>%
  arrange(id_comuna)

# Order columns ----

idc_full <- idc_full%>%
  select(
    #Info común de la base
    id_comuna, comuna, tipo_comuna, id_region, region, poblacion_censo_2024, poblacion_proyeccion_2023_base2017, idh_2023, pobreza_2022,
    #Resultados IDC finales
    idc_2024, idc_2025, idc_ranking_2024, idc_ranking_2025, idc_tramo_2024, idc_tramo_2025,
    #Índices 2024
    a_indice_2024, b_indice_2024, c_indice_2024,
    #Rankings 2024
    a_ranking_2024, b_ranking_2024, c_ranking_2024,
    #Items 2024
    matches("^a_item.*_2024$"),matches("^b_item.*_2024$"),matches("^c_(4b|2m|prof)_item[0-9]+_2024$"),
    #Índices 2025
    a_indice_2025, b_indice_2025, c_indice_2025,
    #Rankings 2025
    a_ranking_2025, b_ranking_2025, c_ranking_2025,
    #Items 2025
    matches("^a_item.*_2025$"),matches("^b_item.*_2025$"),matches("^c_item.*_2025$")
  )

# Label columns ----

# Vector con los nombres y etiquetas vacías
labels_vector <- c(
  
  # Variables generales comunes
  id_comuna = "ID comunal (Código oficial nacional)",
  comuna = "Nombre de la comuna",
  tipo_comuna = "Tipo de comuna",
  id_region = "Número de la Región",
  region = "Nombre de la Región",
  poblacion_censo_2024 = "Población comunal censada en Censo 2024 INE",
  poblacion_proyeccion_2023_base2017 = "Población comunal proyectada para 2023 en Censo 2017 INE",
  idh_2023 = "Indice de Desarrollo Humano Comunal 2023",
  pobreza_2022 = "Proporción de Pobreza Multidimensional Comunal 2022",
  
  #Indices full
  idc_2024 = "Puntaje Indice de Digitalización Comunal 2024", 
  idc_2025 = "Puntaje Indice de Digitalización Comunal 2025", 
  idc_ranking_2024 = "Posición ranking IDC 2024", 
  idc_ranking_2025 = "Posición ranking IDC 2025",
  idc_tramo_2024 = "Tramo IDC 2024", 
  idc_tramo_2025 = "Tramo IDC 2025",
  
  # Índices 2024
  a_indice_2024 = "Puntaje dimensión de conectividad 2024",
  b_indice_2024 = "Puntaje dimensión de municipio digital 2024",
  c_indice_2024 = "Puntaje dimensión de adopción digital educativa 2024",
  
  # Rankings 2024
  a_ranking_2024 = "Posición en la dimensión de conectividad 2024",
  b_ranking_2024 = "Posición en la dimensión de municipio digital 2024",
  c_ranking_2024 = "Posición en la dimensión de adopción digital educativa 2024",
  
  # Items 2024 - a_conectividad (7)
  a_item1_2024 = "Total conexiones fijas residenciales por comuna a diciembre 2023",
  a_item2_2024 = "Tipo de tecnología por comuna a diciembre 2023: ADSL",
  a_item3_2024 = "Tipo de tecnología por comuna a diciembre 2023: FTTX",
  a_item4_2024 = "Tipo de tecnología por comuna a diciembre 2023: MODEM",
  a_item5_2024 = "Tipo de tecnología por comuna a diciembre 2023: OTA",
  a_item6_2024 = "Tipo de tecnología por comuna a diciembre 2023: OTI",
  a_item7_2024 = "Tipo de tecnología por comuna a diciembre 2023: W",
  
  # Items 2024 - b_politico (34)
  b_item1_2024 = "Página web del Municipio permite pago de permiso de circulación en línea ",
  b_item2_2024 = "Página web del Municipio permite pago de multas en línea",
  b_item3_2024 = "Página web del Municipio permite pago de patentes municipales en línea",
  b_item4_2024 = "Página web del Municipio permite pago de derechos de aseo en línea",
  b_item5_2024 = "Página web del Municipio permite pago de derechos varios en línea",
  b_item6_2024 = "Página web del Municipio permite realizar trámites en línea con ClaveUnica",
  b_item7_2024 = "Página web del Municipio permite realizar trámites de la Dirección de Obras Municipales en línea",
  b_item8_2024 = "Página web del Municipio permite realizar trámites de Tarjeta Vecino en línea",
  b_item9_2024 = "Página web del Municipio permite realizar trámites de la Oficina Municipal de Información Laboral en línea",
  b_item10_2024 = "Página web del Municipio permite realizar trámites de la Dirección Municipal de Tránsito en línea",
  b_item11_2024 = "Página web del Municipio permite realizar trámites de patentes municipales en línea",
  b_item12_2024 = "Página web del Municipio permite realizar trámites de la Oficina Municipal de Información, Reclamos y Sugerencias en línea ",
  b_item13_2024 = "Página web del Municipio permite realizar trámites de la Dirección Municipal de Aseo y Ornato en línea ",
  b_item14_2024 = "Municipio cuenta con aplicación para dispositivos móviles",
  b_item15_2024 = "Página web del Municipio permite acceder a la información del Plan Regulador en línea",
  b_item16_2024 = "Página web del Municipio permite acceder a la información de Concursos Públicos en línea",
  b_item17_2024 = "Página web del Municipio permite acceder a las actas del Concejo Municipal en línea ",
  b_item18_2024 = "Página web del Municipio permite acceder a las sesiones del Concejo Municipal en línea ",
  b_item19_2024 = "Página web del Municipio permite acceder a la información de Ordenanzas Municipales en línea",
  b_item20_2024 = "Página web del Municipio permite acceder al Código de Ética Municipal en línea ",
  b_item21_2024 = "Página web del Municipio permite acceder a la información del Plan de Desarrollo Comunal en línea",
  b_item22_2024 = "Página web del Municipio permite solicitar audiencias con autoridades municipales en línea",
  b_item23_2024 = "Página web del Municipio permite acceder a información municipal publicada en el Portal de Transparencia Activa",
  b_item24_2024 = "Página web del Municipio permite solicitar información via Ley de Transparencia",
  b_item25_2024 = "Página web del Municipio cuenta con datos abiertos municipales",
  b_item26_2024 = "Página web del Municipio permite acceder a información de sus Organizaciones Comunitarias en línea",
  b_item27_2024 = "Página web del Municipio permite acceder a información relacionada a la gestión municipal",
  b_item28_2024 = "Página web del Municipio permite acceder a información relacionada a lobby y gestión de intereses",
  b_item29_2024 = "Página web del Municipio permite acceder a información de las compras públicas realizadas en línea",
  b_item30_2024 = "Página web del Municipio cuenta con redes sociales",
  b_item31_2024 = "Página web del Municipio permite acceder a información de subsidios y becas en línea",
  b_item32_2024 = "Página web del Municipio permite realizar denuncias en línea",
  b_item33_2024 = "Página web del Municipio permite acceder a organigrama municipal en línea",
  b_item34_2024 = "Página web del Municipio permite acceder a noticias de la comuna",
  
  # Items 2024 - c_educacion (18) con prefijo en las descripciones
  c_4b_item1_2024 = "[Datos SIMCE 2023 4to básico] En el colegio nos motivan a utilizar herramientas tecnológicas.",
  c_4b_item2_2024 = "[Datos SIMCE 2023 4to básico] Ocupamos la tecnología frecuentemente en el colegio para aprender.",
  c_4b_item3_2024 = "[Datos SIMCE 2023 4to básico] Para mis compañeros(as) es importante saber usar los computadores para aprender.",
  c_4b_item4_2024 = "[Datos SIMCE 2023 4to básico] Los profesores y profesoras nos enseñan a utilizar herramientas tecnológicas.",
  c_4b_item5_2024 = "[Datos SIMCE 2023 4to básico] En el colegio se preocupan de tener los computadores en buen estado.",
  c_4b_item6_2024 = "[Datos SIMCE 2023 4to básico] El colegio cuenta con internet de buena calidad.",
  c_2m_item1_2024 = "[Datos SIMCE 2023 II° medio] En el colegio nos motivan a utilizar herramientas tecnológicas.",
  c_2m_item2_2024 = "[Datos SIMCE 2023 II° medio] Ocupamos la tecnología frecuentemente en el colegio para aprender.",
  c_2m_item3_2024 = "[Datos SIMCE 2023 II° medio] Para mis compañeros(as) es importante saber usar los computadores para aprender.",
  c_2m_item4_2024 = "[Datos SIMCE 2023 II° medio] Los profesores y profesoras nos enseñan a utilizar herramientas tecnológicas.",
  c_2m_item5_2024 = "[Datos SIMCE 2023 II° medio] En el colegio se preocupan de tener los computadores en buen estado.",
  c_2m_item6_2024 = "[Datos SIMCE 2023 II° medio] El colegio cuenta con internet de buena calidad.",
  c_prof_item1_2024 = "[Datos SIMCE 2023 Profesores] El establecimiento entrega capacitaciones a sus funcionarios(as) para mejorar habilidades computacionales.",
  c_prof_item2_2024 = "[Datos SIMCE 2023 Profesores] Las y los profesionales del establecimiento se apoyan entre sí en caso de tener problemas asociados a la tecnología.",
  c_prof_item3_2024 = "[Datos SIMCE 2023 Profesores] El establecimiento se preocupa de que haya alguien a cargo de las herramientas tecnológicas.",
  c_prof_item4_2024 = "[Datos SIMCE 2023 Profesores] El establecimiento se preocupa constantemente de mejorar la infraestructura tecnológica.",
  c_prof_item5_2024 = "[Datos SIMCE 2023 Profesores] El establecimiento cuenta con internet rápido.",
  c_prof_item6_2024 = "[Datos SIMCE 2023 Profesores] El establecimiento cuenta con sala de tecnología equipada con computadores.",
  c_prof_item7_2024 = "[Datos SIMCE 2023 Profesores] Los computadores pueden ser utilizados por cualquiera que requiera información.",
  
  # Índices 2025
  a_indice_2025 = "Puntaje dimensión de conectividad 2025",
  b_indice_2025 = "Puntaje dimensión de municipio digital 2025",
  c_indice_2025 = "Puntaje dimensión de conectividad 2025",
  
  # Rankings 2025
  a_ranking_2025 = "Posición en la dimensión de conectividad 2025",
  b_ranking_2025 = "Posición en la dimensión de municipio digital 2025",
  c_ranking_2025 = "Posición en la dimensión de adopción digital educativa 2025",
  
  # Items 2025 - a_conectividad (7)
  a_item1_2025 = "Total conexiones fijas residenciales por comuna a diciembre 2024",
  a_item2_2025 = "Tipo de tecnología por comuna a diciembre 2024: ADSL",
  a_item3_2025 = "Tipo de tecnología por comuna a diciembre 2024: FTTX",
  a_item4_2025 = "Tipo de tecnología por comuna a diciembre 2024: MODEM",
  a_item5_2025 = "Tipo de tecnología por comuna a diciembre 2024: OTA",
  a_item6_2025 = "Tipo de tecnología por comuna a diciembre 2024: OTI",
  a_item7_2025 = "Tipo de tecnología por comuna a diciembre 2024: W",
  
  # Items 2025 - b_politico (34)
  b_item1_2025 = "Página web del Municipio permite pago de permiso de circulación en línea ",
  b_item2_2025 = "Página web del Municipio permite pago de multas en línea",
  b_item3_2025 = "Página web del Municipio permite pago de patentes municipales en línea",
  b_item4_2025 = "Página web del Municipio permite pago de derechos de aseo en línea",
  b_item5_2025 = "Página web del Municipio permite pago de derechos varios en línea",
  b_item6_2025 = "Página web del Municipio permite realizar trámites en línea con ClaveUnica",
  b_item7_2025 = "Página web del Municipio permite realizar trámites de la Dirección de Obras Municipales en línea",
  b_item8_2025 = "Página web del Municipio permite realizar trámites de Tarjeta Vecino en línea",
  b_item9_2025 = "Página web del Municipio permite realizar trámites de la Oficina Municipal de Información Laboral en línea",
  b_item10_2025 = "Página web del Municipio permite realizar trámites de la Dirección Municipal de Tránsito en línea",
  b_item11_2025 = "Página web del Municipio permite realizar trámites de patentes municipales en línea",
  b_item12_2025 = "Página web del Municipio permite realizar trámites de la Oficina Municipal de Información, Reclamos y Sugerencias en línea ",
  b_item13_2025 = "Página web del Municipio permite realizar trámites de la Dirección Municipal de Aseo y Ornato en línea ",
  b_item14_2025 = "Municipio cuenta con aplicación para dispositivos móviles",
  b_item15_2025 = "Página web del Municipio permite acceder a la información del Plan Regulador en línea",
  b_item16_2025 = "Página web del Municipio permite acceder a la información de Concursos Públicos en línea",
  b_item17_2025 = "Página web del Municipio permite acceder a las actas del Concejo Municipal en línea ",
  b_item18_2025 = "Página web del Municipio permite acceder a las sesiones del Concejo Municipal en línea ",
  b_item19_2025 = "Página web del Municipio  permite acceder a la información de Ordenanzas Municipales en línea",
  b_item20_2025 = "Página web del Municipio permite acceder al Código de Ética Municipal en línea ",
  b_item21_2025 = "Página web del Municipio permite acceder a la información del Plan de Desarrollo Comunal en línea",
  b_item22_2025 = "Página web del Municipio permite solicitar audiencias con autoridades municipales en línea",
  b_item23_2025 = "Página web del Municipio permite acceder a información municipal publicada en el Portal de Transparencia Activa",
  b_item24_2025 = "Página web del Municipio permite solicitar información via Ley de Transparencia",
  b_item25_2025 = "Página web del Municipio cuenta con datos abiertos municipales",
  b_item26_2025 = "Página web del Municipio permite acceder a información de sus Organizaciones Comunitarias en línea",
  b_item27_2025 = "Página web del Municipio permite acceder a información relacionada a la gestión municipal",
  b_item28_2025 = "Página web del Municipio permite acceder a información relacionada a lobby y gestión de intereses",
  b_item29_2025 = "Página web del Municipio permite acceder a información de las compras públicas realizadas en línea",
  b_item30_2025 = "Página web del Municipio cuenta con redes sociales",
  b_item31_2025 = "Página web del Municipio permite acceder a información de subsidios y becas en línea",
  b_item32_2025 = "Página web del Municipio permite realizar denuncias en línea",
  b_item33_2025 = "Página web del Municipio permite acceder a organigrama municipal en línea",
  b_item34_2025 = "Página web del Municipio permite acceder a noticias de la comuna",
  
  # Items 2025 - c_educacion (7)
  c_item1_2025 = "[Datos SIMCE 2024 Profesores] El colegio entrega capacitaciones a sus funcionarios(as) para mejorar habilidades y conocimientos digitales.",
  c_item2_2025 = "[Datos SIMCE 2024 Profesores] Las y los profesionales del colegio se apoyan entre sí en caso de tener problemas con el uso pedagógico de la tecnología.",
  c_item3_2025 = "[Datos SIMCE 2024 Profesores] En el colegio se preocupan de que haya alguien a cargo de las herramientas tecnológicas.",
  c_item4_2025 = "[Datos SIMCE 2024 Profesores] En el colegio se preocupan constantemente de mejorar la infraestructura tecnológica.",
  c_item5_2025 = "[Datos SIMCE 2024 Profesores] El colegio cuenta con Internet estable y rápido.",
  c_item6_2025 = "[Datos SIMCE 2024 Profesores] Los equipos tecnológicos del colegio pueden ser utilizados para fines académicos por cualquier integrante de la comunidad educativa.",
  c_item7_2025 = "[Datos SIMCE 2024 Profesores] El colegio dispone de protocolos y reglamentos sobre el uso adecuado de las Tecnologías de la Información y la Comunicación (TIC)."
)

# Aplicar etiquetas y convertir a double
common <- intersect(names(labels_vector), names(idc_full))

for (col in common) {
  vec <- idc_full[[col]]
  
  if (is.numeric(vec)) {
    # para numéricos usamos haven::labelled (mantiene valores numéricos)
    idc_full[[col]] <- haven::labelled(vec, label = labels_vector[[col]])
  } else {
    # para caracteres / factores / lógicos / etc. ponemos atributo 'label' y no tocamos valores
    attr(idc_full[[col]], "label") <- labels_vector[[col]]
  }
}
# Save codebook ----
view_df(idc_full)

# Save data ----

# Private use
saveRDS(idc_full, file = "data/proc_data/private_data/2025_idc_full.rds")
writexl::write_xlsx(idc_full, path = "data/proc_data/private_data/2025_idc_full.xlsx")  # corregí extensión
write_sav(idc_full, "data/proc_data/private_data/2025_idc_full.sav")

# Public use
saveRDS(idc_full, file = "data/proc_data/public_data/2025_idc_v2.rds")
writexl::write_xlsx(idc_full, path = "data/proc_data/public_data/2025_idc_v2.xlsx")  # corregí extensión
write_sav(idc_full, "data/proc_data/public_data/2025_idc_v2.sav")
