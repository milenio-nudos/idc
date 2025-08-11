### Filtración de comunas no válidas dimensión educativa
### 10.08.2025

# Seteamos enviroment

library(pacman)
p_load(dplyr, ggplot2, haven,stringi)
options(scipen = 999)
rm(list = ls())

# Cargamos los datos
#load("./data/proc_data/private_data/2024_c_educativo_profesores.RData") #2024
source("scripts/2025/c1-proc-individual-level.R") #2025

# Agrupamos datos 2025
c_educativo_2025 <- docente %>%
  group_by(m_id) %>%
  summarise(
    m_name = first(m_name),
    m_idc = first(m_idc),  # Mantener el primer caso de m_idc por comuna
    n_profesores_total = n(),
    n_casos_perdidos = sum(is.na(p_idc)),  # Asumiendo que p_idc es la variable clave
    prop_casos_perdidos = (n_casos_perdidos / n_profesores_total) * 100,
    casos_validos = n_profesores_total - n_casos_perdidos,
    .groups = 'drop',
    c_item1 = as.integer(mean(p_item1, na.rm = TRUE)),
    c_item2 = as.integer(mean(p_item2, na.rm = TRUE)),
    c_item3 = as.integer(mean(p_item3, na.rm = TRUE)),
    c_item4 = as.integer(mean(p_item4, na.rm = TRUE)),
    c_item5 = as.integer(mean(p_item5, na.rm = TRUE)),
    c_item6 = as.integer(mean(p_item6, na.rm = TRUE)),
    c_item7 = as.integer(mean(p_item7, na.rm = TRUE))
    )

#NA por umbral de 10%
c_educativo_2025 <- c_educativo_2025 %>%
  mutate(m_idc = ifelse(prop_casos_perdidos > 90, NA,m_idc))%>%
  select(
    m_id, m_name, c_indice=m_idc, starts_with("c_item")
  )

#saveRDS(c_educativo_2025, file = "data/proc_data/private_data/2025_c_educativo_communal_level.rds")
