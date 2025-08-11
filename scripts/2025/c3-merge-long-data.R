# Preparation --------

source("scripts/2025/c2-proc-communal-level.R")

indice_2024 <- readRDS("~/GitHub/milenio_nudos/idc/data/proc_data/private_data/2024_idc_full.rds")|>
  select(id_comuna,nombre_comuna,c_indice,matches("^c_.*item[0-9]+$"))

indice_2025 <- c_educativo_2025

# Union de muestra de comunas -----------------------------------------

# Seleccionar ID y variable de índice de cada año
c_2024 <- indice_2024 %>%
  rename_with(~ paste0(.x, "_2024"), .cols = -c(1,2))

c_2025 <- indice_2025 %>%
  mutate(id_comuna = as.character(m_id)) %>%
  select(-m_id,-m_name)%>%
  rename_with(~ paste0(.x, "_2025"), .cols = -9)

# Unir ambas bases por m_id
c_long <- full_join(c_2024, c_2025, by = "id_comuna", suffix = c("_2024", "_2025"))

# Añadir columna de regiones
regiones <- tibble::tribble(
  ~region_code, ~region_name,
  "15", "Arica y Parinacota",
  "01", "Tarapacá",
  "02", "Antofagasta",
  "03", "Atacama",
  "04", "Coquimbo",
  "05", "Valparaíso",
  "13", "Metropolitana de Santiago",
  "06", "O'Higgins",
  "07", "Maule",
  "16", "Ñuble",
  "08", "Biobío",
  "09", "La Araucanía",
  "14", "Los Ríos",
  "10", "Los Lagos",
  "11", "Aysén del General Carlos Ibáñez del Campo",
  "12", "Magallanes y de la Antártica Chilena"
)

# Luego, en tu tabla principal:
c_long <- c_long %>%
  mutate(
    region_code = substr(as.character(id_comuna), 1, 2),
    region = regiones$region_name[match(region_code, regiones$region_code)]
  )%>%
  select(id_comuna, nombre_comuna, region, region_code, everything())

names(c_long)

# Estimar índice estandarizado ----
c_long <- c_long%>%
  mutate(c_indice_2025_z = 0.5 + 0.1 * (c_indice_2025 - mean(c_indice_2025, na.rm = TRUE)) / sd(c_indice_2025, na.rm = TRUE),
         c_tramo_2025 = ntile(c_indice_2025_z, 4),
         c_tramo_2025 = labelled(
           as.double(c_tramo_2025),
           labels = c(
             "bajo" = 1,
             "medio bajo" = 2,
             "medio alto" = 3,
             "alto" = 4
           )
  )
)



# Save data ----

c_long <- c_long%>% clean_names()

saveRDS(c_long, "data/proc_data/private_data/2025_c_educativo.rds")
writexl::write_xlsx(c_long, path = "data/proc_data/private_data/2025_c_educativo.xlsx")