# 1. Preparation ---------------------------------------------------------------

pacman::p_load(tidyverse,
               ggplot2)

mother_data_2024 <- readRDS("~/GitHub/milenio_nudos/idc/data/proc_data/private_data/2024_mother_data.rds")
mother_data_2025 <- readRDS("~/GitHub/milenio_nudos/idc/data/proc_data/private_data/2025_mother_data.rds")

indice_2024 <- read_excel("data/proc_data/public_data/2024_idc_v1.xlsx")
indice_2025 <- read_excel("data/proc_data/public_data/2025_idc_v1.xlsx")

# 2. Comparación de muestra de comunas -----------------------------------------

# Seleccionar ID y variable de índice de cada año
base_2024 <- indice_2024 %>%
  select(id_comuna, c_indice, nombre_comuna) %>%
  mutate(m_id = as.character(id_comuna)) %>%
  select(m_id, c_indice, nombre_comuna) %>%
  rename(indice_2024 = c_indice, m_name = nombre_comuna)

base_2025 <- indice_2025 %>%
  mutate(m_id = as.character(m_id)) %>%
  select(m_id, m_idc, m_name) %>%
  rename(indice_2025 = m_idc)

# Unir ambas bases por m_id
comparacion <- full_join(base_2024, base_2025, by = "m_id", suffix = c("_2024", "_2025"))

# Clasificar según presencia de NA
resultado <- comparacion %>%
  mutate(
    estado = case_when(
      is.na(indice_2024) & is.na(indice_2025) ~ "NA en ambos años",
      is.na(indice_2024) & !is.na(indice_2025) ~ "NA solo en 2024 (comuna ganada)",
      !is.na(indice_2024) & is.na(indice_2025) ~ "NA solo en 2025 (comuna perdida)",
      TRUE ~ "Presente en ambos"
    )
  )


# Tabla resumen
tabla_resumen <- resultado%>%
  dplyr::count(estado)

#Comunas ganadas
resultado_na <- resultado %>%
  filter(estado != "Presente en ambos") %>%
  select(m_name_2024, estado) %>%
  arrange(estado)

# Mostrar
print(resultado_na)
print(tabla_resumen)

# 3. Correlación de índices por comuna ----------------------------------------------------

# Filtrar solo comunas presentes en ambos años
datos_completos <- resultado %>%
  filter(!is.na(indice_2024) & !is.na(indice_2025))

# Calcular correlación
r_valor <- cor(datos_completos$indice_2024, datos_completos$indice_2025, use = "complete.obs") %>% round(2)

# Graficar
ggplot(datos_completos, aes(x = indice_2024, y = indice_2025)) +
  geom_point(color = "#6495ED", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -1,
           label = paste0("r = ", r_valor), size = 5, color = "black") +
  labs(
    x = "Todos los estamentos 2024",
    y = "Profesores 2025",
    title = "Relación Adopción digital 2024 y 2025",
    caption = "Las escalas difieren porque en el año 2024 transformamos la escala de 0 a 1"
    ) +
  theme_minimal()