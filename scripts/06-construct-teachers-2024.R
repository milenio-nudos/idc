# Seteamos enviroment

library(pacman)
p_load(dplyr, ggplot2, haven,stringi)
options(scipen = 999)
rm(list = ls())

# Cargamos los datos

load("./data/proc_data/private_data/2024_nudos_simce_prof.RData")

simce_prof_2025 <- readRDS("./data/proc_data/private_data/2025_mother_data.rds")

idc_2024 <- read_excel("data/proc_data/public_data/2024_idc_v1.xlsx")

# Agrupamos datos 2025

comunas_2025 <- simce_prof_2025 %>%
  group_by(m_id) %>%
  summarise(
    m_name = first(m_name),
    m_idc = first(m_idc),  # Mantener el primer caso de m_idc por comuna
    n_profesores_total = n(),
    n_casos_perdidos = sum(is.na(p_idc)),  # Asumiendo que p_idc es la variable clave
    prop_casos_perdidos = (n_casos_perdidos / n_profesores_total) * 100,
    casos_validos = n_profesores_total - n_casos_perdidos,
    .groups = 'drop'
  )

# Filtramos comunas que cumplen el umbral (menos del 90% de casos perdidos)
UMBRAL_EXCLUSION <- 10  # 10% de umbral = máximo 90% de casos perdidos

comunas_validas_2025 <- comunas_2025 %>%
  mutate(m_idc = ifelse(prop_casos_perdidos > 90, NA,m_idc))|>
  arrange(prop_casos_perdidos)  # Ordenar de menor a mayor proporción de casos perdidos

# Agrupamos dataframe 2024
comunas_analisis_2024 <- simce_prof %>%
  group_by(m_id) %>%
  summarise(
    m_name = first(m_name),
    m_nudos_opp = first(m_nudos_opp),  # Mantener el primer caso de m_nudos_opp por comuna
    .groups = 'drop'
  ) %>%
  arrange(m_nudos_opp) 

# Eliminar la fila que no comparten (Rio verde)
comunas_validas_2025 <- comunas_validas_2025 %>%
  filter(m_id != setdiff(comunas_validas_2025$m_id, comunas_analisis_2024$m_id))

# Agregar fila todos los estamentos 2024
idc_2024 <- idc_2024|>
  select(id_comuna, c_indice)|>
  rename(m_idc_2024 = c_indice, m_id = id_comuna)|>
  mutate(m_id = as.integer(m_id))

# Guardar comunas validas
saveRDS(comunas_validas_2025, "./data/proc_data/private_data/comunas_validas_2025.rds")

# Correlación de índices por comuna ----------------------------------------------------

# Combinar los dataframes manteniendo solo las columnas necesarias
resultado <- comunas_validas_2025 %>%
  select(m_id, m_name, m_idc_2025 = m_idc) %>%  # Renombrar para claridad
  inner_join(
    comunas_analisis_2024 %>% select(m_id, m_nudos_opp_2024 = m_nudos_opp),
    by = "m_id"
  )

resultado <- resultado|>
  inner_join(
    idc_2024 |> select(m_id, m_idc_2024),
    by = "m_id"
  )


# Hacer columnas de ranking
comparaciones_educacion <- resultado|>
  arrange(desc(m_nudos_opp_2024))|>
  mutate(ranking_prof_2024 = 1:344)|>
  arrange(desc(m_idc_2024))|>
  mutate(ranking_idc_social_2024 = 1:344)|>
  arrange(desc(m_idc_2025))|>
  mutate(ranking_2025 = 1:344)

# Guardar base comparaciones
saveRDS(comparaciones_educacion, "./data/proc_data/private_data/comparacion_educacion_2024_2025.rds")

# Filtrar solo comunas presentes en ambos años con datos completos
datos_completos <- resultado %>%
 filter(!is.na(m_idc_2025) & !is.na(m_nudos_opp_2024))

# Verificar cuántas comunas tenemos
cat("Comunas con datos en ambos años:", nrow(datos_completos), "\n")

# Calcular correlación
r_valor <- cor(datos_completos$m_nudos_opp_2024, datos_completos$m_idc_2025, use = "complete.obs") %>% round(2)

# Graficar
ggplot(datos_completos, aes(x = m_nudos_opp_2024, y = m_idc_2025)) +
  geom_point(color = "#6495ED", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -1,
           label = paste0("r = ", r_valor), size = 5, color = "black") +
  labs(
    x = "índice educacional 2024 (solo docentes)",
    y = "Índice educacional 2025", 
    title = "Relación entre Índices por Comuna 2024-2025",
    subtitle = paste0("n = ", nrow(datos_completos), " comunas"),
    caption = "Se utilizan solo variables de profesores para el subíndice educacional 2024"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

idc_2025_filtrado <- simce_prof_2025 %>%
  filter(m_id %in% comunas_validas_2025$m_id)

comunas_en_filtrado <- unique(idc_2025_filtrado$m_id)
comunas_esperadas <- comunas_validas_2025$m_id

if (setequal(comunas_en_filtrado, comunas_esperadas)) {
  cat("✓ Las comunas filtradas coinciden perfectamente con las comunas válidas\n")
} else {
  cat("⚠ Hay discrepancias en las comunas filtradas\n")
}

writexl::write_xlsx(idc_2025_filtrado, here::here("data", "proc_data", "public_data", "2025_idc_v2.xlsx"))

