source("scripts/2025/d1-proc-full-idc.R")

names(idc_full)

idc_region <- idc_full%>%
  filter(!is.na(idc_2025)) %>%
  group_by(id_region) %>%
  mutate(
    n_comunas = n_distinct(id_comuna)
  ) %>%
  summarise(
    region = first(region),
    n_comunas = first(n_comunas),
    idc_2025 = mean(idc_2025)
  ) %>%
  mutate(
    ranking_2025 = min_rank(desc(idc_2025))
  )%>%
  mutate(
    ranking_2025 = min_rank(desc(idc_2025)),
    categoria = case_when(
      idc_2025 >= 0.563 & idc_2025 <= 0.679 ~ "Alto",
      idc_2025 >= 0.506 & idc_2025 <= 0.562 ~ "Medio alto",
      idc_2025 >= 0.461 & idc_2025 <= 0.504 ~ "Medio bajo",
      idc_2025 >= 0.286 & idc_2025 <= 0.460 ~ "Bajo",
      TRUE ~ NA_character_
    )
  )



writexl::write_xlsx(idc_region, path = "data/proc_data/private_data/2025_idc_region_level.xlsx")
