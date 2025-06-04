# 0. Preparation ---------------------------------------------------------------
source("scripts/01-proc_data.R")

simce_comuna_2m <- read.csv("data/raw_data/2024_2m_comuna.csv", sep = ";")|>
  select(m_id = cod_com, prom_lect2m_com, prom_mate2m_com)
simce_comuna_6b <- read.csv("data/raw_data/2024_6b_comuna.csv", sep = ";")|>
  select(m_id = cod_com, prom_lect6b_com, prom_mate6b_com)
simce_comuna_4b <- read.csv("data/raw_data/2024_4b_comuna.csv", sep = ";")|>
  select(m_id = cod_com, prom_lect4b_com, prom_mate4b_com)

conectividad_comunal <- read_excel("data/raw_data/2023_conectividad_comunal.xlsx")|>
  select(m_id, nudos)
idh_comunal <- read_excel("data/raw_data/2023_idh_comunal.xlsx")|>
  select(m_id = CODIGO, IDH)
pobreza_multi_comunal <- read_excel("data/raw_data/2022_pobreza_multi_comunal.xlsx")|>
  select(m_id = Código, pobreza_multi=indice)
idc_2024 <- readRDS("data/raw_data/2024_nudos_final.rds")|>
  select(m_id = id_comuna, idc_2024=indice_nudos)

items <- docente|>
  select(starts_with("p_item"))

# 1. Correlations between items ------------------------------------------------

corrplot(rcorr(as.matrix(items))$r, p.mat = rcorr(as.matrix(items))$P, 
                           method = 'color', type = 'lower', insig='blank',
                           tl.col = "black",bg="white",na.label="-",
                           addCoef.col ='black', number.cex = 0.8, diag=FALSE,
                           sig.level = 0.05)


# 2. Item scales ----------------------------------------------------------------
itemscale_2025 <- tab_itemscale(items)
print(itemscale_2025)

# 3. Top and last 10 -----------------------------------------------------------

# Añadir variable "rm"
idc_rm <- idc %>%
  mutate(rm = ifelse(r_id == 13, "Metropolitana", "Región"))

# Usar la misma base para top_bottom
top_bottom <- idc_rm %>%
  arrange(desc(m_idc)) %>%
  slice_head(n = 10) %>%
  bind_rows(
    idc_rm %>% arrange(m_idc) %>% slice_head(n = 10)
  )

# Plot
ggplot(idc_rm, aes(x = m_idc, y = reorder(m_name, m_idc), color = rm)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    data = top_bottom,
    aes(label = m_name, color = rm),  # <- importante: asegurar que el color esté también en esta capa
    size = 3,
    max.overlaps = 50,
    direction = "y"
  ) +
  labs(
    title = "Distribución del índice por comuna",
    x = "Índice (m_idc)",
    y = NULL,
    color = "Zona"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank())

# 4. N de escuelas de los Top 10 --------------------------------------------------

idc|>
  head(10)|>
  select(ranking, m_name, m_ndocentes, m_nescuelas)

# 5. Corr with SIMCE score -----------------------------------------------------

# SIMCE
idc_simce <- merge(select(idc,m_id, idc_2025=m_idc), simce_comuna_2m, by = "m_id", all.x = TRUE)
idc_simce <- merge(idc, simce_comuna_6b, by = "m_id", all.x = TRUE)
idc_simce <- merge(idc, simce_comuna_4b, by = "m_id", all.x = TRUE)

corrplot(rcorr(as.matrix(idc_simce))$r, p.mat = rcorr(as.matrix(idc_simce))$P, 
         method = 'color', type = 'lower', insig='blank',
         tl.col = "black",bg="white",na.label="-",
         addCoef.col ='black', number.cex = 0.8, diag=FALSE,
         sig.level = 0.05)

# 6. Corr with important Indexes -----------------------------------------------
idc_others <- merge(select(idc, m_id, idc_2025=m_idc), idc_2024, by = "m_id", all.x = TRUE)
idc_others <- merge(idc_others, idh_comunal, by = "m_id", all.x = TRUE)
idc_others <- merge(idc_others, pobreza_multi_comunal, by = "m_id", all.x = TRUE)
idc_others <- merge(idc_others, conectividad_comunal, by = "m_id", all.x = TRUE)
corrplot(rcorr(as.matrix(idc_others))$r, p.mat = rcorr(as.matrix(idc_others))$P, 
         method = 'color', type = 'lower', insig='blank',
         tl.col = "black",bg="white",na.label="-",
         addCoef.col ='black', number.cex = 0.8, diag=FALSE,
         sig.level = 0.05)

# 7. N válido por comuna