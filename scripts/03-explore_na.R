# 0. Preparation ---------------------------------------------------------------
source("scripts/01-proc_data.R")

# 1. Total de casos perdidos en el índice por comuna ---------------------------

# 41% de los profesores no tienen índice
sum(is.na(docente$p_idc))/nrow(docente)

# A nivel agregado nos quedamos con 51 comunas menos (n = 295)
sum(is.na(idc))

# Calcular proporción de NA por comuna
na_por_comuna <- docente |>
  group_by(m_id, m_name) |>
  summarise(
    total_docentes = n(),
    n_na = sum(is.na(p_idc)),
    prop_na = n_na / total_docentes,
    .groups = "drop"
  ) |>
  arrange(desc(prop_na))

# Filtrar comunas con más de 75% de NA
etiquetar <- na_por_comuna|> head(10)

# Graficar
ggplot(na_por_comuna, aes(x = reorder(m_name, prop_na), y = prop_na)) +
  geom_col(fill = "#F08080") +
  geom_text_repel(
    data = etiquetar,
    aes(label = paste0(m_name, "\nN=", total_docentes)),
    size = 3,
    nudge_y = 0.02,
    segment.size = 0.3
  ) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Proporción de docentes sin índice por comuna",
    x = NULL,
    y = "% con NA en índice (p_idc)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank())

# 2. Explorar NA por ítem ------------------------------------------------------

# Obtener proporción de NA por ítem
items_na <- docente %>%
  select(starts_with("p_item")) %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "item", values_to = "prop_na")

# Añadir etiquetas y hacer wrap
items_na$label <- str_wrap(get_label(docente[items_na$item]), width = 30)

# Graficar
ggplot(items_na, aes(x = reorder(item, -prop_na), y = prop_na)) +
  geom_col(fill = "#4682B4") +
  geom_text(aes(label = percent(prop_na, accuracy = 1)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Proporción de NA por ítem (docentes)",
    x = "Ítem",
    y = "% de valores perdidos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = items_na$label)

#Evidentemente, los profesores no responden la escala completa...
#Son profesores que se saltan la escala completa, en la mayoría de las comunas.

# 3. Explorar NA en todos los ítems --------------------------------------------
items_list <- c("p_item1", "p_item2", "p_item3", "p_item4", "p_item5", "p_item6", "p_item7")

na_items_summary <- docente %>%
  rowwise() %>%
  mutate(n_na_items = sum(is.na(c_across(all_of(items_list))))) %>%
  ungroup() %>%
  group_by(n_na_items) %>%
  summarise(n = n()) %>%
  arrange(desc(n_na_items))%>%
  mutate(prop = n/sum(n))

print(na_items_summary)
