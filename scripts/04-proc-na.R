# 0. Preparation ---------------------------------------------------------------
source("scripts/01-proc_data.R")

comunas_stats_NAs <- docente %>%
  group_by(m_id) %>%
  summarise(
    m_name = first(m_name),
    n_escuelas_orig_comuna = n_distinct(c_id),
    n_profesores_orig_comuna = n(),
    n_NAs_comuna = sum(is.na(p_idc)),
    prop_NAs_comuna = (n_NAs_comuna / n_profesores_orig_comuna) * 100,
    casos_mantenidos = n_profesores_orig_comuna - n_NAs_comuna,
    etiqueta_prop = paste0(round(prop_NAs_comuna, 1), "%"),
    etiqueta_fraccion = paste0(casos_mantenidos, "/", n_profesores_orig_comuna),
    .groups = 'drop'
  ) %>% 
  select(m_id, m_name, n_profesores_orig_comuna, casos_mantenidos, n_NAs_comuna, prop_NAs_comuna, etiqueta_prop, etiqueta_fraccion) %>% 
  arrange(desc(prop_NAs_comuna))


umbrales <- c(5, 10, 15)

tabla_umbrales <- data.frame(
  umbral_casos = paste0(umbrales, "%"),
  numero_casos_mantenidos = sapply(umbrales, function(x) {
    sum(comunas_stats_NAs$prop_NAs_comuna < (100 - x))
  })
)

# Mostrar la tabla
print(tabla_umbrales)

library(knitr)
library(kableExtra)
tabla_html <- kable(tabla_umbrales,
                    format = "html",
                    col.names = c("Umbral de Casos", "Número de Casos Mantenidos"),
                    caption = "Comunas mantenidas según umbral de casos válidos") %>%
  kable_styling()

# Guardar como archivo HTML
save_kable(tabla_html, "tabla_umbrales.html")

library(ggtext)

datos_grafico <- comunas_stats_NAs %>% 
  filter(prop_NAs_comuna < 100 & prop_NAs_comuna > 75)

grafico_prop_nas <- ggplot(datos_grafico, aes(x = reorder(factor(m_id), -prop_NAs_comuna))) +
  # Barra verde para casos mantenidos
  geom_col(aes(y = casos_mantenidos), fill = "green", alpha = 0.7) +
  # Barra roja para casos con NA (apilada encima)
  geom_col(aes(y = n_NAs_comuna), fill = "red", alpha = 0.7, 
           position = position_nudge(y = datos_grafico$casos_mantenidos)) +
  # Añadir texto con proporción de NAs
  geom_text(aes(y = n_profesores_orig_comuna + 2, 
                label = etiqueta_prop), 
            size = 3, angle = 45, hjust = 0) +
  # Añadir texto con fracción de casos mantenidos dentro de la barra
  geom_text(aes(y = n_profesores_orig_comuna / 2, 
                label = etiqueta_fraccion), 
            size = 2, angle = 0, hjust = 0.5, color = "white", fontface = "bold") +
  # Personalizar ejes y etiquetas
  scale_x_discrete(labels = setNames(datos_grafico$m_name, datos_grafico$m_id)) +
  labs(
    title = "Distribución de Casos con NAs por Comuna",
    subtitle = "Comunas con más de 75% de casos con NAs",
    x = "Comuna",
    y = "Número de Profesores",
    caption = "Verde: Casos mantenidos | Rojo: Casos con NAs"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )
ggsave(grafico_prop_nas, filename = "grafico_prop_nas.png", width = 10, height = 6, dpi = 300, bg="white")

