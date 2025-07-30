# 0. Preparation ---------------------------------------------------------------
source("scripts/01-proc_data.R")

# 1. Creación variable cantidad de escuelas por comuna (pre limpiar NAs) -------

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
# 2. Creación de variable número y porcentaje de NAs por escuela ---------------

escuelas_stats_na <- docente %>%
  # Agrupamos por el identificador único de la escuela
  group_by(c_id) %>% 
  
  # Calculamos n y % de NAs
  summarise(
    c_name = first(c_name), 
    n_escuela = n(),
    n_NAs_escuela = sum(is.na(p_idc)),
    .groups = 'drop' 
  ) %>%
  mutate(
    prop_NAs_escuela = (n_NAs_escuela / n_escuela) * 100
  ) %>%
    select(c_id, c_name, n_escuela, n_NAs_escuela, prop_NAs_escuela)

escuelas_problematicas <- escuelas_stats_na %>%
  filter(prop_NAs_escuela > 90)

escuelas_full_na <- escuelas_stats_na %>%
  filter(prop_NAs_escuela == 100)

cat("Número de escuelas con más del 90% de NAs:", nrow(escuelas_problematicas), "\n")
cat("Total de escuelas", n_distinct(docente$c_id))


# 4. Eliminación de escuelas con más de un 90% de NAs --------------------------

docentes_filtrado <- docente %>%
  filter(!c_id %in% escuelas_problematicas$c_id)

cat("Dimensiones originales de 'docentes':", dim(docente), "\n")
cat("Dimensiones de 'docentes_filtrado':", dim(docentes_filtrado), "\n")

# 5. Creación de variable cantidad y porcentaje de escuelas por comuna (luego de limpiar NAs)

docentes_filtrado <- docentes_filtrado %>%
  group_by(m_id) %>%
  mutate(n_escuelas_post_filtro_comuna = n_distinct(c_id)) %>%
  ungroup()

comunas_stats <- docentes_filtrado %>%
  # Agrupamos por el identificador único de la comuna
  group_by(m_id) %>%
  
  # Rescatamos los valores y el nombre usando first()
  summarise(
    m_name = first(m_name), # Rescatamos el nombre de la comuna
    n_escuelas_original = first(n_escuelas_orig_comuna),
    n_escuelas_mantenidas = first(n_escuelas_post_filtro_comuna),
    .groups = 'drop'
  ) %>%
  
  # Calculamos el porcentaje de escuelas que se mantuvieron
  mutate(
    pct_escuelas_mantenidas = (n_escuelas_mantenidas / n_escuelas_original) * 100
  ) %>%
  
  # Reordenamos las columnas para mayor claridad (opcional)
  select(m_id, m_name, n_escuelas_original, n_escuelas_mantenidas, pct_escuelas_mantenidas)


# 6. Graficar cantidad de escuelas por comuna y proporción que representa-------

library(ggplot2)
library(forcats)

# Cambiar variable en filter (pct_escuelas_mantenidas o n_escuelas_mantenidas) para ver los datos
comunas_plot <- comunas_stats %>%
  filter(pct_escuelas_mantenidas < 10)

# Generamos el gráfico
ggplot(comunas_plot, aes(x = pct_escuelas_mantenidas, y = fct_reorder(m_name, pct_escuelas_mantenidas))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(
    aes(label = paste0("(", n_escuelas_mantenidas, " / ", n_escuelas_original, ")")), 
    hjust = -0.1, 
    size = 3
  ) +
  labs(
    title = "Comunas con menos del 10% de escuelas mantenidas después del filtro de NAs",
    subtitle = "La etiqueta muestra (escuelas mantenidas / escuelas originales)",
    x = "% de escuelas mantenidas",
    y = "Comuna"
  ) +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))


# 7. Análisis de proporción de NAs por nivel del docente (cuarto básico, sexto básico y segundo medio) -----

stats_nivel_educativo <- docente %>%
  group_by(p_grado) %>%
  summarise(
    n_total = n(),
    n_NAs = sum(is.na(p_idc)),
    n_completos = sum(!is.na(p_idc)),
    prop_NAs = (n_NAs / n_total) * 100,
    prop_completos = (n_completos / n_total) * 100,
    .groups = 'drop'
  ) %>%
  # Ordenar por proporción de NAs
  arrange(desc(prop_NAs))

stats_nivel_educativo %>%
  mutate(
    prop_NAs = round(prop_NAs, 1),
    prop_completos = round(prop_completos, 1)
  ) %>%
  select(p_grado, n_total, n_NAs, prop_NAs, n_completos, prop_completos) %>%
  print()

ggplot(stats_nivel_educativo, aes(x = prop_NAs, y = fct_reorder(p_grado, prop_NAs))) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_text(
    aes(label = paste0(round(prop_NAs, 1), "% (", n_NAs, " / ", n_total, ")")), 
    hjust = -0.1, 
    size = 3.5
  ) +
  labs(
    title = "Proporción de valores faltantes (NAs) por nivel educativo del profesor",
    subtitle = "La etiqueta muestra % de NAs (casos con NA / total casos)",
    x = "% de valores faltantes",
    y = "Nivel educativo (p_grado)"
  ) +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme(
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10)
  )

comunas_stats_flt <- comunas_stats %>% 
  filter(n_escuelas_mantenidas > 3)
