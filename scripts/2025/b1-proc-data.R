# Preparación ----

library(readxl)
library(tidyverse)
library(janitor)
library(stringi)
library(sf)
library(kableExtra)
library(spdep)
library(forcats)
library(ggpubr)

# 0. Armar función de normalización a media 0,5 y sd 0,1 ----
normalize_to <- function(x, mean_target = 0.5, sd_target = 0.1) {
  mu  <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)
  z     <- (x - mu) / sigma
  x_norm <- z * sd_target + mean_target
  return(x_norm)
}

# Procesamiento datos 2024 ----

# 1. Cargar y tratar datos para municipio digital 2024
municipal_idc_2024 <- readRDS(
  "data/proc_data/private_data/2024_idc_full.rds"
) %>% 
  clean_names() %>% 
  select(
    cod = id_comuna,
    comuna = nombre_comuna,
    pago_circulacion = b_item1,
    pago_multas = b_item2,
    pago_patentes = b_item3,
    pago_aseo = b_item4,
    pago_varios = b_item5,
    clave_unica = b_item6,
    dom_dig = b_item7,
    tvecino = b_item8,
    omil_dig = b_item9,
    trans_dig = b_item10,
    patentes_dig = b_item11,
    oirs_dig = b_item12,
    ornato_dig = b_item13,
    app = b_item14,
    plan_regulador = b_item15,
    concursos_publicos = b_item16,
    actas_concejo = b_item17,
    sesiones_concejo = b_item18,
    ordenan = b_item19,
    cod_etica = b_item20,
    pladeco = b_item21,
    sol_audiencia = b_item22,
    trans_activa = b_item23,
    acceso_info = b_item24,
    datos_ab = b_item25,
    org_comunitarias = b_item26,
    gestion_publica = b_item27,
    lobby = b_item28,
    compras = b_item29,
    redes = b_item30,
    sub_becas = b_item31,
    denuncias = b_item32,
    org_municipal = b_item33,
    noticias = b_item34,
  ) %>%
  
# 2. Unificar formato
  mutate(
    across(where(is.character), ~ .x %>%
             stri_trans_general("Latin-ASCII") %>%  
             toupper() %>% 
             trimws() %>%  
             str_replace_all("[\u200B\u200C\u200D\uFEFF]", "") %>%  
             str_replace_all("[[:space:]]+", " ")),
    comuna = case_when(
      comuna == "AISEN" ~ "AYSEN",
      comuna == "O'HIGGINS" ~ "OHIGGINS",
      TRUE ~ comuna),
    cod = as.numeric(cod)) %>% 
  
# 3. Calcular promedio de cada dimensión
  mutate(
    mean_tramites = rowMeans(across(pago_circulacion:app), na.rm = TRUE),  
    mean_info = rowMeans(across(plan_regulador:noticias), na.rm = TRUE),
    
    # 4. Calcular subíndice MD y normalizarlo
    puntaje_md = (mean_tramites + mean_info) / 2,
    puntaje_md = normalize_to(puntaje_md, mean_target = 0.5, sd_target = 0.1),
    
    # 5. Categorizar por tramos
    tramo = case_when(
      between(puntaje_md, 0.5649834, 0.7877375) ~ "ALTO",
      between(puntaje_md, 0.4907320, 0.5623315) ~ "MEDIO ALTO",
      between(puntaje_md, 0.4297398, 0.4854283) ~ "MEDIO BAJO",
      between(puntaje_md, 0.1910747, 0.4270880) ~ "BAJO"), 
    # 6. Agregar variable year para diferenciar al unir con datos 2025
    year = "2024",
    
    # 7. Generar variable región
    region = case_when(
      cod >= 1101 & cod <= 1405 ~ "TARAPACA",
      cod >= 2101 & cod <= 2302 ~ "ANTOFAGASTA",
      cod >= 3101 & cod <= 3304 ~ "ATACAMA",
      cod >= 4101 & cod <= 4305 ~ "COQUIMBO",
      cod >= 5101 & cod <= 5804 ~ "VALPARAISO",
      cod >= 6101 & cod <= 6310 ~ "OHIGGINS",
      cod >= 7101 & cod <= 7408 ~ "MAULE",
      cod >= 8101 & cod <= 8314 ~ "BIOBIO",
      cod >= 9101 & cod <= 9211 ~ "ARAUCANIA",
      cod >= 10101 & cod <= 10404 ~ "LOS LAGOS",
      cod >= 11101 & cod <= 11402 ~ "AYSEN",
      cod >= 12101 & cod <= 12402 ~ "MAGALLANES",
      cod >= 13101 & cod <= 13605 ~ "METROPOLITANA",
      cod >= 14101 & cod <= 14204 ~ "LOS RIOS",
      cod >= 15101 & cod <= 15202 ~ "ARICA Y PARINACOTA",
      cod >= 16101 & cod <= 16305 ~ "NUBLE"
    ))%>%
  arrange(desc(puntaje_md)) %>% # 8. Ordenar puntajes de mayor --> menor
  mutate(ranking = row_number()) %>% # 9. Generar ranking con todos los decimales
  mutate(puntaje_md = round(puntaje_md, 3)) %>% # 10. Redondear a 3 decimales
  select(
    cod,
    region,
    comuna,
    year,
    puntaje_md,
    tramo,
    ranking,
    everything()
  )
#  filter(!comuna %in% c( #11. Filtrar comunas que todavía no están para 2025
 #   "DONIHUE",
  #  "PICHIDEGUA",
   # "TOLTEN",
    #"HUALAIHUE",
    #"INDEPENDENCIA"))

# Repetir proceso para 2025 ----

municipal_idc_2025 <- read_excel(
  path = "data/raw_data/b_politico/2025_municipios_digital.xlsx",
  sheet = "ÍNDICE MUNICIPAL DIG 2025"
) %>% 
  clean_names() %>% 
  select(-2) %>% 
  mutate(across(where(is.character), ~ .x %>%
                  stri_trans_general("Latin-ASCII") %>%  
                  toupper() %>% 
                  trimws() %>%  
                  str_replace_all("[\u200B\u200C\u200D\uFEFF]", "") %>%  
                  str_replace_all("[[:space:]]+", " ")  
  )) %>% 
  mutate(
    region = case_when(
      cod >= 1101 & cod <= 1405 ~ "TARAPACA",
      cod >= 2101 & cod <= 2302 ~ "ANTOFAGASTA",
      cod >= 3101 & cod <= 3304 ~ "ATACAMA",
      cod >= 4101 & cod <= 4305 ~ "COQUIMBO",
      cod >= 5101 & cod <= 5804 ~ "VALPARAISO",
      cod >= 6101 & cod <= 6310 ~ "OHIGGINS",
      cod >= 7101 & cod <= 7408 ~ "MAULE",
      cod >= 8101 & cod <= 8314 ~ "BIOBIO",
      cod >= 9101 & cod <= 9211 ~ "ARAUCANIA",
      cod >= 10101 & cod <= 10404 ~ "LOS LAGOS",
      cod >= 11101 & cod <= 11402 ~ "AYSEN",
      cod >= 12101 & cod <= 12402 ~ "MAGALLANES",
      cod >= 13101 & cod <= 13605 ~ "METROPOLITANA",
      cod >= 14101 & cod <= 14204 ~ "LOS RIOS",
      cod >= 15101 & cod <= 15202 ~ "ARICA Y PARINACOTA",
      cod >= 16101 & cod <= 16305 ~ "NUBLE"
    ),
    comuna = case_when(
      comuna == "O'HIGGINS" ~ "OHIGGINS",
      comuna == "PAIGUANO" ~ "PAIHUANO",
      comuna == "TILTIL" ~ "TIL TIL",
      comuna == "TREHUACO" ~ "TREGUACO",
      TRUE ~ comuna
    )
  )%>%
#  filter(
  #  !if_all(everything(), is.na),
   # !comuna %in% c(
    #  "DONIHUE",
     # "PICHIDEGUA",
      #"TOLTEN",
      #"HUALAIHUE",
      #"INDEPENDENCIA"
  #  )) %>%
  mutate(
    year = "2025") %>%
  select(
    cod,
    region,
    comuna,
    year,
    poblacion,
    mean_tramites,
    mean_info,
    everything()
  ) %>%
  mutate(
    mean_tramites = rowMeans(across(pago_circulacion:app), na.rm = TRUE),  
    mean_info = rowMeans(across(plan_regulador:noticias), na.rm = TRUE),
    puntaje_md = (mean_tramites + mean_info) / 2,
    puntaje_md = normalize_to(puntaje_md, mean_target = 0.5, sd_target = 0.1),
    tramo = case_when(
      between(puntaje_md, 0.5649834, 0.7877375) ~ "ALTO",
      between(puntaje_md, 0.4907320, 0.5623315) ~ "MEDIO ALTO",
      between(puntaje_md, 0.4297398, 0.4854283) ~ "MEDIO BAJO",
      between(puntaje_md, 0.1910747, 0.4270880) ~ "BAJO")) %>% 
  arrange(desc(puntaje_md)) %>%
  mutate(ranking = row_number()) %>% 
  mutate(puntaje_md = round(puntaje_md, 3)) %>% 
  select(
    cod,
    region,
    comuna,
    year,
    poblacion,
    puntaje_md,
    tramo,
    ranking,
    everything()
  ) 

all(municipal_idc_2024$comuna %in% municipal_idc_2025$comuna) #Comprobar replicabilidad de comunas

municipal_idc_consolidado <- bind_rows( #Generar consolidado
  municipal_idc_2024,
  municipal_idc_2025
)

# Guardar datos ----

#Limpiar nombres de columnas
municipal_idc_consolidado <- municipal_idc_consolidado%>%
  clean_names()

#Pasar a formato long
municipal_idc_consolidado <- municipal_idc_consolidado %>%
  select(-tramo)%>%
  # Pivotar a long, separando year en el valor y creando columna "variable"
  pivot_longer(
    cols = -c(cod, region, comuna, year),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  # Añadir sufijo con el año
  mutate(variable = paste0(variable, "_", year)) %>%
  # Volver a wide: cada variable_año será una columna
  select(-year) %>%
  pivot_wider(
    names_from = variable,
    values_from = valor
  )

saveRDS(municipal_idc_consolidado,"data/proc_data/private_data/2025_b_municipal.rds")
writexl::write_xlsx(municipal_idc_consolidado, path = "data/proc_data/private_data/2025_b_municipal.xlsx")
