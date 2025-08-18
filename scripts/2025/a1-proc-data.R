###06-08-2025 Armado base dimensión conectividad hogares 

##Cargar base----
library("readxl")
library("writexl")
load("/2025_idc_input_a_conectividad.RData")

##Validación dimensión----

#Creación Conexiones/Población
DCH$C25V <- ifelse(DCH$Población_censo_2024 == 0 | is.na(DCH$Población_censo_2024),
                                  NA,
                                  DCH$Conexiones_fijas_residenciales_2024 / DCH$Población_censo_2024)

#Estandarización
DCH$C25V_z = 0.5 + 0.1 * (DCH$C25V - mean(DCH$C25V, na.rm = TRUE)) / sd(DCH$C25V, na.rm = TRUE)

##Guardar base----

DCH <- DCH %>%
  clean_names() %>%
#Corrección: Se detectaron datos mal ingresados en variable idc_ranking_2024, específicamente en La Florida, Macul, y Coronel, por lo que se modificaron aquí manualmente de acuerdo al criterio que daba el ranking original, para dar continuidad a la clasificación.
mutate(
  ranking_2024 = case_match(
      comuna,
      "La Florida" ~ 68,
      "Macul"      ~ 69,
      "Coronel"    ~ 70,
      .default = ranking_2024)) %>% 
  mutate(ranking_2024 = as.numeric(ranking_2024))

saveRDS(DCH, file = "C:/Users/crist/OneDrive/Escritorio/Pega Toro/NUDOS/Índide de Digitalización Comunal/Índice Procesado/2025_a_conectividad.rds")
write_xlsx(DCH,path = "C:/Users/crist/OneDrive/Escritorio/Pega Toro/NUDOS/Índide de Digitalización Comunal/Índice Procesado/2025_a_conectividad.xlsx")

