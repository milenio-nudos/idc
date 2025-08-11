###06-08-2025 Armado base dimensión conectividad hogares 

##Cargar base----
library("readxl")
library("writexl")
DCH <- read_excel("data/raw_data/a_conectividad/2025_idc_input_a_conectividad.xlsx")

##Validación dimensión----

#Creación Conexiones/Población
DCH$C25V <- ifelse(DCH$Población_censo_2024 == 0 | is.na(DCH$Población_censo_2024),
                                  NA,
                                  DCH$Conexiones_fijas_residenciales_2024 / DCH$Población_censo_2024)

#Estandarización
DCH$C25V_z = 0.5 + 0.1 * (DCH$C25V - mean(DCH$C25V, na.rm = TRUE)) / sd(DCH$C25V, na.rm = TRUE)

#Redonde a 3 decimales
DCH$C25V_z_3 <- round(DCH$C25V_z,3)

##Guardar base----

DCH <- DCH %>%
  clean_names()

saveRDS(DCH, file = "data/proc_data/private_data/2025_a_conectividad.rds")
write_xlsx(DCH,path = "data/proc_data/private_data/2025_a_conectividad.xlsx")
