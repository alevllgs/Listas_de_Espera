library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(readxl)
library(janitor)

Fecha_corte <- "2022-06-03"


Codigo_comunas <-  read_excel("Z:/Alejandro/Listas de Espera/BBDD complementarias/Codigo comunas.xlsx")


LE_Semanal <- read_excel("C:/Users/control.gestion3/Downloads/LE ABIERTA_20220603.xlsx", 
                            sheet = "BD_Sigte")

LE_Semanal <- LE_Semanal %>% 
  select(-SERV_SALUD, -NOMBRES, -PRIMER_APELLIDO, -SEGUNDO_APELLIDO, -PLANO, -EXTREMIDAD, 
         -VIA_DIRECCION, -NOM_CALLE, -RESTO_DIRECCION,-FONO_FIJO, -FONO_MOVIL,
         -EMAIL, -RUN_PROF_SOL, -DV_PROF_RESOL, -NUM_DIRECCION, -DV_PROF_SOL, -RUN_PROF_RESOL, -ID_LOCAL)
LE_Semanal$Fecha_corte <- Fecha_corte

LE_Semanal <- LE_Semanal %>% mutate(FECHA_NAC = substr(as.character(LE_Semanal$FECHA_NAC), start = 1, stop = 10))
LE_Semanal <- LE_Semanal %>% mutate(F_SALIDA = as.character(LE_Semanal$F_SALIDA))
LE_Semanal <- LE_Semanal %>% mutate(F_ENTRADA = as.character(LE_Semanal$F_ENTRADA))
LE_Semanal <- LE_Semanal %>% mutate(fallecido = as.character(LE_Semanal$fallecido))
LE_Semanal <- LE_Semanal %>% mutate(F_CITACION = as.character(LE_Semanal$F_CITACION))

LE_Semanal <- merge(x = LE_Semanal, y = Codigo_comunas, by.x = "COMUNA", by.y = "codigo", all.x = TRUE)
LE_Semanal$Comuna_texto <- LE_Semanal$Nombre
LE_Semanal <- LE_Semanal %>% select(-Provincia, -Región, -Superficie , -Densidad, -`IDH 2005`, -...9, -Latitud , -Longitud, -Nombre, -Población)


write.csv2(LE_Semanal, file="Z:/Alejandro/Listas de Espera/Listas de Espera DATA DEIS/LE_Semanal.csv", row.names = F)