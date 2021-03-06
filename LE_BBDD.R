library(readxl)
library(tidyverse)
library(dplyr)

#este script procesa la bbdd del DataDEIS, debe contener una fecha de corte
#ojo con la bbdd de bloqueados que tiene una linea sobre los nombres de la columna

#fechacorte <- readline(prompt = "Ingresar Fecha de corte, formato 31-01-2021: ")

anio <- "2022"
mes <- "04"



dia <- case_when(
  mes=="01" ~ "31",
  mes=="02" ~ "28",
  mes=="03" ~ "31",
  mes=="04" ~ "30",
  mes=="05" ~ "31",
  mes=="06" ~ "30",
  mes=="07" ~ "31",
  mes=="08" ~ "31",
  mes=="09" ~ "30",
  mes=="10" ~ "31",
  mes=="11" ~ "30",
  mes=="12" ~ "31",
  TRUE ~ "0")

fechacorte <- paste0(anio,"-",mes,"-",dia)

archivo <- paste0("C:/Users/control.gestion3/OneDrive/BBDD Produccion/Listas de Espera/Listas de Espera DATA DEIS/BBDD LE DataDEIS/ABIERTOS_",anio,"_",mes,".xlsx")
errores_carga <- read_excel("C:/Users/control.gestion3/OneDrive/BBDD Produccion/Listas de Espera/Listas de Espera DATA DEIS/Acumulado de errores para dashboard.xlsx",sheet = "ERROR DE CARGA")
errores_rut_provisorio <- read_excel("C:/Users/control.gestion3/OneDrive/BBDD Produccion/Listas de Espera/Listas de Espera DATA DEIS/Acumulado de errores para dashboard.xlsx",sheet = "ERROR RUN PROVISORIO")

BBDD_LE <- read.csv("C:/Users/control.gestion3/OneDrive/BBDD Produccion/Listas de Espera/Listas de Espera DATA DEIS/BBDD_LE_Tableau.csv")
BBDD_LE$FECHA_NAC <- as.Date(BBDD_LE$FECHA_NAC)
BBDD_LE$F_ENTRADA <- as.Date(BBDD_LE$F_ENTRADA)
BBDD_LE$fechacorte <- as.character(BBDD_LE$fechacorte)
BBDD_LE <- BBDD_LE %>% select(-X)


ABIERTOS <- read_excel(archivo, sheet = "Sigte") %>% filter(ESTAB_DEST==109101) %>% mutate(ESTAB_ORIG = as.character(ESTAB_ORIG ))
PRESTACIONES <- read_excel(archivo, sheet = "Prest") %>% mutate(PRESTA_MIN =propuesta_codigo) %>% select(PRESTA_MIN, glosa_grupo, glosa)
ESTABLECIMIENTOS <- read_excel(archivo, sheet = "estab") %>% mutate(ESTAB_ORIG = as.character(cod_deis)) %>% select(ESTAB_ORIG, cod_comuna, nombre_estab, tipoEstablecimiento, nom_comuna)

SENAME <- read_excel(archivo, sheet = "BD_SENAME") %>% select(RUN)
BLOQUEADOS <- read_excel(archivo, sheet = "Bloqueados") %>% select(RUN, SIGTE_ID, DETALLE)
POSTERGADOS <- read_excel(archivo,sheet = "Postergados", skip = 1) %>% select(RUN...2, SIGTE_ID, "Por Motivo de:")

cod_odonto <- c("09-003","09-011","09-010","09-002","09-014","09-007","09-009","09-005","09-001","09-030")
ABIERTOS$fechacorte <- as.Date(fechacorte)

ABIERTOS <- ABIERTOS %>% select(-glosa_tipo_espera,-grupo_glosa, -detalle_glosa, -dias_espera, -Glosa_Origen, -comuna_origen, -Glosa_destino, -comuna_destino, -PLANO, -EXTREMIDAD, -ID_LOCAL, -ss_destino, -nivel_atencion, -SENAME, -Bloqueados)

ABIERTOS <- ABIERTOS %>%
  mutate(PRESTA_MIN = str_replace(ABIERTOS$PRESTA_MIN," ", ""),
         SENAME = ifelse(ABIERTOS$RUN %in% SENAME$RUN, "Si", "No"),
         Bloqueados = ifelse(ABIERTOS$SIGTE_ID %in% BLOQUEADOS$SIGTE_ID, "Si",
                             ifelse(ABIERTOS$SIGTE_ID %in% POSTERGADOS$SIGTE_ID, "Si", "No")),
         Detalle_lista = ifelse(ABIERTOS$TIPO_PREST == 1 & ABIERTOS$PRESTA_MIN %in% cod_odonto, "Odontologica",
                                ifelse(ABIERTOS$TIPO_PREST == 1 & ABIERTOS$PRESTA_MIN == "09-008", "Ortodoncia",
                                       ifelse(ABIERTOS$TIPO_PREST == 1, "Especialidades", " "))),
         Tipo_lista = case_when(
           TIPO_PREST == 1 ~ "Consulta Nueva",
           TIPO_PREST == 2 ~ "Control",
           TIPO_PREST == 3 ~ "Procedimientos",
           TIPO_PREST == 4 ~ "Quirurgica",
           TRUE ~ "No identificada"),
         DiasEspera = difftime(fechacorte, F_ENTRADA, units = "days"))

ABIERTOS <- left_join(x=ABIERTOS, y=PRESTACIONES, by = "PRESTA_MIN")
ABIERTOS <-  left_join(x=ABIERTOS, y=ESTABLECIMIENTOS, by = "ESTAB_ORIG")


ABIERTOS <- ABIERTOS %>% mutate(glosa_grupo = toupper(ABIERTOS$glosa_grupo))
ABIERTOS <- ABIERTOS %>% mutate(glosa_grupo = case_when(
                                  glosa_grupo == "OFTALMOLÓGICA"  ~ "OFTALMOLÓGICA",
                                  glosa_grupo == "OFTALMOLOGÍA"  ~ "OFTALMOLÓGICA",
                                  glosa_grupo == "DERMATOLOGÍA Y TEGUMENTOS"  ~ "DERMATOLOGÍA Y TEGUMENTOS",
                                  glosa_grupo == "NUTRIÓLOGO"  ~ "NUTRIÓLOGO",
                                  glosa_grupo == "OTORRINOLARINGOLOGÍA"  ~ "OTORRINOLARINGOLÓGICA",
                                  glosa_grupo == "OTORRINOLARINGOLÓGICA"  ~ "OTORRINOLARINGOLÓGICA",
                                  glosa_grupo == "GENÉTICA CLÍNICA"  ~ "GENÉTICA CLÍNICA",
                                  glosa_grupo == "PLÁSTICA Y REPARADORA"  ~ "PLÁSTICA Y REPARADORA",
                                  glosa_grupo == "CIRUGÍA PLASTICA Y REPARADORA PEDIÁTRICA"  ~ "PLÁSTICA Y REPARADORA",
                                  glosa_grupo == "CIRUGÍA PLÁSTICA Y REPARADORA"  ~ "PLÁSTICA Y REPARADORA",
                                  glosa_grupo == "TRAUMATOLOGÍA"  ~ "TRAUMATOLOGÍA",
                                  glosa_grupo == "TRAUMATOLOGÍA Y ORTOPEDIA"  ~ "TRAUMATOLOGÍA",
                                  glosa_grupo == "ANESTESIOLOGÍA"  ~ "ANESTESIOLOGÍA",
                                  glosa_grupo == "ORTODONCIA"  ~ "ORTODONCIA",
                                  glosa_grupo == "SALUD OCUPACIONAL"  ~ "SALUD OCUPACIONAL",
                                  glosa_grupo == "CIRUGÍA PEDIÁTRICA"  ~ "CIRUGÍA PEDIÁTRICA",
                                  glosa_grupo == "CIRUGÍA CARDIOVASCULAR"  ~ "CIRUGÍA CARDIOVASCULAR",
                                  glosa_grupo == "DERMATOLOGÍA"  ~ "DERMATOLOGÍA",
                                  glosa_grupo == "CIRUGÍA BUCAL"  ~ "CIRUGÍA BUCAL",
                                  glosa_grupo == "CARDIOLOGÍA"  ~ "CARDIOLOGÍA",
                                  glosa_grupo == "NEUROLOGÍA"  ~ "NEUROLOGÍA",
                                  glosa_grupo == "UROLOGÍA Y NEFROLOGÍA"  ~ "UROLOGÍA",
                                  glosa_grupo == "UROLOGÍA"  ~ "UROLOGÍA",
                                  glosa_grupo == "NEUROCIRUGÍA"  ~ "NEUROCIRUGÍA",
                                  glosa_grupo == "GASTROENTEROLOGÍA"  ~ "GASTROENTEROLOGÍA",
                                  glosa_grupo == "CIRUGÍA DE CABEZA, CUELLO Y MAXILOFACIAL"  ~ "CIRUGÍA DE CABEZA, CUELLO Y MAXILOFACIAL",
                                  glosa_grupo == "CIRUGÍA Y TRAUMATOLOGÍA MÁXILOFACIAL"  ~ "CIRUGÍA DE CABEZA, CUELLO Y MAXILOFACIAL",
                                  glosa_grupo == "CIRUGÍA DE CABEZA Y CUELLO"  ~ "CIRUGÍA DE CABEZA, CUELLO Y MAXILOFACIAL",
                                  glosa_grupo == "ONCOLOGÍA MÉDICA"   ~ "HEMATO-ONCOLOGÍA" ,
                                  glosa_grupo == "HEMATO-ONCOLOGÍA"  ~ "HEMATO-ONCOLOGÍA",
                                  glosa_grupo == "HEMATOLOGÍA"  ~ "HEMATO-ONCOLOGÍA",
                                  glosa_grupo == "ENFERMEDADES RESPIRATORIAS"  ~ "ENFERMEDADES RESPIRATORIAS",
                                  glosa_grupo == "NEFROLOGÍA"  ~ "NEFROLOGÍA",
                                  glosa_grupo == "GINECOLOGÍA"  ~ "GINECOLOGÍA",
                                  glosa_grupo == "GINECOLOGÍA Y OBSTETRICIA"  ~ "GINECOLOGÍA",
                                  glosa_grupo == "ENDOCRINOLOGÍA"  ~ "ENDOCRINOLOGÍA",
                                  glosa_grupo == "MEDICINA FÍSICA Y REHABILITACIÓN"  ~ "MEDICINA FÍSICA Y REHABILITACIÓN",
                                  glosa_grupo == "CIRUGÍA DIGESTIVA"  ~ "CIRUGÍA DIGESTIVA",
                                  glosa_grupo == "PSIQUIATRÍA"  ~ "PSIQUIATRÍA",
                                  glosa_grupo == "REUMATOLOGÍA"  ~ "REUMATOLOGÍA",
                                  glosa_grupo == "ODONTOPEDIATRÍA"  ~ "ODONTOPEDIATRÍA",
                                  glosa_grupo == "INFECTOLOGÍA"  ~ "INFECTOLOGÍA",
                                  glosa_grupo == "PEDIATRÍA"  ~ "PEDIATRÍA",
                                  glosa_grupo == "NUTRIÓLOGO"  ~ "NUTRIÓLOGO", 
                                  glosa_grupo == "ODONTOLOGÍA"  ~ "ODONTOLOGÍA",
                                  glosa_grupo == "INMUNOLOGÍA"  ~ "INMUNOLOGÍA",
                                  glosa_grupo == "DIABETOLOGÍA"  ~ "DIABETOLOGÍA",
                                  glosa_grupo == "INTERVENCIONES UROLOGÍA Y NEFROLOGÍA"  ~ "UROLOGÍA",
                                  glosa_grupo == "INTERVENCIONES TRAUMATOLÓGICAS"  ~ "TRAUMATOLOGÍA",
                                  glosa_grupo == "Intervenciones Oftalmológicas"  ~ "OFTALMOLÓGICA",
                                  glosa_grupo == "CIRUGÍA GENERAL"  ~ "CIRUGÍA PEDIÁTRICA",
                                  glosa_grupo == "Intervenciones ORL"  ~ "OTORRINOLARINGOLÓGICA",
                                  
                                  glosa_grupo == "Intervenciones Quirúrgicas de Cabeza y Cuello"  ~ "CIRUGÍA DE CABEZA, CUELLO Y MAXILOFACIAL",
                                  glosa_grupo == "Intervenciones Dermatológicas"  ~ "DERMATOLOGÍA Y TEGUMENTOS",
                                  
                              
                                  TRUE ~ glosa_grupo), "Pendiente_de_atencion"= ifelse(ABIERTOS$SIGTE_ID %in% errores_rut_provisorio$SIGTE_ID | ABIERTOS$SIGTE_ID %in% errores_carga$SIGTE_ID, "Por eliminar de la LE", "Sigue en LE"))
         

LE_Abiertos <-  ABIERTOS %>% select(-RUN, -DV, -NOMBRES, -PRIMER_APELLIDO, -SEGUNDO_APELLIDO, -glosa, -PRESTA_EST, -SOSPECHA_DIAG, -CONFIR_DIAG)
LE_Abiertos$fechacorte <- as.character(LE_Abiertos$fechacorte)

LE_Abiertos <- rbind(BBDD_LE, LE_Abiertos)
LE_Abiertos$fechacorte  <- as.Date(LE_Abiertos$fechacorte)

No_identificada_especialidad <- ABIERTOS %>% filter(glosa_grupo == "No identificada")

write.csv(ABIERTOS, "C:/Users/control.gestion3/OneDrive/BBDD Produccion/Listas de Espera/Listas de Espera DATA DEIS/BBDD_Completa_LE.csv")
write.csv(LE_Abiertos, "C:/Users/control.gestion3/OneDrive/BBDD Produccion/Listas de Espera/Listas de Espera DATA DEIS/BBDD_LE_Tableau.csv")

rm(BBDD_LE, BLOQUEADOS, errores_rut_provisorio, errores_carga, ESTABLECIMIENTOS, LE_Abiertos, POSTERGADOS, PRESTACIONES, SENAME, archivo, cod_odonto, fechacorte, anio, mes, dia, No_identificada_especialidad)

# BBDD_LE <- read.csv("C:/Users/control.gestion3/OneDrive/BBDD Produccion/Listas de Espera/Listas de Espera DATA DEIS/BBDD_LE_Tableau.csv")
# BBDD_LE <- BBDD_LE %>% filter(fechacorte != "2022-01-31") #me sirve para borrar datos de la BBDD
# write.csv(BBDD_LE, "C:/Users/control.gestion3/OneDrive/BBDD Produccion/Listas de Espera/Listas de Espera DATA DEIS/BBDD_LE_Tableau.csv")
