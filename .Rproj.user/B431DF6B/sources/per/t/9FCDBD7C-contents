library(readxl)
library(tidyverse)
library(dplyr)
library(kableExtra)

EGRESOS <- read_excel("C:/Users/control.gestion3/OneDrive/BBDD Produccion/Listas de Espera/Listas de Espera DATA DEIS/BBDD LE DataDEIS/EGRESOS_2021.xlsx", 
                           sheet = "BD")

EGRESOS <- EGRESOS %>% 
  filter(ESTAB_DEST == "109101") %>% 
  select(-RUN, -DV, -NOMBRES, -PRIMER_APELLIDO, -SEGUNDO_APELLIDO, -PLANO, 
         - EXTREMIDAD, - ss_destino) %>% 
  mutate("dias_espera" = as.numeric(difftime(F_SALIDA, F_ENTRADA, units = "days")),
         grupo_rest = toupper(grupo_rest)) %>% 
  mutate("Especialidad" = case_when(
    grupo_rest == "OFTALMOLÓGICA" ~ "OFTALMOLOGÍA",
    grupo_rest == "OFTALMOLOGÍA" ~ "OFTALMOLOGÍA",
    
    grupo_rest == "NUTRIÓLOGO" ~ "NUTRIÓLOGO PEDIÁTRICO",
    grupo_rest == "GENÉTICA CLÍNICA" ~ "GENÉTICA CLÍNICA",
    grupo_rest == "NEUROCIRUGÍA" ~ "NEUROCIRUGÍA",
    grupo_rest == "PEDIATRÍA" ~ "PEDIATRÍA",
    grupo_rest == "GASTROENTEROLOGÍA" ~ "GASTROENTEROLOGÍA PEDIÁTRICA",
    
    grupo_rest == "PLÁSTICA Y REPARADORA" ~ "CIRUGÍA PLÁSTICA Y REPARADORA PEDIÁTRICA",
    grupo_rest == "CIRUGÍA PLÁSTICA Y REPARADORA" ~ "CIRUGÍA PLÁSTICA Y REPARADORA PEDIÁTRICA",
    grupo_rest == "CIRUGÍA PLASTICA Y REPARADORA PEDIÁTRICA" ~ "CIRUGÍA PLÁSTICA Y REPARADORA PEDIÁTRICA",
    
    grupo_rest == "ANESTESIOLOGÍA" ~ "ANESTESIOLOGÍA",
    
    grupo_rest == "ENFERMEDADES RESPIRATORIAS" ~ "ENFERMEDAD RESPIRATORIA PEDIÁTRICA (BRONCOPULMONAR INFANTIL)",
    grupo_rest == "INFECTOLOGÍA" ~ "INFECTOLOGÍA PEDIÁTRICA",
    grupo_rest == "NEFROLOGÍA" ~ "NEFROLOGÍA PEDIÁTRICA",
    grupo_rest == "PSIQUIATRÍA" ~ "PSIQUIATRÍA PEDIÁTRICA Y DE LA ADOLESCENCIA",
    grupo_rest == "ENDOCRINOLOGÍA" ~ "ENDOCRINOLOGÍA PEDIÁTRICA",
    grupo_rest == "MEDICINA FÍSICA Y REHABILITACIÓN" ~ "MEDICINA FÍSICA Y REHABILITACIÓN PEDIÁTRICA (FISIATRÍA PEDIÁTRICA)",
    grupo_rest == "NEUROLOGÍA" ~ "NEUROLOGÍA PEDIÁTRICA",
    grupo_rest == "REUMATOLOGÍA" ~ "REUMATOLOGÍA PEDIÁTRICA",
    
    grupo_rest == "TRAUMATOLOGÍA Y ORTOPEDIA" ~ "TRAUMATOLOGÍA Y ORTOPEDIA PEDIÁTRICA",
    grupo_rest == "TRAUMATOLOGÍA" ~ "TRAUMATOLOGÍA Y ORTOPEDIA PEDIÁTRICA",
    
    grupo_rest == "GINECOLOGÍA Y OBSTETRICIA" ~ "GINECOLOGÍA PEDIÁTRICA Y DE LA ADOLESCENCIA",
    grupo_rest == "GINECOLOGÍA" ~ "GINECOLOGÍA PEDIÁTRICA Y DE LA ADOLESCENCIA",
    
    grupo_rest == "UROLOGÍA Y NEFROLOGÍA" ~ "UROLOGÍA PEDIÁTRICA",
    grupo_rest == "UROLOGÍA" ~ "UROLOGÍA PEDIÁTRICA",
    
    grupo_rest == "OTORRINOLARINGOLOGÍA" ~ "OTORRINOLARINGOLOGÍA",
    grupo_rest == "OTORRINOLARINGOLÓGICA" ~ "OTORRINOLARINGOLOGÍA",
    
    grupo_rest == "ODONTOPEDIATRÍA" ~ "ODONTOPEDIATRÍA",
    grupo_rest == "ORTODONCIA" ~ "ORTODONCIA",
    grupo_rest == "ODONTOLOGÍA" ~ "ODONTOLOGÍA",
    
    grupo_rest == "CARDIOLOGÍA" ~ "CARDIOLOGÍA PEDIÁTRICA",
    grupo_rest == "CIRUGÍA CARDIOVASCULAR" ~ "CARDIOLOGÍA PEDIÁTRICA",
    
    grupo_rest == "ONCOLOGÍA MÉDICA" ~ "HEMATO-ONCOLOGÍA INFANTIL",
    grupo_rest == "HEMATO-ONCOLOGÍA" ~ "HEMATO-ONCOLOGÍA INFANTIL",
    grupo_rest == "HEMATOLOGÍA" ~ "HEMATO-ONCOLOGÍA INFANTIL",
    
    grupo_rest == "DERMATOLOGÍA Y TEGUMENTOS" ~ "DERMATOLOGÍA",
    grupo_rest == "DERMATOLOGÍA" ~ "DERMATOLOGÍA",
    
    
    grupo_rest == "CIRUGÍA DIGESTIVA" ~ "CIRUGÍA PEDIÁTRICA",
    grupo_rest == "CIRUGÍA PEDIÁTRICA" ~ "CIRUGÍA PEDIÁTRICA",
    
    grupo_rest == "CIRUGÍA DE CABEZA, CUELLO Y MAXILOFACIAL" ~ "CIRUGÍA DE CABEZA, CUELLO Y MAXILOFACIAL",
    grupo_rest == "CIRUGÍA DE CABEZA Y CUELLO" ~ "CIRUGÍA DE CABEZA, CUELLO Y MAXILOFACIAL",
    grupo_rest == "CIRUGÍA Y TRAUMATOLOGÍA MÁXILOFACIAL" ~ "CIRUGÍA DE CABEZA, CUELLO Y MAXILOFACIAL",
    TRUE ~ tolower(grupo_rest)), "Causa_Salida" = toupper(case_when(
      C_SALIDA == "0" ~ "GES",
      C_SALIDA == "1" ~ "Atencion realizada",
      C_SALIDA == "2" ~ "Procedimiento informado",
      C_SALIDA == "4" ~ "Atención otorgada extrasistema",
      C_SALIDA == "5" ~ "Cambio asegurador",
      C_SALIDA == "6" ~ "Renuncia o rechazo voluntario",
      C_SALIDA == "7" ~ "Recuperación espontanea",
      C_SALIDA == "8" ~ "Inasistencia",
      C_SALIDA == "9" ~ "Fallecimiento",
      C_SALIDA == "10" ~ "Solicitud duplicada",
      C_SALIDA == "11" ~ "Contacto no corresponde",
      C_SALIDA == "12" ~ "no corresponde realizar cirugía",
      C_SALIDA == "13" ~ "Traslado coordinado",
      C_SALIDA == "14" ~ "No pertinencia",
      C_SALIDA == "15" ~ "Error de digitación",
      C_SALIDA == "16" ~ "Atencion por resolutividad",
      C_SALIDA == "17" ~ "Atencion por telemedicina",
      C_SALIDA == "18" ~ "Modificacion de la condicion clinico-diagnostica",
      C_SALIDA == "19" ~ "Atención por Hospital Digital",
      C_SALIDA == "99" ~ "Técnico administrativo (Nivel Central)",
      TRUE ~ "No decodificado, ubicar codigo"))) %>% select(-C_SALIDA,-ESTAB_DEST, -glosa_destino, -comuna_destino, -nivel_atencion)

write.csv(EGRESOS, "C:/Users/control.gestion3/OneDrive/BBDD Produccion/Listas de Espera/Listas de Espera DATA DEIS/EGRESOS.csv")

