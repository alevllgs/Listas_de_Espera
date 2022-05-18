library(readxl)
library(tidyverse)
library(dplyr)
library(kableExtra)

Fecha_corte <- "2021-08-31"
Fecha_COMGES <- "2019-03-31"

# ABIERTOS <- read_excel("BBDD Produccion/Listas de Espera/Listas de Espera DATA DEIS/ABIERTOS_20210831.xlsx", 
#                                 sheet = "Sigte", col_types = c("numeric", 
#                                                                "text", "text", "text", "text", "date", 
#                                                                "numeric", "text", "text", "numeric", 
#                                                                "text", "date", "text", "text", "text", 
#                                                                "text", "text", "numeric", "text", 
#                                                                "text", "text", "numeric", "text", 
#                                                                "text", "text", "text", "numeric", 
#                                                                "text", "text", "text"))
# 
# 
# 
# 
# 
# #BBDD de todas las LE----
# ABIERTOS <- ABIERTOS %>% 
#   filter(ESTAB_DEST == "109101",
#          Bloqueados == "N") %>% 
#   select(-RUN, -DV, -NOMBRES, -PRIMER_APELLIDO, -SEGUNDO_APELLIDO, -PLANO, 
#          - EXTREMIDAD, - ss_destino) %>% 
#   mutate("Fecha_corte" = as.Date(Fecha_corte),
#          "Fecha_COMGES" = as.Date(Fecha_COMGES),
#          grupo_glosa = toupper(grupo_glosa)) %>% 
#   mutate("Especialidad" = case_when(
#     grupo_glosa == "OFTALMOLÓGICA" ~ "OFTALMOLOGÍA",
#     grupo_glosa == "OFTALMOLOGÍA" ~ "OFTALMOLOGÍA",
#     
#     grupo_glosa == "NUTRIÓLOGO" ~ "NUTRIÓLOGO PEDIÁTRICO",
#     grupo_glosa == "GENÉTICA CLÍNICA" ~ "GENÉTICA CLÍNICA",
#     grupo_glosa == "NEUROCIRUGÍA" ~ "NEUROCIRUGÍA",
#     grupo_glosa == "PEDIATRÍA" ~ "PEDIATRÍA",
#     grupo_glosa == "GASTROENTEROLOGÍA" ~ "GASTROENTEROLOGÍA PEDIÁTRICA",
#     
#     grupo_glosa == "PLÁSTICA Y REPARADORA" ~ "CIRUGÍA PLÁSTICA Y REPARADORA PEDIÁTRICA",
#     grupo_glosa == "CIRUGÍA PLÁSTICA Y REPARADORA" ~ "CIRUGÍA PLÁSTICA Y REPARADORA PEDIÁTRICA",
#     grupo_glosa == "CIRUGÍA PLASTICA Y REPARADORA PEDIÁTRICA" ~ "CIRUGÍA PLÁSTICA Y REPARADORA PEDIÁTRICA",
#     
#     grupo_glosa == "ANESTESIOLOGÍA" ~ "ANESTESIOLOGÍA",
# 
#     grupo_glosa == "ENFERMEDADES RESPIRATORIAS" ~ "ENFERMEDAD RESPIRATORIA PEDIÁTRICA (BRONCOPULMONAR INFANTIL)",
#     grupo_glosa == "INFECTOLOGÍA" ~ "INFECTOLOGÍA PEDIÁTRICA",
#     grupo_glosa == "NEFROLOGÍA" ~ "NEFROLOGÍA PEDIÁTRICA",
#     grupo_glosa == "PSIQUIATRÍA" ~ "PSIQUIATRÍA PEDIÁTRICA Y DE LA ADOLESCENCIA",
#     grupo_glosa == "ENDOCRINOLOGÍA" ~ "ENDOCRINOLOGÍA PEDIÁTRICA",
#     grupo_glosa == "MEDICINA FÍSICA Y REHABILITACIÓN" ~ "MEDICINA FÍSICA Y REHABILITACIÓN PEDIÁTRICA (FISIATRÍA PEDIÁTRICA)",
#     grupo_glosa == "NEUROLOGÍA" ~ "NEUROLOGÍA PEDIÁTRICA",
#     grupo_glosa == "REUMATOLOGÍA" ~ "REUMATOLOGÍA PEDIÁTRICA",
#     
#     grupo_glosa == "TRAUMATOLOGÍA Y ORTOPEDIA" ~ "TRAUMATOLOGÍA Y ORTOPEDIA PEDIÁTRICA",
#     grupo_glosa == "TRAUMATOLOGÍA" ~ "TRAUMATOLOGÍA Y ORTOPEDIA PEDIÁTRICA",
#     
#     grupo_glosa == "GINECOLOGÍA Y OBSTETRICIA" ~ "GINECOLOGÍA PEDIÁTRICA Y DE LA ADOLESCENCIA",
#     grupo_glosa == "GINECOLOGÍA" ~ "GINECOLOGÍA PEDIÁTRICA Y DE LA ADOLESCENCIA",
#     
#     grupo_glosa == "UROLOGÍA Y NEFROLOGÍA" ~ "UROLOGÍA PEDIÁTRICA",
#     grupo_glosa == "UROLOGÍA" ~ "UROLOGÍA PEDIÁTRICA",
#     
#     grupo_glosa == "OTORRINOLARINGOLOGÍA" ~ "OTORRINOLARINGOLOGÍA",
#     grupo_glosa == "OTORRINOLARINGOLÓGICA" ~ "OTORRINOLARINGOLOGÍA",
#     
#     grupo_glosa == "ODONTOPEDIATRÍA" ~ "ODONTOPEDIATRÍA",
#     grupo_glosa == "ORTODONCIA" ~ "ORTODONCIA",
#     grupo_glosa == "ODONTOLOGÍA" ~ "ODONTOLOGÍA",
#     
#     grupo_glosa == "CARDIOLOGÍA" ~ "CARDIOLOGÍA PEDIÁTRICA",
#     grupo_glosa == "CIRUGÍA CARDIOVASCULAR" ~ "CARDIOLOGÍA PEDIÁTRICA",
#     
#     grupo_glosa == "ONCOLOGÍA MÉDICA" ~ "HEMATO-ONCOLOGÍA INFANTIL",
#     grupo_glosa == "HEMATO-ONCOLOGÍA" ~ "HEMATO-ONCOLOGÍA INFANTIL",
#     grupo_glosa == "HEMATOLOGÍA" ~ "HEMATO-ONCOLOGÍA INFANTIL",
#     
#     grupo_glosa == "DERMATOLOGÍA Y TEGUMENTOS" ~ "DERMATOLOGÍA",
#     grupo_glosa == "DERMATOLOGÍA" ~ "DERMATOLOGÍA",
#     
#     
#     grupo_glosa == "CIRUGÍA DIGESTIVA" ~ "CIRUGÍA PEDIÁTRICA",
#     grupo_glosa == "CIRUGÍA PEDIÁTRICA" ~ "CIRUGÍA PEDIÁTRICA",
#     
#     grupo_glosa == "CIRUGÍA DE CABEZA, CUELLO Y MAXILOFACIAL" ~ "CIRUGÍA DE CABEZA, CUELLO Y MAXILOFACIAL",
#     grupo_glosa == "CIRUGÍA DE CABEZA Y CUELLO" ~ "CIRUGÍA DE CABEZA, CUELLO Y MAXILOFACIAL",
#     grupo_glosa == "CIRUGÍA Y TRAUMATOLOGÍA MÁXILOFACIAL" ~ "CIRUGÍA DE CABEZA, CUELLO Y MAXILOFACIAL",
#     TRUE ~ tolower(grupo_glosa)))

# BBDD todos los Egresos----
EGRESOS <- read_excel("BBDD Produccion/Listas de Espera/Listas de Espera DATA DEIS/EGRESOS.xlsx",
                      sheet = "BD")

EGRESOS <- EGRESOS %>% 
  filter(ESTAB_DEST == "109101") %>% 
  select(-RUN, -DV, -NOMBRES, -PRIMER_APELLIDO, -SEGUNDO_APELLIDO, -PLANO, 
         - EXTREMIDAD, - ss_destino) %>% 
  mutate("Fecha_corte" = as.Date(Fecha_corte),
         "Fecha_COMGES" = as.Date(Fecha_COMGES),
         "dias_espera" = as.numeric(difftime(F_SALIDA, F_ENTRADA, units = "days")),
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
      C_SALIDA == "3" ~ "Indicación Médica para reevaluación",
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
      TRUE ~ "No decodificado, ubicar codigo")))
    
#Egresos LE Ambulatoria----
egresos_ambulatoria <- EGRESOS %>% filter(glosa_tipo_espera == "1.-MEDICA")

tabla_tipo_egresos <- egresos_ambulatoria %>% count(Causa_Salida) %>% arrange(-n)
tabla_tipo_egresos$proporcion <- round(tabla_tipo_egresos$n/sum(tabla_tipo_egresos$n)*100,1)

T <- tabla_tipo_egresos %>% count(Causa_Salida) %>% 
  summarise(n=sum(n))

tabla_tipo_egresos %>%
  kbl(caption = "Egresos según causa de salida") %>%
  kable_classic(lightable_options = "hover") %>% 
  column_spec(2, color = "white",
              background = spec_color(tabla_tipo_egresos$n[1:T$n], end = 0.7),
              popover = paste("n", tabla_tipo_egresos$n[1:T$n]))

#Egresos Atención realizada LE Ambulatoria----
egresos_amb_at_ralizada <- EGRESOS %>% 
  filter(glosa_tipo_espera == "1.-MEDICA" & Causa_Salida == "ATENCION REALIZADA")

tabla_egreso_at_realizada <- egresos_amb_at_ralizada %>% count(Especialidad) %>% arrange(-n)
tabla_egreso_at_realizada$proporcion <- round(tabla_egreso_at_realizada$n/sum(tabla_egreso_at_realizada$n)*100,1)

T <- tabla_egreso_at_realizada %>% count(Especialidad) %>% 
  summarise(n=sum(n))
tabla_egreso_at_realizada %>%
  kbl(caption = "Egresos según especialidad") %>%
  kable_classic(lightable_options = "hover") %>% 
  column_spec(2, color = "white",
              background = spec_color(tabla_egreso_at_realizada$n[1:T$n], end = 0.7),
              popover = paste("n", tabla_egreso_at_realizada$n[1:T$n]))

tabla_egreso_dias_espera  <- egresos_ambulatoria %>% select(Especialidad, dias_espera) %>% group_by(Especialidad)

tabla_egreso_dias_espera <- egresos_ambulatoria %>% filter(Causa_Salida=="ATENCION REALIZADA") %>% 
  group_by(Especialidad) %>% 
  summarise(dias_espera = round(mean(dias_espera),0)) %>%
  ungroup()

# 
# 
# #Lista de Espera Ambulatoria ----
# 
# ambulatoria <- ABIERTOS %>% filter(glosa_tipo_espera == "1.-MEDICA")
# 
# 
# cant_casos <- ambulatoria %>% count(Especialidad)
# 
# 
# ggplot(data=cant_casos, aes(y=reorder(Especialidad,n), x=n)) + 
#   geom_bar(stat="identity", position="dodge")
# 
# 
# 
# ambulatoria$COMGES <- ifelse(ambulatoria$F_ENTRADA <= ambulatoria$Fecha_COMGES, "COMGES", "No COMGES")
#         
# 
# # ambulatoria <- ABIERTOS %>% filter(glosa_tipo_espera == "1.-MEDICA")
# # 
# # odontologica <- ABIERTOS %>% filter(glosa_tipo_espera == "1.-ODONTOLOGICA" |
#                                    glosa_tipo_espera == "1.-ORTODONCIA")
# 
# quirurgica <- ABIERTOS %>% filter(glosa_tipo_espera == "4.-INTERVENCION QUIRURGICA")

# 
# #ambulatoria Estadisticos del boxplot----
# b <- ggplot(ambulatoria) +
#   aes(x = ESTAB_DEST, y = dias_espera) +
#   geom_boxplot(shape = "circle", fill = "#06F4C4") +
#   theme_minimal()
# 
# 
# estadisticos <- boxplot(ambulatoria$dias_espera)
# 
# estadisticos$stats[2,1]
# 
# Summary <- max(summary(ambulatoria$dias_espera))
# 
# unique(ambulatoria$Bloqueados)
# 
# #ambulatoria grafico boxplot dias de espera----
# ggplot(ambulatoria) + 
#   aes(x = ESTAB_DEST, y = dias_espera) +
#   geom_boxplot(shape = "circle", fill = "#06F4C4") +
#   theme_minimal()
# 
# #ambulatoria Grafico de cantidad por especialidad----
# ggplot(data = cant_casos, aes(y = reorder(Especialidad, n), x = n)) +
#   geom_bar(stat="identity", fill="steelblue", color = "black") +
#   theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=5)) +
#   theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1, size=7)) +
#   labs(title=NULL,x = NULL, y = NULL) +
#   geom_text(aes(label = n), vjust=0.5, hjust=0, color="black", position = position_dodge(0), size=2)
# 
# 
# 
# 
# #ambulatoria DF dias de espera
# dias_espera <- ambulatoria %>% select(Especialidad, dias_espera) %>% group_by(Especialidad)
# 
# dias_espera <- ambulatoria %>% 
#   group_by(Especialidad) %>% 
#   summarise(dias_espera = round(mean(dias_espera),0)) %>%
#   ungroup()
# 
# 
# 
# 
# Tabla <- inner_join(dias_espera, tabla_egreso_dias_espera,  by = "Especialidad") %>% 
#   mutate(dias_espera = dias_espera.x,
#          espera_egresos= dias_espera.y,
#          factor= dias_espera.x/dias_espera.y) %>% 
#   select(-dias_espera.x, -dias_espera.y)
# 
# Tabla <-  inner_join(cant_casos, Tabla,  by = "Especialidad") %>% 
#   mutate(Cantidad = n) %>% 
#   select(Especialidad, Cantidad, dias_espera, espera_egresos, factor)

openxlsx::write.xlsx(Tabla,"BBDD Produccion/Listas de Espera/Listas de Espera DATA DEIS/Tabla.xlsx")




