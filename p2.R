options(scipen=9999999)
if (!require('pacman')){install.packages('pacman')
}

pacman::p_load(readr,dplyr,ggplot2,stringr,sf,RColorBrewer)
install.packages("spdep")
install.packages("readxl")
install.packages("tidyr")
install.packages("gridExtra")
library(gridExtra)
library(tidyr)
library(readxl)
library(spdep)
Sys.setenv(OGR_GEOMETRY_ACCEPT_UNCLOSED_RING = "NO")

# ARMADO NACIONAL
base_dir <- "C:/Users/carlo/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Dataton source pro max 3001/carlitos/edos"

carpetas <- list.dirs(base_dir, recursive = FALSE)

lista_shapefiles <- lapply(carpetas, function(carpeta) {
  
  shapefile_path <- file.path(carpeta, "MUNICIPIO.shp")
  
  shp <- st_read(shapefile_path)
  
  shp_transformed <- st_transform(shp, crs = 32614)
  
  return(shp_transformed)
})

shapefiles_nacional <- do.call(rbind, lista_shapefiles)
shapefiles_nacional <- as.data.frame(shapefiles_nacional)
shapefiles_nacional <- shapefiles_nacional[order(shapefiles_nacional$ENTIDAD,shapefiles_nacional$MUNICIPIO),]

#CARGAR LAS BASES DE DATOS Y DEPURAR LA DATA
censo <- read.csv('C:/Users/carlo/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Dataton source pro max 3001/carlitos/data/conjunto_de_datos_iter_00CSV20.csv') 
censo <- as.data.frame(censo)
censo_municipio <- censo[censo$LOC==0,]
censo_municipio <- censo_municipio[censo_municipio$MUN!=0,]
censo_municipio <- censo_municipio %>% select(ENTIDAD,MUN,POBTOT,P_60YMAS,PCON_DISC,PEA,POCUPADA,PDER_SS,POB0_14,PCON_LIMI,GRAPROES,PSINDER,TOTHOG)
censo_municipio$TASA_DEPENDENCIA <- as.numeric(censo_municipio$P_60YMAS)/as.numeric(censo_municipio$PEA)
censo_municipio$TASA_NO_ASEGURADOS <- (as.numeric(censo_municipio$POBTOT) - as.numeric(censo_municipio$PDER_SS))/as.numeric(censo_municipio$POBTOT)
censo_municipio$TASA_DEP_DISCYLIM <- (as.numeric(censo_municipio$PCON_DISC)+as.numeric(censo_municipio$PCON_LIMI))/as.numeric(censo_municipio$PEA)


concentrado_hogar <- read.csv('C:/Users/carlo/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Dataton source pro max 3001/carlitos/data/conjunto_de_datos_concentradohogar_enigh2022_ns.csv')
concentrado_hogar <- as.data.frame(concentrado_hogar)
concentrado_hogar$ubica_geo <- as.character(concentrado_hogar$ubica_geo)
concentrado_hogar$entidad <- ifelse(nchar(concentrado_hogar$ubica_geo) == 4, 
                           substr(concentrado_hogar$ubica_geo, 1, 1),  
                           substr(concentrado_hogar$ubica_geo, 1, 2)) 
concentrado_hogar$municipio <- ifelse(nchar(concentrado_hogar$ubica_geo) == 4, 
                             substr(concentrado_hogar$ubica_geo, 2, 4),
                             substr(concentrado_hogar$ubica_geo, 3, 5))
concentrado_hogar$entidad <- as.numeric(concentrado_hogar$entidad)
concentrado_hogar$municipio <- as.numeric(concentrado_hogar$municipio)
gasto_ing_municipio <- concentrado_hogar %>% group_by(entidad,municipio) %>%
  summarize(
    salud_total= sum(salud,na.rm = TRUE),
    ingreso_total=sum(ing_cor),
    media_ocupados=mean(ocupados)
  )

dnue <- read.csv('C:/Users/carlo/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Dataton source pro max 3001/carlitos/data/denue_inegi_46321-46531_.csv')|>
  mutate(nombre_act <- str_to_lower(nombre_act))|>
  filter(str_detect(nombre_act,'Farm'))
farmacias_sf <- dnue %>%
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>%  
  st_transform(crs = 32614)  `

#CALCULO INDICE RURALIDAD
iter_data <- read.csv("C:/Users/carlo/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Dataton source pro max 3001/raw/ITER_NALCSV20.csv")
tr_data <- read.csv("C:/Users/carlo/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Dataton source pro max 3001/raw/TR_LOCALIDAD_00.csv")

iter_vars <- iter_data %>%
  select(ENTIDAD, MUN, LOC, POBTOT, P3YM_HLI, VIVPAR_HAB,
         VPH_PISOTI, VPH_S_ELEC, VPH_AGUAFV, VPH_NODREN,
         VPH_NDEAED, VPH_SINTIC)

tr_vars <- tr_data %>%
  select(ENT, MUN, LOC, DIS_TRANS, DRENAJE, LIMCALLE,
         RECBASURA, ALUMBRADO, TELEFONO, HABLALI)

data <- left_join(iter_vars, tr_vars, by = c("ENTIDAD" = "ENT", "MUN", "LOC"))

rm(iter_data, tr_data, iter_vars, tr_vars)

dummies1 <- c("DIS_TRANS", "DRENAJE", "LIMCALLE",
              "ALUMBRADO", "TELEFONO", "HABLALI")

dummies2 <- c("RECBASURA")

data[dummies1] <- lapply(data[dummies1], function(x) {
  ifelse(x == 1, 1, 
         ifelse(x == 2 | x == 3, 0, 
                ifelse(x == 9 | is.na(x), NA, 0)))
})

data[dummies2] <- lapply(data[dummies2], function(x) {
  ifelse(x == 3, 1, 
         ifelse(x == 4, 0, 
                ifelse(x == 9 | is.na(x), NA, 0)))
})

numeric_vars <- c("POBTOT", "P3YM_HLI", "VIVPAR_HAB",
                  "VPH_PISOTI", "VPH_S_ELEC", "VPH_AGUAFV", 
                  "VPH_NODREN", "VPH_NDEAED", "VPH_SINTIC")

dummy_vars <- c(dummies1, dummies2)

data[numeric_vars] <- lapply(data[numeric_vars], as.numeric)
data[dummy_vars] <- lapply(data[dummy_vars], as.numeric)

data_mun <- data %>%
  group_by(ENTIDAD, MUN) %>%
  summarise(
    across(all_of(numeric_vars), sum, na.rm = TRUE),
    across(all_of(dummy_vars), ~ mean(.x, na.rm = TRUE))
  )

vars_positive <- c("POBTOT", "VIVPAR_HAB", "DIS_TRANS", "DRENAJE",
                   "LIMCALLE", "RECBASURA", "ALUMBRADO", "TELEFONO")

vars_negative <- c("P3YM_HLI", "VPH_PISOTI", "VPH_S_ELEC",
                   "VPH_AGUAFV", "VPH_NODREN", "VPH_NDEAED",
                   "VPH_SINTIC", "HABLALI")

vars_positive <- vars_positive[vars_positive %in% names(data_mun)]
vars_negative <- vars_negative[vars_negative %in% names(data_mun)]

data_mun[vars_positive] <- lapply(data_mun[vars_positive], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})

data_mun[vars_negative] <- lapply(data_mun[vars_negative], function(x) {
  1 - (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})

all_vars <- c(vars_positive, vars_negative)
num_vars <- length(all_vars)
peso <- 1 / num_vars

data_mun <- data_mun %>%
  rowwise() %>%
  mutate(Indice_Ruralidad = mean(c_across(all_of(all_vars)), na.rm = TRUE))

summary(data_mun$Indice_Ruralidad)

data_mun <- as.data.frame(data_mun)
data_mun <- data_mun %>% select(ENTIDAD,MUN,Indice_Ruralidad)
#data_mun <- data_mun %>%
  #arrange(desc(Indice_Ruralidad))
#write.csv(data_mun, "Indice_Ruralidad_Municipio.csv", row.names = FALSE)

#hist(data_mun$Indice_Ruralidad, breaks = 20,
     #main = "Distribución del Índice de Ruralidad por Municipio",
     #xlab = "Índice de Ruralidad", ylab = "Frecuencia")


#ARMADO DE LA BASE DE DATOS
base_datos <- shapefiles_nacional %>%
  left_join(censo_municipio, by = c('ENTIDAD' = 'ENTIDAD', 'MUNICIPIO' = 'MUN')) %>%
  left_join(gasto_ing_municipio, by = c('ENTIDAD' = 'entidad', 'MUNICIPIO' = 'municipio')) %>%
  left_join(data_mun, by = c('ENTIDAD'='ENTIDAD','MUNICIPIO'='MUN'))

base_datos$promedio_ingreso_hogares <- (as.numeric(base_datos$ingreso_total)/as.numeric(base_datos$PEA))*as.numeric(base_datos$media_ocupados) #en miles trimestrales
base_datos$gasto_salud_pc <- as.numeric(base_datos$salud_total)/as.numeric(base_datos$POBTOT) #EN MILES TRIMESTRALES
base_datos$ingreso_hogares <-as.numeric(base_datos$ingreso_total)/as.numeric(base_datos$TOTHOG)
base_datos$gasto_pob_vul <- as.numeric(base_datos$salud_total)/(as.numeric(base_datos$P_60YMAS)+as.numeric(base_datos$POB0_14)+as.numeric(base_datos$PCON_DISC)+as.numeric(base_datos$PCON_LIMI))
base_datos$GRADOES <- as.numeric(base_datos$GRAPROES)
base_datos$ingreso_total<- as.numeric(base_datos$ingreso_total)
base_datos$salud_total <- as.numeric(base_datos$salud_total)
base_datos$ingreso_pc <- base_datos$ingreso_total/as.numeric(base_datos$POCUPADA)
base_datos$Indice_Ruralidad <- as.numeric(base_datos$Indice_Ruralidad)
base_datos$POBTOT <- as.numeric(base_datos$POBTOT)

base_datos$salud_total[is.na(base_datos$salud_total)] <- mean(base_datos$salud_total, na.rm = TRUE)
base_datos$ingreso_total[is.na(base_datos$ingreso_total)] <- mean(base_datos$ingreso_total, na.rm = TRUE)
base_datos$TASA_NO_ASEGURADOS[is.na(base_datos$TASA_NO_ASEGURADOS)] <- mean(base_datos$TASA_NO_ASEGURADOS, na.rm = TRUE)
base_datos$promedio_ingreso_hogares[is.na(base_datos$promedio_ingreso_hogares)] <- median(base_datos$promedio_ingreso_hogares, na.rm = TRUE)
base_datos$TASA_DEPENDENCIA[is.na(base_datos$TASA_DEPENDENCIA)] <- mean(base_datos$TASA_DEPENDENCIA, na.rm = TRUE)
base_datos$gasto_salud_pc[is.na(base_datos$gasto_salud_pc)] <- median(base_datos$gasto_salud_pc, na.rm = TRUE)
base_datos$ingreso_hogares[is.na(base_datos$ingreso_hogares)] <- median(base_datos$ingreso_hogares, na.rm = TRUE)
base_datos$gasto_pob_vul[is.na(base_datos$gasto_pob_vul)] <- median(base_datos$gasto_pob_vul, na.rm = TRUE)
base_datos$GRADOES[is.na(as.numeric(base_datos$GRADOES))] <- median(as.numeric(base_datos$GRADOES), na.rm = TRUE)
base_datos$TASA_DEP_DISCYLIM[is.na(as.numeric(base_datos$TASA_DEP_DISCYLIM))] <- median(as.numeric(base_datos$TASA_DEP_DISCYLIM), na.rm = TRUE)



# INTERSECCIONES FAMARCIAS MUNICIPIO
intersecciones <- st_intersects(base_datos$geometry, farmacias_sf$geometry)

base_datos$num_farmacias <- lengths(intersecciones)

# CENTROIDES MUNICIPIOS PARA IAF
base_datos$centroides <- st_centroid(base_datos$geometry)


# CALCULO DE W O MATRIZ DE PESOS ESPACIALES
vecinos <- poly2nb(base_datos$geometry, queen = TRUE)

pesos_espaciales <- nb2listw(vecinos, style = "W")  # W es la normalización por fila

summary(pesos_espaciales)

# CALCULO DEL IAF
base_datos$IAF <- 0

for (i in 1:nrow(base_datos)) {
  
  #P1
  contribucion_local <- base_datos$num_farmacias[i] / base_datos$POBTOT[i]  
  
  
  vecinos_i <- vecinos[[i]]
  
  # P2
  contribucion_vecinos <- 0
  for (j in vecinos_i) {
    
    w_ij <- pesos_espaciales$weights[[i]][which(vecinos_i == j)]
    
    d_ij <- st_distance(base_datos$centroides[i], farmacias_sf$geometry[j])
    
    F_j <- base_datos$num_farmacias[j]
    
    contribucion_vecinos <- contribucion_vecinos + (w_ij * (F_j / as.numeric(d_ij)))
  }
  
  base_datos$IAF[i] <- contribucion_local + (contribucion_vecinos / base_datos$POBTOT[i])
}

base_datos$IAF[is.na(base_datos$IAF)] <- mean(base_datos$IAF, na.rm = TRUE)

#REGRESIÓN ESPACIAL
if (!require('spatialreg')) {
  install.packages('spatialreg')
}
library(spatialreg)

#REGRESIÓN ESPACIAL
SAR <- lagsarlm(IAF ~ Indice_Ruralidad + gasto_salud_pc + TASA_NO_ASEGURADOS + TASA_DEPENDENCIA, 
                data = base_datos, 
                listw = pesos_espaciales)
summary(SAR)

#CALCULO VALORES DE LA PREDICCIÓN
coeficientes <- coef(SAR)
base_datos$predicciones <- coeficientes[1] + 
  coeficientes[2] * base_datos$Indice_Ruralidad + 
  coeficientes[3] * base_datos$gasto_salud_pc + 
  coeficientes[4] * base_datos$TASA_NO_ASEGURADOS + 
  coeficientes[5] * base_datos$TASA_DEPENDENCIA

base_datos$error <- base_datos$IAF - base_datos$predicciones

#TOP 200
top_200 <- base_datos %>%
  arrange(error)%>%
  head(200)
write.csv(top_200, "Ubicacion_farm.csv", row.names = FALSE)
















  
