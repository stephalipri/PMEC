rm(list = ls())
cat("\f")

if (! require("rnaturalearth"))  {install.packages("rnaturalearth")}
if (! require("rgdal"))  {install.packages("rgdal")}
if (! require("sf"))  {install.packages("sf")}
if (! require("sp"))  {install.packages("sp")}
if (! require("osmdata"))  {install.packages("osmdata")}
if (! require("maptools"))  {install.packages("maptools")}
if (! require("reshape2"))  {install.packages("reshape2")}
if (! require("dplyr"))  {install.packages("dplyr")}
if (! require("openxlsx"))  {install.packages("openxlsx")}


library(rnaturalearth)
library(rgdal)
library(sf)
library(sp)
library(osmdata)
library(maptools)
library(reshape2)
library(dplyr)
library(openxlsx)


datos <- read.csv(
  url("https://raw.githubusercontent.com/stephalipri/PMEC/main/PMEC_2022_1_All.csv"),
  sep=";", 
  header=TRUE)

head(datos, 2)
str(datos)

datos$latitud <- gsub(",", ".", datos$latitud)
datos$longitud <- gsub(",", ".", datos$longitud)


datos$latitud <- as.numeric(datos$latitud)
datos$longitud <- as.numeric(datos$longitud)

# Leer los datos de los límites de los municipios de Uruguay
municipios <- read.csv(
  url("https://raw.githubusercontent.com/stephalipri/PMEC/main/LimitesMunicipios.csv", ),
  sep=";", 
  header=TRUE)

head(municipios, 2)
str(municipios)

municipios$latMax <- gsub(",", ".", municipios$latMax)
municipios$latMin <- gsub(",", ".", municipios$latMin)
municipios$lonMax <- gsub(",", ".", municipios$lonMax)
municipios$lonMin <- gsub(",", ".", municipios$lonMin)

municipios$latMax <- as.numeric(municipios$latMax)
municipios$latMin <- as.numeric(municipios$latMin)
municipios$lonMax <- as.numeric(municipios$lonMax)
municipios$lonMin <- as.numeric(municipios$lonMin)

head(municipios, 2)

find_municipio <- function(lat, lon, municipios) {
  for (i in 1:nrow(municipios)) {
    municipio <- municipios[i, ]
    if (lat >= municipio$latMin && lat <= municipio$latMax && lon >= municipio$lonMin && lon <= municipio$lonMax) {
      return(municipio$municipio)
    }
  }
  return(NA)
}

find_departamento <- function(lat, lon, municipios) {
  for (i in 1:nrow(municipios)) {
    municipio <- municipios[i, ]
    if (lat >= municipio$latMin  && lat <= municipio$latMax && lon >= municipio$lonMin && lon <= municipio$lonMax) {
      return(municipio$departamen)
    }
  }
  return(NA)
}

datos$MUNICIPIO <- apply(datos[, c("latitud", "longitud")], 1, function(x) find_municipio(x[1], x[2], municipios))
datos$Departamento <- apply(datos[, c("latitud", "longitud")], 1, function(x) find_departamento(x[1], x[2], municipios))



##### ==============================================Daltos Faltantes

SecPoli <- read.csv(
  url("https://raw.githubusercontent.com/stephalipri/PMEC/main/SeccionPolicial.csv", ),
  sep=";", 
  header=TRUE)

head(SecPoli, 2)
str(SecPoli)

SecPoli$latMax <- gsub(",", ".", SecPoli$latMax)
SecPoli$latMin <- gsub(",", ".", SecPoli$latMin)
SecPoli$lonMax <- gsub(",", ".", SecPoli$lonMax)
SecPoli$lonMin <- gsub(",", ".", SecPoli$lonMin)

SecPoli$latMax <- as.numeric(SecPoli$latMax)
SecPoli$latMin <- as.numeric(SecPoli$latMin)
SecPoli$lonMax <- as.numeric(SecPoli$lonMax)
SecPoli$lonMin <- as.numeric(SecPoli$lonMin)

head(SecPoli, 2)

find_Sec <- function(lat, lon, municipios) {
  for (i in 1:nrow(municipios)) {
    municipio <- municipios[i, ]
    if (lat >= municipio$latMin && lat <= municipio$latMax && lon >= municipio$lonMin && lon <= municipio$lonMax) {
      return(municipio$Seccion)
    }
  }
  return(NA)
}

find_DepSecc <- function(lat, lon, municipios) {
  for (i in 1:nrow(municipios)) {
    municipio <- municipios[i, ]
    if (lat >= municipio$latMin  && lat <= municipio$latMax && lon >= municipio$lonMin && lon <= municipio$lonMax) {
      return(municipio$departamen)
    }
  }
  return(NA)
}

datos$Seccion  <- apply(datos[, c("latitud", "longitud")], 1, function(x) find_Sec (x[1], x[2], SecPoli))
datos$DeptoSecc <- apply(datos[, c("latitud", "longitud")], 1, function(x) find_DepSecc(x[1], x[2], SecPoli))

### Corrijo Departamento Montevideo por Canelones

datos$DeptoSecc[datos$MUNICIPIO == "LAS PIEDRAS"] <- "CANELONES"

####----Reviso los datos y les doy formato.

str(datos)
datos <- datos %>%
  mutate(PiAP = as.numeric(gsub(",", ".", PiAP), na.strings = "NA"),
         PgAP = as.numeric(gsub(",", ".", PgAP), na.strings = "NA"),
         PiDP  = as.numeric(gsub(",", ".", PiDP), na.strings = "NA"),
         PgDP  = as.numeric(gsub(",", ".", PgDP), na.strings = "NA"),
         TrabajoFinal  = as.numeric(gsub(",", ".", TrabajoFinal), na.strings = "NA"),
         P1 = as.numeric(gsub(",", ".", P1), na.strings = "NA"),
         R1 = as.numeric(gsub(",", ".", R1), na.strings = "NA"),
         P2 = as.numeric(gsub(",", ".", P2), na.strings = "NA"),
         R2 = as.numeric(gsub(",", ".", R2), na.strings = "NA"),
         AutoEv = as.numeric(gsub(",", ".", AutoEv), na.strings = "NA"),
         EvPares = as.numeric(gsub(",", ".", EvPares), na.strings = "NA"),
         Total = as.numeric(gsub(",", ".", Total), na.strings = "NA"))

#write.csv(datos, "DataReiw.csv")
#write.xlsx(datos, "DataReiw.xlsx")

# Verifico si existen valores vacios
filas_vacias <- !complete.cases(datos)
filas_con_vacios <- datos[filas_vacias, ] 
#View(filas_con_vacios)


# Lleno espacio vacíos

datos <- datos %>%
  mutate(PiAP = if_else(Id == 38, 0, PiAP),
         PiAP = if_else(Id == 80, 1.5, PiAP),
         PgAP = if_else(Id == 38, 1.3, PgAP),
         PgAP = if_else(Id == 80, 2, PgAP),
         Total = if_else(Id == 38, 15, PgAP),
         Total = if_else(Id == 80, 2, PgAP),
         Total = if_else(Id == 292, 60, PgAP))

#### Normalizo por maximo PiAP, PgAP, PiDP, PgDP

normalize_by_group <- function(data, column, group_column) {
  data %>%
    group_by({{group_column}}) %>%
    mutate({{column}} := round({{column}} / max({{column}}[complete.cases({{column}})]), 2)) %>%
    ungroup()
}

datos <- datos %>%
  normalize_by_group(PiAP, Grupo) %>%
  normalize_by_group(PgAP, Grupo) %>%
  normalize_by_group(PiDP, Grupo) %>%
  normalize_by_group(PgDP, Grupo)

####==============================Agrego nuevas variables 

datos <- datos %>%
  mutate(Suma_AP1 = rowSums(datos[, c("PiAP", "PgAP")], na.rm = TRUE),
         Suma_DPyR1 = rowSums(datos[, c("PiDP", "PgDP",  "P2" , "R2", 
                                       "TrabajoFinal")], na.rm = TRUE),
         Suma_APyR2 = rowSums(datos[, c("PiAP", "PgAP", "P1" , "R1", "PiDP", 
                                        "PgDP","TrabajoFinal")], na.rm = TRUE),
         Suma_DP1 = rowSums(datos[, c("R1","PiDP", "PgDP", "TrabajoFinal")], na.rm = TRUE),
         Suma_AR1 = rowSums(datos[, c("PiAP", "PgAP", "P1")], na.rm = TRUE),
         Suma_DR1 = rowSums(datos[, c("PiDP", "PgDP", "P2", "TrabajoFinal")], na.rm = TRUE))

#------Determino el maximo y hallo su mitad para asignar 0/1

# Caso PyR1
MSuma_AP1 <- max(datos$Suma_AP1)
medMSuma_AP1 <- MSuma_AP1 / 2

MSuma_DPyR1 <- max(datos$Suma_DPyR1)
medMSuma_DPyR1 <- MSuma_DPyR1 / 2

# Caso PyR2
MSuma_APyR2 <- max(datos$Suma_APyR2)
medMSuma_APyR2 <- MSuma_APyR2 / 2

# Caso P1yP2
MSuma_DP1 <- max(datos$Suma_DP1)
medMSuma_DP1 <- MSuma_DP1 / 2

# Caso R1yR2
MSuma_AR1 <- max(datos$Suma_AR1)
medMSuma_AR1 <- MSuma_AR1 / 2

MSuma_DR1 <- max(datos$Suma_DR1)
medMSuma_DR1 <- MSuma_DR1 / 2


#### Variable de dedicacion previa. Toma el valor 1 si la suma de las actividades 
### es mayor o igual a la mitad del maixmo obtenido. 

datos <- datos %>%
  mutate(Dedica_AP1 = ifelse(Suma_AP1 >= medMSuma_AP1 & Suma_AP1 <= MSuma_AP1, 1, 0),
         Dedica_DPyR1 = ifelse(Suma_DPyR1 >= medMSuma_DPyR1 & Suma_DPyR1 <= MSuma_DPyR1, 1, 0),
         Dedica_APyR2 = ifelse(Suma_APyR2 >= medMSuma_APyR2 & Suma_APyR2 <= MSuma_APyR2, 1, 0),
         Dedica_DP1 = ifelse(Suma_DP1 >= medMSuma_DP1 & Suma_DP1 <= MSuma_DP1, 1, 0),
         Dedica_AR1 = ifelse(Suma_AR1 >= medMSuma_AR1 & Suma_AR1 <= MSuma_AR1, 1, 0),
         Dedica_DR1 = ifelse(Suma_DR1 >= medMSuma_DR1 & Suma_DR1 <= MSuma_DR1, 1, 0))

### Variable de conocimientos previos. Toma el valor 0 si Repitente igual 0 y 1 
### para quien esta cursando 1 o más veces.

datos$ConPrevio <- ifelse(datos$Repitente == 0, 0, 1)

############================== Armo base para cada caso

#Genero Base para Prueba 1 y la Recuperación 1
PyR1 <- subset(datos, AsistenciaP1 == 1 & AsistenciaR1 == 1, 
               select = c(Id, Grupo, Edad, Sexo, 
                          Dedica_AP1, Dedica_DPyR1, ConPrevio, p11, p21, p31, p41, p51, 
                          p61, p71, p81, p91, p101, r11, r21, r31, r41, r51, 
                          r61, r71, r81, r91, r101))

#write.csv(PyR1, "PyR1.csv")

#Genero Base para Prueba 2 y la Recuperación 2
PyR2 <- subset(datos, AsistenciaP2 == 1 & AsistenciaR2 == 1, 
               select = c(Id, Grupo, Edad, Sexo,
                          Dedica_APyR2, ConPrevio, p12, p22, p32, p42, p52, 
                          p62, p72, p82, p92, p102, r12, r22, r32, r42, r52, 
                          r62, r72, r82, r92, r102))
#write.csv(PyR2, "PyR2.csv")

#SGenero Base para  Prueba 1 y 2
P1yP2 <- subset(datos, AsistenciaP1 == 1 & AsistenciaP2 == 1,
               select = c(Id, Grupo, Edad, Sexo,Dedica_AP1, 
                          Dedica_DP1, ConPrevio, p11, p21, p31, p41, p51, 
                          p61, p71, p81, p91, p101, p12, p22, p32, p42, p52, 
                          p62, p72, p82, p92, p102))

#write.csv(P1yP2, "P1yP2.csv")

#Selecciono las filas que dieron la Recuperación 1 y 2
R1yR2 <- subset(datos, AsistenciaR1 == 1 & AsistenciaR2 == 1,
              select = c(Id, Grupo, Edad, Sexo, Dedica_AR1, 
                         Dedica_DR1, ConPrevio, r11, r21, r31, r41, r51, 
                         r61, r71, r81, r91, r101, r12, r22, r32, r42, r52, 
                         r62, r72, r82, r92, r102))

#write.csv(R1yR2, "R1yR2.csv")