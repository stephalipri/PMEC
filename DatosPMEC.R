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

library(rnaturalearth)
library(rgdal)
library(sf)
library(sp)
library(osmdata)
library(maptools)
library(reshape2)
library(dplyr)

datos <- read.csv(
  url("https://raw.githubusercontent.com/stephalipri/PMEC/main/PMEC_2022_1.csv"),
  sep=";", 
  header=TRUE)

head(datos, 2)
str(datos)

datos$Longitud <- gsub(",", ".", datos$Longitud)
datos$Latitud <- gsub(",", ".", datos$Latitud)

datos$Longitud <- as.numeric(datos$Longitud)
datos$Latitud <- as.numeric(datos$Latitud)

# Leer los datos de los límites de los municipios de Uruguay
municipios <- read.csv(
  url("https://raw.githubusercontent.com/stephalipri/PMEC/e51d9065232dfc9632f78e019aecfc848781523e/LimitesMunicipios.csv", ),
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

datos$MUNICIPIO <- apply(datos[, c("Latitud", "Longitud")], 1, function(x) find_municipio(x[1], x[2], municipios))
datos$Departamento <- apply(datos[, c("Latitud", "Longitud")], 1, function(x) find_departamento(x[1], x[2], municipios))



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

datos$Seccion  <- apply(datos[, c("Latitud", "Longitud")], 1, function(x) find_Sec (x[1], x[2], SecPoli))
datos$DeptoSecc <- apply(datos[, c("Latitud", "Longitud")], 1, function(x) find_DepSecc(x[1], x[2], SecPoli))

#write.csv(datos, "Secciones.csv")

############================== Rasch Model

orden<- c("Id", "Grupo", "Equipo","p11","p21", "p31", "p41", "p51", "p61", "p71",
          "p81", "p91", "p101", "p12", "p22", "p32", "p42", "p52",       
          "p62", "p72", "p82", "p92", "p102", "P1", "P2", "Edad", "Sexo",  "Repitente", "MUNICIPIO", "DeptoSecc")
 
datosT <- datos[orden]

#Paso los datos a un formato long

long <- melt(datosT, id.vars=c("Id","Grupo","Equipo","P1","P2",  "Edad", "Sexo", "Repitente", "MUNICIPIO", "DeptoSecc"), variable.name = "items", 
             value.name = "responses")

long$responses <- as.factor(long$responses)

#-------------------Rasch Model con función glmer del paquete lme4

if (! require("lme4"))  {install.packages("lme4")} 
if (! require("nlme"))  {install.packages("nlme")} # Para usar la función fixed.effects

library("lme4")
library("nlme")

#------------------------------------------------------------
control=glmerControl(optimizer = "optimx", calc.derivs = FALSE, 
                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))

mod1 <-glmer(formula = "responses ~ -1 + items + (1|Id)", family=binomial("logit"),
            data = long, control = control)

mod1

itemMax = max(fixed.effects(mod1))
itemMin = min(fixed.effects(mod1))

stdglmer<-devfun2(mod1, useSc=TRUE)

rasch_params <- data.frame(glmer = round(fixef(mod1),2),
                           Std.Error.glmer = round(attr(stdglmer, "stderr"), 2))


#-------------------Rasch Model con la función tam del paquete TAM

if (! require("TAM"))  {install.packages("TAM")} 

library(TAM)

#--------------------------
mod2<-tam(datosT[4:23], verbose = FALSE) # para evitar mns durante las iteraciones

tamframe<-as.data.frame(mod2$xsi)

rasch_params<-rasch_params %>% 
  mutate(tam = round(tamframe[,1], 2)) %>% 
  mutate(Str.Error.tam = round(tamframe[,2],2))




