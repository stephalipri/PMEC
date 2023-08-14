#------------------------lmer4----------
rm(list = ls())
cat("\f")

if (! require("dplyr"))  {install.packages("dplyr")}
if (! require("optimx"))  {install.packages("optimx")}
if (! require("reshape2"))  {install.packages("reshape2")}
if (! require("openxlsx"))  {install.packages("openxlsx")}
if (! require("TAM"))  {install.packages("TAM")} 
if (! require("lme4"))  {install.packages("lme4")} 

library("dplyr") # para usar la función join y %>% 
library("optimx")
library("reshape2") # Para convertir nombres de columnas en variables id
library("openxlsx")
library("TAM")
library("lme4")

#------------------------Funciones----------

convertir_a_factor <- function(data, columnas_enteros) {
  for (col in columnas_enteros) {
    if (is.integer(data[[col]])) {
      data[[col]] <- as.factor(data[[col]])
    }
  }
  return(data)
}

#------------------------DataBase----------

datos <- read.csv(
  url("https://raw.githubusercontent.com/stephalipri/PMEC/main/PyR1.csv"),
  sep=",", 
  header=TRUE)

head(datos, 2)
str(datos)

#------------------------Limpiando datos----------

datos <- datos[-1]
datos$Sexo <- as.integer(ifelse(datos$Sexo == "F", 0, 1))

#------------------------Convertie a Factor----------

columnas_a_convertir <- c("Grupo", "Sexo", "Dedica_AP1", "Dedica_DPyR1", "ConPrevio", "p11", "p21", "p31", 
                          "p41", "p51", "p61", "p71", "p81", "p91", "p101", "r11", "r21", 
                          "r31", "r41", "r51", "r61", "r71", "r81", "r91", "r101")

datos_FAC <- convertir_a_factor(datos, columnas_a_convertir)

#------------------------Descriptivas----------

output <- summary(datos_FAC)
print(output)

#------------------------Creando tabla resumen----------

All<-data.frame(items=c("p11", "p21", "p31", "p41", 
                           "p51", "p61", "p71", "p81", 
                           "p91", "p101", "r11", "r21",  
                           "r31", "r41", "r51", "r61", "r71", 
                           "r81", "r91", "r101"))

P1 <- data.frame(items=c("p11", "p21", "p31", "p41", 
                         "p51", "p61", "p71", "p81", 
                         "p91", "p101"))


R1 <- data.frame(items=c("r11", "r21",  
                         "r31", "r41", "r51", "r61", "r71", 
                         "r81", "r91", "r101"))

##############################################################################
################################## RASCH #####################################
############################################################################## 

#------------------------Items P1----------

tam_P1<-tam(datos[8:17], verbose = FALSE) # para evitar mns durante las iteraciones

tamframe_P1<-as.data.frame(tam_P1$xsi) # vector de parametros estimados y sus correspondientes S.E.

P1 <- P1 %>% 
  mutate(Rasch_P1= round(tamframe_P1[,1], 2)) %>% 
  mutate(StrError_P1= round(tamframe_P1[,2], 2))

#------------------------Items R1----------

tam_R1<-tam(datos[18:27], verbose = FALSE) # para evitar mns durante las iteraciones

tamframe_R1<-as.data.frame(tam_R1$xsi) # vector de parametros estimados y sus correspondientes S.E.

R1 <- R1 %>% 
  mutate(Rasch_R1= round(tamframe_R1[,1], 2)) %>% 
  mutate(StrError_R1= round(tamframe_R1[,2], 2))


#------------------------Comparando PyR1----------

tam<-tam(datos[8:27], verbose = FALSE) # para evitar mns durante las iteraciones

tamframe<-as.data.frame(tam$xsi) # vector de parametros estimados y sus correspondientes S.E.

All <- All %>% 
  mutate(Rasch = round(tamframe[,1], 2)) %>% 
  mutate(StrError = round(tamframe[,2], 2))

##############################################################################
######################## LATENT REGRESSION RASCH #############################
##############################################################################



##############################################################################
##################################### LLTM ###################################
##############################################################################

#Paso los datos a un formato long

long <- melt(datos, id.vars=c("Id","Grupo","Edad","Sexo","Dedica_AP1","Dedica_DPyR1", 
                              "ConPrevio"), variable.name = "items", 
             value.name = "responses")

#Genero columnas con nueva codificacion

long$Situacion<- ifelse(long$variable %in% c("p11", "p21", "p31", "p41", "p51", "p61", 
                                       "p71", "p81", "p91", "p101"), 1, 0)

long$Tema <- ifelse(long$variable %in% c("p11", "p21", "p31", "p81", "p91", "p101", 
                                      "r11", "r21", "r31", "r81", "r91", "r101"), 1, 0)

long$Practica <- ifelse(long$variable %in% c("p11", "p41", "p51", "p71", "p81", 
                                         "p91", "p101", "r11", "r31", "r41", "r61", 
                                         "r71", "r81", "r91"), 0.5, -1)

long$Abstracto <- ifelse(long$variable %in% c("p11","p21", "p31", "p51", "p61", "p101", 
                                         "r11", "r21", "r31", "r51", 
                                         "r71", "r101"), 0.5, -1)

#Aplicacion Modelo

control=glmerControl(optimizer = "optimx", calc.derivs = FALSE, 
                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))

lltm <- glmer(value ~ -1+ Situacion + Tema + Practica + Abstracto + (1|Id),
              family=binomial("logit"), data=long, control=control)

################ Nueva codificación

#Genero columnas con nueva codificacion

long$Habilidad <- ifelse(long$variable %in% c("p11", "p41", "p51", "p91", "p101", 
                                              "r31", "r61", "r71", "r81", "r91"), 1, 0)

#Aplicacion Modelo

control=glmerControl(optimizer = "optimx", calc.derivs = FALSE, 
                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))

lltm_1<- glmer(value ~ -1+ Situacion + Tema + Habilidad + (1|Id),
              family=binomial("logit"), data=long, control=control)

##############################################################################
######################## LATENT REGRESSION LLTM ##############################
##############################################################################
