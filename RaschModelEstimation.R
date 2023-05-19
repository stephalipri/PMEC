#------------------------lmer4----------
rm(list = ls())
cat("\f")

if (! require("lme4"))  {install.packages("lme4")} 
if (! require("dplyr"))  {install.packages("dplyr")}
if (! require("optimx"))  {install.packages("optimx")}
if (! require("nlme"))  {install.packages("nlme")} # Para usar la función fixed.effects
if (! require("reshape2"))  {install.packages("reshape2")}

library("lme4")
library("dplyr") # para usar la función join y %>% 
library("optimx")
library("nlme")
library("reshape2") # Para convertir nombres de columnas en variables id
#---------------------------------------------

data("VerbAgg")
head(VerbAgg, 5)
str(VerbAgg)
names(VerbAgg)
attach(VerbAgg)
VerbAgg$r2<-ifelse(VerbAgg$r2=="Y",1,0)# versión dicotómica de la respuesta: un factor con niveles N e Y
VerbAgg$Gender<-ifelse(VerbAgg$Gender == "M", 1, 0)# versión dicotómica de la respuesta: un factor con niveles M y F
str(VerbAgg)

#--------------------------BigTable

RaschT<-data.frame(items=c("S1WantCurse", "S1WantScold", "S1WantShout", "S2WantCurse", 
                           "S2WantScold", "S2WantShout", "S3WantCurse", "S3WantScold", 
                           "S3WantShout", "S4WantCurse", "S4WantScold", "S4WantShout",  
                           "S1DoCurse", "S1DoScold", "S1DoShout", "S2DoCurse", "S2DoScold", 
                           "S2DoShout", "S3DoCurse", "S3DoScold","S3DoShout", "S4DoCurse", 
                           "S4DoScold", "S4DoShout"))

#--------------------------------------------- Rasch Model

control=glmerControl(optimizer = "optimx", calc.derivs = FALSE, 
                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))

glmermodel <- glmer(r2~-1+item+(1|id),family=binomial("logit"), data=VerbAgg, 
                    control=control)

summary(glmermodel) ## 367 items value, 60 AIC, BIC values

itemMax = max(fixed.effects(glmermodel)) # función del paquete lme4 function que extrae los valores estimados
itemMin = min(fixed.effects(glmermodel))

stdglmer<-devfun2(glmermodel, useSc=TRUE) # función del paquete lme4 que debulve una función que tiene los standard errors  

RaschT <- RaschT %>% 
  mutate(glmermodel = fixed.effects(glmermodel)) %>% 
  mutate(Std.Error.glmer = attr(stdglmer,"stderr"))

# Otra forma de ajustar el modelo
doubleglmer <- glmer(r2 ~ -1+item+(1|id), data=VerbAgg, family = binomial) # warning messages

doubleglmer <-glmer(r2 ~ -1+item+(1|id), data=VerbAgg, family = binomial,
                 start = list(fixef = fixef(doubleglmer), theta = getME(doubleglmer, "theta")))  # Corrige el warning

# theta -> Estimaciones de parámetros de efectos aleatorios: 
# se parametrizan como los factores Cholesky relativos 
# de cada término de efecto aleatorio.

summary(doubleglmer) #Da los mismos resulatdos que glmermodel

stdoubleglmer<-devfun2(glmermodel, useSc=TRUE)

RaschT <- RaschT %>% 
  mutate(doubleglmer = fixed.effects(doubleglmer)) %>% 
  mutate(Std.Error.doubleglmer = attr(stdoubleglmer, "stderr"))

#------------------- eirm 

if (! require("eirm"))  {install.packages("eirm")}

library("eirm")

eirmodel<-eirm(formula = "r2 ~ -1 + item + (1|id)", data = VerbAgg)
print(eirmodel, difficulty =TRUE)

RaschT <- RaschT %>%
  mutate(eirmmodel = as.data.frame(eirmodel$parameters)[, 1],
         std.error.eirm = as.data.frame(eirmodel$parameters)[, 2])

#--------------------------------------------- eRm

if (! require("eRm"))  {install.packages("eRm")} # Análisis Rasch estándar con estimación máxima verosimilitud condicional (CML)
if (! require("deltaPlotR"))  {install.packages("deltaPlotR")}


library("eRm") # Análisis Rasch estándar con estimación de CML
library(deltaPlotR) 

data("verbal")  # wide data

#-----------Genera el Rasch model con la función RM de eRm
RM <- RM(verbal[1:24], se = T, sum0 = T)
summary(RM)

RMitems<-data.frame(items=c("S1WantScold", "S1WantShout", "S2WantCurse", "S2WantScold", "S2WantShout", "S3WantCurse", 
                            "S3WantScold", "S3WantShout", "S4WantCurse", "S4WantScold", "S4WantShout",   "S1DoCurse", 
                            "S1DoScold",  "S1DoShout",  "S2DoCurse", "S2DoScold", "S2DoShout", "S3DoCurse", 
                           "S3DoScold","S3DoShout", "S4DoCurse", "S4DoScold", "S4DoShout"))

RMitems=cbind(RMitems, as.data.frame(RM$etapar))
RMitems=cbind(RMitems, as.data.frame(RM$se.eta))

RaschT<-full_join(RaschT, RMitems, by ="items")

#------------------------------ Rasch Model con función rasch de ltm
if (! require("ltm"))  {install.packages("ltm")} # Análisis Rasch estándar con estimación CML

library(ltm)

data("verbal")

raschmod <- rasch(verbal[1:24], IRT.param = FALSE) 
#Para que los coeficientes estimados que se devuelven no están en la parametrización IRT estándar. 
#En lugar de eso, se estiman los parámetros de manera directa para cada ítem y para el parámetro de discriminación

summary(raschmod) 

# No existe forma de obtener std.erro de la función rashc. En su lugar se puede hacer lo siguiente

RaschT <- RaschT %>% 
  mutate(rasch = as.data.frame(raschmod$coefficients)[,1]) %>% 
  mutate(str.error.rasch = sqrt(diag(vcov(raschmod)))[1:24])

#---------------------------Función tam del paquete TAM

if (! require("TAM"))  {install.packages("TAM")} 

library(TAM)

data("verbal")

tam<-tam(verbal[1:24], verbose = FALSE) # para evitar mns durante las iteraciones

tamframe<-as.data.frame(tam$xsi)

RaschT <- RaschT %>% 
  mutate(tam = tamframe[,1]) %>% 
  mutate(str.error.tam = tamframe[,2])

RaschModelEstimation<-round(RaschT[,2:13], 2) 

RaschModelEstimation<-cbind(RaschT$items, RaschModelEstimation) 
         
          

 #####------------- Resumen 

resumen <- data.frame(Modelo = c("glmer", "doubleglmer", "RM", "eirm", "Rasch", "tam"),
                      Deviance = round(c(-2 * logLik(glmermodel), -2 * logLik(doubleglmer),
                                         -2 * RM$loglik, -2 * logLik(eirmodel$model), -2 * raschmod$log.Lik, tam$deviance), 0),
                      AIC = round(c(AIC(glmermodel), AIC(doubleglmer), -2 * RM$loglik + 2 * RM$npar,
                                    AIC(eirmodel$model), AIC(raschmod), tam$ic$AIC), 0),
                      BIC = round(c(BIC(glmermodel), BIC(doubleglmer), -2 * RM$loglik + log(316),
                                    BIC(eirmodel$model), BIC(raschmod), tam$ic$BIC), 0))
