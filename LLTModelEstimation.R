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
VerbAgg$r2<-ifelse(VerbAgg$r2=="N",0,1)# versión dicotómica de la respuesta: un factor con niveles N e Y
VerbAgg$Gender<-ifelse(VerbAgg$Gender == "M", 1, 0)# versión dicotómica de la respuesta: un factor con niveles M y F
str(VerbAgg)

#--------------------------BigTable

LLTM<-data.frame(items=c("Intercept","mode", "situ", "Blame", "Express"))

#Agregando codificación

VerbAgg <- VerbAgg %>% mutate(mode = case_when(mode == "do" ~ 1,
                                               mode == "want" ~ 0),
                              situ = case_when(situ == "other" ~ 1,
                                               situ == "self" ~ 0),
                              Blame = case_when(btype == "curse" ~ 1/2,
                                                btype == "scold" ~ 1/2,
                                                btype == "shout" ~ -1),
                              Express = case_when(btype == "curse" ~ 1/2,
                                                  btype == "shout" ~ 1/2,
                                                  btype == "scold" ~ -1))

#------------------------glmer

control=glmerControl(optimizer = "optimx", calc.derivs = FALSE, 
                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))

glmermodel <- glmer(resp ~ 1 + mode + situ  + Blame + Express + (1|id),
              family=binomial("logit"), data=VerbAgg, control=control)

summary(glmermodel)

stglmer<-devfun2(glmermodel, useSc=TRUE)

LLTM <- LLTM %>% 
  mutate(glmermodel = fixed.effects(glmermodel)) %>% 
  mutate(Std.Error.glmer = attr(stglmer, "stderr"))

#------------------- eirm 

if (! require("eirm"))  {install.packages("eirm")}

library("eirm")

eirmodel<-eirm(resp ~ 1 + mode + situ  + Blame + Express + (1|id), data=VerbAgg)

print(eirmodel, difficulty = TRUE)

LLTM <- LLTM %>% 
  mutate(eirmmodel = as.data.frame(eirmodel$parameters)[, 1],
         std.error.eirm = as.data.frame(eirmodel$parameters)[, 2])

#--------------------------------------------- eRm

if (! require("eRm"))  {install.packages("eRm")} # Análisis Rasch estándar con estimación máxima verosimilitud condicional (CML)
if (! require("deltaPlotR"))  {install.packages("deltaPlotR")}


library("eRm") # Análisis Rasch estándar con estimación de CML
library(deltaPlotR) 

data("verbal")  # wide data

#---------------------------------------------LLTM de eRm

#----------- Qmatrix

qmat<- subset.data.frame(VerbAgg,select = c(item, btype, situ, mode)) %>% distinct()

qmat <- qmat %>% mutate( Blame = case_when(btype == "curse" ~ 1/2,
                                                btype == "scold" ~ 1/2,
                                                btype == "shout" ~ -1),
                              Express = case_when(btype == "curse" ~ 1/2,
                                                  btype == "shout" ~ 1/2,
                                                  btype == "scold" ~ -1))

attach(qmat)

qmat <- subset.data.frame(qmat, select = c(mode, situ, Express,Blame)) %>% as.matrix()

#-----------

LLTModel <- LLTM(verbal[1:24], qmat)
summary(LLTModel)

LLTModelitems=data.frame(items = names(LLTModel$etapar), LLTModel$etapar, row.names = NULL)
LLTModelitems=cbind(LLTModelitems, as.data.frame(LLTModel$se.eta))

LLTM<-full_join(LLTM, LLTModelitems, by ="items")

items <- data.frame(items=c("S1WantCurse", "S1WantScold", "S1WantShout", "S2WantCurse", 
                           "S2WantScold", "S2WantShout", "S3WantCurse", "S3WantScold", 
                           "S3WantShout", "S4WantCurse", "S4WantScold", "S4WantShout",  
                           "S1DoCurse", "S1DoScold", "S1DoShout", "S2DoCurse", "S2DoScold", 
                           "S2DoShout", "S3DoCurse", "S3DoScold","S3DoShout", "S4DoCurse", 
                           "S4DoScold", "S4DoShout"))

items <- cbind(items, as.data.frame(LLTModel$betapar),row.names = NULL )
items <- cbind(items, as.data.frame(LLTModel$se.beta))

write.csv(items, "itemsLLTM.csv")

##### --------- Resumen 

resumen <- data.frame(Modelo = c("glmer", "eirm", "LLTM"),
                      Deviance = round(c(-2 * logLik(glmermodel), deviance(eirmodel$model),
                                         -2 * LLTModel$loglik), 0),
                      AIC = round(c(AIC(glmermodel), AIC(eirmodel$model),
                                    -2 * LLTModel$loglik + 2 * LLTModel$npar), 0),
                      BIC = round(c(BIC(glmermodel), BIC(eirmodel$model),
                                    -2 * LLTModel$loglik + log(316)), 0))

write.csv(resumen, "ResumenLLTM.csv")

write.csv(LLTM, "LLTM.csv")
