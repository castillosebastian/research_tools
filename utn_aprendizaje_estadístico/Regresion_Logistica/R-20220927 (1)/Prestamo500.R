## Prestamo 500

setwd("C:/Users/Curso R/Clase 4/Cancer.XLS")
getwd()
library(aod)
library(ggplot2)
library(readxl)
Prestamo500 <- read_excel("C:/Users/Curso R/Clase 4/Prestamo500.xls")
prestamo=Prestamo500

head(prestamo)
str(prestamo)

#Antes de empezar se hace un análisis de las variables Educ y Default.
#hacemos una tabla cruzada de Default versus Educ. 
#para ver que no haya muchas casillas chicas

xtabs(~Default + Educ, data = prestamo) # Observamos que no hay casillas muy chicas

# Convertimos variables categ a factor
#Factorizo las variables dicotómicas

prestamo$Educ<-factor(prestamo$Educ)
prestamo$Default<-factor(prestamo$Default)
head(prestamo)

# a) Ecuación del modelo

modelo<-glm(Default~Edad+Educ+Empleo+Ingreso+Deuing+Creddeu+Otrdeu, data=prestamo, 
            family=binomial)
summary(modelo)
deviance(modelo)
anova(modelo, test = "Chisq") #para analizar las devianzas

# Escribo los valores del exponente Beta
exp(modelo$coefficients)

#para construir IC de los coeficientes basados en verosimilitud
exp(confint(modelo))

#IC para los ODDs=exp(betas): odds ratios e Intervalos de Confianza al 95%
exp(cbind(OR = coef(modelo), confint(modelo)))

## BONDAD DE AJUSTE
#para ver el Test de H-L

library(generalhoslem)
logitgof(prestamo$Default, fitted(modelo))
# Esta prueba me indica que si el p-valor es alto entonces "el ajuste" es bueno. 
# Pero ojo!!! porque está tirando un mensaje que alguna casilla es muy chica en el test! entonces, 
# no es confiable esta prueba porque la aproximación chicuadrado no es buena. 
# No lo deberíamos tomar en cuenta a HL acá. Decidir con otras cosas (AIC por ej)

library(pscl)
pR2(modelo)

# b) Ajustamos el modelo
# Ajustamos con AKAIKE (AIC)

step(modelo)
modelo2<-glm(Default~Edad+Empleo+Deuing+Creddeu, data = prestamo,family = binomial)
summary(modelo2)
# Este es el modelo que mejor ajusta con un AIC de 450.25 según el software

deviance(modelo2)
anova(modelo,modelo2,test="Chisq")

# Escribo los valores del exponente Beta
exp(modelo2$coefficients)

#para construir IC de los coeficientes basados en verosimilitud
exp(confint(modelo2))

#IC para los ODDs=exp(betas): odds ratios e Intervalos de Confianza al 95%
exp(cbind(OR = coef(modelo2), confint(modelo2)))

## BONDAD DE AJUSTE
#para ver el Test de H-L

library(generalhoslem)
logitgof(prestamo$Default, fitted(modelo2))
# Esta prueba me indica que si el p-valor es alto entonces "el ajuste" es bueno. 
# Pero ojo!!! porque está tirando un mensaje que alguna casilla es muy chica en el test! entonces, 
# no es confiable esta prueba porque la aproximación chicuadrado no es buena. 
# No lo deberíamos tomar en cuenta a HL acá. Decidir con otras cosas (AIC por ej)

library(pscl)
pR2(modelo2)

#para calcular residuales
e <- residuals(modelo2, "response")  #residuales comunes: y-pihat
rp <- residuals(modelo2, "pearson") #de Pearson
dev <- residuals(modelo2, "deviance") #res de Devianza

h <- hatvalues(modelo2) #leverages
plot(modelo2)
predis<-modelo2$fitted
plot(predis,h)
identify(predis,h,tolerance=0.15)  #caso 110

#para calcular Cook

p<-2 #numero de predictores
D <- (rp^2*h)/(p*(1-h)^2) # Cook's Distance
plot(prestamo$Default,D)
identify(prestamo$Default,D,tolerance=0.3) #reporta el caso del gráfico con D mayor a 0.3 caso 110 y 163

#para sacar pseudos R2- MacFadden

nullmod <- glm(Default~1, data = prestamo,family="binomial")
nullmod

#otra forma: 

modelo2$null.deviance
R2<-1-logLik(modelo2)/logLik(nullmod)
R2

### Observemos que:
#En step(modelo), cuando pongo Educ en el modelo, al seleccionar lo toma como un todo, o sea
#pone o saca todas las dummies. El siguiente codigo permite que tire algunas dummies  
#asi me doy cuenta si no hay diferencias entre categ:
#### Aplicamo Dummies

library(fastDummies)
Educ_dummy<-dummy_cols(prestamo$Educ)
colnames(Educ_dummy)<-c("Educ","Educ1","Educ2","Educ3","Educ4","Educ5")
Educ_dummy<-Educ_dummy[,-1]
prestamo<-data.frame(prestamo,Educ_dummy)
prestamo

modelo3<-step(glm(Default~Edad+Empleo+Deuing+Creddeu+Educ2+Educ3+Educ4+Educ5, data = prestamo,family = binomial))
summary(modelo3)
#modelo3 deja Educ2 sola, es la única que difiere de la categ base

modelo4<-glm(Default~Edad+Empleo+Deuing+Creddeu+Educ2,data = prestamo,family = binomial)
summary(modelo4)
#En este caso este último modelo4 es el que mejor ajusta con un AIC de 448.58 según el software 

deviance(modelo4)
anova(modelo,modelo4,test="Chisq")

# Escribo los valores del exponente Beta
exp(modelo4$coefficients)

#para construir IC de los coeficientes basados en verosimilitud
exp(confint(modelo4))

#IC para los ODDs=exp(betas): odds ratios e Intervalos de Confianza al 95%
exp(cbind(OR = coef(modelo4), confint(modelo4)))

## BONDAD DE AJUSTE
#para ver el Test de H-L

library(generalhoslem)
logitgof(prestamo$Default, fitted(modelo4))
# Esta prueba me indica que si el p-valor es alto entonces "el ajuste" es bueno. 
# Pero ojo!!! porque está tirando un mensaje que alguna casilla es muy chica en el test! entonces, 
# no es confiable esta prueba porque la aproximación chicuadrado no es buena. 
# No lo deberíamos tomar en cuenta a HL acá. Decidir con otras cosas (AIC por ej)

library(pscl)
pR2(modelo4)

#para calcular residuales
e <- residuals(modelo4, "response")  #residuales comunes: y-pihat
rp <- residuals(modelo4, "pearson") #de Pearson
dev <- residuals(modelo4, "deviance") #res de Devianza

h <- hatvalues(modelo4) #leverages
plot(modelo4)
predis<-modelo4$fitted
plot(predis,h)
identify(predis,h,tolerance=0.15)  #caso 110

#para calcular Cook

p<-2 #numero de predictores
D <- (rp^2*h)/(p*(1-h)^2) # Cook's Distance
plot(prestamo$Default,D)
identify(prestamo$Default,D,tolerance=0.3) #reporta el caso del gráfico con D mayor a 0.3 caso 110 y 163

#para sacar pseudos R2- MacFadden

nullmod <- glm(Default~1, data = prestamo,family="binomial")
nullmod

#otra forma: 

modelo2$null.deviance
R2<-1-logLik(modelo2)/logLik(nullmod)
R2

# Modelo: AIC=457.65, R^2MF=0.3583681,  HL=0.7598
# Modelo2: AIC=450.25, R^2MF=0.3515942, HL=0.3415
# Modelo4: AIC=448.58, R^2MF=0.3569990, HL=0.6486

# Entre estos modelos me quedo con el modelo4, por tener menor AIC, ya que H-L no son confiables en 
# ninguno de los modelos y el R2MF no es muy distinto

# f) Para ver como se clasifica el caso 20

dato20<-prestamo[20,]
predi20<-predict.glm(modelo2,dato20,level=0.95,interval="prediction",type = "response")
predi20

# c), d) y e) Para hacer tabla de clasificacion
##Puntos de corte

prediccion <- ifelse(fitted.values(modelo2) >= 0.5, 1, 0) #fui cambiando a mano distintos pcorte
table(prediccion)
table(prestamo$Default, prediccion)
tabla.clasif <- table(prestamo$Default, prediccion)
accu <- sum(diag(tabla.clasif))/sum(tabla.clasif)
accu

## Codigo para Sensibilidad y Especificidad:
sensitividad<-tabla.clasif[2,2]/sum(tabla.clasif[2,])
sensitividad
especificidad<-tabla.clasif[1,1]/sum(tabla.clasif[1,])
especificidad

# Comparación de clasificación predicha y observaciones

library(vcd)
predicciones <- ifelse(test = modelo2$fitted.values >= 0.5, yes = 1, no = 0)
matriz_confusion <- table(prestamo$Default, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))


## _____Otro punto de corte_______

prediccion<-ifelse(fitted.values(modelo2)>=0.3,1,0) ##fui cambiando a mano distancia
table(prestamo$Default, prediccion)
tabla.clasif<-table(prestamo$Default,prediccion)
tabla.clasif
accu<-sum(diag(tabla.clasif))/sum(tabla.clasif)
accu

## Codigo para Sensibilidad y Especificidad:
sensitividad<-tabla.clasif[2,2]/sum(tabla.clasif[2,])
sensitividad
especificidad<-tabla.clasif[1,1]/sum(tabla.clasif[1,])
especificidad

# Comparación de clasificación predicha y observaciones

library(vcd)
predicciones <- ifelse(test = modelo2$fitted.values >= 0.3, yes = 1, no = 0)
matriz_confusion <- table(prestamo$Default, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

##_______Otro punto de corte_______

prediccion<-ifelse(fitted.values(modelo2)>=0.2,1,0) ##fui cambiando a mano distancia
tabla.clasif<-table(prestamo$Default,prediccion)
tabla.clasif
accu<-sum(diag(tabla.clasif))/sum(tabla.clasif)
accu

## Codigo para Sensibilidad y Especificidad:
sensitividad<-tabla.clasif[2,2]/sum(tabla.clasif[2,])
sensitividad
especificidad<-tabla.clasif[1,1]/sum(tabla.clasif[1,])
especificidad

# Comparación de clasificación predicha y observaciones

library(vcd)
predicciones <- ifelse(test = modelo2$fitted.values >= 0.2, yes = 1, no = 0)
matriz_confusion <- table(prestamo$Default, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))


library(pROC)
roc.mod<-roc(prestamo$Default,fitted.values(modelo2))
attributes(roc.mod)
plot(roc.mod)



