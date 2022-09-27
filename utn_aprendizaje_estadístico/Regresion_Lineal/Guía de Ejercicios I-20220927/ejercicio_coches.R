#ejercicio coches
setwd("C:/Users/Silvia/Desktop/DM-Parana/extras")
getwd()
library(readxl)
coches<- read_excel("coches.xls")
head(coches)
summary(coches)

#una forma de hacer un graf de dispersion
x=coches$peso
y=coches$consumo
win.graph()
plot(x,y,xlab="peso del coche", ylab="consumo")
title("consumo de combustible")

#Para el punto 1:ajusto RLS con regresora "peso"
modRS<-lm(consumo~peso,data = coches)
summary(modRS)
attributes(modRS) #para que muestra lo que saca el modelo, por si tengo que pedirlo

#para graficar los puntos con la recta ajustada:
par(mfrow=c(1,1))
plot(consumo~peso, data=coches, col="green",pch=10)
abline(modRS,col=2)

# Imprimiendo los coeficientes estimados
summary(modRS)$coef
beta=summary(modRS)$coef[2,1]
eebeta=summary(modRS)$coef[2,2]
# intervalos de confianza del 95% para los coef
confint(modRS)
#para ver la tabla anova
anova(modRS)

#imprimo residuales versus pred para analizar supuestos
plot(modRS$fitted,modRS$residuals)
abline(h=0,col=2)
studresi<-rstandard(modRS)

par(mfrow=c(1,3),oma=c(1,1,1,1))
hist(studresi) #histograma de residuales
boxplot(studresi,main="boxplot de residuales")
qqnorm(studresi)
qqline(studresi)
title("Comprobando normalidad de residuales",outer=TRUE)

#para hacer prueba de errores no correlacionados con Durbin Watson
library(lmtest)
dwtest(modRS) #H0:no hay autocorrelac

#para predecir el consumo para un valor de peso=780
predichos<-predict(modRS) #da los predichos para cada valor de peso de la base 
nuevodato <- data.frame(peso = c(780,900)) #para valores de peso=780 y 900
predict(modRS, nuevodato, se.fit = TRUE)

#I confianza para la media y de prediccion para la respuesta
(res.pred1 <- predict(modRS,
                      list(peso= c(200:1800,100)),
                      interval="confidence"))
(res.pred2 <- predict(modRS,
                      list(peso= c(200:1800,100)),
                      interval="prediction"))
#Graficando bandas de confianza y bandas de prediccion
par(mfrow=c(1,1))
plot(consumo~peso, data=coches, xlim=c(200,1800),
     ylim=c(5,30), pch=10)
abline(modRS,col="blue")
lines(c(200:1800,100), res.pred1[, 2], lty = 2)
lines(c(200:1800,100), res.pred1[, 3], lty = 2)
lines(c(200:1800,100), res.pred2[, 2], lty = 2, col = "red")
lines(c(200:1800,100), res.pred2[, 3], lty = 2, col = "red")

#para el punto 2: ajusto modelo regresion múltiple
modRM<-lm(consumo~motor+peso+cv+acel+año+cilindr,data = coches)
summary(modRM)
#se puede usar el codigo del modelo anterior para supuestos, predichos, etc

#para ver puntos influyentes con Cook
influ <- influence.measures(modRM)
dcook<-cooks.distance(modRM)
plot(dcook,coches$id)
boxplot(dcook)
which(as.vector(dcook)>0.1) #para detectar el caso influyente

#si queremos comparar modelos con la prueba F, hay que utilizar el mismo dataset,
#o sea sacar todos los casos con datos perdidos. El codigo para comarar es
anova(modRM,modRS) #para comparar modelo chico vs grande

#para el punto 3: hacer dummies de la variable ORIGEN
modRM2<-lm(consumo~peso+factor(origen),data = coches)
summary(modRM2) #este modelo considera origen1 como categ base y ajusta rectas de = pendiente
modRM3<-lm(consumo~peso+factor(origen)+peso*factor(origen),data = coches)
summary(modRM3)#ajusta la interaccion tambien, o sea permite distinta pendiente
modRM4<-lm(consumo~peso+cv+cilindr+factor(origen)+peso*factor(origen),data = coches)
summary(modRM4) #este considera tambien otras variables
#podemos comparar los modelos usando R^2 ajustado, si se chequean antes los supuestos

#para probar selección automática
library(leaps)
forwRM<-regsubsets(consumo~peso+cv+cilindr+factor(origen), data=coches,method="forward",nvmax = 3)
summary(forwRM) #la salida muestra que se queda con las tres primeras variables
#ver mejor las opciones para que de el modelo..
