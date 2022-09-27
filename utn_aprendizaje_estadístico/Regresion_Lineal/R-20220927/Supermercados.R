##Supermercados

library(readxl)
Supermercados <- read_excel("Posgrado/Supermercados.xls")
Super=Supermercados
head(Super)
summary(Super)

#a.	Ajustar un modelo de regresión que permita predecir las ventas a partir de los gastos de publicidad 
# en radios

summary(fitLS<-lm(ventas~radio, data=Super))
# (i) Parámetros del modelo. Estimaciones de los parámetros Beta del modelo. 
# Contrastes de hipótesis sobre cada parámetro (H0 : Betai = 0). La Prueba t de Student en cada parámetro es:
# intercepto Beta0 = 9.31164, significativo (p<2e-16, menor a 0.01)
# pendiente Beta1 = 0.20250, significativo (p<2e-16, menor a 0.01)
# La Bondad de ajuste R^2 es la medida de la eficacia del modelo de regresión: coeficiente de determinación 
# R^2 Toma valores en el intervalo 0-1. Mide el porcentaje de variabilidad en los datos que viene explicada
# por el modelo, por lo que resulta ser un valor muy alejado de 1 lo que significa que el modelo no es 
# efectivo.
# En este caso R^2 ajustado=0.3287

# (ii) Representación gráfica
par(mfrow=c(1,1))
plot(ventas~radio, data=Super, pch=20)
abline(fitLS,col=2)

# (iii) Intervalos de confianza del 95% para los coef
confint(fitLS)
(res.pred1 <- predict(fitLS,
                      list(radio= c(2.5, 97.5)),
                      interval="confidence"))

#Intervalos de predicción
(res.pred2 <- predict(fitLS,
                      list(radio= c(2.5,97.5)),
                      interval="prediction"))

## Representación del modelo
# aumentamos los límites del gráfico
par(mfrow=c(1,1))
plot(ventas~radio, data=Super, xlim=c(0,50),
     ylim=c(0,30), pch=20)
abline(fitLS,col="blue")
lines(c(2.5,97.5), res.pred1[, 2], lty = 2)
lines(c(2.5,97.5), res.pred1[, 3], lty = 2)
lines(c(2.5,97.5), res.pred2[, 2], lty = 2, col = "red")
lines(c(2.5,97.5), res.pred2[, 3], lty = 2, col = "red")

# b.	Ajustar ahora un modelo de regresión simple para predecir ventas a partir de publicidad en TV 
# y otro modelo tomando como variable regresora los gastos en periódicos. ¿Cuál de los tres modelos 
# le parece más adecuado y por qué?

fitLS_1<-lm(ventas~tv, data=Super)
summary(fitLS_1)
# intercepto Beta0 = 7.032594, significativo (p<2e-16, menor a 0.01)
# pendiente Beta1 = 0.047537, significativo (p<2e-16, menor a 0.01)
# El valor de R^2 ajustado=0.6099, sin embargo no es un valor muy cercano a 1 pero el valor no es malo
# y es bastance aceptable.

summary(fitLS_2<-lm(ventas~periodico, data=Super))
# intercepto Beta0 = 12.35141, significativo (p<2e-16, menor a 0.01)
# pendiente Beta1 = 0.05469, no significativo (p=0.00115, menor a 0.01)
# El valor de R^2 ajustado=0.05212, este valor es muy alejado de 1 lo que significa que el modelo no es 
# efectivo.
##Ninguno de estos modelos es efectivo

# c.	Desarrollar un modelo que permita predecir las ventas a partir de las tres variables disponibles.

fitLM0<-lm(ventas~radio+tv+periodico, data=Super)
summary(fitLM0)

# intercepto Beta0 = 2.938889, significativo (p<2e-16, menor a 0.01)
# pendiente Beta1 = 0.188530, significativo (p<2e-16, menor a 0.01)
# pendiente Beta2 = 0.045765, significativo (p<2e-16, menor a 0.01)
# pendiente Beta3 = -0.001037, no significativo (p=0.86, mayor a 0.01)
# La Bondad de ajuste R^2=0.8972, al ser un valor muy cercano a 1 significa que el modelo es efectivo.
#  Eliminamos el intercepto y comparamos los modelos.

fitLM<-lm(ventas~radio+tv+periodico-1, data=Super)
summary(fitLM)

# Eliminar el intercepto tiene efectos significativos en el modelo (p=9.11e-13): 
# ventas=0.222227*radio+0.053792*tv+0.016816*periodico.
# El modelo es significativo R^2=0.9819

anova(fitLM0, fitLM)

#Colinealidad
library(car)
vif(fitLM0)
vif(fitLM)
# No hay colinealidad entre los datos

## Analizar el cumplimiento de todos los supuestos del modelo
#Imprimo residuales versus pred para analizar supuestos

plot(fitLM$fitted,fitLM$residuals)
abline(h=0,col=2)
studresi<-rstandard(fitLM)

par(mfrow=c(1,3),oma=c(1,1,1,1))
hist(studresi) #histograma de residuales
boxplot(studresi,main="boxplot de residuales")
qqnorm(studresi)
qqline(studresi)
title("Comprobando normalidad de residuales",outer=TRUE)

#para hacer prueba de errores no correlacionados con Durbin Watson
library(lmtest)
dwtest(fitLM) #H0:no hay autocorrelac

par(mfrow=c(1,2))
plot(fitLM) #muestra la verificación de los supuestos

#para ver puntos influyentes con Cook
influ <- influence.measures(fitLM)
dcook<-cooks.distance(fitLM)
plot(dcook,Super$ID)
boxplot(dcook)
which(as.vector(dcook)>0.1) #para detectar el caso influyente


# d.	Evaluar si el modelo ajustado en el punto anterior puede ser mejorado, buscando uno que sea más 
# parsimonioso
#Para probar selección automática
## método hacia adelante (forward)

reg.lm<-lm(ventas~1,data = Super)
fowRM<-step(reg.lm,ventas = ~radio+tv+periodico, direction = "forward")
summary(fowRM)

#O hacer dummies de la variable TV

fitLM2<-lm(ventas~radio+factor(tv),data = Super)
summary(fitLM2) #este modelo considera origen(TV) como categ base y ajusta rectas de = pendiente
fitLM3<-lm(ventas~radio+factor(tv)+radio*factor(tv),data = Super)
summary(fitLM3)#ajusta la interaccion tambien, o sea permite distinta pendiente
fitLM4<-lm(ventas~radio+tv+periodico+factor(tv)+radio*factor(tv),data = Super)
summary(fitLM4) #este considera tambien otras variables
#podemos comparar los modelos usando R^2 ajustado, si se chequean antes los supuestos

#e.	Se quiere predecir las ventas, con el modelo ajustado en d), para los siguientes valores de gastos:
#Podemos hacer la prediccion de nuevos valores mediante:
predict(fitLM, newdata = data.frame(tv = 1125.55, radio = 23.40, periodico = 89.70))
predict(fitLM, newdata = data.frame(tv = 234.00, radio = 85.50, periodico = 65.00))

#########################################################################
modelo<-lm(formula=ventas~tv+radio+periodico,data = Super)
summary(modelo)

# Acorde al p-value obtenido para el coeficiente parcial de regresión de periodico, esta variable no 
# contribuye de forma significativa al modelo. Como resultado de este análisis se concluye que las variables
# tv y radio están asociadas con la cantidad de ventas.

modelo<-update(modelo, .~. -periodico)
summary(modelo)

#Al ser un modelo con dos predictores continuos se puede representar en 3D
par(mfrow=c(1,1))
rango_tv <- range(Super$tv)
nuevos_valores_tv <- seq(from = rango_tv[1], to = rango_tv[2], length.out = 20)
rango_radio <- range(Super$radio)
nuevos_valores_radio <- seq(from = rango_radio[1], to = rango_radio[2],
                            length.out = 20)

predicciones <- outer(X = nuevos_valores_tv, Y = nuevos_valores_radio, 
                      FUN = function(tv, radio) {
                        predict(object = modelo, newdata = data.frame(tv, radio))
                      })

superficie <- persp(x = nuevos_valores_tv, y = nuevos_valores_radio,
                    z = predicciones,
                    theta = 18, phi = 20,
                    col = "lightblue", shade = 0.1,
                    xlab = "tv", ylab = "radio", zlab = "ventas",
                    ticktype = "detailed",
                    main = "Predición ventas ~ TV y Radio")

observaciones <- trans3d(Super$tv, Super$radio, Super$ventas, superficie)
error <- trans3d(Super$tv, Super$radio, fitted(modelo), superficie)
points(observaciones, col = "red", pch = 16)
segments(observaciones$x, observaciones$y, error$x, error$y)

# El modelo lineal a partir del cual se han obtenido las conclusiones asume que el efecto sobre las ventas 
# debido a un incremento en el presupuesto de uno de los medios de comunicación es independiente del 
# presupuesto gastado en los otros.

# En R se puede introducir interacción entre predictores de dos formas, indicando los predictores 
# individuales y entre cuales se quiere evaluar la interacción, o bien de forma directa.
modelo_interaccion<-lm(formula = ventas ~ tv + radio + tv:radio, data = Super)
summary(modelo_interaccion)

# lm(formula = ventas ~ tv * radio, data = Super) es equivalente.

# Al ser un modelo con dos predictores continuos se puede representar en 3D
rango_tv <- range(Super$tv)
nuevos_valores_tv <- seq(from = rango_tv[1], to = rango_tv[2], length.out = 20)
rango_radio <- range(Super$radio)
nuevos_valores_radio <- seq(from = rango_radio[1], to = rango_radio[2], length.out = 20)

# La función outer() permite aplicar una función a cada combinación de los parámetros x, y pasados como 
# argumento, es una alternativa a utilizar expand.grid()
predicciones<-outer(X = nuevos_valores_tv, Y = nuevos_valores_radio, 
                    FUN = function(tv, radio) {
                      predict(object = modelo_interaccion,
                              newdata = data.frame(tv, radio))})

superficie <- persp(x = nuevos_valores_tv, y = nuevos_valores_radio,
                    z = predicciones,
                    theta = 18, phi = 20,
                    col = "lightblue", shade = 0.1,
                    xlab = "tv", ylab = "radio", zlab = "ventas",
                    ticktype = "detailed",
                    main = "Predición ventas ~ TV y Radio")

# Se pueden representar las observaciones a partir de las cuales se ha creado la superficie así como 
# segmentos que midan la distancia respecto al modelo_interaccion generado.
observaciones <- trans3d(Super$tv, Super$radio, Super$ventas, superficie)
error <- trans3d(Super$tv, Super$radio, fitted(modelo_interaccion),
                 superficie)
points(observaciones, col = "red", pch = 16)
segments(observaciones$x, observaciones$y, error$x, error$y)

# Los resultados muestran una evidencia clara de que la interacción tv x radio es significativa y de que el 
# modelo que incorpora la interacción (Adjusted R-squared = 0.9673) es superior al modelo que solo 
# contemplaba el efecto de los predictores por separado (Adjusted R-squared = 0.8956).

# Se puede emplear un ANOVA para realizar un test de hipótesis y obtener un p-value que evalúe la hipótesis 
# nula de que ambos modelos se ajustan a los datos igual de bien.
anova(modelo, modelo_interaccion)
