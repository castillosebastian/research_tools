## REGRESIÓN LINEAL
###Juegos de beisbol

library(readxl)
dataBeisbol <- read.delim("~/Posgrado/dataBeisbol.csv")
datos = dataBeisbol
head(datos)

## 1.Representación gráfica de las observaciones 

library(ggplot2)
ggplot(data = datos, mapping = aes(x = Nro.de.bateos, y = Runs)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'Diagrama de dispersión', x  =  'número  de bateos') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
abline(datos,col=2)


cor.test(x = datos$Nro.de.bateos, y = datos$Runs, method = "pearson")

#El gráfico y el test de correlación muestran una relación lineal, de intensidad considerable (r = 0.61) y 
# significativa (p-value = 0.0003388). Tiene sentido intentar generar un modelo de regresión lineal que 
# permita predecir el número de runs en función del número de bateos del equipo. 

## 2.Cálculo del modelo de regresión lineal simple 

modelo_lineal <- lm(Runs ~ Nro.de.bateos, datos)
modelo_lineal
# lm() devuelve el valor de la variable y para x=0 (intersección) junto con la pendiente de la recta.
# Para ver la información del modelo se requiere summary().
summary(modelo_lineal)

# La primera columna (Estimate) devuelve el valor estimado para los dos parámetros de la ecuación del modelo
# lineal (B0 y B1) que equivalen a la ordenada en el origen y la pendiente.
# Se muestran los errores estándar, el valor del estadístico t y el p-value (dos colas) de cada uno de los 
# dos parámetros. Esto permite determinar si los parámetros son significativamente distintos de 0, es decir, 
# que tienen importancia en el modelo. En los modelos de regresión lineal simple, el parámetro más 
# informativo suele ser la pendiente.
# Para el modelo generado, tanto la ordenada en el origen como la pendiente son significativas 
# (p-values < 0.05).
# El valor de R2 indica que el modelo calculado explica el 37.29% de la variabilidad presente en la variable
# respuesta (runs) mediante la variable independiente (número de bateos).
# El p-value obtenido en el test F (0.0003388) determina que sí es significativamente superior la varianza 
# explicada por el modelo en comparación a la varianza total. Es el parámetro que determina si el modelo es 
# significativo y por lo tanto se puede aceptar.
# El modelo lineal generado sigue la ecuación runs = -2789.2429 + 0.6305 bateos. Por cada unidad que se 
# incrementa el número de bateos, el número de runs aumenta en promedio 0.6305 unidades. 

# 3.Intervalos de confianza para los parámetros del modelo 

confint(modelo_lineal)

# 4.Representación gráfica del modelo 

ggplot(data = datos, mapping = aes(x = Nro.de.bateos, y = Runs)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'Runs ~ número de bateos', x  =  'número  de bateos') +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Se genera una secuencia de valores x_i que abarquen todo el rango de las  observaciones de la variable X

puntos <- seq(from = min(datos$Nro.de.bateos),
              to = max(datos$Nro.de.bateos),
              length.out = 100)
# Se predice el valor de la variable Y junto con su intervalo de confianza para cada uno de los puntos 
# generados. En la función predict() hay que nombrar a los nuevos puntos con el mismo nombre que la variable
# X del modelo.
# Devuelve una matriz.

limites_intervalo <- predict(object = modelo_lineal,
                             newdata = data.frame(Nro.de.bateos = puntos),
                             interval = "confidence", level = 0.95)
head(limites_intervalo, 3) 



# Finalmente se añaden al gráfico las líneas formadas por los límites superior e inferior.

plot(datos$Nro.de.bateos, datos$Runs, col = "firebrick", pch = 19, ylab = "Runs",
     xlab = "número de bateos", main = 'Runs ~ número de bateos')
abline(modelo_lineal, col = 1)
lines(x = puntos, y = limites_intervalo[,2],type = "l", col = 2, lty = 3)
lines(x = puntos, y = limites_intervalo[,3],type = "l", col = 3, lty = 3)

ggplot(data = datos, mapping = aes(x = Nro.de.bateos, y = Runs)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'Diagrama de dispersión', x  =  'número  de bateos') +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Por defecto incluye la región de 95% de confianza.

# 5.Verificar condiciones para poder aceptar un modelo lineal 
# Relación lineal entre variable dependiente e independiente: 
# Se calculan los residuos para cada observación y se representan (scatterplot). Si las observaciones 
# siguen la línea del modelo, los residuos se deben distribuir aleatoriamente entorno al valor 0.

# La función lm() calcula y almacena los valores predichos por el modelo y los
# residuos.
datos$prediccion <- modelo_lineal$fitted.values
datos$residuos   <- modelo_lineal$residuals
head(datos)

ggplot(data = datos, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

#Los residuos se distribuyen de forma aleatoria entorno al 0 por lo que se acepta la linealidad. 

## Distribución normal de los residuos: 
# Los residuos se deben distribuir de forma normal con media 0. Para comprobarlo se recurre a histogramas,
# a los cuantiles normales o a un test de contraste de normalidad.

ggplot(data = datos, aes(x = residuos)) +
  geom_histogram(aes(y = ..density..)) +
  labs(title = "histograma de los residuos") +
  theme_light()

qqnorm(modelo_lineal$residuals)
qqline(modelo_lineal$residuals)

shapiro.test(modelo_lineal$residuals)
# Tanto la representación gráfica como el contraste de hipótesis confirman la distribución normal de los 
#residuos. 

## Varianza constante de los residuos (Homocedasticidad): 
# La variabilidad de los residuos debe de ser constante a lo largo del eje X. Un patrón cónico es 
# indicativo de falta de homogeneidad en la varianza.

ggplot(data = datos, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  geom_smooth(se = FALSE, color = "firebrick") +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Test de Breush-Pagan

library(lmtest)
bptest(modelo_lineal)

# Ni la representación gráfica ni el contraste de hipótesis muestran evidencias que haga sospechar falta de
# homocedasticidad. 

## Autocorrelación de residuos: 
  
# Cuando se trabaja con intervalos de tiempo, es muy importante comprobar que no existe aoutocorrelación 
#de los residuos, es decir que son independientes. Esto puede hacerse detectando visualmente patrones en 
# la distribución de los residuos cuando se ordenan según se han registrado o con el test de Durbin-Watson

ggplot(data = datos, aes(x = seq_along(residuos), y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_line(size = 0.3) +
  labs(title = "Distribución de los residuos", x = "index", y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

#En este caso, la representación de los residuos no muestra ninguna tendencia. 

#6.Identificación de valores atípicos: outliers, leverage y observaciones influyentes 

library(ggrepel)
library(dplyr)
datos$studentized_residual <- rstudent(modelo_lineal)
ggplot(data = datos, aes(x = prediccion, y = abs(studentized_residual))) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
  # se identifican en rojo observaciones con residuos studentized absolutos > 3
  geom_point(aes(color = ifelse(abs(studentized_residual) > 3, 'red', 'black'))) +
  scale_color_identity() +
  #se muestra el equipo al que pertenece la observación atípica
  geom_text_repel(data = filter(datos, abs(studentized_residual) > 3),
                  aes(label = equipos)) +
  labs(title="Distribución de los residuos studentized", x="predicción modelo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

datos %>% filter(abs(studentized_residual) > 3)

which(abs(datos$studentized_residual) > 3)

# El estudio de los residuos studentized identifica al equipo de New_Y. como una 
#posible observación atípica. Esta observación ocupa la posición 7 en la tabla de datos.

# El hecho de que un valor sea atípico o con alto grado de leverage no implica que sea 
# influyente en el conjunto del modelo. Sin embargo, si un valor es influyente, suele ser 
# o atípico o de alto leverage.

library(car)
summary(influence.measures(model = modelo_lineal))

influencePlot(model = modelo_lineal)

# detectan la observación 7 como atípica pero no significativamente influyente. Sí detectan
# como influyente la observación que ocupa la segunda posición. Para evaluar hasta qué punto
# condiciona el modelo, se recalcula la recta de mínimos cuadrados excluyendo esta observación.

ggplot(data = datos, mapping = aes(x = Nro.de.bateos, y = Runs)) +
  geom_point(color = "grey50", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  #se resalta el valor excluido
  geom_point(data = datos[2,], color = "red", size = 2) +
  #se añade la nueva recta de mínimos cuadrados
  geom_smooth(data = datos[-2,], method = "lm", se = FALSE, color = "blue") +
  labs(title  =  'Diagrama de dispersión', x  =  'número  de bateos') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# La eliminación del valor identificado como influyente apenas cambia la recta de mínimos cuadrados.
# Para conocer con exactitud el resultado de excluir la observación se comparan las pendientes de ambos
# modelos.

lm(formula = Runs ~ Nro.de.bateos, data = datos)$coefficients

lm(formula = Runs ~ Nro.de.bateos, data = datos[-2,])$coefficients

## Dado que se satisfacen todas las condiciones para considerar válido un modelo de regresión lineal 
#por mínimos cuadrados y que el p-value indica que el ajuste es significativo, se puede aceptar el modelo
#lineal. A pesar de ello, el valor de R2 no es muy alto por lo que el número de bateos no es muy buen 
# predictor del número de runs. 

#Evaluación de los residuos de un modelo lineal simple mediante gráficos R

par(mfrow = c(1,2))
plot(modelo_lineal)
