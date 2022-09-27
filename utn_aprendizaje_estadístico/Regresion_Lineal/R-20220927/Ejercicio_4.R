## EJERCICIO 4

library(readxl)
demanda <- read.delim2("~/Posgrado/demanda.csv")
View(demanda)
head(demanda)
str(demanda)

summary(demanda[, 1:10])
pairs(demanda[, 1:10])

#Matriz de correlaciones
round(cor(x=demanda, method = "pearson"), 3)

#Representarlo gráficamente
library(GGally)
ggpairs(demanda, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

# Modelo.
modelo<-lm(consumo ~ anio+precio+IPC+PNC+PUC+PPT+PD+PN+PS, data = demanda) 
summary(modelo) # Modelo significativo y R2 alto.

## El modelo con todas las variables introducidas como predictores tiene un R2 alta (0.9975), es capaz de 
# explicar el 99,75% de la variabilidad observada en los años. 
# El p-value del modelo es significativo (2.2e-16) por lo que se puede aceptar que el modelo no es por azar,
# al menos uno de los coeficientes parciales de regresión es distinto de 0. Muchos de ellos no son 
# significativos, lo que es un indicativo de que podrían no contribuir al modelo. 

## Hacemos dummies de la variable EPOCA
modRM2<-lm(consumo~anio+factor(epoca),data = demanda)
summary(modRM2) #este modelo considera epoca1 como categ base y ajusta rectas de = pendiente
modRM3<-lm(consumo~anio+factor(epoca)+anio*factor(epoca),data = demanda)
summary(modRM3)#ajusta la interaccion tambien, o sea permite distinta pendiente
modRM4<-lm(consumo~anio+precio+IPC+factor(epoca)+anio*factor(epoca),data = demanda)
summary(modRM4) #este considera tambien otras variables
#podemos comparar los modelos usando R^2 ajustado, si se chequean antes los supuestos

# Selección de los mejores predictores 

library(leaps)
forward <- regsubsets(consumo~., data = demanda, nvmax = 10,
                                      method = "forward")
forward
# Se identifica el valor máximo de R ajustado
which.max(summary(forward)$adjr2)

coef(object = forward, 9)

summary(forward) #la salida muestra que se queda con las diez primeras variables
#ver mejor las opciones para que de el modelo.

## En este caso se van a emplear la estrategia de stepwise mixto. El valor matemático empleado para 
#determinar la calidad del modelo va a ser Akaike(AIC).
step(object = modelo, direction = "both", trace = 1)

#El mejor modelo resultante del proceso de selección ha sido:
modelo_1<-(lm(consumo~anio+precio+IPC+PNC+PUC+PPT+PN+PS, data = demanda))
summary(modelo_1)

#Intervalo de Confianza
##Es recomendable mostrar el intervalo de confianza para cada uno de los coeficientes parciales de regresión:
confint(lm(consumo~anio+precio+IPC+PNC+PUC+PPT+PN+PS, data = demanda))

#Validación de condiciones para la regresión múltiple lineal 
##Relación lineal entre los predictores numéricos y la variable respuesta: 
# Esta condición se puede validar bien mediante diagramas de dispersión entre la variable dependiente y cada 
# uno de los predictores (como se ha hecho en el análisis preliminar) o con diagramas de dispersión entre 
#cada uno de los predictores y los residuos del modelo. Si la relación es lineal, los residuos deben de 
#distribuirse aleatoriamente en torno a 0 con una variabilidad constante a lo largo del eje X. 
#Esta última opción suele ser más indicada ya que permite identificar posibles datos atípicos.

library(ggplot2)
library(gridExtra)
plot1 <- ggplot(data = demanda, aes(anio, modelo_1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = demanda, aes(precio, modelo_1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = demanda, aes(IPC, modelo_1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = demanda, aes(PNC, modelo_1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot5 <- ggplot(data = demanda, aes(PUC, modelo_1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot6 <- ggplot(data = demanda, aes(PPT, modelo_1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot7 <- ggplot(data = demanda, aes(PN, modelo_1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot8 <- ggplot(data = demanda, aes(PS, modelo_1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8)

# Se cumple la linealidad para todos los predictores, a excepción de PS

##Distribución normal de los residuos: 
qqnorm(modelo_1$residuals)
qqline(modelo_1$residuals)

shapiro.test(modelo_1$residuals)
#Tanto el análisis gráfico como es test de hipótesis confirman la normalidad. 

#Variabilidad constante de los residuos (homocedasticidad): 
## Al representar los residuos frente a los valores ajustados por el modelo, los primeros se tienen que 
#distribuir de forma aleatoria en torno a cero, manteniendo aproximadamente la misma variabilidad a lo largo 
#del eje X. Si se observa algún patrón específico, por ejemplo forma cónica o mayor dispersión en los 
#extremos, significa que la variabilidad es dependiente del valor ajustado y por lo tanto no hay 
#homocedasticidad.

ggplot(data = demanda, aes(modelo_1$fitted.values, modelo_1$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()

library(lmtest)
bptest(modelo)

#No hay evidencias de falta de homocedasticidad. 
# No multicolinialidad: 
  
##Matriz de correlación entre predictores.

library(corrplot)
corrplot(cor(dplyr::select(demanda, anio,precio,IPC,PNC,PUC,PPT,PN,PS)),
         method = "number", tl.col = "black")

##Análisis de Inflación de Varianza (VIF): 
library(car)
vif(modelo_1)

#Existe correlación entre las variables. Hay predictores que muestren una correlación lineal muy alta 
# e inflación de varianza. 

##Autocorrelación:
library(car)
dwt(modelo_1, alternative = "two.sided")
#No hay evidencia de autocorrelación 

##Identificación de posibles valores atípicos o influyentes 

library(dplyr)
demanda$studentized_residual <- rstudent(modelo_1)
ggplot(data = demanda, aes(x = predict(modelo_1), y = abs(studentized_residual))) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
  # se identifican en rojo observaciones con residuos estandarizados absolutos > 3
  geom_point(aes(color = ifelse(abs(studentized_residual) > 3, 'red', 'black'))) +
  scale_color_identity() +
  labs(title = "Distribución de los residuos studentized",
       x = "predicción modelo") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

which(abs(demanda$studentized_residual) > 3)

#No se identifica ninguna observación atípica.

summary(influence.measures(modelo_1))

#En la tabla generada se recogen las observaciones que son significativamente influyentes en al menos uno de
#los predictores (una columna para cada predictor). Las tres últimas columnas son 3 medidas distintas para 
#cuantificar la influencia. 
#A modo de guía se pueden considerar excesivamente influyentes aquellas observaciones para las que:
## Leverages (hat): Se consideran observaciones influyentes aquellas cuyos valores hat superen 2.5((p+1)/n),
#siendo p el número de predictores y n el número de observaciones.
##Distancia Cook (cook.d): Se consideran influyentes valores superiores a 1.

#La visualización gráfica de las influencias se obtiene del siguiente modo:
influencePlot(modelo_1)

##Los análisis muestran varias observaciones influyentes (posición 22 y 24) que exceden los límites de 
#preocupación para los valores de Leverages o Distancia Cook. 
#Estudios más exhaustivos consistirían en rehacer el modelo sin las observaciones y ver el impacto.

##Conclusión
#El modelo lineal múltiple
#Consumo=-2.579e2+3.585anio-4.492e01precio+1.977e02IPC+8.529e01PNC-4.221e1PUC+1.893e1PPT+6.932e1PN+1.571e-02PS

#Es capaz de explicar el 99.75% de la variabilidad observada en lel consumo (R2: 0.9975, R2-Adjusted: 0.9964). 
#El test F muestra que es significativo (p-value: 2.2e-16). Se satisfacen todas las condiciones para este 
#tipo de regresión múltiple. Dos observaciones (posición 22 y 24) podrían estar influyendo de forma notable
#en el modelo.