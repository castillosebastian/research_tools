#EJERCICIO 3
#a) Identifique y analice el conjunto de datos

library(datarium)
data("marketing")
head(marketing)
str(marketing)

summary(marketing)

#b)Graficar
library(ggplot2)
ggplot(marketing, aes(y=sales, x=youtube))+
  geom_point()+stat_smooth(method = "lm")

#c) Plantear el modelo

model <- lm(sales ~ youtube, data = marketing)
summary(model)

names(model)
model$coefficients
Y=beta_0 + beta_1*X + E
Y=8.43911226 + 0.04753664 * X + E

# Tabla ANOVA
library(car)
Anova(model)

#Diagnóstico (LINE)

par(mfrow=c(2,2))
plot(model)

#f) Números de ventas
new <- data.frame(youtube=c(10,100,300))
predict(model,newdata = new)

#g) Intervalo de confianza
confint(model)

predict(model, newdata=new, interval = "confidence")
predict(model, newdata=new, interval = "prediction")

pred.int<-predict(model, interval = "prediction")
mydata<-cbind(marketing, pred.int)
library(ggplot2)
p<-ggplot(mydata, aes(youtube,sales))+
  geom_point()+
  stat_smooth(method = "lm")

p+geom_line(aes(y=lwr), color="red", 
            linetype ="dashed")+
  geom_line(aes(y=upr), color="red", 
            linetype ="dashed")

########
#I confianza para la media y de prediccion para la respuesta
(res.pred1 <- predict(model,
                      list(youtube= c(10:300,100)),
                      interval="confidence"))
(res.pred2 <- predict(model,
                      list(youtube= c(10:300,100)),
                      interval="prediction"))
# #Graficando bandas de confianza y bandas de prediccion
par(mfrow=c(1,1))
plot(sales~youtube, data=marketing, xlim=c(10,300), ylim=c(5,30), pch=20)
abline(model,col="blue")
lines(c(10:300,100), res.pred1[, 2], lty = 2)
lines(c(10:300,100), res.pred1[, 3], lty = 2)
lines(c(10:300,100), res.pred2[, 2], lty = 2, col = "red")
lines(c(10:300,100), res.pred2[, 3], lty = 2, col = "red")




##EJERCICIO 4
library(datarium)
data("marketing")
head(marketing)

#a) Graficamos

library(ggplot2)
ggplot(data = marketing, aes(y=sales, x=youtube))+
  geom_point(aes(color=facebook))

par(mfrow=c(1,1))
library(plot3D)
scatter3D(x=marketing$youtube,
          y=marketing$facebook,
          z=marketing$sales,
          colvar = marketing$newspaper,
          clab = "newpaper",
          xlab="Youtube",
          ylab="Facebook",
          zlab="Sales")

#Matriz de correlaciones
M <- cor(marketing)
M

library(corrplot)
corrplot(M, method = "number", type = "upper")

pairs(sales ~., data = marketing, col=c("blue","red",
                                        "green"))
#utilizar en datos ausentes NA

#b)Plantear el modelo
model_all <-lm(sales ~ youtube + facebook + newspaper,
               data=marketing)
summary(model_all)

sales=3.52+0.04*youtube+0.188*facebook-0.001*newspaper

#c)Determine el RSE y la tasa de error
summary(marketing)

sigma(model_all) #RSE o SIGNA

sigma(model_all)/mean(marketing$sales)*100 # tasa error

# Podemos determinar las tres versiones de coeficiente R^2
summary(model_all)$r.squared # R^2 multiple
summary(model_all)$adj.r.squared #R^2 ajustado
#Calculamos el R parcial
library(rsq)
rsq.partial(model_all)

#d) Intervalos de confianza
confint(model_all)
# Carece de significatividad la v.regresora newspaper por contener al cero

#e) Utilice metodos basados en criterios de informaci?n
library(leaps)
#seleccionar los predictores metodo subsets

modelo_subset <- regsubsets(sales ~., #todas variables explicativas
                            data=marketing,
                            nbest=1, #mostrar 1 mejor modelo 
                            nvmax=NULL, #no hay l?mite en el n?mero de variables
                            method="backward")
summary(modelo_subset)

model_red <- lm(sales ~ youtube + facebook, data = marketing)
summary(model_red)

AIC(model_all,model_red)

BIC(model_all, model_red)

#El modelo reducido se lo considera como el modelo m?s apto.

#f)Plantee y extraiga conclusiones de la tabla de ANOVA
#H0:el modelo NO mejora agregando dicha variable predictora
#H1:el modelo mejora agregando la variable predictora

library(stats)
anova(model_all,model_red)
anova(model_all)

#g)Metodo de selecci?n por pasos
library(MASS)
step.model <- stepAIC(model_all,
                      direction = "backward",#hacia atr?s
                      trace = FALSE) #muestra todos los pasos
summary(step.model)

step.model_1 <- stepAIC(model_all,
                      direction = "forward",#hacia adelante
                      trace = FALSE) #muestra todos los pasos
summary(step.model_1)

step.model_2 <- stepAIC(model_all,
                        direction = "both",#ambas direcciones
                        trace = FALSE) #muestra todos los pasos
summary(step.model_2)

#h)Eval?e la multicolinealidad del modelo
library(car)
vif(model_all)
vif(model_red)

#Importancia de los predictores
library(relaimpo)
crlm<-calc.relimp(model_all)
crlm
plot(crlm)
#La variable newspaper carece de importancia en el modelo general

#i)Diagn?stico del modelo
par(mfrow=c(2,2))
library(ggfortify)
autoplot(model_red)

plot(model_red)
par(mfrow=c(1,1))
plot(model_red,4)

#Linealidad
#grafico del componente+residuos
crPlots(model_red)

#Independencia
#H0:rho=0, no hay correlaci?n entre los residuos
durbinWatsonTest(model_red)
#No rechazo H0, NO hay correlaci?n

#Normalidad
qqPlot(model_red, main = "QQ Plot")
#Se obserban valores centrales que se despegan de la curva normal te?rica

#Homocedasticidad
library(MASS)
#H0:varianza de los errores constante
ncvTest(modelo_red)
#Rechazo H0, es decir la varianza no es constante hay heterocedasticidad
#Varianzas diferentes
spreadLevelPlot(model_red)

##An?lisis de los diferentes tipos de errores
library(broom)
model.diag.metrics<-augment(model_red)
head(model.diag.metrics)

#Detecci?n de los valores at?picos
#Observamos si existen errores estandarizados en valor absoluto >3
which(abs(model.diag.metrics$.std.resid)>3)

#Los outliers detectados son 6 y 131

#Deteccion de valores de apalancamiento
#Valores mayores 2(k+1)/n, k:par?metros y n:n?observaciones
nrow(marketing)
#n=200 y k=2
2*(2+1)/200
which(abs(model.diag.metrics$.hat)>2*(2+1)/200)
#Se detectaron los valores de apalancamiento 6 y 176

model.diag.metrics[abs(model.diag.metrics$.hat)>2*(2+1)/200,]
#Se observa que estos valores no son significativos, por lo que no hay que preocuparse

##Valores Influyentes
#Valores absolutos de distancia de COO?k 4/(n-k-1)
4/(200-2-1)
which(abs(model.diag.metrics$.cooksd)>4/(200-2-1))
#Se observan 16 valores que aparentan ser influyentes
#Para identificar
data.frame(ID=which(abs(model.diag.metrics$.cooksd)>4/(200-2-1)),
           model.diag.metrics[abs(model.diag.metrics$.cooksd)>4/(200-2-1),])
#Ninguno supera la unidad, se analizar? brevemente

library(car)
outlierTest(model_red)
par(mfrow=c(1,1))
avPlots(model_red)
influencePlot(model_red, main ="Influence Plot")

##### Tablita comparativa
library(leaps)
#selecci?n de predictores
subset<-regsubsets(as.matrix(marketing[,-4]), #variables explicativas
                   marketing[,"sales"], #variable respuesta
                   nbest = 1, #mostrar 1 modelo
                   nvmax = NULL,
                   method = "backward")
summary(subset)

res.sum <-summary(subset)
data.frame(Adj.R2=which.max(res.sum$adjr2),
           CP=which.max(res.sum$cp),
           BIC=which.max(res.sum$bic))

##Ejercicio 6
#a)Cargamos los datos
library(readr)
bike<-read_csv("bike.csv")
set.seed(123)
#Primeras 6 observaciones
head(bike)
#Estructura de los datos
str(bike)

#Transformaci?n de las variables categ?ricas a factor
bike$season <-as.factor(bike$season)
bike$mnth <-as.factor(bike$mnth)
bike$holiday <-as.factor(bike$holiday)
bike$weekday <-as.factor(bike$weekday)
bike$workingday <-as.factor(bike$workingday)
bike$weathersit <-as.factor(bike$weathersit)

library(dplyr)
glimpse(bike)

#Estad?sticos b?sicos
summary(bike)
#no se observan datos faltantes NA

#Dividimos 
library(caret)
dim(bike)
# dividimos el conjunto de datos
index <-createDataPartition(bike$cnt,p=0.80, list=FALSE)
train <-bike[index,]
test <-bike[-index,]

dim(train)
dim(test)

#c) Centrado y escalado
Center_Scale_train <-preProcess(train[,-10],
                                method = c("center","scale"))
train<-predict(Center_Scale_train,newdata = train)
summary(train)

#d)Crear variables dummy
d_train<-dummyVars(~.,data = train)

#Elimino varianza cero
nearZeroVar(train,saveMetrics = TRUE)

bike$holiday <-NULL

#e) Planteo el modelo
library(leaps)
modelo_subset <-regsubsets(cnt ~., data = train,
                           nbest = 1, nvmax = NULL,
                           method = "backward")
summary(modelo_subset)

#f)
modelo_red <-stepAIC(modelo,direction = "backward",
                     trace = FALSE)
summary(modelo_red)
modelo_nuevo <-lm(cnt~ season.1+mnth.6+mnth.7+weathersit.1+
                    weathersit.2+temp+hum+windspeed,
                  data = train)
summary(modelo_nuevo)

#g)ANOVA
AIC(modelo,modelo_red,modelo_nuevo)
BIC(modelo,modelo_red,modelo_nuevo)

#h)
vif(modelo_red)
vif(modelo_nuevo)

#i)
crlm=calc.relimp(modelo_red)
plot(crlm)

#j)Diagnostico
plot(modelo_red)
