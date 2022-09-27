setwd("C:/Users/Silvia/Desktop/DM-Parana/teóricas")
getwd()
library(readxl)
data<- read_excel("Auto.xls")
summary(data)
#any(is.na(data$horsepower))
#which(is.na(data$horsepower))    
#datos <- data[complete.cases(data), ]
#otra forma
datos=na.omit(data)
dim(datos)
#lo anterior es para ver que hay casos NA y sacandolos quedan 392, que es lo que usa el ej de CV
#ahora datos corresponde a los datos completos (392)
#selecciono las variables a usar
datos2<-subset(datos,select=c(mpg,cylinders,displacement,horsepower,weight,acceleration))
entreno=sample(seq(length(datos2$mpg)),length(datos2$mpg)*0.80,replace=FALSE) 
train=datos2[entreno,]
test<-datos2[-entreno,]
#ajusto distintos modelos y evalúo indicadores para comparar. 
#no vamos a mirar supuestos, se sobreentiende que esto se hace
library(leaps)
modelo1<-regsubsets(mpg~.,data=train)
#lo que sigue es para que indique cuál de los modelos tiene mejor indicador
resumen <- summary(modelo1)
data.frame(
  Adj.R2 = which.max(resumen$adjr2),
  CP = which.min(resumen$cp),
  BIC = which.min(resumen$bic)
)
#Rta: Adj.R2 CP BIC con todo el dataset
### 1   3  2   2
#esto dice que segun Cp y Bic el mejor es el de 2 variables, segun AdjR2 el de 3 var
# con train queda el modelo 2 mpg~horsepower+weight
#lo que sigue es para graficar todos:

names(resumen)
par(mfrow=c(1,2))
plot(1:5,resumen$rss,xlab="Numero de Variables",ylab="RSS",type="l")
plot(resumen$adjr2,xlab="Numero de Variables",ylab="R2ad",type="l")

par(mfrow=c(1,2))
plot(resumen$cp,xlab="Numero de Variables",ylab="Cp",type='l')
plot(resumen$bic,xlab="Numero de Variables",ylab="BIC",type='l')


#Si elijo el modelo con 2 variables mpg~horsepower+weight, evalúo SSE en test
modelo2<-lm(mpg~horsepower+weight,data=train)
#summary(modelo2)
predi<-predict(modelo2,newdata=test)
mean((test$mpg-predi)^2)
#Si elijo el modelo con 3 variables mpg~cylinders+horsepower+weight, evalúo SSE en test
modelo3<-lm(mpg~cylinders+horsepower+weight,data=train)
#summary(modelo3)
predi3<-predict(modelo3,newdata=test)
mean((test$mpg-predi3)^2)


####Para calcular PRESS
PRESS=function (x)
{#x es un objecto que sale de aplicar lm
  sum(resid(x)^2/(1 - lm.influence(x)$hat)^2)
}
PRESS(modelo2)
#se ve que el modelo 2 tiene menor valor