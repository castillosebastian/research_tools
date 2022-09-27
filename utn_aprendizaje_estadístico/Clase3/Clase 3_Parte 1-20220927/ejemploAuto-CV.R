library(ISLR)
set.seed(1)
attach(Auto)

train=sample(392,196) #el dataset Auto tiene 392 obs, elige 192 al azar 
#ajusto una regresión simple en el conj de train
mod1=lm(mpg~horsepower,data=Auto,subset=train)
#calculo promedio de SSresiduales 
mean((mpg-predict(mod1,Auto))[-train]^2)
#idem con modelo cuadratico
mod2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(mod2,Auto))[-train]^2)
#idem modelo cubico
mod3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(mod3,Auto))[-train]^2)
#se ve que el que tiene menor valor es el cuadratico 

#lo que sigue repite lo mismo para ver que da igual
#set.seed(2)
#train=sample(392,196)
#mod1=lm(mpg~horsepower,subset=train)
#mean((mpg-predict(mod1,Auto))[-train]^2)
#mod2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
#mean((mpg-predict(mod2,Auto))[-train]^2)
#mod3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
#mean((mpg-predict(mod3,Auto))[-train]^2)

# Leave-One-Out Cross-Validation

glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)

library(boot)
modelo1=glm(mpg~horsepower,data=Auto)
# cv.glm(data, glmfit, cost, K) da el err de prediccion estimado
#costo en gral es (respuesta observada,predicha) el default es el prom de SSE
#K=n por default, o sea es LOO
cv.err=cv.glm(Auto,modelo1) 
names(cv.err)
cv.err$delta #el 1ero es la estimación de err, el 2do es un valor ajustado para compensar sesgo
cv.err$K

#ahora calculemos para modelos con polinomicos de distinto grado
cv.error=rep(0,5)
for (i in 1:5){
  modelo=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,modelo)$delta[1]
}
cv.error
#me dio 24.23151 19.24821 19.33498 19.42443 19.03321
# se ve que el cuadratico es menor

# k-Fold Cross-Validation
#lo que sigue calcula K-CV (K=10) para los distintos polinomios de grado i
set.seed(17)
cv.error.5=rep(0,5)
for (i in 1:5){
  modelo=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.5[i]=cv.glm(Auto,modelo,K=10)$delta[1]
}
cv.error.5
#se ve que en todos los casos el menor corresponde a cuadratico
