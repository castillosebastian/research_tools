setwd("C:/Users/Silvia/Desktop/DM-Parana/teóricas")
library(ggplot2)
library(readxl)
datos<- read_excel("Cancer2.xls")
datos$Y <- factor(datos$Y) #Y=no sobrevive
datos$SEXO <- factor(datos$SEXO)
datos$EST4 <- factor(datos$EST4)
datos$PSPI <- factor(datos$PSPI)
summary(datos)
#separo entrenamiento y validación
entreno=sample(seq(length(datos$Y)),length(datos$Y)*0.70,replace=FALSE) 
train=datos[entreno,]
test<-datos[-entreno,]

#a) ajusto el modelo con edad y pérdida de peso inicial
modelo1 <- glm(Y ~ PPI+EDAD, data = train,
                        family = binomial)
summary(modelo1)
anova(modelo1, test = "Chisq") #para analizar las devianzas

#b) ajusto el modelo con todas
modelo2 <- glm(Y ~ ., data = train,
               family = binomial)
summary(modelo2)

#c) selecciono variables con stepwise
backwards = step(modelo2) # Backwards selection es el default
# backwards = step(fullmod,trace=0) muestra sólo el último
formula(backwards)
modelo3<- glm(Y ~ PPI, data = train,
              family = binomial)
summary(modelo3)

#para hacer la curva ROC y sacar AUC. Hacerlo con modelo 1 y 3
library(ROCR)
predi3<-predict(modelo3,newdata=test,type="response")
pred.rl <- prediction(predi3, test$Y)
AUC3 <- performance(pred.rl, "auc")
#para que me de AUC:
AUC3@y.values #da mejor en el modelo 3

c.roc<- performance(pred.rl, "tpr","fpr")
plot(c.roc,main="Curva ROC",col="red")

Accu3 <- performance(pred.rl,"acc")
Accu3@y.values  #muestra con distintos puntos de corte
plot(Accu3)
sens<-performance(pred.rl,"tpr")
esp<-performance(pred.rl,"tnr")
plot(sens,col="green")
plot(esp,add=T,col="red")

# Para graficar la curva Lift con el modelo 3
perf <- performance(pred.rl,"lift","rpp")
plot(perf, main="Curva Lift",xlab="%datos",ylab="Lift",col="red")
#otra forma
library(lift)
plotLift(predi3, test$Y, cumulative = TRUE, n.buckets = 10)

#para hacer tabla de clasificacion
prediccion <- ifelse(predi3 >= 0.1, 1, 0) #fui cambiando a mano
tabla.clasif <- table(test$Y, prediccion)
accu <- sum(diag(tabla.clasif))/sum(tabla.clasif)

#con otra libreria
library(pROC)
roc(test$Y, predi3)
#names(roc(test$Y, predi3))
auc.rl<-roc(test$Y, predi3)$auc
#plot(roc(test$Y, predi3)$sensitivities,type="l",col="red") #hay que poner pcorte en eje x
#plot(roc(test$Y, predi3)$specificities,type="l",col="blue")

#con caret se tienen más indicadores usando confusionMatrix
library(caret)
#para ajustar un modelo previamente completando los NA en train
#preProcValues <- preProcess(train, method = c("knnImpute","center","scale"))
prediccion <- ifelse(predi3 >= 0.1, 1, 0)
prediccion<-as.factor(prediccion) #para poder usar lo que sigue deben ser de =tipo.
confusionMatrix(reference=test$Y,data =prediccion, mode='everything',positive = "1")

