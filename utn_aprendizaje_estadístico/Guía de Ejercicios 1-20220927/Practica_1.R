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

##EJERCICIO 4

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

#b)Plantear el modelo
model_all <-lm(sales ~ youtube+facebook +newspaper,
               data=marketing)
summary(model_all)

sales=3.52+0.04*youtube+0.188*facebook-0.001*newspaper

