#Naive Bayes con R con datos iris:
# primero exploramos los datos
data(iris)
pairs(iris[1:4],main="Iris Data (red=setosa,green=versicolor,blue=virginica)", pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])
summary(iris)

#usamos este paquete para construír un clasificador Naive Bayes:
install.packages("e1071")

library(e1071)
#clasificador<-naiveBayes(iris[,1:4], iris[,5]) 
clasificador<-naiveBayes(Species~.,iris)

#para hacer predicciones sobe los datos de entrenamiento (clasifica casi perfecto porque se ajustó el modelo en estos!):

table(predict(clasificador, iris[,-5]), iris[,5], dnn=list('predicted','actual'))

#para ver la distribución a priori de las clases
clasificador$apriori

#las variables predictoras son todas continuas, se suponen Normales. 
# Lo sig da la media y la desviación de las 3 distrib dependientes (condicionales)
clasificador$tables$Petal.Length

#para graficar las tres densidades normales ajustadas
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")

#para clasificar una nueva observación
#ajusto por ejemplo el modelo sin la última observación:
irisT= iris[-150,]
clasificador1<-naiveBayes(irisT[,1:4], irisT[,5]) 
predict(clasificador1, iris[150,],type = "raw")

