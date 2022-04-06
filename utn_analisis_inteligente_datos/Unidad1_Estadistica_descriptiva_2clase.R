##Instalamos las lilbrerías
#PARA ALGUNA DE ELLAS NECESITAREMOS CARGAR RTOOLS
#https://cran.rstudio.com/bin/windows/Rtools/
install.packages("aplpack") # permite hacer caritas de Chernov
install.packages("corrplot") # permite personalizar colores y estilos de fuente para gráficos
install.packages("ggplot2") # permite realizar gráficos con movimiento
install.packages("plotrix") # permite realizar gráficos de torta con volumen
install.packages("rgl") # permite realizar gráficos en 3D
install.packages("tcltk") # posee comandos de lenguaje de herramientas para la creación de interfases gráficas
install.packages("tcltk2") # posee comandos adicionales a tcltk
installed.packages() # muestra los paquetes que están instalados en el dispositivo
##Cargamos las librerías
#observar lo que pasa en cada caso.
#si no estuviera instalarlo!
library(readxl)
library(grDevices) # Equipos gráficos y soporte para la base y la red de gráficos
library(tcltk)
library(aplpack)
library(corrplot)
library(ggplot2)
library(plotrix)
library(rgl)
library(tcltk2)
library(GGally)
library(tidyverse)
#Modo antiguo de graficación
### Gráfico de una recta y una parábola

x=seq(-3,3,0.5) # asigna valores a la variable x
y=x^2 # define la función cuadrática
w=x+2 # define la función lineal
plot(x,y,col=2,type="p") # grafica los puntos de la función y en rojo
plot(x,y,col=2,type="o") # idem anterior y superpone una línea
plot(x,y,col=2,type="l") # grafica la función y con una línea continua
plot(x,y,col=2,lwd=2,type="l") # idem anterior pero con distinto grosor de línea
plot(x,y,col=2,lwd=2,type="l",ylim=c(-1,10)) # cambia los límites del eje y
abline(0,0) # dibuja una línea horizontal que pasa por el origen de coordenadas
segments(0,-1,0,10) # dibuja una línea vertical que pasa por el origen de coordenadas
points(x,w,col=4,type="l",lwd=2) # superpone la función w en azul
text(locator(1),col=2,"parábola") # coloca un cartel en la posición indicada
text(locator(1),col=4,"recta")
title("Gráfico de una recta y una parábola") # agrega un título al gráfico activo


### Gráficos Estadísticos

### Diagrama circular-Variables Categóricas

IMCinfantil <- read_excel("E:/Documentos/Calzolari/AID-CHAN/IMCinfantil.xlsx")
View(IMCinfantil)
IMCinfantil <- read.csv2("E:/Documentos/Calzolari/AID-CHAN/IMCinfantil.csv") # importa la base IMCinfantil
attach(IMCinfantil) # carga la base en la memoria activa

frec.catpeso<-table(CatPeso) # construye la distribución de frecuencias
pie(frec.catpeso) # dibuja el diagrama circular
pie(frec.catpeso, col=rainbow(25)) # cambia la gama de colores
pie(frec.catpeso, col=rainbow(25),font=8) # cambia el tipo de letra
pie(frec.catpeso, col=rainbow(25),font=8,cex=1.5) # cambia el tamaño de letra
pie(frec.catpeso, col=rainbow(25),font=8,cex=1.5,radius=1) # cambia el tamaño de la torta
pie(frec.catpeso, col=rainbow(25),font=8,cex=1.5,radius=1,border=F) # quita el borde
pie(frec.catpeso, col=rainbow(25),font=8,cex=1.5,radius=1,border=F,main="Gráfico de Torta") # pone nombre


etiquetas<-c("Deficiente","Normal","Obeso","Con sobrepeso") # define etiquetas
pct<-round(frec.catpeso/sum(frec.catpeso)*100) # calcula las frecuencias porcentuales
etiquetas<-paste(etiquetas,pct) # agrega los porcentajes a las etiquetas 
etiquetas<-paste(etiquetas,"%",sep="") # agrega el símbolo % a los porcentajes 
pie(frec.catpeso,labels =etiquetas,col=heat.colors(4,alpha=1)) # otra manera de asignar una paleta de colores
pie(frec.catpeso,labels =etiquetas,col=terrain.colors(4,alpha=1)) # otra manera de asignar una paleta de colores
pie(frec.catpeso,labels =etiquetas,col=topo.colors(4,alpha=1)) # otra manera de asignar una paleta de colores
pie(frec.catpeso,labels =etiquetas,col=cm.colors(4,alpha=1)) # otra manera de asignar una paleta de colores
pie(frec.catpeso,labels =etiquetas,col=cm.colors(4,alpha=1),main="Diagrama circular con etiquetas")

### con volumen perspectiva y sombra

pie3D(frec.catpeso) # grafica una torta con volumen
pie3D(frec.catpeso,labels=etiquetas)
pie3D(frec.catpeso,labels=etiquetas,explode=0.1) # separa los sectores
pie3D(frec.catpeso,labels=etiquetas,explode=0.1,labelcex=0.9) # cambia el tamaño de las etiquetas
pie3D(frec.catpeso,labels=etiquetas,explode=0.1,labelcex=0.9,radius=1.5)
pie3D(frec.catpeso,labels=etiquetas,explode=0.1,labelcex=0.9,radius=1.5,height=0.2) # cambia el alto de la torta
pie3D(frec.catpeso,labels=etiquetas,explode=0.1,labelcex=0.9,radius=1.5,height=0.2,shade=0.6) # sombrea
pie3D(frec.catpeso,labels=etiquetas,explode=0.1,labelcex=0.9,radius=1.5,height=0.2,shade=0.6,col=terrain.colors(4:8,alpha=1))

### Diagramas de barras -barras superpuestas - barras adyacentes


p1=ggplot(IMCinfantil,aes(x=CatPeso))+geom_bar() #diagrama de barras básico
p1
p1b=ggplot(IMCinfantil,aes(x=CatPeso))+geom_bar(fill="red",alpha=0.2) #diagrama de barras básico
p1b
p2=ggplot(IMCinfantil,aes(x=CatPeso))+geom_bar(aes(fill=SEXO))# barras superpuestas por sexo
p2
p3=ggplot(IMCinfantil,aes(x=CatPeso))+geom_bar(aes(fill=SEXO))  +
  coord_flip() +
  theme(legend.position = "top")
p3 # horizontales

p4=ggplot(IMCinfantil,aes(x=CatPeso))+
  geom_bar( aes(x=CatPeso,fill = SEXO),position=position_dodge()) #barras adyacentes
p4


### Gráfico de mosaicos
attach(IMCinfantil) ##pone disponible esta base de datos en la memoria
tabla2=table(EDAD,CatPeso)
par(bg="aliceblue")
mosaicplot(tabla2) # hace un gráfico de mosaicos simple
 mosaicplot(tabla2[,c(1,2,4,3)],col=c("cadetblue1","deepskyblue","deepskyblue4","cyan3"),main="Gráfico de Mosaicos",ylab="Categoría de Peso",xlab="Edad",
            cex=0.8) # este gráfico permite visualizar una tabla de contingencia

### Grafico de bastones-Variables cuantitativas discretas

Modelos<-2010:2016 # ingresa los modelos de los autos
Ventas<-c(2,3,7,4,9,0,5) # ingresa las frecuencias de las ventas de cada modelo

datos.autos=data.frame(Modelos,Ventas)
ggplot(datos.autos,aes(x=Modelos))+
  geom_segment(aes(x=Modelos,y=rep(0,7),xend=Modelos,yend=Ventas,colour="coral"),
               lineend = "round",size=2)

### Diagramas de dispersion en dos y tres variables

avispas<-read.csv("E:/Documentos/Calzolari/AID-CHAN/avispas.csv", sep=";")
detach()
attach(avispas)

plot1=ggplot(avispas, aes(x =X1, y = X2)) +
  geom_point()
plot1
grupo=factor(avispas$Grupo)
plot2=ggplot(avispas, aes(x =X1, y = X2,col=grupo)) +
  geom_point()
plot2

detach()
attach(IMCinfantil)

##### Histogramas

attach(IMCinfantil)

plot3=ggplot(IMCinfantil,aes(x=PESO))+geom_histogram(binwidth = 8)
plot3
plot4=ggplot(IMCinfantil,aes(x=PESO))+geom_histogram(binwidth = 8,color="cadetblue",alpha=0.2)
plot4
plot5=ggplot(IMCinfantil,aes(x=PESO))+geom_histogram(binwidth = 8,fill="cadetblue",alpha=0.2)
plot5
plot6=ggplot(IMCinfantil,aes(x=PESO))+geom_histogram(binwidth = 8,color="aquamarine3",fill="cadetblue",alpha=0.2)
plot6
sexo=factor(IMCinfantil$SEXO)
plot7=ggplot(IMCinfantil,aes(x=PESO,fill=sexo))+geom_histogram(binwidth = 8)
plot7
plot8=ggplot(IMCinfantil,aes(x=TALLA,fill=sexo))+geom_density()
plot8
plot9=ggplot(IMCinfantil,aes(x=PESO,fill=sexo,color=sexo))+geom_freqpoly(binwidth = 8)
plot9
catpeso=factor(CatPeso)
plot10=ggplot(IMCinfantil, aes(x=PESO)) + facet_wrap(~SEXO) +
  geom_histogram(binwidth =8)
plot10
plot11=ggplot(IMCinfantil, aes(x=PESO,fill=catpeso)) + facet_wrap(~SEXO) +
  geom_histogram(binwidth =8)
plot11
plot12=ggplot(IMCinfantil, aes(x=PESO)) + facet_wrap(~SEXO) +
  geom_freqpoly(binwidth =8)
plot12

### Funcion de distribucion empírica

attach(avispas)
par(mfrow=c(1,2)) # dividimos el área de gráficos en dos columnas
plot.ecdf(X1[Grupo=="Chaq.amar"],col="magenta",main="FDE Chaqueta amarilla",ylab="F(x)") # dibuja la función de distribución empírica
plot.ecdf(X1[Grupo=="Neg.peq"],col="chartreuse1",main="FDE Negra pequeña",ylab="F(x)") 

par(mfrow=c(1,1)) # unifica la pantalla de gráficos
n=length(X1)
plot(stepfun(1:(n-1),sort(X1)),main="Función escalonada") # otra manera de definir y graficar la función acumulada
plot(stepfun(1:(n-1),sort(X1)),main="Función escalonada",col="coral",lwd=2,ylab="F(x)")


###### Comparación de distribuciones

y=rchisq(200,df=5) # genera una muestra aleatoria Chi cuadrado con 5 grados de libertad de tamaño 200
qqnorm(y) # compara los cuantiles de esta muestra con los de una normal
qqnorm(y,col="mediumorchid1",pch=18,main="Gráfico cuantil-cuantil con una normal",xlab="Cuantiles teóricos",ylab="Cuantiles muestrales")
qqline(y,col="navy",lwd=1) # agrega una linea bajo el supuesto de normalidad

attach(IMCinfantil)
cc.muj=CC[SEXO=="F"] # guarda los datos de circunferencia de cintura de las mujeres
cc.var=CC[SEXO=="M"] # guarda los datos de circunferencia de cintura de los varones
qqplot(cc.muj,cc.var) # produce un gráfico cuantil-cualtil que si resulta una línea recta las distribuciones coinciden
qqplot(cc.muj,cc.var,col=8:9,pch=18,main="Comparación de distribuciones por sexo",xlab="Circunferencia de cintura en mujeres",ylab="Circunferencia de cintura en varones")

media=mean(c(cc.muj,cc.var))
desvio=sd(c(cc.muj,cc.var))
qqline(c(cc.muj,cc.var), 
       distribution=function(p){qnorm(p,mean=media,sd=desvio)},col="chocolate1") 
# agrega la línea en el caso en que las distribuciones fueran iguales

### Boxplots o Diagramas de caja - Boxplots Paralelos (Variables Continuas)

muestra=c(14,18,24,26,35,39,43,45,56,62,68,92,198)
Md=median(muestra)
summary(muestra)
Q1=quantile(muestra,0.25)
Q3=quantile(muestra,0.75)
DI=Q3-Q1
Q3+1.5*DI
Q1-1.5*DI
Q3+3*DI
Q1-3*DI

attach(IMCinfantil)
plot12=ggplot(IMCinfantil,aes(y=PESO))+geom_boxplot()
plot12 # básico
plot12b=ggplot(IMCinfantil,aes(x=SEXO,y=PESO))+geom_boxplot()
plot12b # por sexo
plot13=ggplot(IMCinfantil,aes(x=SEXO,y=PESO,color=SEXO))+geom_boxplot()
plot13 # con borde de color
plot14=ggplot(IMCinfantil,aes(x=SEXO,y=PESO,fill=SEXO))+geom_boxplot()+ coord_flip()
plot14 # relleno y horizontal

### Graficos para visualizar la  correlacion

attach(IMCinfantil)
base.niños=data.frame(EDAD,PESO,TALLA,IMC,CC) # arma una sub-base con las variables numéricas de IMCinfantil
base.niños$CC=max(base.niños$CC)-base.niños$CC # cambiamos una variable para que correlacione en forma negativa con las restantes
M=cor(base.niños) # calcula la matriz de correlación de las variables de la base
M
corrplot(M,method="circle") # representa la matriz de correlaciones mediante círculos
corrplot(M,method="square") # representa la matriz de correlaciones mediante cuadrados
corrplot(M,method="ellipse") # representa la matriz de correlaciones mediante elipses
corrplot(M,method="number") # representa la matriz de correlaciones mediante números
corrplot(M,method="shade") # representa la matriz de correlaciones mediante sombreandos
corrplot(M,method="pie") # representa la matriz de correlaciones mediante gráficos de torta
corrplot(M,type="upper") # representa sólo la parte superior de la matriz de correlación
corrplot(M,type="lower") # representa sólo la parte inferior de la matriz de correlación
corrplot(M,method="ellipse",type="upper") # permite combinaciones de estilos
corrplot.mixed(M) # representa la matriz de correlación combinando cículos y números
corrplot.mixed(M,lower="circle",upper="shade") # permite combinaciones de estilos por bloques


### Caritas de Chernoff

par(mfrow=c(1,1),adj=0)
par(col.main="blue") # cambia el color de los textos
galle=read.csv("E:/Documentos/Calzolari/AID-CHAN/galletitas.csv", sep=";")
galle.salad=galle[c(1:3,7,15:17),] # agrupa las galletitas saladas
galle.dulce=galle[c(4:6,8:14),] # agrupa las galletitas dulces
faces(galle.salad[,2:6]) # hace un gráfico con las caras de Chernoff
faces(galle.salad[,2:6],nrow.plot=3) # ajusta el alto de las caras
faces(galle.salad[,2:6],ncol.plot=4) # acomoda la cantidad de caras por fila
faces(galle.salad[,2:6],face.type=0) # grafica las caras sin color
faces(galle.salad[,2:6],face.type=2) # cambia el estilo de cara
faces(galle.salad[,2:6],labels=galle.salad[,1]) # etiqueta las caras 
title("Caritas de Chernoff saladas") # ponemos título

faces(galle.dulce[,2:6],nrow.plot=3,ncol.plot=5,face.type=2,labels=galle.dulce[,1])
title("Galletitas Dulces")

### Gráfico de estrellas

par(col.main="black",adj=0.5)
row.names(galle.salad)=galle.salad[,1] # coloca etiquetas al gráfico
stars(galle.salad[,2:6]) # hace un gráfico de estrellas
stars(galle.salad[,2:6],full=T) # dibuja con volumen
stars(galle.salad[,2:6],full=F) # dibuja en perspectiva
stars(galle.salad[,2:6],radius=F) # omite aristas
stars(galle.salad[,2:6],axes=T) # dibuja los ejes
stars(galle.salad[,2:6],frame.plot=T) # recuadra el gráfico 
stars(galle.salad[,2:6],draw.segments=T) # cambia el estilo 
stars(galle.salad[,2:6],col.lines=rainbow(15)) # cambia el color a las líneas
stars(galle.salad[,2:6],cex=0.8,flip.labels=T) # cambia la posición de las etiquetas
stars(galle.salad[,2:6],cex=0.8,flip.labels=F,len=0.8) # cambia el tamaño de las estrellas
stars(galle.salad[,2:6],cex=0.8,flip.labels=F,len=0.8,col.stars=terrain.colors(7)) # colorea los interiores de las estrellas
stars(galle.salad[,2:6],cex=0.8,flip.labels=F,len=0.8,col.stars=terrain.colors(7),ncol=4,frame.plot=T,main="Galletitas saladas")

row.names(galle.dulce)=galle.dulce[,1]     
stars(galle.dulce[,2:6],full=T,draw.segments=T,cex=0.9,len=0.8,ncol=4,frame.plot=T,main="Galletitas dulces")

cars=mtcars[1:9,]
stars(cars,cex=0.7,col.stars=c("red","green","orange","gold","blue",
                        "yellow", "pink","purple","cyan"))
title("Gráfico de Estrellas")

par(mfrow=c(1,3))
stars(galle.salad[,2:6],ncol=2,full=F) 
stars(galle.salad[,2:6],ncol=2,axes=T)
stars(galle.salad[,2:6],ncol=2,col.lines=rainbow(15))

