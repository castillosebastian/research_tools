##Instalamos las lilbrer�as
#PARA ALGUNA DE ELLAS NECESITAREMOS CARGAR RTOOLS
#https://cran.rstudio.com/bin/windows/Rtools/
install.packages("aplpack") # permite hacer caritas de Chernov
install.packages("corrplot") # permite personalizar colores y estilos de fuente para gr�ficos
install.packages("ggplot2") # permite realizar gr�ficos con movimiento
install.packages("plotrix") # permite realizar gr�ficos de torta con volumen
install.packages("rgl") # permite realizar gr�ficos en 3D
install.packages("tcltk") # posee comandos de lenguaje de herramientas para la creaci�n de interfases gr�ficas
install.packages("tcltk2") # posee comandos adicionales a tcltk
installed.packages() # muestra los paquetes que est�n instalados en el dispositivo
##Cargamos las librer�as
#observar lo que pasa en cada caso.
#si no estuviera instalarlo!
library(readxl)
library(grDevices) # Equipos gr�ficos y soporte para la base y la red de gr�ficos
library(tcltk)
library(aplpack)
library(corrplot)
library(ggplot2)
library(plotrix)
library(rgl)
library(tcltk2)
library(GGally)
library(tidyverse)
#Modo antiguo de graficaci�n
### Gr�fico de una recta y una par�bola

x=seq(-3,3,0.5) # asigna valores a la variable x
y=x^2 # define la funci�n cuadr�tica
w=x+2 # define la funci�n lineal
plot(x,y,col=2,type="p") # grafica los puntos de la funci�n y en rojo
plot(x,y,col=2,type="o") # idem anterior y superpone una l�nea
plot(x,y,col=2,type="l") # grafica la funci�n y con una l�nea continua
plot(x,y,col=2,lwd=2,type="l") # idem anterior pero con distinto grosor de l�nea
plot(x,y,col=2,lwd=2,type="l",ylim=c(-1,10)) # cambia los l�mites del eje y
abline(0,0) # dibuja una l�nea horizontal que pasa por el origen de coordenadas
segments(0,-1,0,10) # dibuja una l�nea vertical que pasa por el origen de coordenadas
points(x,w,col=4,type="l",lwd=2) # superpone la funci�n w en azul
text(locator(1),col=2,"par�bola") # coloca un cartel en la posici�n indicada
text(locator(1),col=4,"recta")
title("Gr�fico de una recta y una par�bola") # agrega un t�tulo al gr�fico activo


### Gr�ficos Estad�sticos

### Diagrama circular-Variables Categ�ricas

IMCinfantil <- read_excel("E:/Documentos/Calzolari/AID-CHAN/IMCinfantil.xlsx")
View(IMCinfantil)
IMCinfantil <- read.csv2("E:/Documentos/Calzolari/AID-CHAN/IMCinfantil.csv") # importa la base IMCinfantil
attach(IMCinfantil) # carga la base en la memoria activa

frec.catpeso<-table(CatPeso) # construye la distribuci�n de frecuencias
pie(frec.catpeso) # dibuja el diagrama circular
pie(frec.catpeso, col=rainbow(25)) # cambia la gama de colores
pie(frec.catpeso, col=rainbow(25),font=8) # cambia el tipo de letra
pie(frec.catpeso, col=rainbow(25),font=8,cex=1.5) # cambia el tama�o de letra
pie(frec.catpeso, col=rainbow(25),font=8,cex=1.5,radius=1) # cambia el tama�o de la torta
pie(frec.catpeso, col=rainbow(25),font=8,cex=1.5,radius=1,border=F) # quita el borde
pie(frec.catpeso, col=rainbow(25),font=8,cex=1.5,radius=1,border=F,main="Gr�fico de Torta") # pone nombre


etiquetas<-c("Deficiente","Normal","Obeso","Con sobrepeso") # define etiquetas
pct<-round(frec.catpeso/sum(frec.catpeso)*100) # calcula las frecuencias porcentuales
etiquetas<-paste(etiquetas,pct) # agrega los porcentajes a las etiquetas 
etiquetas<-paste(etiquetas,"%",sep="") # agrega el s�mbolo % a los porcentajes 
pie(frec.catpeso,labels =etiquetas,col=heat.colors(4,alpha=1)) # otra manera de asignar una paleta de colores
pie(frec.catpeso,labels =etiquetas,col=terrain.colors(4,alpha=1)) # otra manera de asignar una paleta de colores
pie(frec.catpeso,labels =etiquetas,col=topo.colors(4,alpha=1)) # otra manera de asignar una paleta de colores
pie(frec.catpeso,labels =etiquetas,col=cm.colors(4,alpha=1)) # otra manera de asignar una paleta de colores
pie(frec.catpeso,labels =etiquetas,col=cm.colors(4,alpha=1),main="Diagrama circular con etiquetas")

### con volumen perspectiva y sombra

pie3D(frec.catpeso) # grafica una torta con volumen
pie3D(frec.catpeso,labels=etiquetas)
pie3D(frec.catpeso,labels=etiquetas,explode=0.1) # separa los sectores
pie3D(frec.catpeso,labels=etiquetas,explode=0.1,labelcex=0.9) # cambia el tama�o de las etiquetas
pie3D(frec.catpeso,labels=etiquetas,explode=0.1,labelcex=0.9,radius=1.5)
pie3D(frec.catpeso,labels=etiquetas,explode=0.1,labelcex=0.9,radius=1.5,height=0.2) # cambia el alto de la torta
pie3D(frec.catpeso,labels=etiquetas,explode=0.1,labelcex=0.9,radius=1.5,height=0.2,shade=0.6) # sombrea
pie3D(frec.catpeso,labels=etiquetas,explode=0.1,labelcex=0.9,radius=1.5,height=0.2,shade=0.6,col=terrain.colors(4:8,alpha=1))

### Diagramas de barras -barras superpuestas - barras adyacentes


p1=ggplot(IMCinfantil,aes(x=CatPeso))+geom_bar() #diagrama de barras b�sico
p1
p1b=ggplot(IMCinfantil,aes(x=CatPeso))+geom_bar(fill="red",alpha=0.2) #diagrama de barras b�sico
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


### Gr�fico de mosaicos
attach(IMCinfantil) ##pone disponible esta base de datos en la memoria
tabla2=table(EDAD,CatPeso)
par(bg="aliceblue")
mosaicplot(tabla2) # hace un gr�fico de mosaicos simple
 mosaicplot(tabla2[,c(1,2,4,3)],col=c("cadetblue1","deepskyblue","deepskyblue4","cyan3"),main="Gr�fico de Mosaicos",ylab="Categor�a de Peso",xlab="Edad",
            cex=0.8) # este gr�fico permite visualizar una tabla de contingencia

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

### Funcion de distribucion emp�rica

attach(avispas)
par(mfrow=c(1,2)) # dividimos el �rea de gr�ficos en dos columnas
plot.ecdf(X1[Grupo=="Chaq.amar"],col="magenta",main="FDE Chaqueta amarilla",ylab="F(x)") # dibuja la funci�n de distribuci�n emp�rica
plot.ecdf(X1[Grupo=="Neg.peq"],col="chartreuse1",main="FDE Negra peque�a",ylab="F(x)") 

par(mfrow=c(1,1)) # unifica la pantalla de gr�ficos
n=length(X1)
plot(stepfun(1:(n-1),sort(X1)),main="Funci�n escalonada") # otra manera de definir y graficar la funci�n acumulada
plot(stepfun(1:(n-1),sort(X1)),main="Funci�n escalonada",col="coral",lwd=2,ylab="F(x)")


###### Comparaci�n de distribuciones

y=rchisq(200,df=5) # genera una muestra aleatoria Chi cuadrado con 5 grados de libertad de tama�o 200
qqnorm(y) # compara los cuantiles de esta muestra con los de una normal
qqnorm(y,col="mediumorchid1",pch=18,main="Gr�fico cuantil-cuantil con una normal",xlab="Cuantiles te�ricos",ylab="Cuantiles muestrales")
qqline(y,col="navy",lwd=1) # agrega una linea bajo el supuesto de normalidad

attach(IMCinfantil)
cc.muj=CC[SEXO=="F"] # guarda los datos de circunferencia de cintura de las mujeres
cc.var=CC[SEXO=="M"] # guarda los datos de circunferencia de cintura de los varones
qqplot(cc.muj,cc.var) # produce un gr�fico cuantil-cualtil que si resulta una l�nea recta las distribuciones coinciden
qqplot(cc.muj,cc.var,col=8:9,pch=18,main="Comparaci�n de distribuciones por sexo",xlab="Circunferencia de cintura en mujeres",ylab="Circunferencia de cintura en varones")

media=mean(c(cc.muj,cc.var))
desvio=sd(c(cc.muj,cc.var))
qqline(c(cc.muj,cc.var), 
       distribution=function(p){qnorm(p,mean=media,sd=desvio)},col="chocolate1") 
# agrega la l�nea en el caso en que las distribuciones fueran iguales

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
plot12 # b�sico
plot12b=ggplot(IMCinfantil,aes(x=SEXO,y=PESO))+geom_boxplot()
plot12b # por sexo
plot13=ggplot(IMCinfantil,aes(x=SEXO,y=PESO,color=SEXO))+geom_boxplot()
plot13 # con borde de color
plot14=ggplot(IMCinfantil,aes(x=SEXO,y=PESO,fill=SEXO))+geom_boxplot()+ coord_flip()
plot14 # relleno y horizontal

### Graficos para visualizar la  correlacion

attach(IMCinfantil)
base.ni�os=data.frame(EDAD,PESO,TALLA,IMC,CC) # arma una sub-base con las variables num�ricas de IMCinfantil
base.ni�os$CC=max(base.ni�os$CC)-base.ni�os$CC # cambiamos una variable para que correlacione en forma negativa con las restantes
M=cor(base.ni�os) # calcula la matriz de correlaci�n de las variables de la base
M
corrplot(M,method="circle") # representa la matriz de correlaciones mediante c�rculos
corrplot(M,method="square") # representa la matriz de correlaciones mediante cuadrados
corrplot(M,method="ellipse") # representa la matriz de correlaciones mediante elipses
corrplot(M,method="number") # representa la matriz de correlaciones mediante n�meros
corrplot(M,method="shade") # representa la matriz de correlaciones mediante sombreandos
corrplot(M,method="pie") # representa la matriz de correlaciones mediante gr�ficos de torta
corrplot(M,type="upper") # representa s�lo la parte superior de la matriz de correlaci�n
corrplot(M,type="lower") # representa s�lo la parte inferior de la matriz de correlaci�n
corrplot(M,method="ellipse",type="upper") # permite combinaciones de estilos
corrplot.mixed(M) # representa la matriz de correlaci�n combinando c�culos y n�meros
corrplot.mixed(M,lower="circle",upper="shade") # permite combinaciones de estilos por bloques


### Caritas de Chernoff

par(mfrow=c(1,1),adj=0)
par(col.main="blue") # cambia el color de los textos
galle=read.csv("E:/Documentos/Calzolari/AID-CHAN/galletitas.csv", sep=";")
galle.salad=galle[c(1:3,7,15:17),] # agrupa las galletitas saladas
galle.dulce=galle[c(4:6,8:14),] # agrupa las galletitas dulces
faces(galle.salad[,2:6]) # hace un gr�fico con las caras de Chernoff
faces(galle.salad[,2:6],nrow.plot=3) # ajusta el alto de las caras
faces(galle.salad[,2:6],ncol.plot=4) # acomoda la cantidad de caras por fila
faces(galle.salad[,2:6],face.type=0) # grafica las caras sin color
faces(galle.salad[,2:6],face.type=2) # cambia el estilo de cara
faces(galle.salad[,2:6],labels=galle.salad[,1]) # etiqueta las caras 
title("Caritas de Chernoff saladas") # ponemos t�tulo

faces(galle.dulce[,2:6],nrow.plot=3,ncol.plot=5,face.type=2,labels=galle.dulce[,1])
title("Galletitas Dulces")

### Gr�fico de estrellas

par(col.main="black",adj=0.5)
row.names(galle.salad)=galle.salad[,1] # coloca etiquetas al gr�fico
stars(galle.salad[,2:6]) # hace un gr�fico de estrellas
stars(galle.salad[,2:6],full=T) # dibuja con volumen
stars(galle.salad[,2:6],full=F) # dibuja en perspectiva
stars(galle.salad[,2:6],radius=F) # omite aristas
stars(galle.salad[,2:6],axes=T) # dibuja los ejes
stars(galle.salad[,2:6],frame.plot=T) # recuadra el gr�fico 
stars(galle.salad[,2:6],draw.segments=T) # cambia el estilo 
stars(galle.salad[,2:6],col.lines=rainbow(15)) # cambia el color a las l�neas
stars(galle.salad[,2:6],cex=0.8,flip.labels=T) # cambia la posici�n de las etiquetas
stars(galle.salad[,2:6],cex=0.8,flip.labels=F,len=0.8) # cambia el tama�o de las estrellas
stars(galle.salad[,2:6],cex=0.8,flip.labels=F,len=0.8,col.stars=terrain.colors(7)) # colorea los interiores de las estrellas
stars(galle.salad[,2:6],cex=0.8,flip.labels=F,len=0.8,col.stars=terrain.colors(7),ncol=4,frame.plot=T,main="Galletitas saladas")

row.names(galle.dulce)=galle.dulce[,1]     
stars(galle.dulce[,2:6],full=T,draw.segments=T,cex=0.9,len=0.8,ncol=4,frame.plot=T,main="Galletitas dulces")

cars=mtcars[1:9,]
stars(cars,cex=0.7,col.stars=c("red","green","orange","gold","blue",
                        "yellow", "pink","purple","cyan"))
title("Gr�fico de Estrellas")

par(mfrow=c(1,3))
stars(galle.salad[,2:6],ncol=2,full=F) 
stars(galle.salad[,2:6],ncol=2,axes=T)
stars(galle.salad[,2:6],ncol=2,col.lines=rainbow(15))

