##Cargamos las librer?as
library(readxl)

##Fijar directorio de trabajo
#setwd("E:/Documentos/Calzolari/AID-CHAN/") # fija la ruta de la carpeta donde se encuentran los archivos con los que se va a trabajar y donde se guardar?n las salidas que se ejecuten 
getwd() # devuelve la ruta de la carpeta de trabajo

##Operaciones b?sicos
2+6 # suma
3*4 # producto
-4/(5-3) # cociente
4^0.2 # potencia
sqrt(9) # ra?z cuadrada
cos(pi) # funciones trigonom?tricas
exp(-2) # funci?n exponencial
factorial(5) # devuelve el factorial de un n?mero

# asignaci?n de un valor a una variable 
a=log(3) 
a<-log(3) # es equivalente al anterior
log(3)->a # es equivalente al anterior
b=log10(0.1) # asignaci?n de un valor a la variable b
c=a*(1-b) # operaci?n entre variables
a # devuelve el valor guardado en a
b # devuelve el valor guardado en a
c # devuelve el valor guardado en a

##secuencias de n?meros

1:5 # n?meros del 1 al 5 orden creciente
2:100 # n?meros del 2 al 100 orden creciente
15:10 # n?meros del 15 al 10 orden decreciente


a=1:5 # reemplaza lo guardado en la variable a
a # devuelve el valor guardado en a


x <- c(1,2,3,4,5,6,7,8) # devuelve los enteros de 1 a 6
x
x <- 1:8 # es equivalente al anterior
x
assign("x", 1:8) # otra manera de asignar valores a un vector
x
x <- seq(1,8) # es equivalente al anterior
x
x1=8:1
x1
y=seq(1,4,0.2) # devuelve los n?meros de 1 a 4 espaciados en 0.2
y
z=rep(1,5) # repite el 1 cinco veces
z
w=rep(1:3,4) # repite del 1 al 3 cuatro veces
w
q=rep(c(1,3,7),2) # repite la concatenaci?n dos veces
q

###valores l?gicos
m <- c(T,F,F,T) # concatena lo pedido
m
n <- rep(c(T,F),3) # repite la concatenaci?n tres veces
n
l=w>2 # asigna una proposici?n a la variable l
w
l # asigna valor de verdad de la proposici?n 
p<-c(T,F,T,F)
p
m&p # devuelve la conjunci?n entre componentes de vectores l?gicos de igual longitud
m|p # devuelve la disyunci?n entre componentes de vectores l?gicos de igual longitud
!m # devuelve la negaci?n de cada componente

###acceso a los elementos de un vector
w
w[5] # devuelve la quinta componente almacenada en el vector w
w[2:4] # davuelve las componentes de 2 a 4 del vector w
w[c(1,4,6)] # devuelve las componentes 1, 4 y 6 del vector w
w[-c(1,4,6)] # devuelve el vector w sin las componentes 1, 4 y 6
rep(c(T,T,F,F),3)    
w[rep(c(T,T,F,F),3)] # devuelve las componentes de w que corresponden con TRUE
!rep(c(T,T,F,F),3) # ! niega en forma l?gica
w[!rep(c(T,T,F,F),3)] # devuelve las componentes de w que corresponden con TRUE


u=c(3,8,2,7,3,2,1)
u==3  # operaci?n l?gica que busca las posiciones de u que guardan el 3
u[u==3]<-4 # en esas posiciones asigna un 4   
u
which(u>=4) # devuelve las posiciones de u que tienen n?meros mayores o iguales a 4
u[which(u>=4)]<-0 # asigna 0 a esas posiciones
u

##nombres a las componentes de un vector

h<-c(2,7,4) 
names(h) # consulta los nombres de las componentes del vector h
names(h)<-c("Alicia", "Pedro", "Lucas") # asigna nombres a las componentes de h
h # verifica la asignaci?n 
h["Alicia"] # devuelve la componente seg?n el nombre

##Vectores de caracteres

f<-"Usted est? aprendiendo r?pidamente" # asigna un vector de caracteres de longitud 1 
f
g<-c("caliente", "tibio", "fr?o") # asigna un vector de caracteres de longitud 3
g
h <- paste(c("a","b"), 2:5, sep="") # genera pares ordenados 
h
t=paste(c("a","b"), 2:5, sep=",") # genera pares ordenados separados por comas
t

##Longitud de un vector

length(w) # devuelve la longitud del vector w
length(g) # devuelve la longitud del vector g
length(c(w,g)) # devuelve la longitud del vector w concatenado con g

##Modo de almacenamiento

mode(w) # devuelve el tipo de datos del vector w, en este caso num?rico
mode(g) # devuelve el tipo de datos del vector g, en este caso de caracteres
mode(m) # devuelve el tipo de datos del vector m, en este caso l?gico
storage.mode(y) # equivalente a mode, en este caso el tipo es "double" que significa de doble precisi?n
storage.mode(w) # en este caso el tipo es entero
storage.mode(g)
storage.mode(m)

##Redondeo de cifras
pi
round(pi,3) # redondea el valor de pi a 3 decimales
signif(pi,2) # devuelve el valor con dos cifras significativas
abs(-8) # devuelve el valor absoluto
trunc(-pi) # devuelve el valor entero del n?mero sin su parte decimal
floor(pi) # devuelve el entero inmediato anterior a pi
ceiling(pi) # devuelve el entero inmediato posterior a pi

##Ordenamiento de Vectores       
n=c(2,1,-8,3)
sort(n) # ordena las componentes num?ricas en orden creciente
g
g[c(2, 3, 1)] # cambia el orden en las componenetes
sort(g) # ordena alfab?ticamente las componentes del vector de caracteres
rev(g) # invierte el orden de las componentes del vector
order(g) # devuelve la posici?n de las componentes del vector en el orden alfab?tico
g[order(g)] # equivalente a sort
q=rep(c(1,3,7),2)  
q
unique(q) # devuelve las cifras que aparecen omitiendo repeticiones   
duplicated(q) # indica cifras repetidas

##Operaciones con vectores
v1<-rep(2,4) #definimos el vector1
v1
v2<-2:5  # definimos el vector 2
v2
-2*v2 # producto de un vector por un escalar
v2+3  # suma 3 a cada componente
v1+3*v2 # combinaci?n lineal de vectores
v1/v2 # divisi?n componente a componente
v1*v2 # producto componente a componente
v1**v2 # potenciaci?n componente a componente


v3=2*v1-3*v2
v3
min(v3) # devuelve el valor m?nimo del vector
max(v3) # devuelve el valor m?ximo del vector
sum(v3) # suma las componentes del vector
cumsum(v3) # devuelve un vector que guarda en cada componente la suma de las anteriores m?s ?sa
prod(v3) # multiplica las componentes de un vector
sum(v1*v2) # producto escalar de dos vectores
v1%*%v2    # producto escalar de dos vectores


ls() # lista todas las variables que se han creado en el espacio de trabajo

###Matrices 

data=1:10
matrix(data,nrow=2,ncol=5) # acomoda los datos por columna en una matriz de nrow filas y ncol columnas
matrix(data,nrow=2,ncol=5,byrow=T)  # # acomoda los datos por fila en una matriz de nrow filas y ncol columnas
matrix(c(2,4,5,6,-8,11),nrow=2)  # acomoda la concatenaci?n por columnas de acuerdo a la cantidad de filas indicada


vec1=seq(2,5)   # asigna valores a un vector
vec2=seq(-5,-2) # asigna valores a otro vector
cbind(vec1,vec2)  # devuelve la matriz que tiene a estos vectores como columnas
rbind(vec1,vec2)  # devuelve laa matriz que tiene a estos vectores como filas


mat1=matrix(data,nrow=2,ncol=5) # asigna valores a una matriz
mat1 # devuelve la matriz
colnames(mat1)<-c("A","B","C","D","E") # asigna nombres a las columnas de la matriz
mat1 # devuelve la matriz, ahora con nombres en sus columnas
rownames(mat1)<-c("2015","2016") # asigna nombres a las filas de la matriz
mat1 # devuelve la matriz, ahora con nombres en sus filas

###Dimensiones y modo de almacenamiento de matrices
dim(mat1) # devuelve la cantidad de filas y de columnas de la matriz
storage.mode(mat1) # devuelve el tipo de valores guardados en la matriz

##Acceso a los elementos de una matriz
mat1[1,2] # devuelve el elemento de la fila 1 y la columna 2 de la matriz
mat1[1,3:5] # devuelve los elementos de la fila 1 y correspondientes a las columnas de 3 a 5
mat1[1,] # devuelve la fila 1 de la matriz
mat1[,2] # devuelve la columna 2 de la matriz
mat1[1,2]<--3.1 # asigna un valor dado en la fila 1 y la columna 2 de la matriz
mat1
storage.mode(mat1) # observar que cambi? el modo de almacenamiento de la matriz

##Operaciones con matrices
mat2<-matrix(seq(10,1),nrow=2,byrow=T) # asigna valores a una nueva matriz
mat2 # devuelve la matriz
t(mat2) # devuelve la matriz traspuesta de la matriz
mat1+mat2 # suma de matrices
mat1-mat2 # resta de matrices
mat2+3 # suma 3 a cada elemento de la matriz
3*mat2 # producto de matriz por escalar
mat1*mat2 # producto elemento a elemento
sqrt(mat2) # ra?z cuadrada de cada elemento de la matriz
sqrt(mat1) # observar que cuando la operaci?n no est? definida devuelve NaN
sqrt(-9) # no est? definido
sqrt(-9+0i) # lo trata como n?mero complejo
exp(mat2) # exponencial a cada elemento de la matriz
log10(mat2) # logaritmo a cada elemento de la matriz
mat3=mat1%*%t(mat2) # asigna a una matriz el producto matricial de dos matrices
mat3 # devuelve el resultado del producto matricial
det(mat3) # devuelve el determinante de la matriz
solve(mat3) # devuelve la matriz inversa de la matriz
1/det(mat3) # devuelve el inverso del determinante
det(solve(mat3)) # devuelve el determinante de la matriz inversa inversa
mat3%*%solve(mat3) # devuelve el producto de una matriz por su inversa; es decir, la matriz identidad
crossprod(mat3,solve(mat3)) # devuelve el producto entre la traspuesta de la primera matriz y la segunda matriz
diag(mat3) # devuelve la diagonal principal de la matriz
sum(diag(mat3)) # devuelve la traza de la matriz
eigen(mat3)$values  # devuelve los autovalores de la matriz
eigen(mat3) # devuelve los autovectores de la matriz
eigen(mat3)$vectors  # es equicvalente al anterior

##Aplicaci?n de operaciones  por fila o columna
mat1
apply(mat1,1,sum) # devuelve la suma de cada fila de la matriz
apply(mat1,2,sum) # devuelve la suma de cada columna de la matriz
apply(mat1,2,min) # devuelve el m?nimo de cada columna de la matriz
apply(mat1,1,mean) # devuelve la media de cada fila de la matriz
apply(mat1,1,median) # devuelve la mediana de cada fila de la matriz
apply(mat1,1,var) # devuelve la varianza de cada fila de la matriz
apply(mat1,1,sd) # devuelve el desv?o est?ndar de cada fila de la matriz
apply(mat1,1,summary) # devuelve un resumen de cada fila de la matriz, incluyendo valor m?nimo, primer cuartil, mediana, media, tercer cuartil y valor m?ximo

##Data Frames
Nombre = c("Ana","Luis","Pedro","Juan","Eva","Jorge") # crea un vector con los nombres
Edad = c(23,24,22,24,25,27) # crea un vector con las edades correspondientes 
Sexo = as.factor(c("F",rep("M",3),"F","M")) # crea un vector como factor con el sexo correspondiente 
levels(Sexo) # devuelve los grupos el vector dado como factor
datos=data.frame(Nombre,Edad,Sexo) # arma un entorno de datos
datos # devuelve el entorno de datos
mean(datos$Edad[datos$Sexo=="F"]) # devuelve el promedio de la edad de las mujeres

##Listas
table(datos[[3]]) # devuelve una tabla de frecuencias del factor Sexo

milista<-list(T,3,"curso R",mat3) # genera una lista de objetos
milista # devuelve la lista creada
names(milista)<-c("valor de verdad","marcas","presente","matriz 3") # asigna nombres a los elementos de la lista
milista # devuelve la lista con nombres 
milista2=list("a"=3,"b"=mat3,"c"=vec1) # genera otra lista
milista2 # devuelve la otra lista


milista$presente # devuelve el elemento guardado seg?n el nombre
milista[[2]] # devuelve el elemento guardado seg?n la posici?n
length(milista) # devuelve la cantidad de elementos de la lista
milista$a<-7 # agrega una componente al final de la lista
milista # devuelve la lista modificada
milista[[2]]<-mat1 # reasigna un valor de una componente seg?n la posici?n
milista # devuelve la lista modificada


lista1=list("mat1"=mat1,"mat2"= mat2, "mat3"= mat3) # genera una lista
lista1 # devuelve la lista
lista1$mat1+lista1$mat2 # devuelve la suma entre el primer y el segundo elemento de la lista

##Partici?n de Listas
split(datos,Sexo) # particiona un entorno de datos a partir del factor Sexo
datos2=data.frame(datos,"Naci?n"=as.factor(c(rep("arg",3),rep("per",3)))) # agrega informaci?n al entorno de datos
datosmujeres=split(datos2,datos2$Sexo)[[1]] # almacena los datos correspondientes a la partici?n por mujeres
split(datosmujeres,datosmujeres$Naci?n) # particiona los nuevos datos por el factor Naci?n; es decir, se ha particionado un data frame por dos factores

##Leemos los datos
IMCinfantil <- read_excel("IMCinfantil.xlsx")
View(IMCinfantil)

head(IMCinfantil) # muestra las seis primeras filas de datos y los nombres de las columnas

IMCinfantil$EDAD # devuelve los datos de la variable por su nombre
IMCinfantil[,2] # devuelve los datos de la variable por su posici?n


table(IMCinfantil$SEXO) # devuelve las frecuencias absolutas de las categor?as de la variable
100*table(IMCinfantil$SEXO)/length(IMCinfantil$SEXO) # calcula las frecuencias porcentuales
sal.sexo=rbind(table(IMCinfantil$SEXO),100*table(IMCinfantil$SEXO)/length(IMCinfantil$SEXO)) # combina las dos frecuencias en una salida
rownames(sal.sexo)=c("frec.abs","frec.porc") # asigna nombre a las filas de la salida
colnames(sal.sexo)=c("Femenino", "Masculino") # asigna nombre a las columnas de la salida
sal.sexo # muestra la salida
round(sal.sexo,2) # redondea la salida a dos d?gitos decimales

table(IMCinfantil$CatPeso)
100*table(IMCinfantil$CatPeso)/length(IMCinfantil$CatPeso)
sal.catpeso=rbind(table(IMCinfantil$CatPeso),100*table(IMCinfantil$CatPeso)/length(IMCinfantil$CatPeso))
rownames(sal.catpeso)=c("frec.abs","frec.porc")
levels(IMCinfantil$CatPeso) # devuelve las categor?as ordenadas alfab?ticamente
colnames(sal.catpeso)=c("Deficiente","Normal","Obeso","Con sobrepeso")
sal.catpeso 
round(sal.catpeso,2)
sal.catpeso[,c(1,2,4,3)] # muestra la salida con el orden indicado en las columnas

dist.conj=table(IMCinfantil$CatPeso, IMCinfantil$SEXO) # devuelve la distribuci?n conjunta 
total=apply(dist.conj,2,sum) # calculo los totales por sexo
dist.porc=round(100*cbind(dist.conj[,1]/total[1],dist.conj[,2]/total[2]),2) # calcula la distribuci?n porcentual por sexo
colnames(dist.porc)<-c("F(%)","M(%)") # asigna nombre a las columnas de la distribuci?n porcentual
sal.conj=cbind(dist.conj,dist.porc) # combina ambas distribuciones
Totales=apply(sal.conj,2,sum) # calcula el total de cada columna
sal.fin=rbind(sal.conj,Totales) # agrega una fila con los totales
sal.fin # muestra la salida final

imc.base=cbind(IMCinfantil [,c(2,4:6,8)]) # arma una base seleccionando variables num?ricas de inter?s
medias=round(apply(imc.base,2,mean),2) # calcula las medias
medianas=round(apply(imc.base,2,median),2) # calcula las medianas
varianzas=round(apply(imc.base,2,var),2) # calcula las varianzas
desv.standard=round(apply(imc.base,2,sd),2) # calcula los desv?os est?ndar
resumenes=rbind(medias,medianas,varianzas,desv.standard) # junta en una tabla los res?menes
resumenes # muestra la salida


Z= imc.base[,3]*100 # guarda los datos de la talla en cent?metros
mean(Z) # calcula la media
mean(Z, trim=0.1) # calcula la media podada al 10%
mean(Z,trim=0.5) # Calcula la media podada al 50%
median(Z) # calcula la mediana 

##Definici?n de Funciones y res?menes estad?sticos
c.var=function(x){ 100*sd(x)/mean(x)} # define la funci?n de coeficiente de variaci?n
c.var(Z) # aplica la funci?n 

quantile(Z, 0.75) # calcula el cualtil 75
quantile(IMCinfantil$EDAD, probs = seq(0, 1, 0.2)) # calcula los cuantiles 0, 20, 40, 60, 80 y 100
quantile(IMCinfantil$TALLA, probs = seq(0, 1, 0.25)) # calcula los cuantiles 0, 25, 50, 75 y 100
DI.edad=quantile(IMCinfantil$EDAD,0.75)-quantile(IMCinfantil$EDAD,0.25)# calcula la desviaci?n intercuartil
DI.edad
DI.Z=quantile(Z,0.75)-quantile(Z,0.25)
DI.Z


mad(Z, constant = 1) # calcula la mediana de los valores absolutos de los desv?os
mad(Z, constant = 1.4826) # multiplica lo anterior por la constante 1.4826
mad(Z) # toma por default la constante 1.4826
abs(mad(Z)-sd(Z)) # mide el apartamiento de normalidad
100*(abs(mad(Z)-sd(Z)))/sd(Z) # calcula el procentaje del apartamiento de normalidad

is.na(Z) # indica los valores que faltan
W<-Z 
W[1]<-NA # asigna un valor perdido en la primera componente del vector W
is.na(W)
mean(W) # devuelve error
mean(W,na.rm=T) # no considera los valores no disponibles
mean(na.omit(W)) # equivalente al anterior
median(W,na.rm=T) # otra funci?n que requiere excluir los valores no disponibles
sd(W,na.rm=T) # otra funci?n que requiere excluir los valores no disponibles


# armar una base numerica dividida seg?n el sexo
base.split=split(IMCinfantil[,c(2,4:6,8)],IMCinfantil$SEXO) 
head(base.split)


# guardar la primera subclase (F) como dataframe
mujeres=as.data.frame(base.split[[1]])
mujeres

# guardar la segunda subclase (M) como dataframe
varones=as.data.frame(base.split[[2]]) 
varones

#resumir de todas las variables de la base mujeres
lapply(mujeres,"summary")
summary(mujeres)

# transformar la salida de lista a vector
x=unlist(lapply(mujeres,"summary")) 
length(x)

# acomoda los elementos del vector en una matriz de modo tal que cada variable ocupe una fila
matrix(unlist(lapply(mujeres,"summary")),nrow=5,ncol=6,byrow=T) 

# guarda los res?menes estad?sticos en "sumary.muj"
sumary.muj=matrix(unlist(lapply(mujeres,"summary")),nrow=5,ncol=6,byrow=T)

# calcula el desv?o est?ndar de las variables de inter?s redondeado a dos d?gitos decimales
sd.mujeres=round(unlist(lapply(mujeres,"sd")),2)


# combinar las columnas indicadas
salida.muj=cbind(sumary.muj[,4],sd.mujeres,sumary.muj[,c(3,2,5)]) 

# poner nombre a las columnas
colnames(salida.muj)<-c("MEDIA","SD","MEDIANA","Q1","Q3")
salida.muj # devuelve la salida

base.porpeso=split(IMCinfantil[,c(2,4:6,8)],IMCinfantil$CatPeso) 
defic=as.data.frame(base.porpeso[[1]]) 
normal=as.data.frame(base.porpeso[[2]]) 
obeso=as.data.frame(base.porpeso[[3]]) 
sobrep=as.data.frame(base.porpeso[[4]]) 

### generaci?n de salida para ni?os con peso deficiente

sumary.defic=matrix(unlist(lapply(defic,"summary")),nrow=5,ncol=6,byrow=T)
sd.defic=round(unlist(lapply(defic,"sd")),2)
cbind(sumary.defic[,4],sd.defic,sumary.defic[,c(3,2,5)])
salida.defic=cbind(sumary.defic[,4],sd.defic,sumary.defic[,c(3,2,5)])
colnames(salida.defic)<-c("MEDIA","SD","MEDIANA","Q1","Q3")
rownames(salida.defic)<-c("EDAD.D","PESO.D","TALLA.D","IMC.D","CC.D")
round(salida.defic ,2)

### replicar para ni?os con peso normal, sobrepeso y obesos





