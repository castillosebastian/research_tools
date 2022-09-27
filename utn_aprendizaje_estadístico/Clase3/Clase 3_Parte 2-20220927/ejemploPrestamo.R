setwd("C:/Users/Silvia/Desktop/DM-Parana/teóricas")
library(pROC)
datos<-read.table("PrestamoSelec.csv",sep=";",dec=",",header=T) 
datos$Default<-as.factor(datos$Default)
train=sample(seq(length(datos$Id)),length(datos$Id)*0.80,replace=FALSE) 
dataTrain=datos[train,]
dataTest<-datos[-train,]

#AJUSTO REG Logística
model.lr <- glm(Default~Edad+Domicilio+Ingreso+Deuing+Creddeu+Otrdeu,data= dataTrain, family = binomial(link="logit"))
summary(model.lr)
p.pred<-predict(model.lr,dataTest,type="response") #esto da las probabilidades predichas!! igual que LRPredict
auc.rl<-roc(dataTest$Default, p.pred)$auc #calcula directamente auc 
#prediccion <- ifelse(p.pred >= 0.5, 1, 0)
#tabla.rl <- table(dataTest$Default, prediccion) 
#accu.rl<-sum(diag(tabla.rl))/sum(tabla.rl)


###NAIVE BAYES 
model.nb<-naiveBayes(Default~Edad+Domicilio+Ingreso+Deuing+Creddeu+Otrdeu,data=dataTrain)
nb.pred<-predict(model.nb,dataTest,type="class")#clase predicha
#tabla.nb<-table(nb.pred,dataTest$Default)
p2.pred<-predict(model.nb,dataTest,type="raw")[,2]
auc.nb<-roc(dataTest$Default, p2.pred)$auc 
#accu.nb<-sum(diag(tabla.nb))/sum(tabla.nb)


####SUPPORT VECTOR MACHINE
model.svm <- svm(Default~Edad+Domicilio+Ingreso+Deuing+Creddeu+Otrdeu,data=dataTrain)
svm.pred<-predict(model.svm,dataTest,type="class")#clase predicha
#p3.pred<-predict(model.svm,dataTest,type="response")#da lo mismo que lo anterior
p3.pred<-predict(model.svm,dataTest,type="raw")#da lo mismo que lo anterior
tabla.svm<-table(svm.pred,dataTest$Default) 
accu.svm<-sum(diag(tabla.svm))/sum(tabla.svm)
p3.pred<-as.numeric(p3.pred)
auc.svm<-auc(dataTest$Default, p3.pred)
#auc.svm<-roc(dataTest$Default, p3.pred)$auc #esto da lo mismo!!!

#### C5.0
library(C50)
model.C5 <- C5.0(Default~Edad+Domicilio+Ingreso+Deuing+Creddeu+Otrdeu,data=dataTrain)
prediccion <- predict(model.C5,newdata=dataTest)
prediccion<-as.numeric(prediccion)
auc.C5<-auc(dataTest$Default, prediccion)
