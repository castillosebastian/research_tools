#setwd("C:/Users/Silvia/Desktop/DM-Parana/teóricas")
library (ISLR)
#fix(Hitters )
names(Hitters )
sum(is.na(Hitters)) #muestra que hay NA
Hitters=na.omit(Hitters) #tiramos todos los NA, quedan 263 casos
dim(Hitters)
x=model.matrix(Salary~.,Hitters)[,-1] #porque glmnet necesita asi. Si hace falta crea automat dummies
y=Hitters$Salary
set.seed(666)
train<-sample(1:nrow(x),nrow(x)*0.70,replace=FALSE)
test=(-train)
y.test=y[test]

# Ridge Regression
library(glmnet)
grid=10^seq(10,-5,length=50) #p/dar una secuencia de lambdas
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid)
#con alpha=0 hace Ridge, con 1 Lasso
#glmnet estandariza las variables
#grid es para que recorra estos 100' lambdas
dim(coef(ridge.mod)) # son 20 coef para cada lambda
ridge.mod$lambda[50] #muestra el lambda 50'
coef(ridge.mod)[,50] #muestra los coeficientes
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[10]#muestra el lambda 10'
coef(ridge.mod)[,10] #ver que a mayor lambda dan menores betas
sqrt(sum(coef(ridge.mod)[-1,10]^2))
predict(ridge.mod,s=4,type="coefficients")[1:20,] #para obtener coef con otro lambda=4
#para obtener predicciones en test
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

#lo que sigue es para elegir el lambda por CV!!!
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
#Para verlo con todos los datos
ridge.todo=glmnet(x,y,alpha=0)
predict(ridge.todo,type="coefficients",s=bestlam)[1:20,]
#notar en lo anterior que ningun coef se anula, a diferencia de lasso

# The Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out2=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out2)
bestlam2=cv.out2$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam2,newx=x[test,])
mean((lasso.pred-y.test)^2)
lasso.todo=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(lasso.todo,type="coefficients",s=bestlam2)[1:20,]
lasso.coef  #muchos los ajusta en 0!!
lasso.coef[lasso.coef!=0]
