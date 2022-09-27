## E-mails

library(openintro)
data(email)
head(email)
str(email)

# observar que winner y number ya están como factores, por eso no hace falta convertirlas.
# la variable respuesta, si es dicotomica, no hace falta convertirla en factor, La toma igualmente.
# Se podría hacer tablas de y versus esta..

xtabs(~spam + to_multiple, data = email) # Observamos que no hay casillas muy chicas
xtabs(~spam + format, data = email) # Observamos que no hay casillas muy chicas
xtabs(~spam + attach, data = email) # hay casillas muy grandes con ceros!!
xtabs(~spam + dollar, data = email) #hay casillas muy grandes con ceros!!!
xtabs(~spam + winner, data = email) # hay casillas muy chicas
xtabs(~spam + inherit, data = email) # hay casillas chicas con ceros
xtabs(~spam + number, data = email) # hay casillas chicas

#a) se genera el modelo completo introduciendo todas las variables como predictores.

modelo_completo<-glm(spam~to_multiple+format+cc+attach+dollar+
                         winner+inherit+password+re_subj+exclaim_subj,
                       data = email,family = binomial)
summary(modelo_completo)

# Excluímos aquellos predictores cuyo p-values no es significativo. 
# El resultado es el siguiente modelo.

modelo_final <- glm(spam~to_multiple+format+attach+dollar+winner+
                      inherit+password+re_subj,
                    data = email,family = binomial)
summary(modelo_final)

# Este es el modelo que mejor ajusta con un AIC de 1957.56 según el software

modelo_final1 <- glm(spam~to_multiple+format+attach+dollar+winner+
                      +re_subj,data = email,family = binomial)
summary(modelo_final1)

step(modelo_completo)

deviance(modelo_final)
anova(modelo_completo,modelo_final,test="Chisq")

# Escribo los valores del exponente Beta
exp(modelo_final$coefficients)

#para construir IC de los coeficientes basados en verosimilitud
exp(confint(modelo_completo))
exp(confint(modelo_final))

#IC para los ODDs=exp(betas): odds ratios e Intervalos de Confianza al 95%
exp(cbind(OR = coef(modelo_final), confint(modelo_final)))

## BONDAD DE AJUSTE
#para ver el Test de H-L

library(generalhoslem)
logitgof(email$spam, fitted(modelo_final))
# Esta prueba me indica que si el p-valor es alto entonces "el ajuste" es bueno. 
# Pero ojo!!! porque está tirando un mensaje que alguna casilla es muy chica en el test! entonces, 
# no es confiable esta prueba porque la aproximación chicuadrado no es buena. 
# No lo deberíamos tomar en cuenta a HL acá. Decidir con otras cosas (AIC por ej)

library(generalhoslem)
logitgof(email$spam, fitted(modelo_completo)) # No ajusta bien el modelos según H-L

library(pscl)
pR2(modelo_final)

#para calcular residuales
e <- residuals(modelo_final, "response")  #residuales comunes: y-pihat
rp <- residuals(modelo_final, "pearson") #de Pearson
dev <- residuals(modelo_final, "deviance") #res de Devianza

h <- hatvalues(modelo_final) #leverages
plot(modelo_final)
predis<-modelo_final$fitted
plot(predis,h)
identify(predis,h,tolerance=0.15)  #caso 110

#para calcular Cook

p<-2 #numero de predictores
D <- (rp^2*h)/(p*(1-h)^2) # Cook's Distance
plot(email$spam,D)
identify(email$spam,D,tolerance=0.3) #reporta el caso del gráfico con D mayor a 0.3 caso 250 y 824

#para sacar pseudos R2- MacFadden

nullmod <- glm(spam~1, data = email,family="binomial")
nullmod

#otra forma: 

modelo_final$null.deviance
R2<-1-logLik(modelo_final)/logLik(nullmod)
R2

# Modelo_final: AIC=1957.56, R^2MF=0.2041788,  HL=0.2108
#Se puede comparar el valor real (si realmente es spam) con el predicho por el modelo.

library(vcd)
predicciones <- ifelse(test = modelo_final$fitted.values > 0.5, yes = 1, no = 0)
matriz_confusion <- table(modelo_final$model$spam, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))
