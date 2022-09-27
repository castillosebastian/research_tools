##Ejercicio 6

library(readxl)
demanda <- read.delim2("~/Posgrado/demanda.csv")
head(demanda)
str(demanda)

summary(demanda[, 2:5])
pairs(demanda[, 2:5])


# Observamos el rango de años.
range(demanda$ano)

# Previo a los años 70.
demanda_previa <- demanda[demanda$ano < 70, ] 

# Descriptivos.
pairs(demanda_previa[, 2:5])
summary(demanda_previa[, 2:5])

# Modelo.
modelo_previa <- lm(consumo ~ precio * IPC, data = demanda_previa) 
summary(modelo_previa) # Modelo significativo y R2 alto.

# Ecuación de predicción seria:
# consumo = 1231 - 1372*precio - 0.1469*IPC + 0.1887*precio*IPC

# Podemos hacer la predicción de nuevos valores mediante:
predict(modelo_previa, newdata = data.frame(precio = 1, IPC = 6800))

# Posterior a los años 70, incluyendo estos.
demanda_posterior <- demanda[demanda$anio >= 70, ]

# Descriptivos.
pairs(demanda_posterior[, 2:5])
summary(demanda_posterior[, 2:5])

# Modelo.
modelo_posterior <- lm(consumo ~ precio * IPC, data = demanda_posterior)
summary(modelo_posterior)  

# Como la interacción en estos años no es significativa, simplificamos el modelo.
modelo_posterior2 <- lm(consumo ~ precio + IPC, data = demanda_posterior)
summary(modelo_posterior2) # El modelo no es significativo

# Ecuación de predicción seria:
# consumo = 244.932723 + 1.762477*precio - 0.001604*IPC

# Podemos hacer la predicción de nuevos valores mediante:
predict(modelo_posterior2, newdata = data.frame(precio = 1.7, IPC = 8500))
