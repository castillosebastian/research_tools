## Matrícula

library(readxl)
Matricula <- read_excel("C:/Users/Curso R/TEMAS/Matricula.xlsx")
datos=Matricula
head(datos)
str(datos)

# Representar las observaciones es útil para intuir si la variable independiente escogida
# está relacionada con la variable respuesta y, por lo tanto, puede ser un buen predictor.

library(ggplot2)
table(datos$matricula)

ggplot(data = datos, aes(x = factor(matricula), y = matematica, color = matricula)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")

#Parece existir una diferencia entre la nota de las personas con matrícula y sin matrícula.

datos$matricula <- factor(datos$matricula)
str(datos)
plot(datos$matematica,datos$matricula, col = "red",  main = "dispersion de datos", 
     ylab = "matricula",xlab = "matemáticas")

# a) Modelo de regresión logística
modelo <- glm(matricula ~ matematica, data = datos, family = "binomial")
summary(modelo)

# b) Intervalo de confianza
confint(object = modelo, level = 0.95 )

# c) Graficar
### MEDIANTE BASE GRAPHICS SIN INTERVALOS DE CONFIANZA
# Codificación 0,1 de la variable respuesta

datos$matricula <- as.character(datos$matricula)
datos$matricula <- as.numeric(datos$matricula)

plot(matricula ~ matematica, datos, col = "darkblue",
     main = "Modelo regresión logística",
     ylab = "P(matrícula=1|matemáticas)",
     xlab = "matemáticas", pch = "I")

# type = "response" devuelve las predicciones en forma de probabilidad en lugar de en 
# log_ODDs
curve(predict(modelo, data.frame(matematica = x), type = "response"),
      col = "firebrick", lwd = 2.5, add = TRUE)

### MEDIANTE GGPLOT2 INCLUYENDO INTERVALOS DE CONFIANZA

datos$matricula <- as.character(datos$matricula)
datos$matricula <- as.numeric(datos$matricula)

# Se crea un vector con nuevos valores interpolados en el rango de observaciones.
nuevos_puntos <- seq(from = min(datos$matematica), to = max(datos$matematica),
                     by = 0.5)


# Predicciones de los nuevos puntos según el modelo. 
# Si se indica se.fit = TRUE se devuelve el error estándar de cada predicción
# junto con el valor de la predicción (fit).

predicciones <- predict(modelo, data.frame(matematica = nuevos_puntos),
                        se.fit = TRUE)

# Mediante la función logistica evaluada en los parametros ajustados se
#encuentran las probabilidades predichas para el nuevo conjunto de datos

predichos<- exp(predicciones$fit) / (1 + exp(predicciones$fit))

# Se calcula el límite inferior y superior del IC del 95% sustrayendo e
# incrementando el logODDs de cada predicción 1.95*SE. Una vez calculados los
# logODDs del intervalo se transforman en probabilidades con la función logit.

limite_inferior       <- predicciones$fit - 1.96 * predicciones$se.fit
limite_inferior_logit <- exp(limite_inferior) / (1 + exp(limite_inferior))
limite_superior       <- predicciones$fit + 1.96 * predicciones$se.fit
limite_superior_logit <- exp(limite_superior) / (1 + exp(limite_superior))

# Se crea un dataframe con los nuevos puntos y sus predicciones

datos_curva <- data.frame(matematica = nuevos_puntos,
                          probabilidad_matricula = predicciones_logit,
                          limite_inferior_logit = limite_inferior_logit, 
                          limite_superior_logit = limite_superior_logit)

ggplot(datos, aes(x = matematica, y = matricula)) +
  geom_point(aes(color = as.factor(matricula)), shape = "I", size = 3) + 
  geom_line(data = datos_curva, aes(y = probabilidad_matricula),
            color = "firebrick") + 
  geom_line(data = datos_curva, aes(y = limite_inferior_logit),
            linetype = "dashed") + 
  geom_line(data = datos_curva, aes(y = limite_superior_logit),
            linetype = "dashed") + 
  theme_bw() +
  labs(title = "Modelo regresión logística matrícula ~ nota matemáticas",
       y = "P(matrícula = 1 | matemáticas)", y = "matemáticas") + 
  theme(legend.position = "null") +
  theme(plot.title = element_text(hjust = 0.5))

# d) Evaluar el modelo
# Diferencia de residuos
# En R, un objeto glm almacena la "deviance" del modelo, así como la "deviance"
# del modelo nulo. 

dif_residuos <- modelo$null.deviance - modelo$deviance
dif_residuos

# Grados libertad

df <- modelo$df.null - modelo$df.residual
df

# p-value

p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)
p_value
paste("Diferencia de residuos:", round(dif_residuos, 4))

# El mismo cálculo se puede obtener directamente con:
anova(modelo, test = "Chisq")

#e) Comparación de clasificación predicha y observaciones

library(vcd)
predicciones <- ifelse(test = modelo$fitted.values > 0.5, yes = 1, no = 0)
matriz_confusion <- table(modelo$model$matricula, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

