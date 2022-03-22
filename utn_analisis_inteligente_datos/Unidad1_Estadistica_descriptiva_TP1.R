# TP1
# https://rstudio-pubs-static.s3.amazonaws.com/647537_aa2563a0272f4e7dbc8852012951a00e.html
# https://www.r-bloggers.com/2020/11/skewness-and-kurtosis-in-statistics/#:~:text=Skewness%20values%20and%20interpretation&text=A%20rule%20of%20thumb%20states,1%20or%20greater%20than%201

IMCinfantil <- readxl::read_excel("~/research_tools/utn_analisis_inteligente_datos/IMCinfantil.xlsx")

head(IMCinfantil, 1)
skimr::skim(IMCinfantil)

# IMCinfantil
IMCinfantil %>% 
  ggplot(aes(SEXO, IMC)) +
  geom_boxplot() +
  geom_quasirandom(alpha = 0.3) +
  theme_bw()

# Las variables de la muestra presentan coeficientes de variación altos, lo que implica que existe una 
# importante variación den los datos, particularmente sucede en PESO y PIMC.
IMCinfantil %>%
  dplyr::select(-c(SEXO, CatPeso, PACIENTE)) %>% 
  summarise_all(raster::cv, na.rm =T)

# Medidas de forma de la distribución: Sesgo y Curtosis
IMCinfantil %>%
  dplyr::select(-c(SEXO, CatPeso, PACIENTE)) %>% 
  summarise_all(moments::skewness)

IMCinfantil %>%
  dplyr::select(-c(SEXO, CatPeso, PACIENTE)) %>% 
  summarise_all(moments::kurtosis)

# Correlacion
df = IMCinfantil %>% dplyr::select_if(is.numeric) %>% select(-PACIENTE)
M <- cor(df)
head(round(M,2))
#corrplot(M, method="number")
corrplot.mixed(M, order = 'AOE')

df <- list()
df$IMCinfantil <- readxl::read_excel("~/research_tools/utn_analisis_inteligente_datos/IMCinfantil.xlsx")
df$gorriones <- readxl::read_excel("~/research_tools/utn_analisis_inteligente_datos/gorriones.xlsx")
df$Internet2013 <- readxl::read_excel("~/research_tools/utn_analisis_inteligente_datos/Internet2013.xlsx")
df$recepcionistas <- readxl::read_excel("~/research_tools/utn_analisis_inteligente_datos/recepcionistas.xlsx")

DataExplorer::create_report(df$IMCinfantil)


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


# otros estadìsticos
datos_matriz <- datos %>% 
  pivot_wider(names_from = variables, values_from = valores) #%>% 
  #mutate(juez = as.integer(str_remove_all(juez, '[a-z]')))


matriz_varianza <- datos_matriz %>% 
  select_if(is.numeric) %>% 
  var(., use="complete.obs") %>% 
  round(., 2)   

matriz_covarianza <- datos_matriz %>% 
  select_if(is.numeric) %>% 
  cov(., use="complete.obs") %>% 
  round(., 2)   

matriz_correlacion <- datos_matriz %>% 
  select_if(is.numeric) %>% 
  cor(., use="complete.obs") %>% 
  round(., 2)   
 
corrplot::corrplot.mixed(matriz_correlacion ,order = 'AOE')

determinante <- det(matriz_covarianza)


