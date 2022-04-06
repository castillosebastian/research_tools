# componentes principales
source("~/research_tools/utn_analisis_inteligente_datos/Unidad0_repos_and_tools.R")

# https://rpubs.com/Cristina_Gil/PCA
df_raw <- readxl::read_excel("~/research_tools/utn_analisis_inteligente_datos/nadadores.xlsx")
df = df_raw %>% dplyr::select(-Nadador)

# Las componentes principales son una combinación lineal de las variables originales
# específicamente son son autovectores que se toman de la matriz de correlaciones

df_pca_conv =  prcomp(df, center = TRUE, scale. = F)

df_pca_corr =  prcomp(df, center = TRUE, scale. = T)

# Veamos los loadings. Le dice rotation porque la matriz de rotaciòn representa una rotaciòn del espacio euclideano
df_pca_corr$rotation

# vectores de los scores
df_pca_corr$x

# resumen
summary(df_pca_corr)

# Tabla
df_pca_corr %>%
  tidy(matrix = "eigenvalues")

df_pca_corr %>%
  tidy(matrix = "eigenvalues") %>% 
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))) 



