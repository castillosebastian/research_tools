source("Unidad0_repos_and_tools.R")
pacman::p_load("data.table", "dtplyr", "dplyr")

data_train <- fread("data/dataset_train.csv")
skimr::skim(data_train)

# En este ejercicio vamos a crear una serie de variables sobre el dataset original. 
# Luego vamos a experimentar con cross validation y grid search para tunear hiperparámetros. 
# Finalmente, reproducimos el dataset en el de predict para generar el archivo de entrega.
# 
# Generar una variable de NDVI por fecha
# Filtrar el dataset para quedarnos con las bandas NDVI para cada fecha
# Separar el dataset de entrenamiento en 3 partes:
#   3.1 Train
# 3.2 Validación
# 3.3 Test
# Usando los datasets de los puntos 3.1 y 3.2, ajustar una búsqueda por grid search para un Random Forest 
# utilizando cross validation con 5 folds
# Medir el mejor modelo del punto 4 en el dataset 3.3
# Reproducir la creación del dataset para dataset_predict.csv
# Generar el archivo clases.csv con el formato solicitado

df = data_train %>% 
  pivot_longer(!c(id, cultivo), names_to = "variable", values_to = "value") %>% as_tibble() %>% 
  mutate(band = str_remove_all(str_sub(variable, 1,3), "_"),  
         fecha = str_remove_all(str_sub(variable, -11), "_"))  %>% 
  select(-variable) %>%
  pivot_wider(names_from = band, values_from = value) %>% 
  mutate(foto_mes =  format(as.Date(fecha), "%Y%m")) %>% 
  select(-fecha) %>% 
  rowwise() %>% 
  mutate(nvdi = (B8-B4)/(B8+B4), 
         # EVI(a="NIR"/B8,b="Red"B4,c="Blue"/B2,Pixel.Depth) = 2.5*(NIR - Red) / (NIR + 6*Red - 7.5*Blue + 1) https://rdrr.io/cran/LSRS/man/EVI.html
         evi =  2.5*(B8 - B4) / (B8 + 6*B4 - 7.5*B2 + 1)) %>% 
  mutate(foto_mes = as.numeric(foto_mes))

write.table(df, "~/R/experimet_template/data/raw/data_train.csv", col.names = T, row.names = F, sep = ",")

data_predict <- fread("data/dataset_predict.csv")

df = data_predict %>% 
  pivot_longer(!c(id), names_to = "variable", values_to = "value") %>% as_tibble() %>% 
  mutate(band = str_remove_all(str_sub(variable, 1,3), "_"),  
         fecha = str_remove_all(str_sub(variable, -11), "_"))  %>% 
  select(-variable) %>%
  pivot_wider(names_from = band, values_from = value) %>% 
  mutate(foto_mes =  format(as.Date(fecha), "%Y%m")) %>% 
  select(-fecha) %>% 
  rowwise() %>% 
  mutate(nvdi = (B8-B4)/(B8+B4), 
         # EVI(a="NIR"/B8,b="Red"B4,c="Blue"/B2,Pixel.Depth) = 2.5*(NIR - Red) / (NIR + 6*Red - 7.5*Blue + 1) https://rdrr.io/cran/LSRS/man/EVI.html
         evi =  2.5*(B8 - B4) / (B8 + 6*B4 - 7.5*B2 + 1)) %>% 
  mutate(foto_mes = as.numeric(foto_mes))


write.table(df, "~/R/experimet_template/data/raw/data_predict.csv", col.names = T, row.names = F, sep = ",")


df


