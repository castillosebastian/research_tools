library(RSelenium)
library(lubridate)
library(tidyverse)
library(rvest)

#https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html

# inicio el server de selenium
rdriver <- rsDriver(browser = "firefox")

# creo un cliente para darle instrucciones
obj <- rdriver$client

# navego a la pagina inicial
obj$navigate("https://votaciones.diputados.gob.ar/")

# extraigo la informacion de la pagina para usarla con rvest
html <- obj$getPageSource()[[1]]
# Esto es scraping clasico, extraigo el html con rvest
html <- read_html(html)
# Encuentro la tabla de sesiones (a pesar de que no se ve completa, carga completa)
tabla <- html %>% 
  html_elements(xpath = '//tbody[@id="container-actas"]')
# Extraigo la fecha de la primera fila, primera celda
fecha <- tabla %>% 
  html_element(xpath = '//tr[1]/td[1]') %>% 
  html_text() %>% 
  dmy_hm() # uso esta funcion de lubridate para convertir el texto a fecha

# Extraigo la url de la sesion de la primera fila, ultima celda (segundo link)
url <- tabla %>% 
  html_element(xpath = '//tr[1]/td[5]//a[2]') %>%
  html_attr("href")
# Genero una tablita con los datos anteriores
datos_iniciales <- as_tibble(cbind(fecha=as.character(fecha),url=url))
i <- 2 # Como ya tengo la primera linea, continuo con la segunda
# Establezco una fecha limite hasta donde llegar, porque ya tengo datos
fecha_limite <- ymd("2019-01-01")
# Ahora itero hasta que la fecha sea inferior a mi fecha limite
while(fecha >= fecha_limite){
  fecha <- tabla %>% 
    html_element(xpath = paste0('//tr[',i,']/td[1]')) %>% 
    html_text() %>% 
    dmy_hm()
  # Si hay segundo link en la ultima celda de la fila, ejecutara la opcion else de este if. En caso de que sea NA es porque no existe el segundo link. En este caso el link que me interesa es el primero y se ejecutara la primera instruccion de este if
  if(tabla %>% 
     html_element(xpath = paste0('//tr[',i,']/td[5]//a[2]')) %>% 
     html_attr("href") %>% 
     is.na()){
    url <- tabla %>% 
      html_element(xpath = paste0('//tr[',i,']/td[5]//a')) %>%
      html_attr("href")
  }else{
    url <- tabla %>% 
      html_element(xpath = paste0('//tr[',i,']/td[5]//a[2]')) %>%
      html_attr("href")
  }
  # Agrego los nuevos datos a la tabla
  datos_iniciales <- bind_rows(datos_iniciales,as_tibble(cbind(fecha=as.character(fecha),url=url)))
  i <- i+1
}

# La url me trajo solamente el subdominio, le agrego el dominio
datos_iniciales <- datos_iniciales %>% 
  mutate(url=paste0("https://votaciones.diputados.gob.ar",url))


# Vamos a descargar los datos de la primera sesion de la lista
obj$navigate(datos_iniciales$url[1])
html <- obj$getPageSource()[[1]]
html <- read_html(html)
# La informacion de la sesion
info_sesion <- html %>% 
  html_element(xpath = '//div[@class="row"]/div/h5/b') %>% 
  html_text()

info_sesion <- info_sesion %>% 
  tibble() %>% 
  separate(col = 1,into = c("periodo","reunion","acta"),sep = "-") %>% 
  mutate(periodo=str_extract(periodo,"\\d+"),
         reunion=str_extract(reunion,"\\d+"),
         acta=str_extract(acta,"\\d+"))
# El titulo del expediente
expediente <- html %>% 
  html_element(xpath = '//div/ul/h4') %>% 
  html_text() %>% 
  trimws()
# Agrego la informacion de nuevas columnas a la tabla
info_sesion$expediente <- expediente
info_sesion$fecha <- datos_iniciales$fecha[1]
info_sesion$url <- datos_iniciales$url[1]
# Descargo el CSV
botonCSV <- obj$findElement(using = "partial link text", value = "CSV")
Sys.sleep(2) # Espero 2 segundos para darle tiempo a encontrar el boton
botonCSV$clickElement()

# Ahora vamos a repetir por cada una de las sesiones de la lista
for(s in 2:nrow(datos_iniciales)){
  # Un tryCatch por si algo falla
  tryCatch({obj$navigate(datos_iniciales$url[s])
  html <- obj$getPageSource()[[1]]
  html <- read_html(html)
  # le cambio el nombre a la variable para que no pise la otra
  info_ses <- html %>% 
    html_element(xpath = '//div[@class="row"]/div/h5/b') %>% 
    html_text()
  
  info_ses <- info_ses %>% 
    tibble() %>% 
    separate(col = 1,into = c("periodo","reunion","acta"),sep = "-") %>% 
    mutate(periodo=str_extract(periodo,"\\d+"),
           reunion=str_extract(reunion,"\\d+"),
           acta=str_extract(acta,"\\d+"))
  
  expediente <- html %>% 
    html_element(xpath = '//div/ul/h4') %>% 
    html_text() %>% 
    trimws()
  
  info_ses$expediente <- expediente
  info_ses$fecha <- datos_iniciales$fecha[s]
  info_ses$url <- datos_iniciales$url[s]
  
  info_sesion <- bind_rows(info_sesion,info_ses)
  
  botonCSV <- obj$findElement(using = "partial link text", value = "CSV")
  Sys.sleep(2) # Espero 2 segundos para darle tiempo a encontrar el boton
  botonCSV$clickElement()}, error = function(e) {
    # Si algo falla, imprimo el numero de iteracion, la url que fallo, y el mensaje de error
    print(s)
    print(datos_iniciales$url[s])
    print(e)
  }
  )
}

# Finalizo la actividad
obj$close()
rdriver$server$stop()
gc()
