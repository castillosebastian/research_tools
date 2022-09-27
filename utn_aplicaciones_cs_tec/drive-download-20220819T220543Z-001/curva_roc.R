
library(tidyverse)

tb_entrega %>% 
  



library(pROC)


roc <- roc(tb_entrega$clase01,tb_entrega$pred,grid=T)

plot(roc,col="red",xlim=c(1,0))

# plot(roc2,col="red",xlim=c(1,0), add=T)


