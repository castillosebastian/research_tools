# principal
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, dtplyr, log4r,
               lubridate, Rcpp, yaml, rlist, ggplot2, 
               ROCR, methods, Matrix, caret, rsample, jtools)
options(scipen = 999)
