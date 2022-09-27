require("data.table")
require("lightgbm")
#################### Parametros para cambiar ######################
setwd("/Volumes/GoogleDrive/Mi unidad/99yo/teach/DMCT-UTN/DMCT-UTN2022/datos/covid")
# path a la carpeta base
p_carpeta_base <- "./datasets"
# nombre del archivo a importar como dataset
p_archivo_dataset <- "covid_utn2022.rds"
# nombre de la columna que usare como clase
# columna_clase <- "resultado"
columna_clase <- "resultado"

p_objective <- "binary" # esto si es un problema con clase numérica. Si la clase es binaria, cambiar por "binary"
p_parametro_optimizar <- "auc" # cambiar por lo que estemos estimando. Si es un problema de clasificacion, cambiar por "auc"
# Consultar lista en https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
# IMPORTANTE: no usar los alias de esa lista, sino la denominación principal
ksemilla_azar  <- 102191  #Aqui poner la propia semilla

###################################################################
#------------------------------------------------------------------------------
setwd(p_carpeta_base)
# Aqui el metodo corresponde a un archivo rds. Cambiar por el metodo adecuado para su tipo de archivo
dsLearn <- readRDS(p_archivo_dataset)
dsLearn <- as.data.table(dsLearn)

#paso la clase a binaria que tome valores {0,1}  enteros
dsLearn[ , clase01 := ifelse( resultado=="muerte", 1L, 0L) ]
#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dsLearn), c(columna_clase,"clase01") )

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dsLearn[ , campos_buenos, with=FALSE]),
                        label= dsLearn[["clase01"]] )

param_basicos  <- list( objective= p_objective,
                        metric= p_parametro_optimizar, # rmse o el que prefiera, definido al inicio
                        first_metric_only= TRUE,
                        boost_from_average= TRUE,
                        feature_pre_filter= FALSE,
                        verbosity= -100,
                        seed= ksemilla_azar,
                        max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                        min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                        lambda_l1= 0.0,         #por ahora, lo dejo fijo
                        lambda_l2= 0.0,         #por ahora, lo dejo fijo
                        max_bin= 31,            #por ahora, lo dejo fijo
                        num_iterations= 9999,    #un numero muy grande, lo limita early_stopping_rounds
                        force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
                        # ,min_data_in_leaf = 2
)

param_variable <- list(
  learning_rate=0.1
  ,feature_fraction=0.5
  ,min_data_in_leaf=500
  ,num_leaves=100
)

#el parametro discolo, que depende de otro
esr  <- list(  early_stopping_rounds= as.integer(50 + 5/param_variable$learning_rate) )

param_completo  <- c( param_basicos, param_variable, esr )

set.seed( ksemilla_azar )
kfolds  <- 5   # cantidad de folds para cross validation

modelocv  <- lgb.cv( data= dtrain,
                     stratified= TRUE, #sobre el cross validation
                     nfold= kfolds,    #folds del cross validation
                     param= param_completo,
                     verbose= -100
)

modelocv$record_evals$valid[[p_parametro_optimizar]]$eval

unlist(modelocv$record_evals$valid[[p_parametro_optimizar]]$eval)[ modelocv$best_iter ]

