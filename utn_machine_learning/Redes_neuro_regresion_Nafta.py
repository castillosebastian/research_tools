# -*- coding: utf-8 -*-

"""
@author: Franco
"""
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPRegressor
from sklearn.metrics import mean_squared_error,mean_absolute_error, r2_score
import pandas as pd

# param
LAYERS_SIZES= (16, 5) ## LAYERS_SIZES= ( 16, 5)  cantidad de neuronas ocultas en cada capa
MAX_ITE= 2000
ALPHA_LEARNING= 0.005#learning_rate_init : double, optional, default 0.001
REG_ALPHA= 1e-60
fxsolver= 'adam'##, sgd, adam, lbfgs
TOLERANCE=  1e-5 #tol : float, optional, default 1e-4
ACT_FX= 'tanh'#{identity, logistic, tanh, relu},
NORMALIZAR=1

#%% Cargar dataset ################################
dataset_path="data/consumo_nafta.csv"
data= pd.read_csv(dataset_path)
values= data.values # convierte de Pandas a NumPy array
x= values[:,0]
y= values[:,1]

#%% Dividir dataset y Calcular descriptores
# Dividir Training/testing
porc_test= 0.4
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size= porc_test)     

#%% Normalizar los datos
from sklearn.preprocessing import StandardScaler  
if (NORMALIZAR):
    scaler = StandardScaler()  
    # Don't cheat - fit only on training data
    scaler.fit(x_train.reshape(-1, 1))  
    x_train = scaler.transform(x_train.reshape(-1, 1))  
    # apply same transformation to test data
    x_test = scaler.transform(x_test.reshape(-1, 1))  
    # para los graficos
    x= scaler.transform(x.reshape(-1, 1))
    # y
    scaler.fit(y_train.reshape(-1, 1))  
    y_train = scaler.transform(y_train.reshape(-1, 1))  
    # apply same transformation to test data
    y_test = scaler.transform(y_test.reshape(-1, 1))  
    # para los graficos
    y= scaler.transform(y.reshape(-1, 1))
    
#%% entrenar modelo
modelo= MLPRegressor(solver= fxsolver, hidden_layer_sizes=LAYERS_SIZES, tol= TOLERANCE, alpha= REG_ALPHA, activation=ACT_FX, max_iter=MAX_ITE).fit(x_train.reshape(-1, 1) ,y_train)

#%% graficar datos originales (consumo de nafta)
plt.scatter(x_train,y_train, c='blue', s=40, alpha=0.9, label= 'Training')
plt.scatter(x_test,y_test, c='red', s=50, alpha=0.5, label='Testing')
plt.xlabel('veocidad km/h')
plt.ylabel('km/Litro')
plt.legend()
plt.grid()

#%% plotear curva aprendida
x_plot= np.linspace(min(x),max(x),100) #- puntos a plotear
y_plot= modelo.predict(x_plot.reshape(-1, 1)) # calcular descriptores para los nuevos puntos
plt.plot(x_plot, y_plot, 'g--', linewidth=5)

#%% scores
# predice
y_train_predict= modelo.predict(x_train.reshape(-1, 1) )
y_test_predict= modelo.predict(x_test.reshape(-1, 1) )
error_train= mean_squared_error(y_train_predict, y_train)
print("Error en training: %s" % error_train)
error_test= mean_squared_error(y_test_predict, y_test)
print("Error en testing: %s" % error_test)
mae_error= mean_absolute_error(scaler.inverse_transform(y_test_predict), scaler.inverse_transform(y_test))
print("Error abs. medio: %.2f" % mae_error)
r2_score= r2_score(y_train_predict, y_train)
print("r2_score: %.2f" % r2_score)
