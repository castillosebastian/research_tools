# Aprendizaje Automático
# Docente: Franco Ronchetti
# Ejemplo de Regresión lineal con Sk Learn

# -*- coding: utf-8 -*-
import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
import pandas as pd
from sklearn.preprocessing import StandardScaler  


#%% Cargar dataset 
data= pd.read_csv("data/boston.csv") 
x= data.drop('medv', axis=1)
y= data['medv']
n,d= x.shape
print("El dataset tiene %d registros, de %d dimensiones" %(n, d))

#%% visualizar distribución de los features
data.hist()

#%% Normalizar los datos
NORMALIZAR= 1

if (NORMALIZAR):
    scaler = StandardScaler()  
    scaler.fit(x)  
    x= scaler.transform(x)
    
#%% Entrenar modelo de Regresión Lineal
# crea el objeto modelo
modelo= LinearRegression()

#%% entrena
modelo.fit(x, y)
# predice
y_predict= modelo.predict(x)
#coeficientes (parámetros del modelo)
w= modelo.coef_
b= modelo.intercept_
# scores
mse_error= mean_squared_error(y_predict, y)
print("Error cuad. medio: %.2f" % mse_error)

mae_error= mean_absolute_error(y_predict, y)
print("Error abs. medio: %.2f" % mae_error)

r2_score= r2_score(y, y_predict)
print("r2_score: %.2f" % r2_score)

#%% plot coeficientes
plt.figure()
plt.bar(range(d+1), np.concatenate((w,[b])))
labels= data.columns.values; labels[-1]= 'Bias'
plt.xticks(range(w.shape[0]+1), labels, rotation=35)
plt.grid()

#%% Plot valores esperados vs valores predichos
plt.figure()
plt.scatter(y, y_predict,alpha=0.7, edgecolor='k'), plt.grid()
plt.xlabel("valores reales"), plt.ylabel("Valores predichos")


# %%
