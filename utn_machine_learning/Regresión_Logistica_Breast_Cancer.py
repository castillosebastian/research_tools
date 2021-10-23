# -*- coding: utf-8 -*-
"""
Created on Thu Jul 19 23:13:00 2018

@author: Franco

Ejercicio de Clasificación Binaria.

Dataset utilizado: Breast Cancer. 2 clases. 

Clasificador: Regresión logística (perceptron)
"""
from sklearn.model_selection import train_test_split
from sklearn.linear_model import  LogisticRegression
from sklearn.metrics import  f1_score, recall_score, precision_score, accuracy_score
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

#%% Cargar datos
# Una vez cargados los datos, verificar las variables x e y en el explorador de variables.
# breast_cancer.csv posee el dataset completo
# breast_cancer_radius.csv posee el dataset con solo un feature (radio del tumor)
db_name= 'data/breast_cancer_radius.csv'
df= pd.read_csv(db_name)
x= df['mean radius'].values  #df['mean radius'] # para probar con sólo con el radio del tumor
y= df['target'].values

#%% Dividir Training y Testing
porc_test= 0.1
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size= porc_test)
print("%d datos para training" %x_train.shape[0])
print("%d datos para testing" %x_test.shape[0])

#%% Entrenar modelo de Regresión Logística
# crea el objeto modelo
# si se quiere dar más peso a una clase, parametrizar con class_weight.
#   por ejemplo class_weight={1:0.9, 0:0.1}
modelo= LogisticRegression() 
# entrenar
modelo.fit(x_train.reshape(-1, 1), y_train)
# obtener coeficientes. Sólo necesarios si se quiere graficar o interpretar el modelo.
w= modelo.coef_
b= modelo.intercept_
print("Coeficientes: w= %.2f,  b= %.2f" % (w, b))

#%% calcular métricas para training y validar con el Test_set
y_pred_train= modelo.predict(x_train.reshape(-1, 1)) # el modelo predice para cada dato una de las clases conocidas (0 o 1)
print("\nTrain   Accuracy: %.2f" % accuracy_score(y_train, y_pred_train ))
print("       Precision: %.2f" % precision_score(y_train, y_pred_train) )
print("          Recall: %.2f" % recall_score(y_train, y_pred_train ))
print("       f-measure: %.2f" % f1_score(y_train, y_pred_train))

y_pred= modelo.predict(x_test.reshape(-1, 1))
print("\nTest    Accuracy: %.2f" % accuracy_score(y_test, y_pred ))
print("       Precision: %.2f" % precision_score(y_test, y_pred) )
print("          Recall: %.2f" % recall_score(y_test, y_pred ))
print("       f-measure: %.2f" % f1_score(y_test, y_pred))

#%% graficar datos y curva entrenada
plt.figure()
plt.scatter(df['mean radius'], df['target'], c=df['target'], s=50)
plt.xlabel('mean radius')
plt.grid()
##graficar curva aprendida (para 1 feature)
x_graph= np.linspace(0,30)
y_graph= modelo.predict_proba(x_graph.reshape(-1,1))[:,1]
plt.plot(x_graph,y_graph)


