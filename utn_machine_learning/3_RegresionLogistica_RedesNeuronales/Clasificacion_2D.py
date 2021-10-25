import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import accuracy_score,classification_report
import pandas as pd
import utn_utils1  # funciones útiles de visualización para Aprendizaje Automático

#%% Cargar dataset
db_name= '2D_circulos.csv' # evaluar con otros data

df= pd.read_csv('data/'+ db_name)
data= df.values
x= data[:,0:-1]
y= data[:,-1] # suponemos que el target está en la última columna. Cuidado con esto!
n_clases= np.max(y)+1

#%% PARAM
PLOT_CONF_MAT= 1
PLOT_ROC= 0
PLOT_BOUNDARY= 1
PLOT_SIGMOID= 0
NORMALIZAR= 1

# parámetros de la red
LAYERS_SIZES= (20) # cantidad de neuronas ocultas en cada capa
MAX_ITE= 500
TOLERANCE=  1e-4 #tol : float, optional, default 1e-4
SOLVER= 'adam'##, sgd, adam, lbfgs
ACT_FX= 'tanh'#{identity, logistic, tanh, relu},
alpha= 0.003 #default=0.001

#%% Dividir Training/testing
porc_test= 0.3
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size= porc_test)
print("\t %d datos para training" %x_train.shape[0])
print("\t %d datos para testing" %x_test.shape[0])

#%% Normalizar los datos
from sklearn.preprocessing import StandardScaler  
if (NORMALIZAR):
    scaler = StandardScaler()  
    # Don't cheat - fit only on training data
    scaler.fit(x_train)  
    x_train = scaler.transform(x_train)  
    # apply same transformation to test data
    x_test = scaler.transform(x_test)  
    # para los graficos
    x= scaler.transform(x)
    
#%% Entrenamiento del modelo
modelo= MLPClassifier(hidden_layer_sizes=LAYERS_SIZES, max_iter=MAX_ITE, tol= TOLERANCE, verbose= True, activation= ACT_FX, learning_rate_init=alpha, solver=SOLVER)
# modelo= LogisticRegression(class_weight={1:0.5, 0:0.5}) 
# puede probar con otros clasificadores. Ej. KNeighborsClassifier(n_neighbors=5)

modelo.fit(x_train,y_train)

# w= modelo.coefs_
# b= modelo.intercepts_

#%% eficacia del modelo
y_pred_train= modelo.predict(x_train)
print("*****************************************************")
print("Train   Accuracy: %.2f" % accuracy_score(y_train, y_pred_train ))
print(classification_report(y_train, y_pred_train))

y_pred= modelo.predict(x_test)
print("*****************************************************")
print("Test   Accuracy: %.2f" % accuracy_score(y_test, y_pred ))
print(classification_report(y_test, y_pred))
print("*****************************************************")

y_pred_proba= np.max(modelo.predict_proba(x_test), axis=1)
print("Análisis de probabilidades: mean %.2f,  std %.2f" % (y_pred_proba.mean(), y_pred_proba.std()))

#%% plot boundary
if ((PLOT_BOUNDARY) & (x.shape[1]==2)):
    AA_utils.plot_frontera_de_decision_2D(modelo, x_train, y_train, x_test=x_test, y_test=y_test, sigmoid=PLOT_SIGMOID)  

#%% ROC
if (PLOT_ROC & (n_clases==2)):
    AA_utils.plot_ROC_curve(modelo, x_test, y_test)

#%% CONFUSION MATRIX
if (PLOT_CONF_MAT):
    y_pred= modelo.predict(x_test)
    AA_utils.plot_confusion_matrix(y_test, y_pred)

