
import matplotlib.pyplot as plt
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import f1_score, recall_score, precision_score, accuracy_score,classification_report
import AA_utils # funciones útiles de visualización para Aprendizaje Automático

#%% PARAM
PLOT_CONF_MAT= 1 
PLOT_COEF= 1
NORMALIZAR= 0

#%%
X= np.load('data/MNIST.npy')
y= np.load('data/MNIST_labels.npy')

#%% Dividir Training/testing
porc_test= 0.3
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size= porc_test)
print("\t %d datos para training" %X_train.shape[0])
print("\t %d datos para testing" %X_test.shape[0])

if (NORMALIZAR):
    scaler = StandardScaler()
    X_train = scaler.fit_transform(X_train)
    X_test = scaler.transform(X_test)

#%% Entrenar modelo
modelo = LogisticRegression(solver='saga', tol=0.1) # Saga es más rápido para dataset muy grandes
modelo.fit(X_train, y_train)

#%% eficacia del modelo
y_pred_train= modelo.predict(X_train)
print("\nTrain   Accuracy: %.2f" % accuracy_score(y_train, y_pred_train ))
y_pred= modelo.predict(X_test)
print("\nTest   Accuracy: %.2f" % accuracy_score(y_test, y_pred ))
print(classification_report(y_test, y_pred))

#%% CONFUSION MATRIX
if (PLOT_CONF_MAT):
    y_pred= modelo.predict(X_test)
    AA_utils.plot_confusion_matrix(y_test, y_pred)
    
#%% Visualizar una imagen aleatoria
i=  np.random.randint(0, X_test.shape[0])
img_i= np.reshape(X_test[i, :], (28,28))
plt.figure(), plt.imshow(img_i, 'gray'), plt.title('Dato aleatorio')
x_i= X_test[i, :]
#x_nuevo[0]=x_test[nuevo, :]
y_i= y_test[i]
y_predict= modelo.predict(x_i.reshape(1, -1))
print("\nElemento aleatorio del Test Set:\nEl modelo predice: %f " % (y_predict) )
print("El valor real es: %f" % (y_i))

#%% visualizar pesos de clasificación lineal para MNIST
if (PLOT_COEF):
    w= modelo.coef_
    
    im_max= np.abs( np.max(w)); im_min= np.abs( np.min(w))
    colorbar_max= max(im_max, im_min)
    w_r= np.reshape(w, (10, 28,28))
    i= 8
    classi= w_r[i,:,:] 
    
    fig, ax = plt.subplots(2, 5)
    plt.suptitle('Coeficientes aprendidos para cada clase, \nagrupados en forma matricial de 28x28')
    index=0
    from mpl_toolkits.axes_grid1 import make_axes_locatable
    
    for row in range(0,2):
        for col in range(0,5):
    #        ax_im= ax[row][col].imshow(w_r[:,:,index] , cmap='seismic', vmin= -colorbar_max, vmax=colorbar_max)
            colorbar_max= max(np.abs( np.max(w_r[index,:,:] )), np.abs( np.min(w_r[index,:,:] )))
            ax_im= ax[row][col].imshow(w_r[index,:,:] , cmap='seismic', vmin= -colorbar_max, vmax=colorbar_max)
            ax[row][col].get_xaxis().set_visible(False)
            ax[row][col].get_yaxis().set_visible(False)
            ax[row][col].set_title(str(index))
            divider = make_axes_locatable(ax[row][col])
            cax = divider.append_axes("right", size="5%", pad=0.05)
    
            fig.colorbar(ax_im, ax= ax[row][col], cax=cax) 
            index += 1