# -*- coding: utf-8 -*-
"""
Created on Fri Jul 20 17:39:46 2018

@author: Franco Ronchetti

Funciones utilitarias para visualizacióny procesamiento de imágenes. Curso Aprendizaje Automático.
"""
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix, roc_curve, roc_auc_score, precision_recall_curve
import numpy as np
from skimage.color import rgb2gray
from skimage.feature import hog
import itertools
from mpl_toolkits.mplot3d import Axes3D
from skimage import exposure

# Crea y Grafica una matriz de confusión
# PARAM:
#       real_target = vector con valores esperados
#       pred_target = vector con valores calculados por un modelo
#       classes = lista de strings con los nombres de las clases.
def plot_confusion_matrix(real_target, pred_target, classes=[],  normalize=False, title='Matriz de confusión', cmap=plt.cm.Blues):

    from sklearn.metrics import confusion_matrix
    import itertools
    if (len(classes)==0):
        classes= [str(i) for i in range(int(max(real_target)+1))]  # nombres de clases consecutivos
    cm= confusion_matrix(real_target, pred_target)
    if normalize:
        cm = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
    plt.figure()
    plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.title(title)
    plt.colorbar()
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=45)
    plt.yticks(tick_marks, classes)

    fmt = '.2f' if normalize else 'd'
    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, format(cm[i, j], fmt),
                 horizontalalignment="center",
                 color="white" if cm[i, j] > thresh else "black", size= 16)

#    plt.tight_layout()
    plt.ylabel('True label')
    plt.xlabel('Predicted label')
    
# Grafica la curva ROC para el modelo y los datos pasados como argumentos 
# Además, imprime el área bajo la curva
def plot_ROC_curve(modelo, x, y):
    # probabilidades para los datos
    y_score = modelo.predict_proba(x)[:,1] # se queda con la clase 1
    # Create true and false positive rates
    false_positive_rate, true_positive_rate, threshold = roc_curve(y, y_score)
    # Precision-recall curve
    precision, recall, _ = precision_recall_curve(y, y_score)
    
    # ROC
    plt.figure()
    plt.title('ROC. Receiver Operating Characteristic')
    plt.plot(false_positive_rate, true_positive_rate, label='ROC curve (area = %0.2f)' % roc_auc_score(y, y_score))
    plt.plot([0, 1], ls="--")
    plt.plot([0, 0], [1, 0] , c=".7"), plt.plot([1, 1] , c=".7")
    plt.legend(loc="lower right")
    plt.ylabel('True Positive Rate (Recall)')
    plt.xlabel('False Positive Rate (1- Especificidad)')
    plt.show()
    
    # precision-recall curve
    plt.figure()
    plt.step(recall, precision, color='b', alpha=0.2, where='post')
    plt.fill_between(recall, precision, step='post', alpha=0.2, color='b')
    plt.xlabel('Recall')
    plt.ylabel('Precision')
    plt.ylim([0.0, 1.05])
    plt.xlim([0.0, 1.0])
    plt.title('Precision-Recall curve')   
    
# imprime los puntos para un dataset bidimensional junto con la frontera de decisión del modelo
def plot_2d_points_and_boundary(x, y, modelo, sigmoid= False, poliF=False):
    # nueva figura
    plt.figure()
    # gráfico con la predicción aprendida
    x_min, x_max = x[:, 0].min() - 1, x[:, 0].max() + 1
    y_min, y_max = x[:, 1].min() - 1, x[:, 1].max() + 1
    xx, yy = np.meshgrid(np.arange(x_min, x_max, (x_max- x_min)/200),
                     np.arange(y_min, y_max, (y_max- y_min)/200))
    if (poliF):
        Z= poliF.fit_transform(np.c_[xx.ravel(), yy.ravel()])[:,1:]
    else:
        Z= np.c_[xx.ravel(), yy.ravel()]  
        
    if (sigmoid):
        Z = modelo.predict_proba(Z)[:,1]
        titulo= 'Función de transferencia del modelo'
    else: 
        Z = modelo.predict(Z)
        titulo= 'Frontera de decisión del modelo'
    Z = Z.reshape(xx.shape)
    plt.contourf(xx, yy, Z, alpha=0.3)#,  cmap='RdBu')    
    plt.colorbar()
    plt.title(titulo)
    # puntos con las clases
    plt.scatter(x[:,0], x[:,1], c=y)
    
# imprime los puntos para un dataset bidimensional junto con la frontera de decisión del modelo
def plot_frontera_de_decision_2D(modelo, x, y, x_test=0, y_test=0, title="",detail=0.02, sigmoid=False):

    assert x.shape[1]==2,f"x debe tener solo dos variables de entrada (tiene {x.shape[1]})"
    # nueva figura
    plt.figure()
    # gráfico con la predicción aprendida
    x_min, x_max = x[:, 0].min() - 1, x[:, 0].max() + 1
    y_min, y_max = x[:, 1].min() - 1, x[:, 1].max() + 1
    xx, yy = np.meshgrid(np.arange(x_min, x_max, detail),
                         np.arange(y_min, y_max, detail))

    Z = np.c_[xx.ravel(), yy.ravel()]

    if (sigmoid & (max(y)==1)): # no plotea sigmoidea si hay más de 2 clases
        Z = modelo.predict_proba(Z)[:,1]
        titulo= 'Función de transferencia del modelo'
    else: 
        Z = modelo.predict(Z)
        titulo= 'Frontera de decisión del modelo'

    Z = Z.reshape(xx.shape)
    plt.contourf(xx, yy, Z, alpha=0.3)  # ,  cmap='RdBu')
    plt.colorbar()
    plt.title(titulo)

    # puntos con las clases
    plt.scatter(x[:, 0], x[:, 1], marker="o", c=y, label= 'Training')
    if (isinstance(x_test, int)==False):
        plt.scatter(x_test[:,0], x_test[:,1], marker="+",c=y_test, alpha=0.8, s=80, label= 'Testing') 
        plt.legend() 
        
def print_classification_report(y_true, y_pred):
    from sklearn.metrics import mean_squared_error, f1_score,  recall_score, precision_score, accuracy_score

    n_clases= (max(y_true)+1)
    # probs to class number
    y_pred = np.argmax(y_pred,axis = 1) 
    print("   Accuracy: %.2f    soporte: %d" % (accuracy_score(y_true, y_pred), y_true.shape[0]))
    if (n_clases==2):
        print("  Precision: %.2f" % precision_score(y_true, y_pred) )
        print("     Recall: %.2f" % recall_score(y_true, y_pred ))
        print("  f-measure: %.2f" % f1_score(y_true, y_pred))
        
###############################################################################################
# IMágenes ###################################################################################
def preprocesar_img(img, feature):
    # parámetros del HOG
    hog_pixels_per_cell=(8, 8)
    hog_cells_per_block= (1, 1)
    hog_bins= 9
    # preprocesar una imagen aplicando el feature pasados como argumento
    # convertir a escala de grises
    img_= rgb2gray(img)  

    if (feature=='hog'):
        img_feature= hog(img_, orientations=hog_bins, pixels_per_cell=hog_pixels_per_cell, cells_per_block=hog_cells_per_block)

    if (feature=='gray'):
        img_size= img_.shape[0]
        img_feature= img_.reshape(1,img_size*img_size)
    return img_feature

def ver_imagen_hog(img):
    # parámetros del HOG
    hog_pixels_per_cell=(8, 8)
    hog_cells_per_block= (1, 1)
    hog_bins= 9
    # convertir a grises
    img= rgb2gray(img) 
    fd_hog, img_hog= hog(img, orientations=hog_bins, pixels_per_cell=hog_pixels_per_cell, 
                     cells_per_block=hog_cells_per_block, visualise=True)
    
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(8, 4), sharex=True, sharey=True)
     
    ax1.axis('off')
    ax1.imshow(img, cmap=plt.cm.gray)
    ax1.set_title('Input image')
     
    # Rescale histogram for better display
    img_hog = exposure.rescale_intensity(img_hog, in_range=(0, 10))   
    ax2.axis('off')
    ax2.imshow(img_hog, cmap=plt.cm.gray)
    ax2.set_title('Histogram of Oriented Gradients')
    plt.show() 
    
def sliding_window(classifier, image, windowSize, stepSize, threshold =0.5, feature= 'hog'):
    # imagen temporal para marcar la detección
    image2= image
    # matriz de probs para la clase 1
    probs= np.zeros((image.shape[1], image.shape[0]))
    # slide a window across the image
    for y in range(0, image.shape[0]-windowSize, stepSize):
        for x in range(0, image.shape[1]-windowSize, stepSize):
            wind_image= image[y:y + windowSize, x:x + windowSize,:]
            # preprocesar y aplicar clasificador sobre la ventana
            img_preproc= preprocesar_img(wind_image, feature)
            #ver_imagen_hog(wind_image)
            probs[x,y]= classifier.predict_proba(img_preproc.reshape(1, -1))[0,1]  
            if probs[x,y]> threshold:
                # dibujar contorno rojo
                image2[y:y + windowSize, x]= [1, 0, 0]
                image2[y:y + windowSize, x + windowSize]= [1, 0, 0]
                image2[y, x:x + windowSize]= [1, 0, 0]
                image2[y+ windowSize, x:x + windowSize]= [1, 0, 0]
                
    return probs, image2

###########################################################################################
# CLUSTERING ##############################################################################
## punto elbow, K-means
def graficar_punto_elbow(X, N, GMM=False):
    from sklearn.cluster import KMeans
    from sklearn.mixture import  GaussianMixture
    
    plt.figure()
    Nc = range(1, N+1)
    if (GMM):
        kmeans = [GaussianMixture(n_components=i) for i in Nc]
    else:
        kmeans = [KMeans(n_clusters=i) for i in Nc]

    # score = [kmeans[i].fit(X).score(X) for i in range(len(kmeans))]
    if (GMM):
        score = [kmeans[i].fit(X).score(X) for i in range(len(kmeans))]
        scoreBIC = [kmeans[i].fit(X).bic(X) for i in range(len(kmeans))]
    else:
        score = [kmeans[i].fit(X).inertia_ for i in range(len(kmeans))]

    plt.plot(Nc,score, 'b')
    plt.xlabel('Number of Clusters')
    plt.title('Elbow Curve')
    plt.show()
    plt.grid()
    if (GMM):
        plt.ylabel('log(verosimilitud)')
        plt.figure()
        plt.plot(Nc, scoreBIC, 'r--'); plt.title('BIC Curve'); plt.xlabel('Number of Clusters')
        plt.ylabel('Bayesian information criterion'); plt.show(); plt.grid()
    else:
        plt.ylabel('Promedio de sumas de distancias')
    

# índice Silhouette
def graficar_indice_silhouette(X, N, GMM=False):
    from sklearn.cluster import KMeans
    from sklearn.metrics import silhouette_score
    from sklearn.mixture import  GaussianMixture
    
    plt.figure()
    Nc = range(2, N+1)
    if (GMM):
        # kmeans = [GaussianMixture(n_components=i) for i in Nc]
        print("Sin terminar silhouette para GMM")
    else:
        kmeans = [KMeans(n_clusters=i) for i in Nc]

    # score = [kmeans[i].fit(X).score(X) for i in range(len(kmeans))]
    score = [silhouette_score(X, kmeans[i].fit(X).predict(X)) for i in range(len(kmeans))]
    score
    plt.plot(Nc,score)
    plt.xlabel('Número de Clusters')
    plt.ylabel('Promedio de índice silhouette')
    plt.title('Índice silhouette para distintos K')
    plt.show()
    plt.grid()
    
# grafico     
def graficar_indice_silhouette_k(X, n_clusters):
    from sklearn.metrics import silhouette_samples, silhouette_score
    import matplotlib.cm as cm
    from sklearn.cluster import KMeans
    
    # Compute the silhouette scores for each sample
    clusterer = KMeans(n_clusters=n_clusters)
    cluster_labels = clusterer.fit_predict(X)
    cluster_labels = clusterer.predict(X)
    silhouette_avg = silhouette_score(X, cluster_labels)
    sample_silhouette_values = silhouette_samples(X, cluster_labels)
    plt.figure()
    ax1= plt.gca()
    y_lower = 10
    for i in range(n_clusters):
        # Aggregate the silhouette scores for samples belonging to
        # cluster i, and sort them
        ith_cluster_silhouette_values = \
            sample_silhouette_values[cluster_labels == i]

        ith_cluster_silhouette_values.sort()

        size_cluster_i = ith_cluster_silhouette_values.shape[0]
        y_upper = y_lower + size_cluster_i

        color = cm.nipy_spectral(float(i) / n_clusters)
        ax1.fill_betweenx(np.arange(y_lower, y_upper),
                          0, ith_cluster_silhouette_values,
                          facecolor=color, edgecolor=color, alpha=0.7)

        # Label the silhouette plots with their cluster numbers at the middle
        ax1.text(-0.05, y_lower + 0.5 * size_cluster_i, str(i))

        # Compute the new y_lower for next plot
        y_lower = y_upper + 10  # 10 for the 0 samples

    ax1.set_title("Silhouette plot para cada Cluster.")
    ax1.set_xlabel("Coeficientes silhouette")
    ax1.set_ylabel("Cluster label")

    # The vertical line for average silhouette score of all the values
    ax1.axvline(x=silhouette_avg, color="red", linestyle="--")

    ax1.set_yticks([])  # Clear the yaxis labels / ticks
    ax1.set_xticks([-0.1, 0, 0.2, 0.4, 0.6, 0.8, 1])

 
    
def graficar_GMM(data, modelo, labels= False, probs= False):
    from matplotlib.colors import LogNorm
    # display predicted scores by the model as a contour plot
    cant_points= 200
    x = np.linspace(min(data[:, 0])*0.9, max(data[:, 0])*1.1, cant_points)
    y = np.linspace(min(data[:, 1])*0.9, max(data[:, 1])*1.1,cant_points)
    X, Y = np.meshgrid(x, y)
    XX = np.array([X.ravel(), Y.ravel()]).T
    if (probs):
        Z = np.exp(modelo.score_samples(XX))
    else:   
        Z = -modelo.score_samples(XX)
    Z = Z.reshape(X.shape)
    
    # plot surf
#    fig = plt.figure()
#    ax = fig.add_subplot(111, projection='3d')
#    ax = Axes3D(fig)
##    ax.plot_surface(X, Y, Z, cmap='coolwarm', linewidth=0)
#    plt.scatter(data[:, 0], data[:, 1], s=60, c='r')
#    centros= modelo.means_
#    plt.scatter(centros[:, 0], centros[:, 1],marker='*', c='red',s=600, alpha=0.8)
#    ax.contourf(X, Y, Z)

    
    # plot contour
    centros= modelo.means_
    cant_gauss= centros.shape[0]
#    CS = plt.contour(X, Y, Z, norm=LogNorm(vmin=1.0, vmax=100.0), levels=np.logspace(0, 3, 20))
    if (probs):
        CS = plt.contour(X, Y, Z, levels=np.linspace(0, np.max(Z), cant_gauss*10),  linewidths=2 )
        plt.title('Probabilidades de la GMM')
    else:
        CS = plt.contour(X, Y, Z, norm=LogNorm(vmin=1.0, vmax=100.0), levels=np.logspace(0, 3, 20),  linewidths=2)
        plt.title('Negative log-likelihood de la GMM')
#    plt.colorbar(CS, shrink=0.8, extend='both')
    plt.colorbar(CS, shrink=0.5)
    if (labels):
        etiq= modelo.predict(data)
        plt.scatter(data[:, 0], data[:, 1], s=30, c=etiq)
    else:
        plt.scatter(data[:, 0], data[:, 1], s=30)
    plt.scatter(centros[:, 0], centros[:, 1],marker='o', c='red',s=100, alpha=0.8)
    plt.grid()
    plt.axis('tight')
    plt.show() 
    
# graficar curvas de entrenamiento
def plot_training_curves(history, acc=True):
    # summarize history for accuracy
    if (acc):
        plt.figure()
        plt.plot(history.history['acc'])
        plt.plot(history.history['val_acc'])
        plt.title('model accuracy')
        plt.ylabel('accuracy')
        plt.xlabel('epoch')
        plt.legend(['train', 'test'], loc='upper left')
        plt.grid()
    # summarize history for loss
    plt.figure()
    plt.plot(history.history['loss'])
    plt.plot(history.history['val_loss'])
    plt.title('model loss')
    plt.ylabel('loss')
    plt.xlabel('epoch')
    plt.legend(['train', 'test'], loc='upper left')
    plt.grid()
