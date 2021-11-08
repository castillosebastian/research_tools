# -*- coding: utf-8 -*-
"""
Created on Tue Sep  4 11:07:12 2018

@author: Franco
"""
# Imports varios ----------------------------------------
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.cluster import KMeans
import AA_utils 
from sklearn.metrics import  silhouette_score

#%% Parametros ----------------------------
k= 3

# cargar dataset
dataset= 'calabazas.csv'
df=pd.read_csv('datasets/'+dataset)
# transformar a numpy
# data= df.values

#%% visualizar curva elbow y silhouette
AA_utils.graficar_punto_elbow(df, 16)
AA_utils.graficar_indice_silhouette(df, 16)

#%% Entrenar modelo
kmeans = KMeans(n_clusters=k)
kmeans.fit(df)

# clasificar cada patrón con los centroides
kmeans_labels = kmeans.predict(df)

# centroides
centers = kmeans.cluster_centers_

#%% analisis del clustering
silhouette_avg = silhouette_score(df, kmeans_labels)
print("K =", k, "The average silhouette_score is :", silhouette_avg)
AA_utils.graficar_indice_silhouette_k(df, k)


#%% visualizar predicción
plt.figure()
data= df.values
plt.scatter(data[:, 0], data[:, 1], c=kmeans_labels, s=50, cmap='viridis')
# visualizar centros
plt.scatter(centers[:, 0], centers[:, 1],marker='*', c='red',s=600, alpha=0.8)

