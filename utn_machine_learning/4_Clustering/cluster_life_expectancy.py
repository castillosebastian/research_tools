#%% Imports varios ----------------------------------------
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.cluster import KMeans
import AA_utils 
from sklearn.metrics import  silhouette_score
from sklearn.preprocessing import MinMaxScaler

#%% Parametros ----------------------------
k= 2

#%% cargar dataset
dataset= 'who_life_expectancy.csv'
df=pd.read_csv('datasets/'+dataset)

#%% # Limpieza
df = df.drop(columns=['Country', 'Year', 'Status', 'Population'])
df = df.dropna()

#%% Normalizaciòn
scaler = MinMaxScaler()
df = scaler.fit_transform(df)

#%% visualizar curva elbow y silhouette
AA_utils.graficar_punto_elbow(df, 16)
AA_utils.graficar_indice_silhouette(df, 16)

#%% Entrenar modelo
kmeans = KMeans(n_clusters=k)
kmeans.fit(df)

#%% clasificar cada patrón con los centroides
kmeans_labels = kmeans.predict(df)

#%% centroides
centers = kmeans.cluster_centers_

#%% analisis del clustering
silhouette_avg = silhouette_score(df, kmeans_labels)
print("K =", k, "The average silhouette_score is :", silhouette_avg)
AA_utils.graficar_indice_silhouette_k(df, k)

#--------------- 3K--------------------
#%% Parametros ----------------------------
k= 3

#%% cargar dataset
dataset= 'who_life_expectancy.csv'
df=pd.read_csv('datasets/'+dataset)

#%% # Limpieza
df = df.drop(columns=['Country', 'Year', 'Status', 'Population'])
df = df.dropna()

#%% Normalizaciòn
scaler = MinMaxScaler()
df = scaler.fit_transform(df)

#%% visualizar curva elbow y silhouette
AA_utils.graficar_punto_elbow(df, 16)
AA_utils.graficar_indice_silhouette(df, 16)

#%% Entrenar modelo
kmeans = KMeans(n_clusters=k)
kmeans.fit(df)

#%% clasificar cada patrón con los centroides
kmeans_labels = kmeans.predict(df)

#%% centroides
centers = kmeans.cluster_centers_

#%% analisis del clustering
silhouette_avg = silhouette_score(df, kmeans_labels)
print("K =", k, "The average silhouette_score is :", silhouette_avg)
AA_utils.graficar_indice_silhouette_k(df, k)

