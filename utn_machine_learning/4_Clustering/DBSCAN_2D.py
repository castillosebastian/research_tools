# -*- coding: utf-8 -*-
""" 
Created on Wed Jun  2 12:56:38 2021

@author: Franco
"""
from sklearn.cluster import DBSCAN
from sklearn import metrics
import numpy as np
import matplotlib.pyplot as plt
import AA_utils 
import pandas as pd

# cargar dataset
# dataset= 'calabazas.csv'
# dataset= 'calabazas2.csv'
dataset= '2D_moons.csv'
# dataset= '2D_circulos.csv'
# dataset= 'simple_blobs.csv'
# dataset= 'simple_blobs_2.csv'
df= pd.read_csv('datasets/'+dataset)
data= df.values

#%%
NORMALIZAR= 1
from sklearn.preprocessing import StandardScaler  
if (NORMALIZAR):
    scaler = StandardScaler()  
    scaler.fit(data)  
    data= scaler.transform(data)
    
# PARAM
EPS= 0.3
MINSAM= 5

# Compute DBSCAN
db = DBSCAN(eps=EPS, min_samples=MINSAM).fit(data)
labels = db.labels_

# Number of clusters in labels, ignoring noise if present.
n_clusters_ = len(set(labels)) - (1 if -1 in labels else 0)
n_noise_ = list(labels).count(-1)

print('Num estimado de clusters: %d' % n_clusters_)
print('Num estimado de outliers: %d / %d total' %( n_noise_, len(data)))
print("Silhouette Coefficient: %0.3f" % metrics.silhouette_score(data, labels))

#%% visualizar predicción
plt.figure()
noise_maks= labels==-1
core_samples_mask = np.zeros_like(db.labels_, dtype=bool)
core_samples_mask[db.core_sample_indices_] = True
# plot RUIDO
x= data[noise_maks] # datos RUIDO (outliers)
plt.scatter(x[:, 0], x[:, 1], color=[0.2, 0.2, 0.2, 0.8], s=20, cmap='viridis', label='ruido',marker ='x')
# plot datos CORE en clusters
x= data[~noise_maks & core_samples_mask]
plt.scatter(x[:, 0], x[:, 1], c=labels[~noise_maks & core_samples_mask], s=30, cmap='viridis', label='Cores')
# plot datos NO CORE en clusters
x= data[~noise_maks & ~core_samples_mask]
plt.scatter(x[:, 0], x[:, 1], c=labels[~noise_maks & ~core_samples_mask], s=20, cmap='viridis', label='Borders',marker ='D')

title= "eps= {}, min_samples= {}, #clusters= {}".format(EPS, MINSAM, n_clusters_)
plt.title(title)
plt.legend()


# %%
