# -*- coding: utf-8 -*-
"""
Created on Sat Oct  3 10:14:21 2020

@author: Franco
"""

import matplotlib.pyplot as plt
import AA_utils 
from sklearn.mixture import  GaussianMixture
import pandas as pd
plt.close('all')

# cargar dataset
df=pd.read_csv('datasets/calabazas2.csv')
data= df.values

#%% fit a Gaussian Mixture Model 
k= 4
modelo = GaussianMixture(n_components=k, verbose =True, covariance_type = 'full')
modelo.fit(data)
print('Score del modelo: ', modelo.score(data))

#%%  graficar GMM (2D)
AA_utils.graficar_GMM(data, modelo, probs=False, labels=False)

#%% curvas
AA_utils.graficar_punto_elbow(data, 20, GMM=1)

