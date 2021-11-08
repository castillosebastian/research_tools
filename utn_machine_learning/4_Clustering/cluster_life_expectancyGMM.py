
import matplotlib.pyplot as plt
import AA_utils 
from sklearn.mixture import  GaussianMixture
import pandas as pd
plt.close('all')

# cargar dataset
dataset= 'who_life_expectancy.csv'
df=pd.read_csv('datasets/'+dataset)
# Limpieza
df = df.drop(columns=['Country', 'Year', 'Status', 'Population'])
df = df.dropna()

# Normalizaciòn
scaler = MinMaxScaler()
df = scaler.fit_transform(df)
data = df

#%% fit a Gaussian Mixture Model 
k= 4

model = GaussianMixture(n_components=k) # crea el modelo
model.fit(data) # entrena el modelo con EM
scores= model.score_samples(data) # log(verosimilitud) de cada patrón
labels= model.predict(data) # etiqueta de cada dato (clusters)
probs= model.predict_proba(data)
print('Score del modelo: ', model.score(data))


#%%  graficar GMM (2D)
AA_utils.graficar_GMM(data, model, probs=False, labels=False)

#%% curvas
AA_utils.graficar_punto_elbow(data, 20, GMM=1)

