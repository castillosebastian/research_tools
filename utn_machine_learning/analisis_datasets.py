#%%
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
from sklearn.preprocessing import MinMaxScaler, StandardScaler

#%%  Cargar archivo
df = pd.read_csv("data/iris.csv")
# df = pd.read_csv("data/diabetes.csv") 
# df = pd.read_csv("data/weatherAUS.csv") 
# df = pd.read_csv("data/who_life_expectancy.csv") 

# Primeros registros del dataset
df.head()

#%% análisis estadístico del dataset (atributos numéricos)
df.describe()

# histograma por atributo (feature)
df.hist()

# matriz de correlación entre atributos
corr= df.corr()
sns.heatmap(corr, annot=True, vmin=-1, vmax=1)

# boxplot por features
df.boxplot()
# boxplot por feature, agrupado por name.
df.boxplot(by='name')

# Convertir atributo NAME. nominal a categórico
Labels= np.unique(df.name)
df.name= pd.Categorical(df.name)
df.name= df.name.cat.codes

# scatter por pares de variables (features)
pd.plotting.scatter_matrix(df, c=df.name)

# calcular valores nulos por columna
df.isnull().sum()

# borrar registros que posean valores nulos
df= df.dropna()

#%% Normalizar los datos (exceptuando el nombre de la flor ( la clase)) 
x= df.drop('name', axis=1)
scaler = StandardScaler()  
scaler.fit(x)  
x= scaler.transform(x)
    