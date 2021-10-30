import json
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
#open
f = open('data/corpus_textosfallos.json', encoding="utf8")
d = open('data/corpus_fallosmetdat.json', encoding="utf8")
fallos = json.load(f)
fdatos = json.load(d)
#explorar fdatos
df = pd.DataFrame(fdatos)
chart = sns.countplot(df['materia'], palette='RdBu')
chart.set_xticklabels(chart.get_xticklabels(), rotation=45)
chart2 = sns.countplot(df['tipo_fallo'], palette='RdBu')
# Filtrando bd para trabajar con categorías con observaciones
df['materia'].value_counts()
df['tipo_fallo'].value_counts()
materias_elegidas = ['CIVIL Y COMERCIAL','PENAL','LABORAL']
tipo_felegidos = ["Sentencia Interlocutoria", 'Sentencia Definitiva']
#
dfxmat = df[df['materia'].notna()]
dfxmat = df[df['materia'].isin(materias_elegidas)]
dfxmat['materia'].value_counts()
#
dfxtf = df[df['tipo_fallo'].isin(tipo_felegidos)]

# Solución:
# Vectorizar los fallos Doc2vec
# Generar modelo predictivo para las clases de materias
# Evaluar modelo
# https://github.com/susanli2016/NLP-with-Python/blob/master/Doc2Vec%20Consumer%20Complaint_3.ipynb
# https://github.com/susanli2016/Machine-Learning-with-Python/blob/master/Consumer_complaints.ipynb
# https://github.com/google-research/bert/blob/master/predicting_movie_reviews_with_bert_on_tf_hub.ipynb
# https://github.com/ArmandDS/bert_for_long_text/blob/master/final_bert_long_docs.ipynb
# https://towardsdatascience.com/implementing-multi-class-text-classification-with-doc2vec-df7c3812824d








# has at least 5000 words?
# algo