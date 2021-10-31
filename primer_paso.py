# %% 
import pandas as pd
import numpy as np
import os
import matplotlib as plt
os.getcwd()
df = pd.read_csv('data/boston.csv')
print(df.hist())
# %%
