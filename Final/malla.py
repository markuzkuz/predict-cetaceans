import numpy as np
import pandas as pd

df = pd.read_csv("df_final.csv")

# Truncar hacia abajo a 0.5° de resolución
df['latitude'] = np.floor(df['latitude'] / 0.5) * 0.5
df['longitude'] = np.floor(df['longitude'] / 0.5) * 0.5

agrupadas = df.groupby(['latitude', 'longitude']).median(numeric_only=True).reset_index()
agrupadas.to_csv("df_malla.csv", index=False)
print("CSV agregado generado con malla 0.5°x0.5°.")
