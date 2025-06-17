import pandas as pd

df = pd.read_csv("./resultados_globales/mlotst_valores.csv")

# Mostrar los rangos de latitud y longitud
print("Latitudes:", df['latitude'].min(), "→", df['latitude'].max())
print("Longitudes:", df['longitude'].min(), "→", df['longitude'].max())

# Comprobar cuántas filas están en América (longitud < 0)
print("Filas con longitud < 0:", (df['longitude'] < 0).sum())

# Comprobar si hay datos duplicados en lat-lon
print("Filas únicas:", df.drop_duplicates(subset=["latitude", "longitude"]).shape[0])
print("Filas totales:", df.shape[0])
