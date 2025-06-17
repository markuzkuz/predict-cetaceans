import pandas as pd

# Cargar el archivo combinado
df = pd.read_csv("df_final.csv")

# Mostrar forma inicial
print(f"Dimensiones originales: {df.shape}")

# Identificar columnas de variables (excluyendo lat/long)
variable_columns = [col for col in df.columns if col not in ["latitude", "longitude"]]

# Eliminar filas donde TODAS las variables son NaN
df_clean = df.dropna(subset=variable_columns, how='all')

# Eliminar columnas que están completamente vacías (opcional pero recomendable)
df_clean = df_clean.dropna(axis=1, how='all')

# Mostrar forma final
print(f"Dimensiones después de limpiar: {df_clean.shape}")

# Guardar el resultado limpio
df_clean.to_csv("df_final_limpio.csv", index=False)
print(" Archivo limpio guardado como: df_final_limpio.csv")
