import pandas as pd

# Cargar el dataset completo
df = pd.read_csv("df_agregado_0.5grados.csv")

# Mostrar resumen de nulos por columna
print("Resumen de valores nulos por variable:\n")
print(df.isna().sum())

# Porcentaje de valores nulos por variable
print("\nPorcentaje de valores nulos por variable:\n")
print(df.isna().mean() * 100)

# Filas completamente vacías excepto lat/lon
vars_solo = [col for col in df.columns if col not in ["latitude", "longitude"]]
filas_vacias = df[vars_solo].isna().all(axis=1).sum()
print(f"\nNúmero de filas con todas las variables NaN: {filas_vacias} / {len(df)}")

# Cuántas filas quedarían si exigimos todas las variables no nulas
df_completo = df.dropna(subset=vars_solo)
print(f"\nFilas con todas las variables presentes: {len(df_completo)}")

# Alternativa: permitir hasta 2 variables faltantes
df_con_2_max_na = df[df[vars_solo].isna().sum(axis=1) <= 2]
print(f"Filas permitiendo hasta 2 variables con NaN: {len(df_con_2_max_na)}")

# Guardar estas versiones por si quieres analizarlas en R
df_completo.to_csv("df_sin_nulos.csv", index=False)
df_con_2_max_na.to_csv("df_max_2_nulos.csv", index=False)
