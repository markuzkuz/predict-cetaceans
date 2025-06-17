import pandas as pd
from pathlib import Path
from tqdm import tqdm

# Directorio donde están los CSVs
csv_dir = Path("./resultados_globales")
csv_files = list(csv_dir.glob("*_valores.csv"))

merged_df = None

for csv_file in tqdm(csv_files, desc="Combinando CSVs"):
    var_name = csv_file.stem.replace("_valores", "")
    df = pd.read_csv(csv_file)

    # Renombrar tercera columna con el nombre de la variable
    old_col = df.columns[2]
    df = df.rename(columns={old_col: var_name})

    # Merge progresivo
    if merged_df is None:
        merged_df = df
    else:
        merged_df = pd.merge(merged_df, df, on=["latitude", "longitude"], how="outer")

# Verificación de duplicados
duplicates = merged_df.duplicated(subset=["latitude", "longitude"]).sum()
if duplicates > 0:
    print(f"️ Encontrados {duplicates} duplicados de coordenadas. Eliminando...")
    merged_df = merged_df.drop_duplicates(subset=["latitude", "longitude"])
else:
    print(" No se encontraron duplicados de coordenadas.")

# Verificación de cobertura de longitudes
print("\n Resumen de longitudes:")
print(merged_df["longitude"].describe())
print("\n Frecuencia por deciles de longitud:")
print(pd.cut(merged_df["longitude"], bins=10).value_counts().sort_index())

# Guardar resultado combinado
merged_df.to_csv("df_final.csv", index=False)
print("Archivo combinado guardado como: df_final.csv")
