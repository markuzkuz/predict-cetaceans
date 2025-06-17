import pandas as pd
from pathlib import Path
from functools import reduce

# Directorio con los CSVs por variable
input_dir = Path("./resultados_spaciales_medianas_0.5")
output_path = Path("dataset_dinamico_malla.csv")

# Leer y almacenar todos los CSVs
csv_files = list(input_dir.glob("*_valores.csv"))
dfs = []

for f in csv_files:
    df = pd.read_csv(f)
    varname = f.stem.replace("_valores", "")
    df = df.rename(columns={varname: varname})
    dfs.append(df)

# Unir todos los datasets dinámicos por lat, lon, year
df_merged = reduce(lambda left, right: pd.merge(left, right, on=["latitude", "longitude", "year"], how="outer"), dfs)

# Guardar CSV final
df_merged.to_csv(output_path, index=False)
print(f"Dataset combinado guardado en: {output_path}")
