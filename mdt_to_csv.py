import xarray as xr
import numpy as np
import pandas as pd
from pathlib import Path

# Configuración
dataset_path = Path("/Users/marc/Documents/Màster Bioinformàtica/TFM/Python/PH_SEALEVEL_GLO_PHY_MDT_008_063/cnes_obs-sl_glo_phy-mdt_my_0.125deg_P20Y_202012/mdt_hybrid_cnes_cls22_cmems2020_global.nc")
output_dir = Path("resultados_PH_SEALEVEL")
output_dir.mkdir(exist_ok=True)

# Cargar dataset
ds = xr.open_dataset(dataset_path)

# Convertir longitudes de 0–360 a -180–180 si es necesario
if ds["longitude"].max() > 180:
    ds = ds.assign_coords(longitude=(((ds["longitude"] + 180) % 360) - 180)).sortby("longitude")

# Variables a procesar
variables = ['mdt', 'u', 'v']
stats = []

# Calcular estadísticas globales por variable
for var in variables:
    data = ds[var].squeeze()  # quitar dimensión 'time' si hay solo una
    values = data.values.flatten()
    values = values[~np.isnan(values)]

    # Estadísticos globales
    stats.append({
        "variable": var,
        "mean": np.mean(values),
        "sd": np.std(values),
        "min": np.min(values),
        "max": np.max(values),
        "iqr": np.percentile(values, 75) - np.percentile(values, 25)
    })

    # Guardar datos con malla geográfica
    lat = ds["latitude"].values
    lon = ds["longitude"].values

    # Crear tabla plana con coordenadas y valores
    lon_grid, lat_grid = np.meshgrid(lon, lat)
    var_values = data.values

    flat_df = pd.DataFrame({
        "latitude": lat_grid.flatten(),
        "longitude": lon_grid.flatten(),
        var: var_values.flatten()
    })

    flat_df = flat_df.dropna()  # quitar filas con NaN

    flat_outfile = output_dir / f"{var}_valores.csv"
    flat_df.to_csv(flat_outfile, index=False)
    print(f"Guardado: {flat_outfile}")

# Guardar estadísticas globales
df_stats = pd.DataFrame(stats)
stats_file = output_dir / "PH_SEALEVEL_estadisticas.csv"
df_stats.to_csv(stats_file, index=False)
print(f"Estadísticas globales guardadas en: {stats_file}")
