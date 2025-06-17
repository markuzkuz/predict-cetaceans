import xarray as xr
import numpy as np
import pandas as pd
from pathlib import Path

# Configuración
dataset_path = Path("/Users/marc/Documents/Màster Bioinformàtica/TFM/Python/global_omi_health_carbon_ph_trend_1985_P20230930.nc")
output_dir = Path("resultados_PH_trend")
output_dir.mkdir(exist_ok=True)

# Cargar dataset
ds = xr.open_dataset(dataset_path)

# Convertir longitudes a [-180, 180] si es necesario
if "lon" in ds.coords and ds["lon"].max() > 180:
    ds = ds.assign_coords(lon=(((ds["lon"] + 180) % 360) - 180)).sortby("lon")
elif "longitude" in ds.coords and ds["longitude"].max() > 180:
    ds = ds.assign_coords(longitude=(((ds["longitude"] + 180) % 360) - 180)).sortby("longitude")

# Filtrar años 1993–2022
if "time" in ds.dims or "time" in ds.coords:
    ds = ds.sel(time=slice("1993", "2022"))

# Variables a procesar
variables = ['ph_trend', 'ph_trend_uncertainty']
stats = []

for var in variables:
    data = ds[var].squeeze()  # eliminar dimensiones extras (como time si solo hay una)

    values = data.values.flatten()
    values = values[~np.isnan(values)]  # eliminar NaNs

    stats.append({
        "variable": var,
        "mean": float(np.mean(values)),
        "sd": float(np.std(values)),
        "min": float(np.min(values)),
        "max": float(np.max(values)),
        "iqr": float(np.percentile(values, 75) - np.percentile(values, 25))
    })

    # Extraer malla geográfica
    lat = ds["lat"].values if "lat" in ds.coords else ds["latitude"].values
    lon = ds["lon"].values if "lon" in ds.coords else ds["longitude"].values
    lon_grid, lat_grid = np.meshgrid(lon, lat)
    var_values = data.values

    # Crear DataFrame plano con coordenadas y valores
    flat_df = pd.DataFrame({
        "latitude": lat_grid.flatten(),
        "longitude": lon_grid.flatten(),
        var: var_values.flatten()
    })

    flat_df = flat_df.dropna()

    # Guardar como CSV
    out_file = output_dir / f"{var}_valores.csv"
    flat_df.to_csv(out_file, index=False)
    print(f"Guardado: {out_file}")

# Guardar estadísticas globales
df_stats = pd.DataFrame(stats)
stats_file = output_dir / "PH_trend_estadisticas.csv"
df_stats.to_csv(stats_file, index=False)
print(f"Estadísticas globales guardadas en: {stats_file}")
