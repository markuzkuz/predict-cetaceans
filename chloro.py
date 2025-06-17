import xarray as xr
import numpy as np
import pandas as pd
from pathlib import Path
from tqdm import tqdm

# Configuración
base_dir = Path("/Users/marc/Documents/Màster Bioinformàtica/TFM/Python/CLOROPHIL_A_MULTIOBS_GLO_BIO_BGC_3D_REP_015_010/cmems_obs-mob_glo_bgc-chl-poc_my_0.25deg-climatology_P1M-m_202411")
vars = ["chl", "bbp", "poc", "chl_error", "bbp_error", "poc_error"]
out_dir = Path("resultados_estadisticas_clorofila_05deg")
out_dir.mkdir(exist_ok=True)

# Lista para almacenar los DataFrames ya replicados
replicated_dfs = []

def process_variable(varname):
    all_months = []

    for month in tqdm(range(1, 13), desc=f"Procesando {varname}"):
        nc_file = base_dir / f"cmems_obs-mob_glo_bgc-chl-poc_my_0.25deg-climatology_P1M-m_{month:02d}_P202411.nc"
        
        if not nc_file.exists():
            print(f"Falta archivo mes {month}: {nc_file}")
            continue

        ds = xr.open_dataset(nc_file)
        if varname not in ds:
            print(f"Variable {varname} no está en el archivo {nc_file.name}")
            continue

        data = ds[varname]
        drop_dims = [dim for dim in data.dims if dim not in ['latitude', 'longitude']]
        data = data.mean(dim=drop_dims, skipna=True)

        lon, lat = ds["longitude"].values, ds["latitude"].values
        lon_grid, lat_grid = np.meshgrid(lon, lat)

        flat_df = pd.DataFrame({
            "longitude": lon_grid.flatten(),
            "latitude": lat_grid.flatten(),
            varname: data.values.flatten()
        }).dropna()

        all_months.append(flat_df)

    if not all_months:
        print(f"No se encontraron datos válidos para {varname}")
        return

    # Promediar los 12 meses por celda
    df_all = pd.concat(all_months)

    # Malla 0.5º
    df_all["lat_bin"] = (df_all["latitude"] // 0.5) * 0.5
    df_all["lon_bin"] = (df_all["longitude"] // 0.5) * 0.5
    df_agg = df_all.groupby(["lat_bin", "lon_bin"])[varname].mean().reset_index()
    df_agg = df_agg.rename(columns={"lat_bin": "latitude", "lon_bin": "longitude"})

    # Replicar por año
    years = range(1998, 2023)
    df_replicated = pd.concat([df_agg.assign(year=year) for year in years], ignore_index=True)

    # Añadir al conjunto
    replicated_dfs.append(df_replicated)

    # Guardado individual (opcional)
    out_file = out_dir / f"{varname}_valores_anual_05deg.csv"
    df_replicated.to_csv(out_file, index=False)
    print(f"Guardado: {out_file}")

# Ejecutar todos los pasos
for varname in vars:
    process_variable(varname)

# Unir todos los dataframes replicados
print("\nUniendo variables en un único CSV final...")
from functools import reduce
df_clorofila_final = reduce(lambda left, right: pd.merge(left, right, on=["latitude", "longitude", "year"], how="outer"), replicated_dfs)

# Guardar resultado final
final_file = out_dir / "df_clorofila_completo.csv"
df_clorofila_final.to_csv(final_file, index=False)
print(f"\nArchivo combinado guardado como: {final_file}")
