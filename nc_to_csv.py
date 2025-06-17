import xarray as xr
import numpy as np
import pandas as pd
from pathlib import Path
from tqdm import tqdm

# Configuración
base_dir = Path("/Volumes/T7/TFM_data/GLOBAL_MULTIYEAR_PHY_001_030/cmems_mod_glo_phy_my_0.083deg_P1M-m_202311")
vars = ["mlotst", "zos", "bottomT", "thetao", "so", "sithick", "siconc"]
years = range(1993, 2022)
out_dir = Path("resultados_spaciales_medianas_0.5")
out_dir.mkdir(exist_ok=True)

def process_variable_median(varname):
    print(f"\nProcesando variable: {varname}")
    all_data = []

    for year in tqdm(years, desc=f"{varname}", position=1, leave=False):
        year_path = base_dir / str(year)
        nc_files = [f for f in year_path.glob("*.nc") if f.suffix == ".nc" and not f.name.startswith("._")]

        if not nc_files:
            print(f"[{varname}] No hay archivos para el año {year}")
            continue

        try:
            ds = xr.open_mfdataset(nc_files, combine="by_coords", parallel=False, engine="netcdf4")
        except Exception as e:
            print(f"[{varname}] Error abriendo archivos {year}: {e}")
            continue

        if varname not in ds:
            print(f"[{varname}] No está en los archivos de {year}")
            ds.close()
            continue

        data = ds[varname]

        # Promedio mensual → promedio anual
        drop_dims = [dim for dim in data.dims if dim not in ['lat', 'latitude', 'lon', 'longitude']]
        data_annual = data.mean(dim=drop_dims, skipna=True)

        ds.close()

        # Coordenadas estándar
        lat_name = 'lat' if 'lat' in data_annual.coords else 'latitude'
        lon_name = 'lon' if 'lon' in data_annual.coords else 'longitude'

        # Longitudes 0–360 → -180–180 si hace falta
        if (data_annual[lon_name] > 180).any():
            data_annual = data_annual.assign_coords({lon_name: (((data_annual[lon_name] + 180) % 360) - 180)})
            data_annual = data_annual.sortby(lon_name)

        # Convertir a DataFrame
        df = data_annual.to_dataframe(name=varname).reset_index()

        # Filtrar NaNs
        df = df.dropna(subset=[varname])

        # Redondear a malla de 0.5°
        df["lat_grid"] = (df[lat_name] * 2).round() / 2
        df["lon_grid"] = (df[lon_name] * 2).round() / 2

        # Agrupar por celda y calcular mediana espacial
        df_grouped = df.groupby(["lat_grid", "lon_grid"], as_index=False)[varname].median()
        df_grouped["year"] = year

        all_data.append(df_grouped)

    # Concatenar todos los años
    if not all_data:
        print(f"[{varname}] No se generó ningún dato.")
        return

    df_all = pd.concat(all_data, ignore_index=True)
    df_all = df_all.rename(columns={"lat_grid": "latitude", "lon_grid": "longitude"})

    out_file = out_dir / f"{varname}_valores.csv"
    df_all.to_csv(out_file, index=False)
    print(f"[{varname}] CSV guardado: {out_file}")

# Ejecutar
for varname in tqdm(vars, desc="Variables", position=0):
    process_variable_median(varname)
