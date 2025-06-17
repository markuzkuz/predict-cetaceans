import xarray as xr
ds = xr.open_dataset("/Users/marc/Documents/Màster Bioinformàtica/TFM/Python/global_omi_health_carbon_ph_trend_1985_P20230930.nc", engine="netcdf4")
print(ds)
