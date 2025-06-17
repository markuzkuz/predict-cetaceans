import xarray as xr
import os

# Global bounding box
LAT_MIN, LAT_MAX = -90, 90
LON_MIN, LON_MAX = -180, 180

# Multi-year period
START_DATE = "2010-01-01"
END_DATE = "2023-12-31"

# Dataset and variable configuration
VARIABLES = {
    "sst": {
        "url": "https://www.ncei.noaa.gov/erddap/griddap/AEC_gomex_model_climo_3d.csv?water_temp%5B(2000-12-31T00:00:00Z):1:(2000-12-31T00:00:00Z)%5D%5B(0.0):1:(-5000.0)%5D%5B(18.09165):1:(30.73087)%5D%5B(-98.0):1:(-77.36)%5D,water_u%5B(2000-12-31T00:00:00Z):1:(2000-12-31T00:00:00Z)%5D%5B(0.0):1:(-5000.0)%5D%5B(18.09165):1:(30.73087)%5D%5B(-98.0):1:(-77.36)%5D,water_v%5B(2000-12-31T00:00:00Z):1:(2000-12-31T00:00:00Z)%5D%5B(0.0):1:(-5000.0)%5D%5B(18.09165):1:(30.73087)%5D%5B(-98.0):1:(-77.36)%5D,salinity%5B(2000-12-31T00:00:00Z):1:(2000-12-31T00:00:00Z)%5D%5B(0.0):1:(-5000.0)%5D%5B(18.09165):1:(30.73087)%5D%5B(-98.0):1:(-77.36)%5D,sphy%5B(2000-12-31T00:00:00Z):1:(2000-12-31T00:00:00Z)%5D%5B(0.0):1:(-5000.0)%5D%5B(18.09165):1:(30.73087)%5D%5B(-98.0):1:(-77.36)%5D,diatoms%5B(2000-12-31T00:00:00Z):1:(2000-12-31T00:00:00Z)%5D%5B(0.0):1:(-5000.0)%5D%5B(18.09165):1:(30.73087)%5D%5B(-98.0):1:(-77.36)%5D,mezoo%5B(2000-12-31T00:00:00Z):1:(2000-12-31T00:00:00Z)%5D%5B(0.0):1:(-5000.0)%5D%5B(18.09165):1:(30.73087)%5D%5B(-98.0):1:(-77.36)%5D,mizoo%5B(2000-12-31T00:00:00Z):1:(2000-12-31T00:00:00Z)%5D%5B(0.0):1:(-5000.0)%5D%5B(18.09165):1:(30.73087)%5D%5B(-98.0):1:(-77.36)%5D",
        "var": "sst"
    },
    "chlorophyll": {
        "url": "https://www.ncei.noaa.gov/erddap/griddap/USM_VIIRS_DAP.html",
        "var": "chlorophyll"
    },
    "salinity": {
        "url": "https://www.ncei.noaa.gov/erddap/griddap/nceiPH55salinityMonthly.nc",
        "var": "salinity"
    },
    "depth": {
        "url": "https://www.ncei.noaa.gov/erddap/griddap/etopo180.nc",
        "var": "altitude"
    }
}

OUTPUT_FOLDER = "downloaded_env_data"
os.makedirs(OUTPUT_FOLDER, exist_ok=True)

def download_variable(name, config):
    print(f"Downloading {name}...")
    ds = xr.open_dataset(config["url"])
    
    # Subset by time and space
    if "time" in ds.dims:
        ds = ds.sel(time=slice(START_DATE, END_DATE))
    ds = ds.sel(lat=slice(LAT_MIN, LAT_MAX), lon=slice(LON_MIN, LON_MAX))
    
    # Save to local NetCDF
    file_path = os.path.join(OUTPUT_FOLDER, f"{name}.nc")
    ds[[config["var"]]].to_netcdf(file_path)
    print(f"Saved {name} to {file_path}")
    return ds

def main():
    datasets = {}
    for name, cfg in VARIABLES.items():
        try:
            datasets[name] = download_variable(name, cfg)
        except Exception as e:
            print(f"Failed to download {name}: {e}")

if __name__ == "__main__":
    main()
