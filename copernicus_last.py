import copernicusmarine

# Paso 1: Generar la lista de archivos disponibles
copernicusmarine.get(
    dataset_id='cmems_mod_glo_phy_my_0.083deg_P1M-m',
    create_file_list='file_list.txt'
)

# Paso 2: Leer la lista y filtrar los archivos de 2019, 2020 y 2021
with open('file_list.txt', 'r') as f:
    lines = f.readlines()

filtered_lines = [line for line in lines if any(year in line for year in ['2019', '2020', '2021'])]

# Paso 3: Guardar la lista filtrada en un nuevo archivo
with open('filtered_file_list.txt', 'w') as f:
    f.writelines(filtered_lines)

# Paso 4: Descargar los archivos filtrados
copernicusmarine.get(
    dataset_id='cmems_mod_glo_phy_my_0.083deg_P1M-m',
    file_list='filtered_file_list.txt',
    output_directory='/Volumes/T7/TFM_data'
)