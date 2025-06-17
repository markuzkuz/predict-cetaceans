# Predict Cetaceans:

Modelado predictivo de la distribución espacial de grandes cetáceos migratorios (Balaenoptera musculus, Physeter macrocephalus y Orcinus orca) mediante algoritmos de aprendizaje automático.
Este repositorio contiene los scripts y recursos desarrollados como parte del Trabajo de Fin de Máster en Bioinformática de la UNIR.

## Objetivo

Desarrollar modelos de distribución de especies mediante algoritmos de aprendizaje automático para predecir la presencia potencial de tres especies de cetáceos, integrando datos de presencia y variables ambientales derivadas de fuentes oceánicas abiertas.

##  Especies modeladas

- Balaenoptera musculus (Ballena azul)
- Physeter macrocephalus (Cachalote)
- Orcinus orca (Orca)

##  Estructura del repositorio

- `chloro.py`: procesamiento de clorofila mensual a partir de archivos NetCDF.
- `copernicus_last.py`: extracción de datos recientes desde el servicio Copernicus.
- `df_final.py`: unión final de variables ambientales en un único CSV para el modelado.
- `extract_copernicus.py`: automatiza la descarga de archivos .nc desde Copernicus.
- `mdt_to_csv.py`: convierte datos de altura media del mar (MDT) a formato CSV.
- `nc_to_csv.py`: convierte archivos NetCDF genéricos a CSV.
- `noaa_erddap.py`: descarga datos desde NOAA usando la API de ERDDAP.
- `open.py`: función para abrir NetCDF y explorar estructura/variables.
- `ph_to_csv.py`: extrae datos de pH y tendencias oceánicas y los convierte a CSV.
- `prueba.py`: script auxiliar de pruebas.
- `resultados_*`: carpetas con resultados intermedios por variable (estadísticos, mallas, mapas, etc.).

### Final/

Scripts dedicados al procesamiento, análisis y modelado final:

- `df_final.py`: unifica todos los CSV en un solo dataframe con merge por coordenadas y año.
- `limpiar_df.py`: elimina valores `NA` y filas vacías del dataset final.
- `malla.py`: genera mallas espaciales regulares de resolución 0.5° x 0.5°.
- `prueba_dataset.py`: script para inspección y validación del dataset unificado.
- `unir_dinamico.py`: une únicamente las variables dinámicas a partir de múltiples CSVs.
- `TFM_fisicas.R`: análisis exploratorio en R de las variables físicas (correlación, PCA, etc.).
- `TFM_biologicas.R`: análisis de variables biológicas, unión con físicas y modelado con algoritmos de ML (Random Forest, SVM, etc.).


##  Algoritmos de aprendizaje supervisado

- Random Forest
- Support Vector Machines (SVM)
- XGBoost
- k-Nearest Neighbors (kNN)
- Naive Bayes
- Redes neuronales (ANN)
- MaxEnt (solo-presencia)

##  Variables ambientales

- Temperatura del fondo (bottomT)
- Temperatura superficial (thetao)
- Salinidad (so)
- Capa de mezcla (mlotst)
- Espesor de hielo (sithick)
- Nivel del mar (zos)
- Concentración de hielo marino (siconc)
- Velocidad hacia el este del hielo marino (usi)
- Velocidad hacia el norte del hielo marino (vsi)
- Velocidad este del agua (uo)
- Velocidad norte del agua (vo)
- pH oceánico y su tendencia e incertidumbre (ph_trend y ph_tend_uncertainity)
- Clorofila-a, bbp, poc (valores mensuales medios y sus respectivos errores)
- Altura media del nivel del mar y componentes horizontales y verticales (mdt, u, v)

## ️ Requisitos:

- Python 3.9+
- Bibliotecas: `pandas`, `numpy`, `xarray`, `netCDF4`, `matplotlib`, `scikit-learn`, `requests`

Recomendado -- Usar entorno virtual


Máster Universitario en Bioinformática — UNIR
Trabajo Fin de Máster (2025)
source venv/bin/activate
pip install -r requirements.txt



