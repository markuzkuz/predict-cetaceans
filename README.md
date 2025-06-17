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
```{r, echo=FALSE, results='asis'}
cat("
predicts-cetaceans/
├── chloro.py             # Procesamiento de clorofila mensual
├── copernicus_last.py    # Extracción de variables Copernicus
├── df_final.py           # Unión de datasets y generación de CSV final
├── extract_copernicus.py # Automatización de descarga .nc Copernicus
├── mdt_to_csv.py         # Conversión de mapas MDT a CSV
├── nc_to_csv.py          # Conversión genérica de NetCDF a CSV
├── noaa_erddap.py        # Acceso a datos NOAA vía ERDDAP API
├── open.py               # Función para abrir NetCDF y comprobar su estructura y variables
├── ph_to_csv.py          # Conversión de dataset de pH a CSV
├── prueba.py             # Script auxiliar de pruebas
├── resultados_*          # Directorios con resultados intermedios por variable
└── Final/
    ├── df_final.py           # Unir los distintos CSV y combinarlos con un merge
    ├── limpiar_df.py         # Eliminar valores NA y filas completamente vacías
    ├── malla.py              # Generar la malla de resolución 0.5º x 0.5º
    ├── prueba_dataset.py     # Script de pruebas para el dataset final
    ├── unir_dinamico.py      # Unir todos los datasets de las variables dinámicas
    ├── TFM_fisicas.R         # Script en R para análisis exploratorio y gráficos de variables físicas
    └── TFM_biologicas.R      # Script en R para análisis de variables biológicas y modelado final ")


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
- 

## ️ Requisitos:

- Python 3.9+
- Bibliotecas: `pandas`, `numpy`, `xarray`, `netCDF4`, `matplotlib`, `scikit-learn`, `requests`

Recomendado -- Usar entorno virtual


Máster Universitario en Bioinformática — UNIR
Trabajo Fin de Máster (2025)
source venv/bin/activate
pip install -r requirements.txt



