library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2) # Para gráficos

# Definir el directorio donde tienes los archivos
setwd("~/Documents/TFM_R/TFM_R")
# Cargar cada archivo .tsv
balaenoptera <- read_tsv("./OBIS/balaenoptera_ocurrence.tsv")
physeter <- read_tsv("./OBIS/physeter_ccurrence.tsv")
orcinus <- read_tsv("./OBIS/Orcinus_occurrence.tsv")

# Verificar los datos cargados
glimpse(balaenoptera)
glimpse(physeter)
glimpse(orcinus)


# Convertir la columna 'dateIdentified' a datetime (si es necesario)
balaenoptera$dateIdentified <- as.POSIXct(balaenoptera$dateIdentified, format="%Y-%m-%d", tz="UTC")
physeter$dateIdentified <- as.POSIXct(physeter$dateIdentified, format="%Y-%m-%d", tz="UTC")
orcinus$dateIdentified <- as.POSIXct(orcinus$dateIdentified, format="%Y-%m-%d", tz="UTC")

# Convertir la columna 'georeferencedDate' a datetime (si es necesario)
balaenoptera$georeferencedDate <- as.POSIXct(balaenoptera$georeferencedDate, format="%Y-%m-%d", tz="UTC")
physeter$georeferencedDate <- as.POSIXct(physeter$georeferencedDate, format="%Y-%m-%d", tz="UTC")
orcinus$georeferencedDate <- as.POSIXct(orcinus$georeferencedDate, format="%Y-%m-%d", tz="UTC")

balaenoptera$verbatimEventDate <- as.POSIXct(balaenoptera$verbatimEventDate, format="%Y-%m-%d", tz="UTC")
physeter$verbatimEventDate <- as.POSIXct(physeter$verbatimEventDate, format="%Y-%m-%d", tz="UTC")
orcinus$verbatimEventDate <- as.POSIXct(orcinus$verbatimEventDate, format="%Y-%m-%d", tz="UTC")


# Asegurarse de que las columnas 'day' sean del mismo tipo en todos los datasets
balaenoptera$day <- as.integer(balaenoptera$day)
physeter$day <- as.integer(physeter$day)
orcinus$day <- as.integer(orcinus$day)

# Asegurarse de que las columnas 'fieldNumber' sean del mismo tipo en todos los datasets
balaenoptera$fieldNumber <- as.character(balaenoptera$fieldNumber)
physeter$fieldNumber <- as.character(physeter$fieldNumber)
orcinus$fieldNumber <- as.character(orcinus$fieldNumber)

# Asegurarse de que las columnas 'month' sean del mismo tipo en todos los datasets
balaenoptera$month <- as.double(balaenoptera$month)
physeter$month <- as.double(physeter$month)
orcinus$month <- as.double(orcinus$month)

# Asegurarse de que las columnas 'organismQuantity' sean del mismo tipo en todos los datasets
balaenoptera$organismQuantity <- as.double(balaenoptera$organismQuantity)
physeter$organismQuantity <- as.double(physeter$organismQuantity)
orcinus$organismQuantity <- as.double(orcinus$organismQuantity)

# Asegurarse de que las columnas 'verbatimLatitude y verbatimLongitude' sean del mismo tipo en todos los datasets
balaenoptera$verbatimLatitude <- as.double(balaenoptera$verbatimLatitude)
physeter$verbatimLatitude <- as.double(physeter$verbatimLatitude)
orcinus$verbatimLatitude <- as.double(orcinus$verbatimLatitude)

balaenoptera$verbatimLongitude <- as.character(balaenoptera$verbatimLongitude)
physeter$verbatimLongitude <- as.character(physeter$verbatimLongitude)
orcinus$verbatimLongitude <- as.character(orcinus$verbatimLongitude)


# Si quieres juntar los tres en un solo dataframe para compararlos fácilmente:
datos_combinados <- bind_rows(balaenoptera, physeter, orcinus)


# Distribución de registros por año y especie

# Utilizaremos originalScientificName que contiene el nombre de cada especie:
ggplot(datos_combinados, aes(x = year, fill = originalScientificName)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  theme_minimal() +
  labs(title = "Número de registros por año y especie", x = "Año", y = "Número de registros", fill = "Especie")

# Ejemplo 2: Número de registros por especie
datos_combinados %>%
  group_by(originalScientificName) %>%
  summarise(total_registros = n()) %>%
  arrange(desc(total_registros))

# Especificar las tres especies principales que quieres conservar
especies_filtradas <- c("Physeter macrocephalus", "Balaenoptera musculus", "Orcinus orca")

# Filtrar el dataframe para las tres especies del estudios
datos_filtrados <- datos_combinados %>%
  filter(originalScientificName %in% especies_filtradas)

# Verifica los resultados
datos_filtrados %>%
  group_by(originalScientificName) %>%
  summarise(total = n())

view(datos_filtrados)

# Histograma general para las tres especias:
ggplot(filter(datos_filtrados, year >= 1900), aes(x = year, fill = originalScientificName)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  theme_minimal() +
  labs(title = "Número de registros por año y especie", 
       x = "Año", 
       y = "Número de registros", 
       fill = "Especie") +
  theme(plot.title = element_text(hjust = 0.5))

# Histograma de avistamientos por año por especie
ggplot(filter(datos_filtrados, year >= 1900), aes(x = year, fill = originalScientificName)) +
  geom_histogram(binwidth = 5, color = "black") +
  facet_wrap(~ originalScientificName, scales = "free_y") +
  scale_x_continuous(
    breaks = seq(1900, max(datos_filtrados$year, na.rm = TRUE), by = 50)
  ) +
  theme_minimal() +
  labs(
    title = "Distribución temporal de avistamientos por especie",
    x = "Año",
    y = "Número de avistamientos",
    fill = "Especie"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),  # Quita los nombres de los gráficos
    axis.text.x = element_text(angle = 45, hjust = 1)  # mejora legibilidad
  )


# Gráfico de dispersión con líneas de tendencia por especie

# Asegurarse de tener columna 'year'
datos_filtrados <- datos_filtrados %>%
  filter(year >= 1993, year <= 2022)


# Resumir número de registros por año y especie
conteo_anual <- datos_filtrados %>%
  group_by(originalScientificName, year) %>%
  summarise(n = n(), .groups = "drop")

# Gráfico final
ggplot(conteo_anual, aes(x = year, y = n, color = originalScientificName)) +
  geom_point(size = 1.2, alpha = 0.6) +
  stat_smooth(method = "loess", se = FALSE, linewidth = 1, linetype = "dashed") +
  facet_wrap(~ originalScientificName, scales = "fixed") +  # misma escala para comparar
  ylim(0, 2000) +
  theme_minimal() +
  labs(title = "Tendencia temporal de avistamientos por especie",
       x = "Año", y = "Número de registros", color = "Especie") +
  scale_color_manual(values = colores_asignados) +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),            # Oculta títulos de facetas
    legend.position = "bottom"
  )

# Prueba de normalidad para cada especie
library(dplyr)
resultados_norm <- conteo_anual %>%
  group_by(originalScientificName) %>%
  summarise(shapiro_test = shapiro.test(n)$p.value)

print(resultados_norm) # Claramente distribución no normal


# Realizar la prueba de bondad de ajuste chi-cuadrado
# Ejemplo para una distribución Poisson
tabla_contingencia <- table(conteo_anual$n)
esperado_poisson <- dpois(as.numeric(names(tabla_contingencia)), lambda = mean(tabla_contingencia))

chisq.test(tabla_contingencia, p = esperado_poisson, rescale.p = TRUE) # Tampoco poisson


# Probando haciendo una transformación logarítmica:
ggplot(conteo_anual, aes(x = log(n))) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", alpha = 0.6) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ originalScientificName)


# Realizar la transformación logarítmica
conteo_anual$log_n <- log(conteo_anual$n + 1)  # Añadir 1 para evitar log(0)

# Realizar la prueba de Shapiro-Wilk sobre los datos transformados
shapiro_test <- shapiro.test(conteo_anual$log_n)

# Mostrar los resultados
shapiro_test #Siguen teniendo distribuciones no normales


# Boxplot de profundidades por especie
ggplot(filter(datos_filtrados, !is.na(depth)), 
       aes(x = originalScientificName, y = depth, fill = originalScientificName)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) +
  theme_minimal() +
  labs(
    title = "Distribución de profundidades por especie (solo con datos válidos)",
    x = "Especie",
    y = "Profundidad (m)",
    fill = "Especie"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )



#Ver registros que contienen profundidad:
datos_con_profundidad <- datos_filtrados %>%
  filter(!is.na(depth))

datos_con_profundidad %>%
  group_by(originalScientificName) %>%
  summarise(registros_con_profundidad = n(),
            max_depth = max(depth, na.rm = TRUE),
            median_depth = median(depth, na.rm = TRUE))


# Distribución geográfica de las especies:
ggplot(datos_filtrados, aes(x = decimalLongitude, y = decimalLatitude, color = originalScientificName)) +
  geom_point(alpha = 0.5, size = 1) +
  theme_minimal() +
  labs(title = "Distribución geográfica de avistamientos",
       x = "Longitud",
       y = "Latitud",
       color = "Especie")


install.packages(c("ggplot2", "rnaturalearth", "rnaturalearthdata", "sf", "RColorBrewer"))

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(RColorBrewer)

# Cargar datos de los continentes
continentes <- ne_countries(scale = "medium", returnclass = "sf")

# Crear el gráfico con coordenadas y capa base de continentes
ggplot() +
  geom_sf(data = continentes, fill = "lightgrey", color = "white") +  # Capa base de continentes
  geom_point(data = datos_filtrados, aes(x = decimalLongitude, y = decimalLatitude, color = originalScientificName), 
             size = 1, alpha = 0.7) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +  # Usamos coord_sf() para coordenadas geográficas
  theme_minimal() +
  labs(title = "Distribución geográfica de avistamientos por especie", 
       x = "Longitud", y = "Latitud", color = "Especie") +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5))  # Centrar el título



# Mapa individual por especie

# Filtrar los datos por cada especie:
physeter_data <- filter(datos_filtrados, originalScientificName == "Physeter macrocephalus")
orcinus_data <- filter(datos_filtrados, originalScientificName == "Orcinus orca")
balaenoptera_data <- filter(datos_filtrados, originalScientificName == "Balaenoptera musculus")

sum(is.na(physeter_data$date_year))
physeter_data_clean <- physeter_data[!is.na(physeter_data$date_year), ]
range(physeter_data_clean$date_year)

orcinus_data_clean <- orcinus_data[!is.na(orcinus_data$date_year), ]
range(orcinus_data_clean$date_year)

balaenoptera_data_clean <- balaenoptera_data[!is.na(balaenoptera_data$date_year), ]
range(balaenoptera_data_clean$date_year)

# Establecer colores automáticos de ggplot2 en gráfico combinado
orden_especies <- c("Balaenoptera musculus", "Orcinus orca", "Physeter macrocephalus")

# Asignar colores según ese orden
colores_especies <- scales::hue_pal()(length(orden_especies))
colores_asignados <- setNames(colores_especies, orden_especies)




# Crear un gráfico para Physeter macrocephalus:
ggplot() +
  geom_sf(data = continentes, fill = "lightgrey", color = "white") +
  geom_point(data = physeter_data, aes(x = decimalLongitude, y = decimalLatitude),
             color = colores_asignados["Physeter macrocephalus"],
             size = 1, alpha = 0.7) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
  theme_minimal() +
  labs(title = "Distribución geográfica de avistamientos (Physeter macrocephalus)", 
       x = "Longitud", y = "Latitud") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )


 
# Crear un gráfico para Orcinus orca:
ggplot() +
  geom_sf(data = continentes, fill = "lightgrey", color = "white") +
  geom_point(data = orcinus_data, aes(x = decimalLongitude, y = decimalLatitude),
             color = colores_asignados["Orcinus orca"],
             size = 1, alpha = 0.7) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
  theme_minimal() +
  labs(title = "Distribución geográfica de avistamientos (Orcinus orca)", 
       x = "Longitud", y = "Latitud") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


# Crear un gráfico para Balaenoptera musculus:
ggplot() +
  geom_sf(data = continentes, fill = "lightgrey", color = "white") +
  geom_point(data = balaenoptera_data, aes(x = decimalLongitude, y = decimalLatitude),
             color = colores_asignados["Balaenoptera musculus"],
             size = 1, alpha = 0.7) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
  theme_minimal() +
  labs(title = "Distribución geográfica de avistamientos (Balaenoptera musculus)", 
       x = "Longitud", y = "Latitud") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


################## ALGORITMOS ###########################
#Cargar las variables fisicoquímicas:

setwd('~/Documents/TFM_R/TFM_R')
dinamic_data <- read_csv("df_dynamic_malla.csv")
static_data <- read_csv("df_static_malla.csv")
chloro_data <- read_csv("df_chloro_malla.csv")

chloro_data <- chloro_data %>%
  mutate(
    latitude = round(latitude * 2) / 2,
    longitude = round(longitude * 2) / 2
  )


df_maxent <- dinamic_data %>%
  left_join(static_data, by = c("latitude" = "latitude", "longitude" = "longitude"))

df_maxent <- df_maxent %>%
  left_join(chloro_data, by = c("latitude", "longitude", "year"))

summary(chloro_data)
View(df_maxent)

library(tidyverse)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(GGally)
library(naniar)
library(corrplot)


# Cargar datasets de avistamientos:
balaenoptera <- read_tsv("OBIS/balaenoptera_ocurrence.tsv")
physeter <- read_tsv("OBIS/physeter_ccurrence.tsv")
orcinus <- read_tsv("OBIS/Orcinus_occurrence.tsv")


# Filtrar columnas y años
variables_seleccionadas <- c(
  "id", "date_year", "decimalLatitude", "decimalLongitude", 
  "bathymetry", "originalScientificName", 
  "basisOfRecord", "coordinateUncertaintyInMeters", "samplingProtocol"
)

balaenoptera_filtrado <- balaenoptera %>%
  filter(date_year >= 1993, date_year <= 2022) %>%
  dplyr:::select(any_of(variables_seleccionadas))

physeter_filtrado <- physeter %>%
  filter(date_year >= 1993, date_year <= 2022) %>%
  dplyr:::select(any_of(variables_seleccionadas))

orcinus_filtrado <- orcinus %>%
  filter(date_year >= 1993, date_year <= 2022) %>%
  dplyr:::select(any_of(variables_seleccionadas))

# Combinar todos en un solo dataframe
datos_obis <- bind_rows(balaenoptera_filtrado, physeter_filtrado, orcinus_filtrado)


# Vista rápida del resultado
glimpse(datos_obis)
table(df_combined$species)

head(dinamic_data)
head(static_data)
head(chlorofila_data)
head(datos_obis)

View(datos_obis$bathymetry)


summary(datos_filtrados$decimalLatitude)
summary(datos_filtrados$decimalLongitude)
colnames(datos_obis)

# Unir por coordenadas redondeadas y año

# Añadir grillas espaciales
datos_obis <- datos_obis %>%
  mutate(
    lon_grid = round(decimalLongitude * 2) / 2,
    lat_grid = round(decimalLatitude * 2) / 2,
    year = date_year,
    species = originalScientificName
  )

# Unión con datos dinámicos
df_combined_dynamic <- datos_obis %>%
  left_join(dinamic_data, by = c("lat_grid" = "latitude", "lon_grid" = "longitude", "year"))

View(df_combined_dynamic)

# Unión con datos estáticos
df_combined <- df_combined_dynamic %>%
  left_join(static_data, by = c("lat_grid" = "latitude", "lon_grid" = "longitude"))

df_combined <- df_combined %>%
  mutate(species_clean = case_when(
    grepl("Balaenoptera musculus", species) ~ "Balaenoptera musculus",
    grepl("Orcinus orca", species) ~ "Orcinus orca",
    grepl("Physeter", species) ~ "Physeter macrocephalus",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(species_clean))

# Contar observaciones por especie
df_combined %>%
  count(species_clean)

View(df_combined)
glimpse(df_combined)
summary(df_combined)

# Proporción de NAs por variable
df_combined %>% summarise_all(~ mean(is.na(.))) %>% pivot_longer(everything()) %>%
  ggplot(aes(x = reorder(name, -value), y = value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Variable", y = "Proporción de NA") +
  coord_flip() +
  theme_minimal()

# Visualización de patrones de NA
vis_miss(df_combined)

# Solo variables numéricas
df_num <- df_combined %>% select(where(is.numeric)) %>% drop_na()

cor_matrix <- cor(df_num)
corrplot(cor_matrix, method = "color", type = "lower", tl.cex = 0.7)


res_pca <- PCA(df_num, scale.unit = TRUE, graph = FALSE)
fviz_eig(res_pca)  # Varianza explicada

# Visualización de individuos (presencias)
fviz_pca_ind(res_pca, geom = "point", col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Visualización de variables
fviz_pca_var(res_pca,
             col.var = "contrib", # Colores por contribución a los ejes principales
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)



# Unión con datos de clorofila (solo desde 1998)
df_combined_1998 <- df_combined %>%
  filter(date_year >= 1998, date_year <= 2022)

df_combined_1998 <- df_combined_1998 %>%
  mutate(
    lat_grid = round(decimalLatitude * 2) / 2,
    lon_grid = round(decimalLongitude * 2) / 2
  )

# Unir por lat/lon grid y año
df_combined_chloro <- df_combined_1998 %>%
  left_join(chloro_data, 
            by = c("lat_grid" = "latitude", "lon_grid" = "longitude", "date_year" = "year"))


View(df_combined_chloro)

# Proporción de NAs por variable con chloro
df_combined_chloro %>% summarise_all(~ mean(is.na(.))) %>% pivot_longer(everything()) %>%
  ggplot(aes(x = reorder(name, -value), y = value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Variable", y = "Proporción de NA") +
  coord_flip() +
  theme_minimal()

# Visualización de patrones de NA
vis_miss(df_combined)

# Solo variables numéricas
df_num_chloro <- df_combined_chloro %>% select(where(is.numeric)) %>% drop_na()

cor_matrix_chloro <- cor(df_num_chloro)
corrplot(cor_matrix_chloro, method = "color", type = "lower", tl.cex = 0.7)


res_pca_chloro <- PCA(df_num_chloro, scale.unit = TRUE, graph = FALSE)
fviz_eig(res_pca_chloro)  # Varianza explicada

# Visualización de individuos (presencias)
fviz_pca_ind(res_pca_chloro, geom = "point", col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Visualización de variables
fviz_pca_var(res_pca_chloro,
             col.var = "contrib", # Colores por contribución a los ejes principales
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

###############################

# Preparar df_combined_chloro NOoOoooooooo:

library(tidyverse)
library(caret)
library(randomForest)

# --- PREPROCESSING STEPS (applies once globally) ---
df_chloro_processed <- df_combined_chloro %>%
  # Remove variables with "_error"
  dplyr:::select(-matches("_error")) %>%
  # Scale chlorophyll variables
  mutate(across(any_of(c("chl", "bbp", "poc")), scale, .names = "{.col}_scaled"))

# Define species of interest
especies_objetivo <- c("Balaenoptera musculus", "Orcinus orca", "Physeter macrocephalus")

# Create a list to store RFE results
rfe_resultados <- list()
variables_por_especie <- list()

# LOOP for each species
for (especie in especies_objetivo) {
  cat("▶ Procesando especie:", especie, "\n")
  
  # --- 1. BINARIZE PRESENCE ---
  df_rfe <- df_chloro_processed %>%
    mutate(presencia = ifelse(species_clean == especie, 1, 0)) %>%
    dplyr:::select(presencia, where(is.numeric)) %>%
    drop_na()
  
  # --- 2. BALANCE DATASET ---
  pres <- df_rfe %>% filter(presencia == 1)
  aus  <- df_rfe %>% filter(presencia == 0) %>%
    sample_n(nrow(pres), replace = TRUE)
  df_sample <- bind_rows(pres, aus)
  df_sample$presencia <- as.factor(df_sample$presencia)
  
  # --- 3. RFE VARIABLE SELECTION ---
  set.seed(123)
  control <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
  rfe_result <- rfe(
    df_sample %>% dplyr:::select(-presencia),
    df_sample$presencia,
    sizes = seq(5, min(30, ncol(df_sample) - 1), by = 5),
    rfeControl = control
  )
  
  # Save outputs
  rfe_resultados[[especie]] <- rfe_result
  variables_por_especie[[especie]] <- predictors(rfe_result)
  cat("  ➤ Variables seleccionadas:", paste(predictors(rfe_result), collapse = ", "), "\n\n")
}

# 1. Obtener la unión de todas las variables seleccionadas
todas_vars_seleccionadas <- unique(unlist(variables_por_especie))

# 2. Añadir las variables clave necesarias para el modelado
variables_fijas <- c("species_clean", "species", "decimalLatitude", "decimalLongitude")

# 3. Construir el nuevo df_combined_chloro final
df_combined_chloro <- df_chloro_processed %>%
  dplyr:::select(all_of(c(variables_fijas, todas_vars_seleccionadas))) %>%
  drop_na()

# Verifica resultado
glimpse(df_combined_chloro)

#############NOOOOOOO#######################

############ AIXOOOOOO #########

# Para las tres especies - MODELAR ESPECIE:

install.packages(c("gbm", "naivebayes", "klaR", "caret", "pROC", "e1071", "randomForest", "ggplot2"))


library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(pROC)
library(naivebayes)
library(class)
library(gbm)

modelar_especie <- function(nombre_especie, df) {
  cat("---- Modelando para:", nombre_especie, "----\n")
  
  # --- Crear identificador de celda espacial
  df <- df %>%
    mutate(
      celda = paste0(round(lat_grid, 3), "_", round(lon_grid, 3)),
      presencia = ifelse(species_clean == nombre_especie, 1, 0)
    )
  
  # --- Identificar celdas únicas de presencia
  celdas_presencia <- df %>%
    filter(presencia == 1) %>%
    distinct(celda)
  
  # --- Filtrar presencias (una por celda)
  pres <- df %>%
    filter(presencia == 1) %>%
    distinct(celda, .keep_all = TRUE)
  
  # --- Generar pseudo-ausencias en celdas sin presencia
  aus <- df %>%
    filter(!(celda %in% celdas_presencia$celda)) %>%
    distinct(celda, .keep_all = TRUE) %>%
    filter(presencia == 0)
  
  # --- Balancear 1:1
  if (nrow(aus) < nrow(pres)) {
    warning("⚠️ Menos pseudo-ausencias que presencias. Usando 'replace = TRUE'")
    aus <- sample_n(aus, nrow(pres), replace = TRUE)
  } else {
    aus <- sample_n(aus, nrow(pres), replace = FALSE)
  }
  
  # --- Dataset final balanceado
  df_balanced <- bind_rows(pres, aus) %>%
    dplyr:::select(presencia, where(is.numeric)) %>%
    drop_na()
  
  # --- Particionar en train/test
  set.seed(123)
  train_idx <- createDataPartition(df_balanced$presencia, p = 0.8, list = FALSE)
  train <- df_balanced[train_idx, ]
  test  <- df_balanced[-train_idx, ]
  
  X_train <- train %>% dplyr:::select(-presencia)
  y_train <- train$presencia
  X_test  <- test %>% select(-presencia)
  y_test  <- test$presencia
  
  # --- Escalar para KNN
  preProc <- preProcess(X_train, method = c("center", "scale"))
  X_train_scaled <- predict(preProc, X_train)
  X_test_scaled  <- predict(preProc, X_test)
  
  # --- Modelos ---
  glm_model <- glm(presencia ~ ., data = train, family = "binomial")
  glm_probs <- predict(glm_model, test, type = "response")
  roc_glm <- roc(y_test, glm_probs)
  
  rf_model <- randomForest(as.factor(presencia) ~ ., data = train)
  rf_probs <- predict(rf_model, test, type = "prob")[,2]
  roc_rf <- roc(y_test, rf_probs)
  
  svm_model <- svm(as.factor(presencia) ~ ., data = train, probability = TRUE)
  svm_pred <- predict(svm_model, test, probability = TRUE)
  svm_probs <- attr(svm_pred, "probabilities")[,2]
  roc_svm <- roc(y_test, svm_probs)
  
  gbm_model <- gbm(presencia ~ ., data = train, distribution = "bernoulli",
                   n.trees = 100, interaction.depth = 3, shrinkage = 0.1,
                   n.minobsinnode = 10, verbose = FALSE)
  gbm_probs <- predict(gbm_model, test, n.trees = 100, type = "response")
  roc_gbm <- roc(y_test, gbm_probs)
  
  nb_model <- naive_bayes(as.factor(presencia) ~ ., data = train)
  nb_probs <- predict(nb_model, test, type = "prob")[,2]
  roc_nb <- roc(y_test, nb_probs)
  
  knn_pred <- knn(train = X_train_scaled, test = X_test_scaled,
                  cl = as.factor(y_train), k = 5, prob = TRUE)
  knn_probs <- ifelse(knn_pred == "1", attr(knn_pred, "prob"), 1 - attr(knn_pred, "prob"))
  roc_knn <- roc(y_test, knn_probs)
  
  # --- Curva ROC
  plot(roc_glm, col = "blue", main = paste("ROC -", nombre_especie), legacy.axes = TRUE)
  lines(roc_rf, col = "forestgreen")
  lines(roc_svm, col = "darkred")
  lines(roc_gbm, col = "orange")
  lines(roc_nb, col = "purple")
  lines(roc_knn, col = "black")
  legend("bottomright", legend = c(
    paste("GLM (AUC =", round(auc(roc_glm), 3), ")"),
    paste("RF (AUC =", round(auc(roc_rf), 3), ")"),
    paste("SVM (AUC =", round(auc(roc_svm), 3), ")"),
    paste("GBM (AUC =", round(auc(roc_gbm), 3), ")"),
    paste("NB (AUC =", round(auc(roc_nb), 3), ")"),
    paste("KNN (AUC =", round(auc(roc_knn), 3), ")")
  ), col = c("blue", "forestgreen", "darkred", "orange", "purple", "black"), lwd = 2)
  
  # --- Importancia RF
  rf_importance <- importance(rf_model) %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    rename(importance = MeanDecreaseGini) %>%
    mutate(modelo = "RF", especie = nombre_especie)
  
  # --- Importancia GBM
  gbm_importance <- summary(gbm_model, plotit = FALSE) %>%
    as.data.frame() %>%
    rename(variable = var, importance = rel.inf) %>%
    mutate(modelo = "GBM", especie = nombre_especie)
  
  # --- Tabla de AUCs
  aucs <- tibble(
    especie = nombre_especie,
    auc_glm  = as.numeric(auc(roc_glm)),
    auc_rf   = as.numeric(auc(roc_rf)),
    auc_svm  = as.numeric(auc(roc_svm)),
    auc_gbm  = as.numeric(auc(roc_gbm)),
    auc_nb   = as.numeric(auc(roc_nb)),
    auc_knn  = as.numeric(auc(roc_knn))
  )
  
  # --- Guardar modelos en disco
  saveRDS(rf_model,  paste0("modelo_rf_", gsub(" ", "_", nombre_especie), ".rds"))
  saveRDS(gbm_model, paste0("modelo_gbm_", gsub(" ", "_", nombre_especie), ".rds"))
  
  return(list(
    aucs = aucs,
    rf_importance = rf_importance,
    gbm_importance = gbm_importance,
    modelos = list(glm = glm_model, rf = rf_model, svm = svm_model,
                   gbm = gbm_model, nb = nb_model)
  ))
}

# Ahora con pseudoausencias aleatorias 

modelar_especie <- function(nombre_especie, df) {
  cat("---- Modelando para:", nombre_especie, "----\n")
  
  # --- Crear identificador de celda espacial
  df <- df %>%
    mutate(
      celda = paste0(round(lat_grid, 3), "_", round(lon_grid, 3)),
      presencia = ifelse(species_clean == nombre_especie, 1, 0)
    )
  
  # --- Identificar celdas únicas de presencia
  celdas_presencia <- df %>%
    filter(presencia == 1) %>%
    distinct(celda)
  
  # --- Filtrar presencias (una por celda)
  pres <- df %>%
    filter(presencia == 1) %>%
    distinct(celda, .keep_all = TRUE)
  
  # --- Generar pseudo-ausencias en celdas sin presencia
  aus <- df %>%
    filter(!(celda %in% celdas_presencia$celda)) %>%
    distinct(celda, .keep_all = TRUE) %>%
    filter(presencia == 0)
  
  # --- Balancear 1:1
  if (nrow(aus) < nrow(pres)) {
    warning("⚠️ Menos pseudo-ausencias que presencias. Usando 'replace = TRUE'")
    aus <- sample_n(aus, nrow(pres), replace = TRUE)
  } else {
    aus <- sample_n(aus, nrow(pres), replace = FALSE)
  }
  
  # --- Dataset final balanceado
  df_balanced <- bind_rows(pres, aus) %>%
    select(presencia, where(is.numeric)) %>%
    drop_na()
  
  # --- Particionar en train/test
  set.seed(123)
  train_idx <- createDataPartition(df_balanced$presencia, p = 0.8, list = FALSE)
  train <- df_balanced[train_idx, ]
  test  <- df_balanced[-train_idx, ]
  
  X_train <- train %>% select(-presencia)
  y_train <- train$presencia
  X_test  <- test %>% select(-presencia)
  y_test  <- test$presencia
  
  # --- Escalar para KNN
  preProc <- preProcess(X_train, method = c("center", "scale"))
  X_train_scaled <- predict(preProc, X_train)
  X_test_scaled  <- predict(preProc, X_test)
  
  # --- Modelos ---
  glm_model <- glm(presencia ~ ., data = train, family = "binomial")
  glm_probs <- predict(glm_model, test, type = "response")
  roc_glm <- roc(y_test, glm_probs)
  
  rf_model <- randomForest(as.factor(presencia) ~ ., data = train)
  rf_probs <- predict(rf_model, test, type = "prob")[,2]
  roc_rf <- roc(y_test, rf_probs)
  
  svm_model <- svm(as.factor(presencia) ~ ., data = train, probability = TRUE)
  svm_pred <- predict(svm_model, test, probability = TRUE)
  svm_probs <- attr(svm_pred, "probabilities")[,2]
  roc_svm <- roc(y_test, svm_probs)
  
  gbm_model <- gbm(presencia ~ ., data = train, distribution = "bernoulli",
                   n.trees = 100, interaction.depth = 3, shrinkage = 0.1,
                   n.minobsinnode = 10, verbose = FALSE)
  gbm_probs <- predict(gbm_model, test, n.trees = 100, type = "response")
  roc_gbm <- roc(y_test, gbm_probs)
  
  nb_model <- naive_bayes(as.factor(presencia) ~ ., data = train)
  nb_probs <- predict(nb_model, test, type = "prob")[,2]
  roc_nb <- roc(y_test, nb_probs)
  
  knn_pred <- knn(train = X_train_scaled, test = X_test_scaled,
                  cl = as.factor(y_train), k = 5, prob = TRUE)
  knn_probs <- ifelse(knn_pred == "1", attr(knn_pred, "prob"), 1 - attr(knn_pred, "prob"))
  roc_knn <- roc(y_test, knn_probs)
  
  # --- Curva ROC
  plot(roc_glm, col = "blue", main = paste("ROC -", nombre_especie), legacy.axes = TRUE)
  lines(roc_rf, col = "forestgreen")
  lines(roc_svm, col = "darkred")
  lines(roc_gbm, col = "orange")
  lines(roc_nb, col = "purple")
  lines(roc_knn, col = "black")
  legend("bottomright", legend = c(
    paste("GLM (AUC =", round(auc(roc_glm), 3), ")"),
    paste("RF (AUC =", round(auc(roc_rf), 3), ")"),
    paste("SVM (AUC =", round(auc(roc_svm), 3), ")"),
    paste("GBM (AUC =", round(auc(roc_gbm), 3), ")"),
    paste("NB (AUC =", round(auc(roc_nb), 3), ")"),
    paste("KNN (AUC =", round(auc(roc_knn), 3), ")")
  ), col = c("blue", "forestgreen", "darkred", "orange", "purple", "black"), lwd = 2)
  
  # --- Importancia RF
  rf_importance <- importance(rf_model) %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    rename(importance = MeanDecreaseGini) %>%
    mutate(modelo = "RF", especie = nombre_especie)
  
  # --- Importancia GBM
  gbm_importance <- summary(gbm_model, plotit = FALSE) %>%
    as.data.frame() %>%
    rename(variable = var, importance = rel.inf) %>%
    mutate(modelo = "GBM", especie = nombre_especie)
  
  # --- Tabla de AUCs
  aucs <- tibble(
    especie = nombre_especie,
    auc_glm  = as.numeric(auc(roc_glm)),
    auc_rf   = as.numeric(auc(roc_rf)),
    auc_svm  = as.numeric(auc(roc_svm)),
    auc_gbm  = as.numeric(auc(roc_gbm)),
    auc_nb   = as.numeric(auc(roc_nb)),
    auc_knn  = as.numeric(auc(roc_knn))
  )
  
  # --- Guardar modelos en disco
  saveRDS(rf_model,  paste0("modelo_rf_", gsub(" ", "_", nombre_especie), ".rds"))
  saveRDS(gbm_model, paste0("modelo_gbm_", gsub(" ", "_", nombre_especie), ".rds"))
  
  return(list(
    aucs = aucs,
    rf_importance = rf_importance,
    gbm_importance = gbm_importance,
    modelos = list(glm = glm_model, rf = rf_model, svm = svm_model,
                   gbm = gbm_model, nb = nb_model)
  ))
}



# Resultados modelos sin chloro:

res_physeter <- modelar_especie("Physeter macrocephalus", df_combined)
res_orcinus  <- modelar_especie("Orcinus orca", df_combined)
res_balaenop <- modelar_especie("Balaenoptera musculus", df_combined)

resultados_auc <- bind_rows(
  res_physeter$aucs,
  res_orcinus$aucs,
  res_balaenop$aucs
)

resultados_auc



#### RESULTADOS CHLORO ####
res_chloro_physeter <- modelar_especie("Physeter macrocephalus", df_combined_chloro)
res_chloro_orcinus  <- modelar_especie("Orcinus orca", df_combined_chloro)
res_chloro_balaenop <- modelar_especie("Balaenoptera musculus", df_combined_chloro)

resultados_chloro_auc <- bind_rows(
  res_chloro_physeter$aucs,
  res_chloro_orcinus$aucs,
  res_chloro_balaenop$aucs
)

resultados_chloro_auc
#View(resultados_chloro_auc)


resultados_auc_long <- resultados_chloro_auc %>%
  pivot_longer(cols = starts_with("auc"), names_to = "modelo", values_to = "AUC")

ggplot(resultados_auc_long, aes(x = modelo, y = AUC, fill = especie)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de AUC por modelo y especie",
       x = "Modelo", y = "AUC") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Importancia de las variables SIN chloro
importancias_rf <- bind_rows(
  res_physeter$rf_importance,
  res_orcinus$rf_importance,
  res_balaenop$rf_importance
)

importancias_gbm <- bind_rows(
  res_physeter$gbm_importance,
  res_orcinus$gbm_importance,
  res_balaenop$gbm_importance
)


importancias_rf

# Unir todas las importancias
importancias_total <- bind_rows(importancias_rf, importancias_gbm)

# Gráfico
ggplot(importancias_total, aes(x = reorder(variable, importance), y = importance, fill = modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ especie, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Importancia de Variables por Modelo y Especie SIN CHLORO",
    x = "Variable",
    y = "Importancia",
    fill = "Modelo"
  ) +
  theme_minimal()


# Importancia variables CHLORO: 
importancias_chloro_rf <- bind_rows(
  res_chloro_physeter$rf_importance,
  res_chloro_orcinus$rf_importance,
  res_chloro_balaenop$rf_importance
) %>%
  filter(!variable %in% vars_excluir)

names(res_chloro_balaenop)

importancias_chloro_gbm <- bind_rows(
  res_chloro_physeter$gbm_importance,
  res_chloro_orcinus$gbm_importance,
  res_chloro_balaenop$gbm_importance
)


vars_excluir <- c("year", "date_year", "lon_grid", "lat_grid", "chl_scaled")

importancias_chloro_total <- bind_rows(importancias_chloro_rf, importancias_chloro_gbm)
str(importancias_chloro_total)


# Gráfico con exclusión
importancias_chloro_total %>%
  filter(!variable %in% vars_excluir) %>%
  ggplot(aes(x = reorder(variable, importance), y = importance, fill = modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ especie, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Importancia de Variables por Modelo y Especie",
    x = "Variable",
    y = "Importancia",
    fill = "Modelo"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


View(importancias_chloro_total)

# Comparación chloro
resultados_auc$tipo <- "Sin clorofila"
resultados_chloro_auc$tipo <- "Con clorofila"

resultados_comparados <- bind_rows(resultados_auc, resultados_chloro_auc)

resultados_comparados_long <- resultados_comparados %>%
  pivot_longer(cols = starts_with("auc"), names_to = "modelo", values_to = "AUC")

ggplot(resultados_comparados_long, aes(x = modelo, y = AUC, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ especie) +
  labs(title = "Comparación de AUC con y sin variables de clorofila",
       x = "Modelo", y = "AUC", fill = "Modelo de Datos") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Seleccionar solo RF y GBM
resultados_filtrados <- resultados_comparados %>%
  select(especie, auc_rf, auc_gbm, tipo)

# Convertir a formato largo
resultados_long <- resultados_filtrados %>%
  pivot_longer(cols = starts_with("auc"), names_to = "modelo", values_to = "AUC") %>%
  mutate(modelo = recode(modelo, auc_rf = "Random Forest", auc_gbm = "GBM"))

# Gráfico
ggplot(resultados_long, aes(x = modelo, y = AUC, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ especie) +
  labs(title = "Comparación de AUC para GBM y Random Forest con/sin Clorofila",
       x = "Modelo", y = "AUC", fill = "Tipo de Datos") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))





# MODELOS
# Cargar paquetes necesarios
library(dplyr)
library(caret)
library(tidyr)
library(raster)
library(ggplot2)
library(randomForest)

library(dplyr)
library(raster)
library(ggplot2)

# 1. Preparar datos
especie <- "Physeter macrocephalus"
especie_clean <- gsub(" ", "_", especie)
#View(especie_clean)

rf_model <- readRDS(paste0("modelo_rf_", especie_clean, ".rds"))
modelo_path <- paste0("modelo_rf_", especie_clean, ".rds")
print(modelo_path)  # Debería ser: modelo_rf_Balaenoptera_musculus.rds

# Verifica que lo estés cargando bien
rf_model <- readRDS(modelo_path)

# Asegúrate de que el modelo se entrenó para esa especie:
print(rf_model$call)  # Verifica la fórmula: presencia ~ ...

vars_modelo <- names(rf_model$forest$xlevels)

df_combined_chloro %>%
  dplyr:::select(all_of(vars_modelo)) %>%
  summarise_all(~ mean(is.na(.))) %>%
  pivot_longer(everything()) %>%
  arrange(desc(value))


grid_base <- df_combined_chloro %>%
  dplyr:::select(all_of(vars_modelo))

# 2. Predecir
rf_probs <- predict(rf_model, newdata = grid_base, type = "prob")[, 2]

df_pred_grid <- grid_base %>%
  mutate(prob = rf_probs) %>%
  dplyr:::select(decimalLongitude, decimalLatitude, prob)

# 3. Crear grilla regular de alta resolución
res <- 5  # 5km aprox
x_range <- range(df_pred_grid$decimalLongitude)
y_range <- range(df_pred_grid$decimalLatitude)

r_template <- raster(
  xmn = floor(x_range[1]),
  xmx = ceiling(x_range[2]),
  ymn = floor(y_range[1]),
  ymx = ceiling(y_range[2]),
  res = res,
  crs = "+proj=longlat +datum=WGS84"
)

# 4. Rasterizar usando valores medios por celda
rast_rf <- rasterize(
  x = df_pred_grid[, c("decimalLongitude", "decimalLatitude")],
  y = r_template,
  field = df_pred_grid$prob,
  fun = mean,
  na.rm = TRUE
)

# ⚠️ Verifica si quedó vacío
if (all(is.na(values(rast_rf)))) {
  stop("El raster resultante está vacío. Verifica coordenadas y resolución.")
}

# 5. Guardar y visualizar
writeRaster(rast_rf, paste0("mapa_RF_", especie_clean, "_HIRES.tif"),
            format = "GTiff", overwrite = TRUE)

# Visualizar con ggplot
df_r <- as.data.frame(rast_rf, xy = TRUE)
names(df_r)[3] <- "prob"

library(rnaturalearthdata)
library(rnaturalearth)
library(sf)

continentes <- ne_countries(scale = "small", returnclass = "sf")

ggplot(df_r, aes(x = x, y = y, fill = prob)) +
  geom_tile() +
  geom_sf(data = continentes, fill = "gray90", color = "gray40", size = 0.2, inherit.aes = FALSE) +
  scale_fill_viridis_c(option = "C", limits = c(0, 1), na.value = "transparent") +
  coord_sf(xlim = range(df_r$x), ylim = range(df_r$y), expand = FALSE) +
  labs(
    title = paste("Predicción RF (alta resolución) -", especie),
    x = "Longitud", y = "Latitud", fill = "Probabilidad"
  ) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))






cat("Filas con NA por fila:", sum(rowSums(is.na(df_combined_chloro)) > 0), "\n")
cat("Columnas con NA por variable:\n")
print(colSums(is.na(df_combined_chloro)))


########## BIOMOD2 #########
install.packages("biomod2")
library(biomod2)
library(terra)
library(dplyr)
library(tibble)

# 1. Lista de especies
especies <- c("Balaenoptera musculus", "Orcinus orca", "Physeter macrocephalus")

# 2. Variables predictoras (excluye coordenadas y etiquetas)
vars_predictoras <- setdiff(names(df_combined_chloro), c(
  "species", "species_clean", "decimalLatitude", "decimalLongitude", 
  "lat_grid", "lon_grid", "lat", "lon", "prob_dummy", "pred_rf"
))

# Crear grilla regular
df_raster_ready <- df_combined_chloro %>%
  mutate(
    lon_grid = round(decimalLongitude * 2) / 2,
    lat_grid = round(decimalLatitude * 2) / 2
  ) %>%
  group_by(lon_grid, lat_grid) %>%
  summarise(across(all_of(vars_predictoras), mean, na.rm = TRUE), .groups = "drop") %>%
  rename(x = lon_grid, y = lat_grid)


# Crear raster stack
library(raster)

raster_list <- lapply(vars_predictoras, function(v) {
  df_temp <- df_raster_ready %>%
    dplyr::select(x, y, z = all_of(v))  # renombra directamente aquí
  
  raster::rasterFromXYZ(df_temp)
})



raster_stack <- raster::stack(raster_list)
names(raster_stack) <- vars_predictoras



# 4. Iterar por especie
library(biomod2)

for (esp in especies) {
  cat("▶ Modelando especie:", esp, "\n")
  esp_cod <- gsub(" ", "_", esp)
  
  myResp <- ifelse(df_combined_chloro$species_clean == esp, 1, 0)
  myRespXY <- df_combined_chloro[, c("decimalLongitude", "decimalLatitude")]
  
  biomod_data <- BIOMOD_FormatingData(
    resp.var = myResp,
    expl.var = raster_stack,
    resp.xy = myRespXY,
    resp.name = esp_cod,
    filter.raster = TRUE
  )
  
  biomod_options <- BIOMOD_ModelingOptions()
  
  biomod_model_out <- BIOMOD_Modeling(
    biomod_data,
    models = c("GLM", "RF", "GBM", "SRE")
  )
  
  
  eval <- get_evaluations(biomod_model_out)
  print(eval)
}


# MAX ENT: 
library(dplyr)
library(dismo)
library(rJava)
library(terra)
library(raster)
library(tidyr)
library(dplyr)

View(df_maxent)
# Crear un raster base si no lo tienes
vars_predictoras <- df_maxent %>%
  drop_na() %>%
  dplyr:::select(latitude, longitude, so, mlotst, bottomT, thetao, zos, sithick, siconc, u, v, mdt, ph_trend, chl, bbp, poc)

# Convertir a SpatRaster
rstack <- rast(as.data.frame(vars_predictoras), type = "xyz")  # x=lon, y=lat, z=variable

rstack_raster <- raster::stack(rstack)


library(dismo)
View(datos_obis)

# Extraer coordenadas de presencias para una especie (ej. physeter)
presencias_physeter <- datos_obis %>%
  filter(species == "Physeter macrocephalus") %>%
  dplyr:::select(decimalLongitude, decimalLatitude) %>%
  distinct()  # evitar duplicados

coordinates(presencias_physeter) <- ~decimalLongitude + decimalLatitude

# Extraer valores de predictores para puntos de presencia
valores_presencias <- raster::extract(rstack_raster, presencias_physeter)

# Filtrar presencias con predictores no NA
presencias_filtradas <- presencias_physeter[complete.cases(valores_presencias), ]


maxent_model <- maxent(
  x = rstack_raster,
  p = presencias_filtradas,
  args = c("randomseed", "replicates=5", "replicatetype=crossvalidate")
)

print(maxent_model)

# 1. Seleccionar una réplica (ej. la 1ª)
modelo_rep1 <- maxent_model@models[[1]]

# 2. Predecir sobre el stack original de predictores
crs(rstack_raster)
crs(rstack_raster) <- "EPSG:4326"
pred_maxent <- raster::predict(rstack_raster, modelo_rep1)

# 3. Convertir a SpatRaster para operar con terra
pred_spat <- terra::rast(pred_maxent)

# 4. Corregir la orientación del raster
pred_fix <- terra::t(pred_spat)               # Intercambia ejes X e Y
pred_fix <- terra::flip(pred_fix, "vertical")        # Voltea en eje vertical
pred_fix <- terra::flip(pred_fix, "horizontal")        # Voltea en eje vertical


# 5. Visualizar para verificar
plot(pred_fix, main = "Predicción MaxEnt - Physeter macrocephalus")

# 6. Guardar en formato GeoTIFF con nombre claro
terra::writeRaster(
  pred_fix,
  filename = "orcinus_maxent_rep1.tif",
  overwrite = TRUE
)

