# Empezar a explorar los datos GLOBAL: 

install.packages("tidyverse")

library(tidyverse)

# Definir el directorio donde están los archivos
data_dir <- "/Users/marc/Documents/Màster Bioinformàtica/TFM/Python/resultados_estadisticas"

# Listado de archivos CSV
files <- list.files(data_dir, pattern = "_estadisticas.csv$", full.names = TRUE)

# Leer y unir todos los CSV con columna de variable
datos_global <- files %>%
  map_df(~ {
    df <- read_csv(.x)
    df$variable <- tools::file_path_sans_ext(basename(.x)) %>% str_replace("_estadisticas", "")
    return(df)
  })

# Verificar estructura
glimpse(datos_global)

# Comprobación de valores faltantes (NA):

is.na(datos_global)
colSums(is.na(datos_global))
rowSums(is.na(datos_global))

# Visualización: evolución temporal de la media por variable
ggplot(datos_global, aes(x = year, y = mean, color = variable)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Evolución temporal de la media por variable",
       x = "Año", y = "Valor medio", color = "Variable")

# Boxplots por variable
ggplot(datos_global, aes(x = variable, y = mean, fill = variable)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribución de medias por variable",
       x = "Variable", y = "Valor medio") +
  theme(legend.position = "none")


####### NORMALIZACIÓN DE LOS DATOS ##############

# Filtrar solo los valores de 'mean'
medias <- datos_global %>%
  select(variable, year, mean)

# Normalizar por variable (z-score)
medias_norm <- medias %>%
  group_by(variable) %>%
  mutate(mean_scaled = scale(mean)) %>%
  ungroup()

# Gráfico
ggplot(medias_norm, aes(x = year, y = mean_scaled, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  theme_minimal() +
  labs(
    title = "Evolución comparativa de la media (normalizada) por variable",
    x = "Año",
    y = "Media normalizada (z-score)",
    color = "Variable"
  ) +  theme(
    plot.title = element_text(hjust = 0.5)  # centers the title
  )

#Boxplots per variable
ggplot(medias_norm, aes(x = "", y = mean_scaled, fill = variable)) +
  geom_violin(trim = FALSE, alpha = 0.6, color = "gray40") +
  geom_boxplot(width = 0.1, outlier.shape = 16, outlier.size = 1.5,
               outlier.color = "black", color = "black", fill = "white") +
  stat_summary(fun = median, geom = "point", shape = 95, size = 5, color = "red") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Distribución normalizada por variable",
    x = NULL,
    y = "Valor medio normalizado (z-score)"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold")
  )




# Histogramas por variable
ggplot(medias_norm, aes(x = mean, fill = variable)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", alpha = 0.6) +
  geom_density(alpha = 0.3, color = "red", size = 1) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(
    title = "Distribución de medias por variable con curva de densidad",
    x = "Valor medio normalizado", y = "Densidad"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )


####### Correlación entre variables ######


# Convertimos a formato wide: cada variable como una columna
datos_wide <- datos_global %>%
  select(variable, year, mean) %>%
  pivot_wider(names_from = variable, values_from = mean)

# Quitamos columna 'year' para el cálculo
matriz_cor <- cor(datos_wide %>% select(-year), use = "complete.obs", method = "pearson")
print(matriz_cor)

# Librería para visualizar la matriz de correlación
install.packages("corrplot")
library(corrplot)

# Heatmap de la matriz de correlación:
corrplot(matriz_cor, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black",
         tl.srt = 45, number.cex = 0.7)

title(main = "Mapa de calor de la matriz de correlación", line = 3, cex.main = 1.2)


  

####### DATASET CHLOROFILA ############

# Paquetes
library(tidyverse)

# Ruta a los archivos
ruta_clorofila <- "/Users/marc/Documents/Màster Bioinformàtica/TFM/Python/resultados_estadisticas_clorofila"

# Cargar los archivos
chl <- read_csv(file.path(ruta_clorofila, "chl_estadisticas.csv")) %>% mutate(variable = "chl")
bbp <- read_csv(file.path(ruta_clorofila, "bbp_estadisticas.csv")) %>% mutate(variable = "bbp")
poc <- read_csv(file.path(ruta_clorofila, "poc_estadisticas.csv")) %>% mutate(variable = "poc")

# Cargar los errores (todavía no se tratarán)
# chl_err <- read_csv(file.path(ruta_clorofila, "chl_error_estadisticas.csv")) %>% mutate(variable = "chl_error")

# Unir todos en un solo dataset
datos_clorofila <- bind_rows(chl, bbp, poc) %>%
  pivot_longer(cols = c(mean, sd, min, max, iqr), 
               names_to = "statistic", 
               values_to = "value") %>%
  mutate(month = as.integer(month))

view(datos_clorofila)
str(datos_clorofila)
is.na(datos_clorofila)
colSums(is.na(datos_clorofila))
rowSums(is.na(datos_clorofila))

# Filtrar solo la media: 

datos_clorofila <- datos_clorofila %>%
  filter(statistic == "mean")

# Normalización tipo Z-score
clorofila_norm <- datos_clorofila %>%
  group_by(variable) %>%
  mutate(value_norm = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)) %>%
  ungroup()

# Visualizar las medias: 
# Solo medias todas las variables juntas
clorofila_norm %>% 
  filter(statistic == "mean") %>%
  ggplot(aes(x = factor(month), y = value_norm, fill = variable)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Media mensual de variables bio-ópticas (1998–2022)",
       x = "Mes", y = "Media", fill = "Variable") +
  scale_x_discrete(labels = month.abb) +
  theme(plot.title = element_text(hjust = 0.5))  # Centrar título


# Separar las variables:

library(ggplot2)

clorofila_norm %>%
  filter(statistic == "mean") %>%
  ggplot(aes(x = factor(month), y = value_norm)) +
  geom_col(fill = "#69b3a2") +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Medias mensuales de variables bio-ópticas (1998–2022)",
    x = "Mes",
    y = "Valor medio"
  ) +
  scale_x_discrete(labels = month.abb) +
  theme(plot.title = element_text(hjust = 0.5))  # Centrar título


# Otras estadísticas con facet_grid
clorofila_norm %>%
  ggplot(aes(x = factor(month), y = value_norm)) +
  geom_line(group = 1, color = "steelblue") +
  facet_grid(statistic ~ variable, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Estadísticas mensuales por variable (1998–2022)",
    x = "Mes", y = "Valor normalizado"
  ) +
  scale_x_discrete(labels = month.abb) +
  theme(plot.title = element_text(hjust = 0.5))  # Centrar título

# Realizar una matriz de correlación:

# Convertir datos largos a formato ancho
datos_clorofila_wide <- clorofila_norm %>%
  select(month, variable, value_norm) %>%
  pivot_wider(names_from = variable, values_from = value_norm)

# Calcular matriz de correlación
matriz_cor <- cor(datos_clorofila_wide %>% select(-month), use = "complete.obs")

library(corrplot)

corrplot(matriz_cor, method = "color", type = "upper",
         addCoef.col = "black",        # Números de correlación en negro
         tl.col = "black",              # Títulos en negro
         tl.srt = 45,                   # Rotar títulos 45 grados
         number.cex = 0.7)              # Tamaño de números






