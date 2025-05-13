# Borrar variables de memoria
rm(list=ls(all=TRUE))
graphics.off()
cat("\014")

# Cargamos librerías necesarias
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)

# Cargamos el conjunto de datos desde un archivo CSV
hotel_data <- read.csv("hotel_bookings.csv", header = TRUE, stringsAsFactors = FALSE)

# Vemos las dimensiones del dataset de datos (número de filas y columnas)
dim(hotel_data)

# Vemos las primeras filas del dataset
head(hotel_data)

# Obtenemos los nombres de las columnas
colnames(hotel_data)

# Vemos la estructura y los tipos de variables del dataset
str(hotel_data)

# Obtenemos un resumen estadístico de las variables
summary(hotel_data)

# Convertimos las variables enteras a factores
hotel_data$hotel <- as.factor(hotel_data$hotel)
hotel_data$is_canceled <- as.factor(hotel_data$is_canceled)
hotel_data$arrival_date_year <- as.factor(hotel_data$arrival_date_year)
hotel_data$arrival_date_month <- as.factor(hotel_data$arrival_date_month)
hotel_data$arrival_date_week_number <- as.factor(hotel_data$arrival_date_week_number)
hotel_data$arrival_date_day_of_month <- as.factor(hotel_data$arrival_date_day_of_month)
hotel_data$meal <- as.factor(hotel_data$meal)
hotel_data$country <- as.factor(hotel_data$country)
hotel_data$agent <- as.factor(hotel_data$agent)
hotel_data$company <- as.factor(hotel_data$company)
hotel_data$market_segment <- as.factor(hotel_data$market_segment)
hotel_data$distribution_channel <- as.factor(hotel_data$distribution_channel)
hotel_data$is_repeated_guest <- as.factor(hotel_data$is_repeated_guest)
hotel_data$reserved_room_type <- as.factor(hotel_data$reserved_room_type)
hotel_data$assigned_room_type <- as.factor(hotel_data$assigned_room_type)
hotel_data$deposit_type <- as.factor(hotel_data$deposit_type)
hotel_data$customer_type <- as.factor(hotel_data$customer_type)
hotel_data$reservation_status <- as.factor(hotel_data$reservation_status)

# Verificamos la estructura del dataset con la conversión de las variables
str(hotel_data)

hotel_data$country[hotel_data$country == "NULL"]<-NA
hotel_data$agent[hotel_data$agent == "NULL"]<-NA
hotel_data$company[hotel_data$company == "NULL"]<-NA

# Visualizamos las columnas que contienen valores faltantes
na_columns <- colSums(is.na(hotel_data)) > 0
na_columns

# Vemos la cantidad total de valores faltantes en cada columna
colSums(is.na(hotel_data))

View(hotel_data)

# Reemplazamos valores faltantes para la variable 'children' con la mediana
hotel_data$children.median <- ifelse(is.na(hotel_data$children), median(hotel_data$children, na.rm = TRUE), hotel_data$children)

# Función para encontrar la moda, el valor más frecuente
get_mode <- function(v) {
  uniq_vals <- unique(v)                  # Obtenemos los valores únicos
  uniq_vals[which.max(tabulate(match(v, uniq_vals)))]   # Devolvemos el valor con mayor frecuencia
}

# Reemplazamos los valores faltantes de las variables 'country' y 'agent' con la moda respectiva de cada una
hotel_data$country[is.na(hotel_data$country)] <- get_mode(hotel_data$country)
hotel_data$agent[is.na(hotel_data$agent)] <- get_mode(hotel_data$agent)

# Eliminamos la variable 'company' del dataset por su gran cantidad de datos faltantes
hotel_data <- hotel_data %>% select(-company)

hotel_data_limpio <- hotel_data

hotel_data_limpio <- hotel_data_limpio %>% select(-children.median)

# Verificar que la columna ha sido eliminada
colnames(hotel_data_limpio)

# Visualizamos las columnas que contienen valores faltantes
na_columns <- colSums(is.na(hotel_data_limpio)) > 0
na_columns

# Verificar nuevamente la cantidad de valores faltantes después del tratamiento
colSums(is.na(hotel_data_limpio))

View(hotel_data_limpio)

# Función para identificar outliers usando el IQR
identificar_outliers <- function(df) {
  outliers <- list()
  columnas_con_outliers <- c()
  
  for (i in 1:ncol(df)) {
    if (is.numeric(df[[i]])) {
      Q1 <- quantile(df[[i]], 0.25, na.rm = TRUE)
      Q3 <- quantile(df[[i]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      limite_inferior <- Q1 - 1.5 * IQR
      limite_superior <- Q3 + 1.5 * IQR
      indices_outliers <- which(df[[i]] < limite_inferior | df[[i]] > limite_superior)
      
      if (length(indices_outliers) > 0) {
        outliers[[colnames(df)[i]]] <- indices_outliers
        columnas_con_outliers <- c(columnas_con_outliers, colnames(df)[i])
      }
    }
  }
  
  if (length(columnas_con_outliers) > 0) {
    cat("Columnas con outliers:", paste(columnas_con_outliers, collapse = ", "), "\n")
    for (columna in columnas_con_outliers) {
      cat("Número de outliers en", columna, ": ", length(outliers[[columna]]), "\n")
    }
  } else {
    cat("No se encontraron outliers en ninguna columna.\n")
  }
  
  return(outliers)
}

# Identificamos las columnas con outliners en el dataset limpio
outliers_encontrados <- identificar_outliers(hotel_data_limpio)

# Lista de columnas con outliers
columnas_con_outliers <- names(outliers_encontrados)

# Función para identificar y reemplazar outliers usando el IQR
reemplazar_outliers_IQR <- function(df) {
  df_sin_outliers <- df
  for (i in 1:ncol(df)) {
    if (is.numeric(df[[i]])) {
      # Calcular Q1, Q3 y el IQR
      Q1 <- quantile(df[[i]], 0.25, na.rm = TRUE)
      Q3 <- quantile(df[[i]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      # Definir los límites para los outliers
      limite_inferior <- Q1 - 1.5 * IQR
      limite_superior <- Q3 + 1.5 * IQR
      
      # Reemplazar los valores fuera de los límites con la mediana
      mediana <- median(df[[i]], na.rm = TRUE)
      df_sin_outliers[[i]] <- ifelse(df[[i]] < limite_inferior | df[[i]] > limite_superior, mediana, df[[i]])
    }
  }
  
  return(df_sin_outliers)
}

# Aplicamos la función al dataset para tratar los valores atípicos
hotel_data_limpio_sin_outliers <- reemplazar_outliers_IQR(hotel_data_limpio)

# Visualizamos lado a lado de todas las columnas numéricas y guardar en archivos PNG
columnas_numericas <- sapply(hotel_data_limpio, is.numeric)

for (columna in names(hotel_data_limpio)[columnas_numericas]) {
  # Creamos un archivo PNG para cada par de gráficos
  png(filename = paste0("grafico_", columna, ".png"), width = 800, height = 600)
  
  # Configuramos la ventana para mostrar dos gráficos lado a lado
  par(mfrow = c(1, 2))
  
  # Grafica con outliers
  boxplot(hotel_data_limpio[[columna]], main = paste(columna, "\nCon outliers"), col = "red", ylab = columna)
  
  # Grafica sin outliers
  boxplot(hotel_data_limpio_sin_outliers[[columna]], main = paste(columna, "\nSin outliers"), col = "green", ylab = columna)
  
  # Cerramos el archivo PNG
  dev.off()
}

# Reiniciamos la disposición gráfica a un solo gráfico
par(mfrow = c(1, 1))

# Guardamos el dataset limpio sin outliers en un archivo CSV
write.csv(hotel_data_limpio_sin_outliers, "hotel_data_limpio_sin_outliers.csv", row.names = FALSE)

hotel_data1 <- hotel_data

# Cargar los datos
hotel_data <- read_csv("hotel_data_limpio_sin_outliers.csv")

# Contamos las reservas por tipo de hotel
reservas_por_hotel <- hotel_data %>% count(hotel)

# Gráfico de barras
ggplot(reservas_por_hotel, aes(x = hotel, y = n, fill = hotel)) +
  geom_bar(stat = "identity") +
  labs(title = "Reservas por tipo de hotel", x = "Tipo de hotel", y = "Cantidad de reservas") +
  theme_minimal()

# Contamos reservas por año
reservas_por_ano <- hotel_data %>% count(arrival_date_year)

# Gráfico de líneas
ggplot(reservas_por_ano, aes(x = arrival_date_year, y = n)) +
  geom_line(group = 1) +
  geom_point() +
  labs(title = "Demanda de reservas a lo largo del tiempo", x = "Año", y = "Cantidad de reservas") +
  theme_minimal()

# Contamos las reservas por mes
reservas_por_mes <- hotel_data %>% count(arrival_date_month)

# Ordenamos los meses
reservas_por_mes$arrival_date_month <- factor(reservas_por_mes$arrival_date_month,
                                              levels = c("January", "February", "March", "April", "May", "June", 
                                                         "July", "August", "September", "October", "November", "December"))

# Gráfico de barras
ggplot(reservas_por_mes, aes(x = arrival_date_month, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Reservas por mes del año", x = "Mes", y = "Cantidad de reservas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Contamos las reservas por mes
reservas_por_mes <- hotel_data %>% count(arrival_date_month)

# Ordenamos los meses
reservas_por_mes$arrival_date_month <- factor(reservas_por_mes$arrival_date_month,
                                              levels = c("January", "February", "March", "April", "May", "June", 
                                                         "July", "August", "September", "October", "November", "December"))

# Gráfico de barras
ggplot(reservas_por_mes, aes(x = arrival_date_month, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Reservas por mes del año", x = "Mes", y = "Cantidad de reservas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Creamos la columna que suma niños y bebés
hotel_data1 <- hotel_data1 %>%
  mutate(children_and_babies = children + babies)

# Contamos las reservas que incluyen niños y/o bebés
reservas_con_ninos_bebes <- hotel_data1 %>%
  filter(children_and_babies > 0) %>%
  count(children_and_babies)

# Gráfico de barras
ggplot(reservas_con_ninos_bebes, aes(x = factor(children_and_babies), y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Reservas que incluyen niños y/o bebés", x = "Número de niños/bebés", y = "Cantidad de reservas") +
  theme_minimal()

# Contamos las reservas que requieren espacios de estacionamiento
reservas_con_estacionamiento <- hotel_data1 %>%
  filter(required_car_parking_spaces > 0) %>%
  count(required_car_parking_spaces)

# Gráfico de barras
ggplot(reservas_con_estacionamiento, aes(x = factor(required_car_parking_spaces), y = n)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Reservas que requieren estacionamiento", 
       x = "Espacios de estacionamiento requeridos", y = "Cantidad de reservas") +
  theme_minimal()

# Filtramos reservas canceladas y las contamos por mes
reservas_canceladas_por_mes <- hotel_data %>%
  filter(is_canceled == 1) %>%
  count(arrival_date_month)

# Gráfico de barras
ggplot(reservas_canceladas_por_mes, aes(x = arrival_date_month, y = n)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Cancelaciones de reservas por mes", x = "Mes", y = "Cantidad de cancelaciones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculamos la duración promedio de las estancias por tipo de hotel
duracion_promedio <- hotel_data %>%
  mutate(total_stay = stays_in_week_nights + stays_in_weekend_nights) %>%
  group_by(hotel) %>%
  summarise(promedio_estancia = mean(total_stay))

# Gráfico
ggplot(duracion_promedio, aes(x = hotel, y = promedio_estancia, fill = hotel)) +
  geom_bar(stat = "identity") +
  labs(title = "Duración promedio de las estancias por tipo de hotel", x = "Tipo de hotel", y = "Duración promedio de estancia (Noches)") +
  theme_minimal()

# Contar la cantidad de reservas por país
reservas_por_pais <- hotel_data %>% 
  count(country) %>% 
  arrange(desc(n))  # Ordenar de mayor a menor

# Filtramos para mostrar los 15 países con más reservas
reservas_por_pais_top15 <- reservas_por_pais %>%
  top_n(15, n)
  
# Gráfico de los 15 principales países
ggplot(reservas_por_pais_top15, aes(x = reorder(country, -n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 15 países con más reservas", x = "País", y = "Cantidad de reservas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
