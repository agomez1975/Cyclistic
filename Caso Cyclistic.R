# PREPARAR

#instalación de las librerías requeridas
install.packages("tidyverse") # análisis y manipulación de datos 
install.packages("ggplot2") # visualización de datos
install.packages("janitor") # Comparar nombres de columnas names de cada archivo ******* NO *******
install.packages("ggmap") # visualización de mapas ******* NO *******
install.packages("geosphere") # análisis y cálculo de distancias ******* NO *******
install.packages("lubridate") # análisis y manipulación de datos

# carga de las librerías instaladas
library(tidyverse) # análisis y manipulación de datos 
library(ggplot2) # visualización de datos
library(janitor) # Comparar nombres de columnas names de cada archivo ******* NO *******
library(ggmap) # visualización de mapas ******* NO *******
library(geosphere) # análisis y cálculo de distancias ******* NO *******
library(lubridate) # análisis y manipulación de datos

#importación de los datos
apr23 <- read_csv("Documents/DA Projects/Caso Cyclistic/data_csv/202304-divvy-tripdata.csv")
may23 <- read_csv("Documents/DA Projects/Caso Cyclistic/data_csv/202305-divvy-tripdata.csv")
jun23 <- read_csv("Documents/DA Projects/Caso Cyclistic/data_csv/202306-divvy-tripdata.csv")
jul23 <- read_csv("Documents/DA Projects/Caso Cyclistic/data_csv/202307-divvy-tripdata.csv")
ago23 <- read_csv("Documents/DA Projects/Caso Cyclistic/data_csv/202308-divvy-tripdata.csv")
sep23 <- read_csv("Documents/DA Projects/Caso Cyclistic/data_csv/202309-divvy-tripdata.csv")
oct23 <- read_csv("Documents/DA Projects/Caso Cyclistic/data_csv/202310-divvy-tripdata.csv")
nov23 <- read_csv("Documents/DA Projects/Caso Cyclistic/data_csv/202311-divvy-tripdata.csv")
dec23 <- read_csv("Documents/DA Projects/Caso Cyclistic/data_csv/202312-divvy-tripdata.csv")
jan24 <- read_csv("Documents/DA Projects/Caso Cyclistic/data_csv/202401-divvy-tripdata.csv")
feb24 <- read_csv("Documents/DA Projects/Caso Cyclistic/data_csv/202402-divvy-tripdata.csv")
mar24 <- read_csv("Documents/DA Projects/Caso Cyclistic/data_csv/202403-divvy-tripdata.csv")

#Verificación de los datos
## verificación de la consistencia de los datos
colnames(apr23)
colnames(may23)
colnames(jun23)
colnames(jul23)
colnames(ago23)
colnames(sep23) 
colnames(oct23)
colnames(nov23)
colnames(dec23)
colnames(jan24)
colnames(feb24)
colnames(mar24)

## verificación de la estructura de los datos (dbl, chr, date)
str(apr23)
str(may23)
str(jun23)
str(jul23)
str(ago23)
str(sep23)  
str(oct23)
str(nov23)
str(dec23)
str(jan24)
str(feb24)
str(mar24)

# PROCESAR

## unificación de los archivos mensuales en un solo archivo

all_data_trips <- bind_rows(apr23, may23, jun23, jul23, ago23, sep23, oct23, nov23, dec23, jan24, feb24, mar24)


## renombro las columnas
all_data_trips <- rename(all_data_trips
       ,trip_id = ride_id
       ,bikeid = rideable_type 
       ,start_time = started_at  
       ,end_time = ended_at  
       ,from_station_name = start_station_name 
       ,from_station_id = start_station_id 
       ,to_station_name = end_station_name 
       ,to_station_id = end_station_id 
       ,usertype = member_casual)

# Elimino las columnas que no se van a utilizar
all_data_trips <- all_data_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng)) # REVISAR SI SE QUITAN LAS COLUMNAS DE LAS ESTACIONES

# verificamos los datos
colnames(all_data_trips) # Listado de nombres de columnas
nrow(all_data_trips) # Cuántas filas hay en el conjunto de datos
dim(all_data_trips) # Dimensiones del conjunto de datos
head(all_data_trips) # Comprobamos las 6 primeras filas del conjunto de datos
str(all_data_trips) # Listado de columnas y tipos de datos
summary(all_data_trips) # Resumen estadístico de los datos.

table(all_data_trips$usertype) # verifico la cantidad de tipos que hay en usertype
table(all_data_trips$bikeid) # verifico la cantidad de tipos que hay en bikeid

# Añado columnas day, month, year
all_data_trips <- all_data_trips %>% 
  mutate(year = format(as.Date(start_time), "%Y")) %>% # extrae el año
  mutate(month = format(as.Date(start_time), "%B")) %>% # extrae el mes
  mutate(date = format(as.Date(start_time), "%d")) %>% # extrae el día
  mutate(day_of_week = format(as.Date(start_time), "%A")) %>% # extrae el día de la semana
  mutate(ride_length = difftime(end_time, start_time)) %>% # crea la columna ride_lenght
  mutate(start_time_t = strftime(start_time, "%H")) # NO SE QUE HACE AUN

# convierto 'ride_length' en valor númerico
all_data_trips <- all_data_trips %>% 
  mutate(ride_length = as.numeric(ride_length))

# verifico la conversión de ride_lenght
is.numeric(all_data_trips$ride_length)

# remuevo los valores negativos de ride lenght
all_data_trips_clean <- all_data_trips[!(all_data_trips$ride_length <= 0),]

# ANALISIS
mean(all_data_trips_clean$ride_length) # calcula el promedio de los viajes
median(all_data_trips_clean$ride_length) # calcula la mediana, valor que ocupa el lugar central de todos los datos
max(all_data_trips_clean$ride_length) # cálcula el máximo recorrido
min(all_data_trips_clean$ride_length) # cálcula el mínimo recorrido

summary(all_data_trips_clean$ride_length) # calcula los cuatro datos anteriores 

# Comparativo segun usuarios
aggregate(all_data_trips_clean$ride_length ~ all_data_trips_clean$usertype, FUN = mean) # cálcula el promedio de viajes/usuario
aggregate(all_data_trips_clean$ride_length ~ all_data_trips_clean$usertype, FUN = median) # cálcula la mediana de viajes/usuario
aggregate(all_data_trips_clean$ride_length ~ all_data_trips_clean$usertype, FUN = max) # cálcula el máximo de viajes/usuario
aggregate(all_data_trips_clean$ride_length ~ all_data_trips_clean$usertype, FUN = min) # cálcula la mínima de viajes/usuario

# Comparativo de los promedios por día de la semana entre usuarios
aggregate(all_data_trips_clean$ride_length ~ all_data_trips_clean$usertype + all_data_trips_clean$day_of_week, FUN = mean)
# los días de la semana estan desordenados... los ordenare
all_data_trips_clean$day_of_week <- ordered(all_data_trips_clean$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
#nuevamente comparo los promediios por dia de la semana entre usuarios
aggregate(all_data_trips_clean$ride_length ~ all_data_trips_clean$usertype + all_data_trips_clean$day_of_week, FUN = mean)

# Analizo los tipos de usuario por dia de la semana
all_data_trips_clean %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  # creo un campo wday del día de la semana
  group_by(usertype, weekday) %>%  # agrupo por tipo de usuario y dia de la semana
  summarise(number_of_rides = n()							#calculo el número de viajes y el promedio de duración 
            ,average_duration = mean(ride_length)) %>% 		# calculo el promedio de duración
  arrange(usertype, weekday)	

# Visualizo l número de viajes por usuario
all_data_trips_clean %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")

# Creo una visualización por promedio de duración
all_data_trips_clean %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")

#COMPARTIR
#exporto mi archivo limpio a .csv para la presentación en Tableau
counts <- aggregate(all_data_trips_clean$ride_length ~ all_data_trips_clean$usertype + all_data_trips_clean$day_of_week, FUN = mean)
write.csv(counts, file = '/Users/andresgomez/Documents/DA Projects/Caso Cyclistic/avg_ride_length.csv')

write.csv(all_data_trips_clean, file = '/Users/andresgomez/Documents/DA Projects/Caso Cyclistic/avg_ride_length.csv')


#¿En qué se diferencian los socios anuales y los ciclistas ocasionales con respecto al uso de las bicicletas de Cyclistic?
#revisar usuarios x estacion de pronto se pueda localizar la estrategía de marketing
# Para tableau
# cantidad de members vs casual ---- counts
# trips por horas members vs casual --- counts
# trips por mes members vs casual --- falta
# promedio duración de viaje members vs casual
# tipo de bike preferido members vs casual
# trips por dia de la semana members vs casual
# trips por estación members vs casual
