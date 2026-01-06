# preparacion del entorno
install.packages("tidyverse")
library(tidyverse)

install.packages("skimr")
library("skimr")

install.packages("janitor") 
library("janitor")
 
trips_2019 <- read_csv("Divvy_Trips_2019_Q1 - Divvy_Trips_2019_Q1.csv")
trips_2020 <- read_csv("Divvy_Trips_2020_Q1 - Divvy_Trips_2020_Q1.csv")

# revision de los datasets
colnames(trips_2019)
skim_without_charts(trips_2019)
glimpse(trips_2019)
head(trips_2019)

colnames(trips_2020)
skim_without_charts(trips_2020)
glimpse(trips_2020)
head(trips_2020)


# manejo de columnas (para que coincidan al hacer la union)
clean_names(trips_2019)
clean_names(trips_2020)

trips_2019 <- trips_2019 %>%  
  rename(trip_duration = tripduration,    #renombramiento
         start_station_id = from_station_id,
         start_station_name = from_station_name,
         end_station_id = to_station_id,
         end_station_name = to_station_name,
         user_type = usertype) %>% 
  mutate(trip_id = as.character(trip_id)) %>% #cambio de TIPO DE DATO, de dbl a chr para que coincida con 2020
  select(-c(bikeid, gender, birthyear))   #eliminacion de columnas no utiles



trips_2020 <- trips_2020 %>%  
  rename(trip_id = ride_id,        #renombramiento
         start_time = started_at,
         end_time = ended_at,
         user_type = member_casual) %>%
  mutate(trip_duration = as.numeric(difftime(end_time, start_time, units="secs"))) %>% #nueva columna que estaba solo en 2019
  select(-c(rideable_type, start_lat, start_lng, end_lat, end_lng)) #eliminacion de columnas no utiles
  


#normalizacion de columna user_type
distinct(trips_2019, user_type) # Subscriber - Customer
distinct(trips_2020, user_type) # member - casual


#voy a usar el formato de 2020, 'member' y 'casual'
trips_2019 <- trips_2019 %>% 
  mutate(user_type = ifelse(user_type == 'Subscriber', 'member', user_type),
         user_type = ifelse(user_type == 'Customer', 'casual', user_type))


#union de ambos datasets luego de la normalización
all_trips <- bind_rows(trips_2019, trips_2020)

#creacion de archivo csv con el nuevo dataset
write_csv(all_trips, "all_trips_clean.csv")

# Creación de un backup del dataset  
all_trips_raw <- read_csv("all_trips_clean.csv")




###### TRIP_DURATION ######

# Se obrservan múltiples viajes con una duración exagerada
all_trips %>% 
  arrange(-trip_duration)

# Se observan múltiples viajes con una duración negativa
all_trips %>% 
  arrange(trip_duration)

# 7696 viajes duran MENOS de 1 MINUTO
all_trips %>% 
  count(trip_duration <= 60)

# 483 viajes duran MÁS de 1 DÍA
all_trips %>% 
  count(trip_duration >= 86400)

# Previo al filtro, el promedio de duración de viajes es de 1184 segundos
all_trips %>% 
  drop_na() %>% 
  summarize(mean_trip_duration = mean(trip_duration))

# Luego de filtrar, el promedio de duración de viajes es de 827 segundos
all_trips %>% 
  filter(trip_duration <= 86400) %>% 
  filter(trip_duration >= 60) %>% 
  summarize(mean_trip_duration = mean(trip_duration))

# Se filtran y se eliminan viajes de MENOS de 1 MINUTO y de MÁS de 1 DÍA de duración
all_trips <- all_trips %>% 
  filter(trip_duration <= 86400) %>% 
  filter(trip_duration >= 60)

