#Pomos Daten
library(readr)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tmap)

posmo_sevi <- read_delim("data_posmo/posmo_2023-01-01T00_00_00+01_00-2023-04-30T23_59_59+02_00.csv", delim=",") 

posmo_sevi
str(posmo_sevi)

Sys.timezone()
Sys.setenv(TZ = "UTC")

posmo_sevi$datetime
#UTC

st_crs(posmo_sevi["lon_x"])
#Coordinate Reference System: NA

sum(is.na(posmo_sevi$lon_x))
sum(is.na(posmo_sevi$lat_y))
#7 Werte NA

#Löschen von NA Werten in den Koordinaten
posmo_sevi <- posmo_sevi |> filter(!is.na(lon_x))

#WSG Koordinatensystem 
posmo_sevi <- st_as_sf(posmo_sevi, coords = c("lon_x", "lat_y"), crs = 4326 , remove = F)

st_crs(posmo_sevi["lon_x"])

#Nur die geometry Spalte ist transformiert...
posmo_sevi <- st_transform(posmo_sevi, crs = 2056)

sum(is.na(posmo_sevi$month))

posmo_sevi$month <- month(posmo_sevi$datetime)
sum(is.na(posmo_sevi$month))

unique(posmo_sevi$weekday)
unique(posmo_sevi$week_mode)
posmo_sevi$week_mode <- ifelse(posmo_sevi$weekday %in% c("Sat", "Sun"), "weekend", "weekday")

posmo_sevi %>%
  mutate(date = as.Date(datetime)) %>%
  arrange(date)

#Karte erstellen
ggplot(posmo_sevi) +
  geom_sf()

#Nur zu Fuss
unique(posmo_sevi$transport_mode)

pedestrian <- posmo_sevi |> 
  filter(transport_mode=="Walk")

ggplot(pedestrian) +
  geom_sf()
