############
#Excercise 2
############

library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(tmap)

Sys.setenv(TZ = "UTC")
###Task 1

#Daten einlesen
wildschwein_BE <- read_delim("wildschwein_BE_2056.csv", ",")

#Datensatz mit geografischen Koordinaten in eine räumliche Geometrie umwandeln 
#z.B. Breiten und Längengrad in Punkte, Linien oder Polygone
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

###Task 2

#Timelag zwischen den einzelnen Datenerhebungen berechnen
wildschwein_BE$timelag_sec <- as.integer(difftime(lead(wildschwein_BE$DatetimeUTC), wildschwein_BE$DatetimeUTC, units = "sec"))

#Anzahl der Tiere nach Chat GPT
zusammengefasst <- aggregate(wildschwein_BE, by = list(wildschwein_BE$TierName), FUN = length)
#Nach Unterricht von Nils
wildschwein_BE$TierName |> unique()

#Ein Plot um zu sehen, wann die Daten der einzelnen Tiere erhoben wurden
ggplot(wildschwein_BE,aes(DatetimeUTC,TierName))+
  geom_point()
#Die Tier-Daten wurden also in etwa gleichzeitig erhoben

#Wann wurden die Daten erhoben? Wieso bis 80 wenn es nur 60 sec pro Minute gibt?
ggplot(wildschwein_BE, aes(timelag_sec/60))+
  geom_histogram(binwidth = 1)+
  lims(x = c(0, 5000/60))+
  scale_y_log10()

#Einen einzelnen Tag separieren und die Datenerhebungen visalisieren
wildschwein_BE |> 
  filter(DatetimeUTC < "2014-08-24") |> 
  ggplot(aes(DatetimeUTC,timelag_sec, colour = TierName))+
  geom_point()+
  geom_line()
#Am Tag wurden weniger Datenpunkte erhoben als in der Nacht

###Task 3

#Euklidische Distanz berechnet zwischen Zeile 1 und 2
#Dabei ist lead(E)/lead(N) der Wert in der Zeile 2
#Zwischen zwei Tieren bleibt aber der unerwünschte Wert bestehen
wildschwein_BE <- wildschwein_BE |> 
  group_by(TierName) |> 
  mutate(steplength_m=sqrt((E-lead(E))^2+(N-lead(N))^2))

#m/sec als neue Spalte hibzufügen...also in Zeile 1 wird die durchschnittsgeschwindigkeit
#zwischen Zeile 1 und Zeile 2 angegeben...
wildschwein_BE <- wildschwein_BE |> 
  mutate(speed_ms = steplength_m/timelag_sec)

hist(wildschwein_BE$speed_ms, breaks=10)
hist(log10(wildschwein_BE$speed_ms),breaks=100)

###Task 4

#Daten einlesen
caro <- read_delim("caro60.csv", delim=",")

#st_as_sf
caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE)


#Jede 3. Zeile aus caro in neuem Datenset
#Ab der 1. Zeile mittels seq(1,...)
#Ab der 3. Zeile mittels seq(3,...)
caro3 <- caro[seq(1, nrow(caro), by=3),]
nrow(caro)
nrow(caro)/3
nrow(caro3)

#Jede 6. Zeile aus caro in neuem Datenset
caro6 <-  caro[seq(1, nrow(caro), by=6),]
nrow(caro)
nrow(caro)/6
nrow(caro6)

#Jede 9. Zeile aus caro in neuem Datenset
caro9 <-  caro[seq(1, nrow(caro), by=9),]
nrow(caro)
nrow(caro)/9
nrow(caro9)

#timelag berechnen
#Mit mutate & lead()
caro <- caro |> 
  mutate(timelag_sec=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "sec" )))
caro3 <- caro3 |> 
  mutate(timelag_sec=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "sec" )))
caro6 <- caro6 |> 
  mutate(timelag_sec=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "sec" )))
caro9 <- caro9 |> 
  mutate(timelag_sec=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "sec" )))

#steplength
caro <- caro |> 
  group_by(TierName) |> 
  mutate(steplength_m=sqrt((E-lead(E))^2+(N-lead(N))^2))
caro3 <- caro3 |> 
  group_by(TierName) |> 
  mutate(steplength_m=sqrt((E-lead(E))^2+(N-lead(N))^2))
caro6 <- caro6 |> 
  group_by(TierName) |> 
  mutate(steplength_m=sqrt((E-lead(E))^2+(N-lead(N))^2))
caro9 <- caro9 |> 
  group_by(TierName) |> 
  mutate(steplength_m=sqrt((E-lead(E))^2+(N-lead(N))^2))

#speed in m/s
caro <- caro |> 
  mutate(speed_ms=steplength_m/timelag_sec)
caro3 <- caro3 |> 
  mutate(speed_ms=steplength_m/timelag_sec)
caro6 <- caro6 |> 
  mutate(speed_ms=steplength_m/timelag_sec)
caro9 <- caro9 |> 
  mutate(speed_ms=steplength_m/timelag_sec)


#Plots und Karten

ggplot()+
  labs(x = "Zeit", y = "Geschwindigkeit (m/s)") +
  ggtitle(paste("Geschwindigkeit von Caro"))+
  geom_line(data=caro, aes(DatetimeUTC,speed_ms, color="caro"))+
  geom_line(data=caro3, aes(DatetimeUTC,speed_ms, color= "caro3"))+
  geom_line(data=caro6, aes(DatetimeUTC,speed_ms, color="caro6"))+
  geom_line(data=caro9, aes(DatetimeUTC,speed_ms, color="caro9"))+
  scale_color_manual(values = c("orange4", "orange", "navy", "magenta"))+
  theme_classic()

  
tmap_mode("view")
tm_shape(caro) +
  tm_dots(size = 0.1, col = "red") +
  tm_shape(caro3) +
  tm_dots(size = 0.1, col = "green") +
  tm_shape(caro6) +
  tm_dots(size = 0.1, col = "blue") +
  tm_shape(caro9) +
  tm_dots(size = 0.1, col = "yellow") +
  tm_layout(legend.show = TRUE) 


# Farben definieren
my_colors <- c("red", "green", "blue", "yellow")

# Karten erstellen
tm_shape(caro) +
  tm_dots(size = 0.5, col = my_colors[1]) +
  tm_shape(caro3) +
  tm_dots(size = 0.5, col = my_colors[2]) +
  tm_shape(caro6) +
  tm_dots(size = 0.5, col = my_colors[3]) +
  tm_shape(caro9) +
  tm_dots(size = 0.5, col = my_colors[4]) +
  tm_lines(lwd = 0.5, col = my_colors, 
           ids = c("caro", "caro3", "caro6", "caro9"),
           ignore.na = TRUE) +
  tm_layout(legend.show = TRUE) +
  tmap_mode("view")
#Ich komme hier nicht weiter mit den Linien....

#Versuch in ggplot2
ggplot()+
  geom_point(data=caro, aes(E,N, color="caro"))+
  geom_point(data=caro3, aes(E,N, color="caro3"))+
  scale_color_manual(values = c("orange4", "navy")) +
  geom_line(data=caro, aes(x=E, y=N, group="caro"), color="orange4") +
  geom_line(data=caro3, aes(x=E, y=N, group="caro3"), color="navy") +
  xlab("Longitude") +
  ylab("Latitude")
#Hier sind die Linien nicht in zeitlicher Reihenfolge

#Task 6
#Pomos Daten
library(readr)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tmap)

posmo_sevi <- read_delim("posmo/posmo_2023-01-01T00_00_00+01_00-2023-04-30T23_59_59+02_00.csv", delim=",") 

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

