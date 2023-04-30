############
#Excercise 2
############

library(dplyr)
library(readr)
library(sf)
library(ggplot2)


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

###Task 

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
#Mit mutate & lag()
caro_lag <- caro |> 
  mutate(caro, timelag_sec=as.integer(difftime(DatetimeUTC, lag(DatetimeUTC))))
#Mit mutate & lead()
caro_lead <- caro |> 
  mutate(caro, timelag_sec=(lead(DatetimeUTC),))

