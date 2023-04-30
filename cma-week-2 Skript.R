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

  

tm_shape(caro) +
  tm_dots(size = 0.1, col = "red") +
  tm_shape(caro3) +
  tm_dots(size = 0.1, col = "green") +
  tm_shape(caro6) +
  tm_dots(size = 0.1, col = "blue") +
  tm_shape(caro9) +
  tm_dots(size = 0.1, col = "yellow") +
  tm_layout(legend.show = TRUE) +
  tm_lines(caro, col = "red") +
  tm_lines(caro3, col = "green") +
  tm_lines(caro6, col = "blue") +
  tm_lines(caro9, col = "yellow") +
  tmap_mode("view")


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
caro_sorted <- caro|> group_by((DatetimeUTC)) |> summarise(caro_sorted)
caro3_sorted <- caro3|> group_by((DatetimeUTC))|> summarise(caro3_sorted)
caro6_sorted <- caro6|> group_by((DatetimeUTC))|> summarise(caro6_sorted)
caro9_sorted <- caro9 |> group_by((DatetimeUTC))|> summarise(caro9_sorted)

ggplot() +
  geom_point(data=caro_sorted, aes(E, N, color="caro")) +
  geom_point(data=caro3_sorted, aes(E, N, color="caro3")) +
  geom_point(data=caro6_sorted, aes(E, N, color="caro6")) +
  geom_point(data=caro9_sorted, aes(E, N, color="caro9")) +
  geom_line(data=bind_rows(caro_sorted, caro3_sorted, caro6_sorted, caro9_sorted),
            aes(E, N, group=TierName, color=TierName)) +
  labs(x="Longitude", y="Latitude", color="Data") +
  theme_bw()


ggplot()+
  geom_point(data=caro, aes(E,N, color="caro"))+
  geom_point(data=caro3, aes(E,N, color="caro3"))+
  scale_color_manual(values = c("orange4", "navy")) +
  geom_line(data=caro, aes(x=E, y=N, group="caro"), color="orange4") +
  geom_line(data=caro3, aes(x=E, y=N, group="caro3"), color="navy") +
  xlab("Longitude") +
  ylab("Latitude")
 
str(caro)

