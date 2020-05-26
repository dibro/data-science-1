# Das Script stellt den Zusammenhang vom Umsatz und dem Wettercode dar.

# https://wetterkanal.kachelmannwetter.com/was-ist-der-ww-code-in-der-meteorologie/
# http://www.seewetter-kiel.de/seewetter/daten_symbole.htm

library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)

# Daten importieren
umsatzdaten <- read_csv("umsatzdaten_gekuerzt.csv")
wetterdaten <- read_csv("wetter.csv")

umsatz_wetter_daten <- left_join(umsatzdaten, wetterdaten)


########################
# Wettercode
########################
# 1. Betrachtung über alle Warengruppen hinweg
# a. Balkendiagramm 
p1 <- umsatz_wetter_daten %>%
  group_by(Wettercode) %>%
  summarise( 
    durchschnitt_Umsatz=mean(Umsatz)
  ) 

ggplot(p1)+
  geom_bar(aes(x = Wettercode, y = durchschnitt_Umsatz), stat = "identity", fill="forestgreen", alpha=0.5)+
  ylab("durchschnittlicher Umsatz") 


# 2. Betrachtung nach Warengruppen 
# a. Balkendiagramm
p2 <- umsatz_wetter_daten %>%
  group_by(Warengruppe, Wettercode) %>%
  summarise( 
    mean_Umsatz=mean(Umsatz))

ggplot(p2)+
  geom_bar(aes(x = Wettercode, y = mean_Umsatz), stat = "identity", fill="forestgreen", alpha=0.5)+
  facet_wrap(~ Warengruppe) + ylab("durchschnittlicher Umsatz pro Warengruppe") 

