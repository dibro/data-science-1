# Das Script stellt den Zusammenhang vom Umsatz und der Wettercodekategorie dar.
# Wettercodekategorie ist eine neue Variable, die die Wettercodes gruppiert.

# https://wetterkanal.kachelmannwetter.com/was-ist-der-ww-code-in-der-meteorologie/
# http://www.seewetter-kiel.de/seewetter/daten_symbole.htm

# Wettercode  0 -  9: Kategorie 1  - Bewölkung, Dunst, Rauch, Staub oder Sand
# Wettercode 10 - 19: Kategorie 2  - Trockenereignisse
# Wettercode 20 - 29: Kategorie 3  - Ereignisse der letzten Stunde, aber nicht zur Beobachtungszeit
# Wettercode 30 - 39: Kategorie 4  - Staubsturm, Sandsturm, Schneefegen oder -treiben
# Wettercode 40 - 49: Kategorie 5  - Nebel oder Eisnebel
# Wettercode 50 - 59: Kategorie 6  - Sprühregen
# Wettercode 60 - 69: Kategorie 7  - Regen
# Wettercode 70 - 79: Kategorie 8  - Schnee
# Wettercode 80 - 89: Kategorie 9  - Schauer
# Wettercode 90 - 99: Kategorie 10 - Gewitter

library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)

# Daten importieren
umsatzdaten <- read_csv("umsatzdaten_gekuerzt.csv")
wetterdaten <- read_csv("wettercodekategorie.csv")

umsatz_wetter_daten <- left_join(umsatzdaten, wetterdaten)


########################
# Wettercodekategorie
########################
# 1. Betrachtung über alle Warengruppen hinweg
# a. Balkendiagramm 
p1 <- umsatz_wetter_daten %>%
  group_by(Wettercodekategorie) %>%
  summarise( 
    durchschnitt_Umsatz=mean(Umsatz)
  ) 

ggplot(p1)+
  geom_bar(aes(x = Wettercodekategorie, y = durchschnitt_Umsatz), stat = "identity", fill="forestgreen", alpha=0.5)+
  ylab("durchschnittlicher Umsatz") 


# 2. Betrachtung nach Warengruppen 
# a. Balkendiagramm
p2 <- umsatz_wetter_daten %>%
  group_by(Warengruppe, Wettercodekategorie) %>%
  summarise( 
    mean_Umsatz=mean(Umsatz))

ggplot(p2)+
  geom_bar(aes(x = Wettercodekategorie, y = mean_Umsatz), stat = "identity", fill="forestgreen", alpha=0.5)+
  facet_wrap(~ Warengruppe) + ylab("durchschnittlicher Umsatz pro Warengruppe") 

