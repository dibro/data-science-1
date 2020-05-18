####Zusammenfassung:
#Bewölkung ist eine gute Variable zur Vorhersage vom Umsatz (pro Warengruppe)
#Windgeschwindigkeit ist eine schlechte Variable zur Vorhersage vom Umsatz (pro Warengruppe)


# Einbinden benötigter Bibliotheken
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)


#Daten importieren
umsatzdaten <- read.csv("C:/Users/Sophia Fehrenbacher/Desktop/R Kurs/data-science-1/umsatzdaten_gekuerzt.csv")
wetter <- read.csv("C:/Users/Sophia Fehrenbacher/Desktop/R Kurs/data-science-1/wetter.csv")


#Daten zu einem Datensatz zusammenfügen
daten <- left_join(umsatzdaten, wetter)
View(daten)


########################
#BEWÖLKUNG
########################
#1. Betrachtung über alle Warengruppen hinweg
#a.Balkendiagramm 
b1 <- daten %>%
  group_by(Bewoelkung) %>%
  summarise( 
    durchschnitt_Umsatz=mean(Umsatz)
  ) 

ggplot(b1)+
  geom_bar(aes(x = Bewoelkung, y = durchschnitt_Umsatz), stat = "identity", fill="forestgreen", alpha=0.5)+
  ylab("durchschnittlicher Umsatz") 
#sieht so aus, als ob ab einer Bewölkung von 3 die Leute weniger zum Bäcker gehen


#b. Korrelationstest
cor.test(daten$Bewoelkung,daten$Umsatz,method="pearson")
#Korrelation ist negativ und signifikant unterschiedlich von 0 (1% Signifikanz Level)
#->Je stärker die Bewölkung, desto weniger Umsatz 
#könnte daran liegen, eine stärkere Bewölkung mit stärkerem Regen, Schnee oder Unwetter zusammenhängt
#Bewölkung scheint eine gute Variable zur Vorhersage vom Umsatz zu sein



#2. Betrachtung nach Warengruppen 
#a. Balkendiagramm
b2 <- daten %>%
  group_by(Warengruppe, Bewoelkung) %>%
  summarise( 
    mean_Umsatz=mean(Umsatz))

ggplot(b2)+
  geom_bar(aes(x = Bewoelkung, y = mean_Umsatz), stat = "identity", fill="forestgreen", alpha=0.5)+
  facet_wrap(~ Warengruppe) + ylab("durchschnittlicher Umsatz") 
#auf den ersten Blick scheinen die Warengruppen 1,4,6 (Brot, Konditorei, Saisonbrot) relativ weniger
#mit der Bewölkung zu schwanken als die anderen Warengruppen (Brötchen, Croissant, Kuchen)


#b. Korrelationstest
cor.test(daten[daten$Warengruppe==1,]$Bewoelkung,daten[daten$Warengruppe==1,]$Umsatz,method="pearson")
#negativ signifikant (1% Signifikanz Level)
cor.test(daten[daten$Warengruppe==2,]$Bewoelkung,daten[daten$Warengruppe==2,]$Umsatz,method="pearson")
#negativ signifikant (1% Signifikanz Level)
cor.test(daten[daten$Warengruppe==3,]$Bewoelkung,daten[daten$Warengruppe==3,]$Umsatz,method="pearson")
#negativ signifikant (1% Signifikanz Level)
cor.test(daten[daten$Warengruppe==4,]$Bewoelkung,daten[daten$Warengruppe==4,]$Umsatz,method="pearson")
#insignifikant
cor.test(daten[daten$Warengruppe==5,]$Bewoelkung,daten[daten$Warengruppe==5,]$Umsatz,method="pearson")
#negativ signifikant (1% Signifikanz Level)
cor.test(daten[daten$Warengruppe==6,]$Bewoelkung,daten[daten$Warengruppe==6,]$Umsatz,method="pearson")
#negativ signifikant (10% Signifikanz Level)

#Warengruppe 4 (Konditorei) und Bewölkung scheinen nicht (linear) zusammenzuhängen (etwas merkwürdig)
#Alle anderen Warengruppen hängen mit der Bewölkung negativ und signifikant zusammen
#->Je stärker die Bewölkung, desto weniger Umsatz (weil Regen, Schnee, Unwetter)
#Bewölkung scheint eine gute Variable zur Vorhersage vom Umsatz, auch nach Warengruppen, zu sein





########################
#Windgeschindigkeit
########################
#1. Betrachtung über alle Warengruppen hinweg
#a.Balkendiagramm 
w1 <- daten %>%
  group_by(Windgeschwindigkeit) %>%
  summarise( 
    wind_durchschn_umsatz=mean(Umsatz)
  ) 

ggplot(w1)+
  geom_bar(aes(x = Windgeschwindigkeit, y = wind_durchschn_umsatz), stat = "identity", fill="forestgreen", alpha=0.5)+
  ylab("durchschnittlicher Umsatz") 
#Balkendiagramm ist nicht ganz aufschlussreich; bei wenig Windigescheschwindigkeit 
#ist der Umsatz geringer als bei mittlerer; ein paar Ausreißer bei höherer Windgeschw. 


#b. Korrelationstest
cor.test(daten$Windgeschwindigkeit,daten$Umsatz,method="pearson")
#Korrelation ist positiv und signifikant unterschiedlich von 0 auf 10 Prozent Signifikanz Level
#unerwarteter Zusammenhang; dahinter müssten noch andere, wichtigere Variablen stecken 
#z.B. geringe Windgeschwindigkeit: es kann z.B. extrem heißes oder extrem kaltes Wetter sein
#insgesamt scheint Windgeschwindigkeit alleine als Variable wenig nützlich



#2. Betrachtung pro Warengruppe
#a. Balkendiagramm
w2 <- daten %>%
  group_by(Warengruppe, Windgeschwindigkeit) %>%
  summarise( 
    wind_mean_Umsatz=mean(Umsatz))

ggplot(w2)+
  geom_bar(aes(x = Windgeschwindigkeit, y = wind_mean_Umsatz), stat = "identity", fill="forestgreen", alpha=0.5)+
  facet_wrap(~ Warengruppe) + ylab("durchschnittlicher Umsatz") 
#Diagramme sind für mich wenig aufschlussreich


#b.Korrelationen
cor.test(daten[daten$Warengruppe==1,]$Windgeschwindigkeit,daten[daten$Warengruppe==1,]$Umsatz,method="pearson")
#schwach positiv signifikant (10% Signifikanz Level)
cor.test(daten[daten$Warengruppe==2,]$Windgeschwindigkeit,daten[daten$Warengruppe==2,]$Umsatz,method="pearson")
#insignifikant
cor.test(daten[daten$Warengruppe==3,]$Windgeschwindigkeit,daten[daten$Warengruppe==3,]$Umsatz,method="pearson")
#insignifikant
cor.test(daten[daten$Warengruppe==4,]$Windgeschwindigkeit,daten[daten$Warengruppe==4,]$Umsatz,method="pearson")
#insignifikant
cor.test(daten[daten$Warengruppe==5,]$Windgeschwindigkeit,daten[daten$Warengruppe==5,]$Umsatz,method="pearson")
#insignifikant
cor.test(daten[daten$Warengruppe==6,]$Windgeschwindigkeit,daten[daten$Warengruppe==6,]$Umsatz,method="pearson")
#insignifikant

#insgesamt ist Windgeschwindigkeit als Variable wenig aussagekräftig

