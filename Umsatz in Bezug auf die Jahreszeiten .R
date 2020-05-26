umsatz<-read.csv("umsatzdaten_gekuerzt.csv")
View(umsatz)
umsatz$Wochentag <- weekdays(as.Date(umsatz$Datum))

aggregate(Umsatz~Warengruppe+Wochentag, data=umsatz, mean)
boxplot(Umsatz~Wochentag+Warengruppe, data=umsatz)


umsatz$Monat<- month(as.POSIXlt(umsatz$Datum, format="%Y-%m-%d"))
View(umsatz)

umsatz_frueh<-subset(umsatz, Monat==3|Monat==4|Monat==5)
umsatz_somm<-subset(umsatz, Monat==6|Monat==7|Monat==8)
umsatz_herb<-subset(umsatz, Monat==9|Monat==10|Monat==11)
umsatz_wint<-subset(umsatz, Monat==12|Monat==1|Monat==2)

as.factor(umsatz_frueh$Jahreszeit<-"Fruehling")
as.factor(umsatz_somm$Jahreszeit<-"Sommer")
as.factor(umsatz_herb$Jahreszeit<-"Herbst")
as.factor(umsatz_wint$Jahreszeit<- "Winter")

umsatz2<-rbind(umsatz_frueh, umsatz_somm, umsatz_herb, umsatz_wint)
umsatz2$Jahreszeit<-as.factor(umsatz2$Jahreszeit)

summary(umsatz2)
str(umsatz2)

aggregate(Umsatz~Wochentag+Warengruppe+Jahreszeit, data=umsatz2, mean)
par(mfrow=c(1,1))
boxplot(Umsatz~Wochentag+Warengruppe+Jahreszeit, data=umsatz2)


boxplot(Umsatz~Wochentag+Jahreszeit, data=umsatz2[umsatz2$Warengruppe==1,])
boxplot(Umsatz~Wochentag+Jahreszeit, data=umsatz2[umsatz2$Warengruppe==2,])
boxplot(Umsatz~Wochentag+Jahreszeit, data=umsatz2[umsatz2$Warengruppe==3,])
boxplot(Umsatz~Wochentag+Jahreszeit, data=umsatz2[umsatz2$Warengruppe==4,])
boxplot(Umsatz~Wochentag+Jahreszeit, data=umsatz2[umsatz2$Warengruppe==5,])
boxplot(Umsatz~Wochentag+Jahreszeit, data=umsatz2[umsatz2$Warengruppe==6,])


par(mar=c(5,2,4,0.01)+0.1)
par(mfrow=c(2,3))
boxplot(Umsatz~Jahreszeit, data=umsatz2[umsatz2$Warengruppe==1,], main="Warengruppe 1", col=c("darkolivegreen2","goldenrod1","coral3","cornflowerblue"))
boxplot(Umsatz~Jahreszeit, data=umsatz2[umsatz2$Warengruppe==2,], main="Warengruppe 2", col=c("darkolivegreen2","goldenrod1","coral3","cornflowerblue"))
boxplot(Umsatz~Jahreszeit, data=umsatz2[umsatz2$Warengruppe==3,], main="Warengruppe 3", col=c("darkolivegreen2","goldenrod1","coral3","cornflowerblue"))
boxplot(Umsatz~Jahreszeit, data=umsatz2[umsatz2$Warengruppe==4,], main="Warengruppe 4", col=c("darkolivegreen2","goldenrod1","coral3","cornflowerblue"))
boxplot(Umsatz~Jahreszeit, data=umsatz2[umsatz2$Warengruppe==5,], main="Warengruppe 5", col=c("darkolivegreen2","goldenrod1","coral3","cornflowerblue"))
boxplot(Umsatz~Jahreszeit, data=umsatz2[umsatz2$Warengruppe==6,], main="Warengruppe 6", col=c("darkolivegreen2","goldenrod1","coral3","cornflowerblue"))

