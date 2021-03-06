###################################################
### Vorbereitung der Umgebung ####

# Falls nicht installiert ggf. folgendes Paket ausführen
#install.packages("fastDummies")

# Umgebungsvariablen löschen
remove(list = ls())

# Einbinden benötigter Funktionsbibliotheken
library(readr)
library(fastDummies)

library(dplyr)

###################################################
### Funktionsdefinitionen ####

#' Title Fast creation of normalized variables
#' Quickly create normalized columns from numeric type columns in the input data. This function is useful for statistical analysis when you want normalized columns rather than the actual columns.
#'
#' @param .data An object with the data set you want to make normalized columns from.
#' @param norm_values Dataframe of column names, means, and standard deviations that is used to create corresponding normalized variables from.
#'
#' @return A data.frame (or tibble or data.table, depending on input data type) with same number of rows an dcolumns as the inputted data, only with normalized columns for the variables indicated in the norm_values argument.
#' @export
#'
#' @examples
norm_cols <- function (.data, norm_values = NULL) {
  for (i in 1:nrow(norm_values)  ) {
    .data[[norm_values$name[i]]] <- (.data[[norm_values$name[i]]] - norm_values$mean[i]) / norm_values$sd[i]
  }
  return (.data)
}


#' Title Creation of a Dataframe including the Information to Standardize Variables
#' This function is meant to be used in combination with the function norm_cols
#'
#' @param .data A data set including the variables you want to get the means and standard deviations from.
#' @param select_columns A vector with a list of variable names for which you want to get the means and standard deviations from.
#'
#' @return A data.frame (or tibble or data.table, depending on input data type) including the names, means, and standard deviations of the variables included in the select_columns argument.
#' @export
#'
#' @examples
get.norm_values <- function (.data, select_columns = NULL) {
  result <- NULL
  for (col_name in select_columns) {
    mean <- mean(.data[[col_name]], na.rm = TRUE)
    sd <- sd(.data[[col_name]], na.rm = TRUE)
    result <- rbind (result, c(mean, sd))
  }
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  result <- data.frame (select_columns, result, stringsAsFactors = FALSE)
  names(result) <- c("name", "mean", "sd")
  return (result)
}


###################################################
### Datenimport ####

# Einlesen der Daten
#house_pricing <- read_csv("https://raw.githubusercontent.com/opencampus-sh/sose20-datascience/master/house_pricing_test.csv")
umsatzdaten <- read_csv("umsatzdaten_gekuerzt.csv")

neues_datum <- as.Date("2019-06-04")

umsatzdaten[nrow(umsatzdaten) + 1,] = list(neues_datum, 1, 0)
umsatzdaten[nrow(umsatzdaten) + 1,] = list(neues_datum, 2, 0)
umsatzdaten[nrow(umsatzdaten) + 1,] = list(neues_datum, 3, 0)
umsatzdaten[nrow(umsatzdaten) + 1,] = list(neues_datum, 4, 0)
umsatzdaten[nrow(umsatzdaten) + 1,] = list(neues_datum, 5, 0)
umsatzdaten[nrow(umsatzdaten) + 1,] = list(neues_datum, 6, 0)

wetter <- read_csv("wetter.csv")

wetter$Wettercode[is.na(wetter$Wettercode)]<-0

#Neue Variable - Regen/Kein Regen 

wetter$Regen <- wetter$Wettercode
wetter$Regen[wetter$Regen<80]<-0
wetter$Regen[wetter$Regen>=80]<-1
wetter$Regen[is.na(wetter$Regen)]<-1

numsatz <- left_join(umsatzdaten,wetter)
numsatz <- numsatz[ -c(7) ]

feiertage <- read.csv("feiertage.csv", sep= ";")
# Splittet durch ';' getrennte Daten in eigene Spalten

kiwo <- read.csv("kiwo.csv")

kiwo$Datum <- as.Date(kiwo$Datum)
# Kovertiert die Spalte 'Datum' in das 'Date'-Format

numsatz$Wochentag <- weekdays(as.Date(numsatz$Datum))
numsatz <- left_join(numsatz, unique(kiwo)) 
#Durch unique werden Extraspalten wie 'Datum.x' verhindert

#cbind(umsatzdaten,umsatz_wetter_daten$Wettercodekategorie)

feiertage$Datum <- as.Date(feiertage$Datum) 
# Kovertierte die Spalte 'Datum' in das 'Date'-Format

feiertage$Feiertag <- 1 #alle Feiertage mit 1 codiert


numsatz <- left_join(numsatz, unique(feiertage)) 
# unique verhindert Extraspalten wie 'Datum.y'
numsatz <- left_join(numsatz, kiwo)

numsatz$KielerWoche[is.na(numsatz$KielerWoche)] <-0 
#NA zu O
numsatz$Feiertag[is.na(numsatz$Feiertag)] <- 0 
#NA zu 0
numsatz$Heiligerabend_dummy[is.na(numsatz$Heiligerabend_dummy)] <- 0
numsatz$Silvester_dummy[is.na(numsatz$Silvester_dummy)] <- 0
#NA zu 0

numsatz$Bewoelkung[is.na(numsatz$Bewoelkung)] <- 0
numsatz$Windgeschwindigkeit[is.na(numsatz$Windgeschwindigkeit)] <- 0
numsatz$Regen[is.na(numsatz$Regen)] <- 0

any(is.na(numsatz$Datum))
any(is.na(numsatz$Warengruppe))
any(is.na(numsatz$Umsatz))
any(is.na(numsatz$Bewoelkung))
any(is.na(numsatz$Windgeschwindigkeit))
any(is.na(numsatz$Regen))
any(is.na(numsatz$Wochentag))
any(is.na(numsatz$KielerWoche))
any(is.na(numsatz$Feiertag))
any(is.na(numsatz$Heiligerabend_dummy))
any(is.na(numsatz$Silvester_dummy))

numsatz=numsatz[!is.na(numsatz$Temperatur),]

#View(numsatz)

###################################################
### Datenaufbereitung ####

# Rekodierung von kategoriellen Variablen (zu Dummy-Variablen)
#dummy_list <- c("view", "condition")
#house_pricing_dummy = dummy_cols(house_pricing, dummy_list)
dummy_list <- c("Warengruppe", "Wochentag", "Regen")
numsatz_dummy = dummy_cols(numsatz, dummy_list)

# Definition von Variablenlisten für die Dummies, um das Arbeiten mit diesen zu erleichtern

Warengruppe_dummies = c('Warengruppe_1', 'Warengruppe_2', 'Warengruppe_3', 'Warengruppe_4', 'Warengruppe_5', 'Warengruppe_6')
#Wochentag_dummies = c('Wochentag_Monday', 'Wochentag_Tuesday', 'Wochentag_Wednesday', 'Wochentag_Thursday', 'Wochentag_Friday', 'Wochentag_Saturday', 'Wochentag_Sunday')
Wochentag_dummies = c('Wochentag_Montag', 'Wochentag_Dienstag', 'Wochentag_Mittwoch', 'Wochentag_Donnerstag', 'Wochentag_Freitag', 'Wochentag_Samstag', 'Wochentag_Sonntag')

Regen_dummies = c('Regen_0', 'Regen_1')

# Standardisierung aller Feature Variablen und der Label Variable
#norm_list <- c("price", "sqft_lot", "bathrooms", "grade", "waterfront", view_dummies, condition_dummies) # Liste aller Variablen
#norm_values_list <- get.norm_values(house_pricing_dummy, norm_list)    # Berechnung der Mittelwerte und Std.-Abw. der Variablen
#house_pricing_norm <- norm_cols(house_pricing_dummy, norm_values_list) # Standardisierung der Variablen

norm_list <- c("Umsatz", "Temperatur", Warengruppe_dummies, Wochentag_dummies, Regen_dummies)
# Liste aller Variablen
norm_values_list <- get.norm_values(numsatz_dummy, norm_list)   
# Berechnung der Mittelwerte und Std.-Abw. der Variablen
numsatz_norm <- norm_cols(numsatz_dummy, norm_values_list) 
# Standardisierung der Variablen


###################################################
### Definition der Feature-Variablen und der Label-Variable ####

# Definition der Features (der unabhängigen Variablen auf deren Basis die Vorhersagen erzeugt werden sollen)
features = c('Temperatur', Warengruppe_dummies, Wochentag_dummies, Regen_dummies)
# Definition der Label-Variable (der abhaengigen Variable, die vorhergesagt werden soll) sowie
label = 'Umsatz'

###################################################
### Definition von Trainings- und Testdatensatz ####

# Zufallszähler setzen, um die zufällige Partitionierung bei jedem Durchlauf gleich zu halten
set.seed(1)

numsatz_subset <- numsatz
numsatz_subset <- numsatz_subset[-c(10804:10809), ]

numsatz_norm_subset <- numsatz_norm
numsatz_norm_subset <- numsatz_norm_subset[-c(10804:10809), ]

# Bestimmung der Indizes des Traininsdatensatzes
train_ind <- sample(seq_len(nrow(numsatz_norm_subset)), size = floor(0.66 * nrow(numsatz_norm_subset)))

# Teilen in Trainings- und Testdatensatz
train_dataset = numsatz_norm_subset[train_ind, features]
test_dataset = numsatz_norm_subset[-train_ind, features]

# Selektion der Variable, die als Label definiert wurde
train_labels = numsatz_norm_subset[train_ind, label]
test_labels = numsatz_norm_subset[-train_ind, label]
