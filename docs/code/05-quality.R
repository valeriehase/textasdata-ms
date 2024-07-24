#### 1. Pakete laden und Daten einlesen ####
  
#install.packages("tidyverse)
#install.packages("quanteda")
#install.packages("RCurl")
#install.packages("caret")

library("tidyverse")
library("quanteda")
library("RCurl")
library("caret")

# Daten laden
url <-  getURL("https://raw.githubusercontent.com/valeriehase/textasdata-ms/main/data/data_tvseries.csv")
data <- read.csv2(text = url)

# Preprocessing
tokens <- tokens(data$Description,
                 what = "word", #Tokenisierung, hier zu Wörtern als Analyseeinheit
                 remove_punct = TRUE, #Entfernung von Satzzeichen
                 remove_numbers = TRUE) %>% #Entfernung von Zahlen
  
  # Kleinschreibung
  tokens_tolower()

# Text-as-Data Repräsentation als Document-Feature-Matrix
dfm <- tokens %>% 
  dfm() 

# Diktionär erstellen
diktionär_crime <- dictionary(list(crime = c("crim*", "police*", "gun*", 
                                             "shot*", "dead*", "murder*", 
                                             "kill*", "court*", "suspect*", 
                                             "witness*", "arrest*", "officer*", 
                                             "verdict*")))

#Diktionär anwenden
crime_tvshows <- dfm %>% 
  dfm_weight(scheme = "prop") %>% 
  dfm_lookup(dictionary = diktionär_crime)

# Ergebnis für die weitere Analyse in einen Data Frame umwandeln
crime_tvshows <- convert(crime_tvshows, 
                         to = "data.frame") %>%
  
  # Umwandlung in tibble-Format
  as_tibble %>%
  
  # Wir ergänzen zunächst wieder die Serientitel
  mutate(Title = data$Title) %>%
  
  # Wir erstellen eine Variable, die Texte als
  # "1" (Krimi) oder "0" (kein Krimi) identifiziert
  mutate(crime_binary = 1,
         crime_binary = replace(crime_binary,
                                crime == 0,
                                0)) %>%
  
  # Sortierung der Variablen
  select(Title, crime, crime_binary)

#Ausgabe der Ergebnisse
head(crime_tvshows)

#Ausgabe der Crime vs. Non-Crime Serien: prozentual
crime_tvshows %>%
  
  # absolute Anzahl jeder Sentiment-Art (n)
  count(crime_binary) %>%
  
  # Ausgabe in Prozent (perc)
  mutate(perc = prop.table(n)*100,
         perc = round(perc, 2))

#### 2. Validierung ####

# Sample für die manuelle Codierung
sample <- data %>%
  
  #Erstellung der Variable ID
  mutate(ID = paste0("ID", 1:nrow(data))) %>%
  
  # Stichprobe ziehen
  slice_sample(n = 30) %>%
  
  # Variable Manual Coding hinzufügen
  mutate(Manual.Coding = NA) %>%
  
  # Reduktion auf die drei relevanten Variablen
  select(ID, Description, Manual.Coding)

# Datei ausschreiben
write.csv2(sample, "validation_dictionary.csv")

# Codieren!

# Datei wieder einlesen
sample_coded <- read.csv2("validation_dictionary_coded.csv")

# Manuelle und automatisierte Codierung vergleichen
confusion <- crime_tvshows %>%
  
  # Erstellung der ID Variable für das Matching
  mutate(ID = paste0("ID", 1:nrow(data))) %>%
  
  # Match mit den codierten Daten 
  right_join(sample_coded) %>%
  
  # Reduktion auf die relevanten Variablen
  select(ID, crime_binary, Manual.Coding) %>%
  mutate(crime_binary = as.factor(crime_binary),
         Manual.Coding = as.factor(Manual.Coding)) %>%
  
  # Anpassung der Variablennamen
  rename(automated = crime_binary,
         manual = Manual.Coding) 

#Ausgabe der Ergebnisse
head(confusion)

# Berechnung der Validität
confusionMatrix(data = confusion$automated,
                reference = confusion$manual, 
                mode = "prec_recall", 
                positive = "1")