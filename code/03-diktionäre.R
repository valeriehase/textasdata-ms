#### 1. Pakete laden und Daten einlesen ####
  
#install.packages("tidyverse)
#install.packages("RCurl")
#install.packages("quanteda")

library("tidyverse")
library("RCurl")
library("quanteda")

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

#### 2. Off-the-Shelf Diktionäre ####

##### 2.1 Diktionär auswählen #####
# Wir schauen uns das Diktionär an
data_dictionary_LSD2015 %>% 
  head()

##### 2.2 Features aus Diktionär identifizieren #####
sentiment_tvshows <- dfm %>% 
  
  # Suche nach Features aus Diktionär
  # Gewichtung relativ zur Anzahl aller Wörter
  dfm_weight(scheme = "prop") %>% 
  dfm_lookup(dictionary = data_dictionary_LSD2015[1:2])

# Ausgabe der Ergebnisse
sentiment_tvshows %>% 
  head()

#Allererste Beobachtung
data$Description[1]

##### 2.3 Texte klassifizieren #####
# Ergebnis für die weitere Analyse in einen Dataframe umwandeln
sentiment_tvshows <- convert(sentiment_tvshows, 
                             to = "data.frame") %>%
  
  # Umwandlung in tibble-Format
  as_tibble %>%
  
  # Wir ergänzen zunächst wieder die Serientitel & das "Parental-Rating"
  mutate(Title = data$Title,
         Parental.Rating = data$Parental.Rating) %>%
  
  # Wir erstellen eine Variable, die Texte als
  # "neutral", "positiv" oder "negativ" identifiziert
  
  # Zunächst gelten alle Texte als "neutral"
  mutate(sentiment = "neutral",
         
         # Falls mehr pos. als neg: "positiv"
         sentiment = replace(sentiment,
                             positive > negative,
                             "positiv"),
         
         # Falls mehr neg. als pos.: "negativ"
         sentiment = replace(sentiment,
                             positive < negative,
                             "negativ")) %>%
  
  # Sortierung der Variablen
  select(Title, Parental.Rating, positive, negative, sentiment)

# Ausgabe des Ergebnis
sentiment_tvshows %>%
  head()

# Anzahl neutral, negativer und positiver Texte?
sentiment_tvshows %>%
  
  # absolute Anzahl jeder Sentiment-Art (n)
  count(sentiment) %>%
  
  # Ausgabe in Prozent (perc)
  mutate(perc = prop.table(n)*100,
         perc = round(perc, 2))

# Negativste Serien
sentiment_tvshows %>% 
  arrange(desc(negative)) %>% 
  slice(1:5)

# Positivste Serien
sentiment_tvshows %>% 
  arrange(desc(positive)) %>% 
  slice(1:5)

##### 2.4 Gruppenvergleich #####
sentiment_tvshows <- sentiment_tvshows %>% 
  
  #Erstellen einer "Rating.Adults"-Klassifizierungs-Variable
  mutate(Rating.Adults = "für Kinder",
         Rating.Adults = replace(Rating.Adults,
                                 Parental.Rating == "TV-MA",
                                 "für Erwachsene"))

#Wir schauen uns die Ergebnisse an
head(sentiment_tvshows)

#Wir schauen uns die Ergebnisse an
head(sentiment_tvshows)

#Visualisierung
plot <- sentiment_tvshows %>%
  
  # Wir berechnen die gruppierten Häufigkeiten
  group_by(Rating.Adults) %>%
  
  # absolute Anzahl jeder Sentiment-Art (n)
  count(sentiment) %>%
  
  # Ausgabe in Prozent (perc)
  mutate(perc = prop.table(n)*100,
         perc = round(perc, 2)) %>%
  
  # Wir heben die Gruppierung auf
  ungroup()

# Visualisierung
ggplot(plot, aes(fill = Rating.Adults, y = perc, x = sentiment)) + 
  
  # Wir kreiern den entsprechenden Graphen
  geom_bar(stat ="identity", position = "fill") +
  
  # Wir fügen Achsenbeschriftungen hinzu
  labs(y = "% negatives, neutrales und positives Sentiment", 
       x = "Sentiment",
       title = "Sentiment in TV-Serien",
       colour = "Sentiment-Kategorie") +
  
  # Wir ändern das Background-Design
  theme_light()

#### 3. Organische Diktionäre ####

##### 3.1. Diktionär erstellen ##### 
diktionär_crime <- dictionary(list(crime = c("crim*", "police*", "gun*", 
                                             "shot*", "dead*", "murder*", 
                                             "kill*", "court*", "suspect*", 
                                             "witness*", "arrest*", "officer*", 
                                             "verdict*")))

##### 3.2. Features aus Diktionär identifizieren ##### 
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
  # "crime" oder "non-Crime" identifiziert
  mutate(crime_binary = "crime",
         crime_binary = replace(crime_binary,
                                crime == 0,
                                "non-crime")) %>%
  
  # Sortierung der Variablen
  select(Title, crime, crime_binary)

#Ausgabe der Ergebnisse
head(crime_tvshows)

##### 3.3. Texte klassifizieren #####
#Ausgabe der Crime vs. Non-Crime Serien
crime_tvshows %>%
  
  # absolute Anzahl jeder Sentiment-Art (n)
  count(crime_binary) %>%
  
  # Ausgabe in Prozent (perc)
  mutate(perc = prop.table(n)*100,
         perc = round(perc, 2))

#### Aufgabe 1 📌 ####

##### Aufgabe 1.1 #####
# Könnt ihr analysieren, wie viel Prozent der Serien Science-Fiction Serien sind?

##### Aufgabe 1.2 #####
# Könnt ihr analysieren, welche fünf Science-Fiction Serien die (nach Publikums-Votum laut Number.of.Votes) beliebtesten Serien sind?