#### 1. Pakete laden und Daten einlesen ####
  
#install.packages("tidyverse)
#install.packages("quanteda")
#install.packages("quanteda.textplots")
#install.packages("RCurl")

library("tidyverse")
library("quanteda")
library("quanteda.textplots")
library("RCurl")

##### 1.1 Textdaten aus einer lokalen Datei einlesen #####
# data <- read.csv2("data_tvseries.csv")

##### 1.2 Textdaten von einer URL downloaden #####

url <-  getURL("https://raw.githubusercontent.com/valeriehase/textasdata-ms/main/data/data_tvseries.csv")
data <- read.csv2(text = url)

#Check der Daten
head(data)

#### 2. Preprocessing ####
  
##### 2.1 Bereinigung (z. B. Encoding-Probleme) ##### 
  
data %>%
  
  #Auswahl der Variable "Description"
  select(Description) %>% 
  
  #Reduktion auf ersten Text
  slice(1)

###### 2.1.1 Encoding-Probleme ######

#Beispiel-Satz
string <- "Schöne Grüße aus München"

#Encoding prüfen
Encoding(string)

#Encoding testweise ändern
Encoding(string) <- "latin1"
string

#Mit Hilfe von regulären Ausdrücken bereinigen
string %>% 
  
  #Ersatz für falsches Encoding "ö"
  gsub(pattern = "Ã¶", replacement ="ö") %>% 
  
  #Ersatz für falsches Encoding "ü"
  gsub(pattern = "Ã¼", replacement = "ü") %>% 
  
  #Ersatz für falsches Encoding "ß"
  gsub(pattern = "ÃŸ", replacement = "ß") 

###### 2.1.2 Datenbereinigung mit regulären Ausdrücken ######

#Schauen wir uns den Titel an
data %>%
  select(Title) %>%
  head(5)

#Entfernung der Zeichen vor dem Titel der TV-Serie
data <- data %>%
  mutate(Title = gsub("^[0-9]+[[:punct:]] ", "", Title))

#So sieht das Ergebnis aus:
data %>%
  select(Title) %>%
  head(5)

###### 2.1.3 Datenfilterung mit regulären Ausdrücken ######

#TV-Serien, die sich um Drama drehen
data %>%
  
  # filtern aller TV_Serien, die "Drama" in der Beschreibung beinhalten
  filter(grepl("[D|d]rama", Description)) %>%
  
  # Inspektion der ersten fünf Titel
  select(Title) %>%
  head(5)

#TV-Serien, die sich um Drama oder Crime drehen
data %>%
  
  # filtern aller TV_Serien, die "Drama"und "Crime" in der Beschreibung beinhalten
  filter(grepl("[D|d]rama|[C|c]rime", Description)) %>%
  
  # Inspektion der ersten fünf Titel
  select(Title) %>%
  head(5)

###### 2.1.4 Aufgabe 1 📌 ######

# Basis: Alle Serien identifizieren, die in Deutschland spielen?
  
# Fortgeschritten: Alle Serien identifizieren, in denen es um Superhelden geht und "*superhero/superheroes*” in der Variable `Description` mit "*fancy R programmers*“ ersetzen?

##### 2.2 Normalisierung #####
tokens <- tokens(data$Description,
                 what = "word", #Tokenisierung, hier zu Wörtern als Analyseeinheit
                 remove_punct = TRUE, #Entfernung von Satzzeichen
                 remove_numbers = TRUE) %>% #Entfernung von Zahlen
  
  #Kleinschreibung
  tokens_tolower() %>% 
  
  #Entfernung von Stoppwörtern
  tokens_remove(stopwords("english")) %>% 
  
  #Stemming
  tokens_wordstem()

#So sah unser erster Text vor dem Preprocessing aus
data$Description[1]

#Und so danach
tokens[1]

###### 2.2.1 Mehr Infos zur Entfernung von Stoppwörtern ######
Es gibt verschiedene Möglichkeiten, Stoppwörter zu entfernen. Am einfachsten ist dies mithilfe der im `quanteda`-Paket integrierten Stoppwortlisten möglich. Diese sind in mehreren Sprachen verfügbar, darunter auch Deutsch. 

#Wörter aus der quanteda Stoppwortliste entfernen
stoppwörter <- stopwords("english")
stoppwörter <- stoppwörter[!stoppwörter %in% c("i", "me")]

#Beispielhafte Anwendung 
tokens(data$Description,
       what = "word", #Tokenisierung, hier zu Wörtern als Analyseeinheit
       remove_punct = TRUE, #Entfernung von Satzzeichen
       remove_numbers = TRUE) %>% #Entfernung von Zahlen
  
  #Kleinschreibung
  tokens_tolower() %>% 
  
  #Entfernung von Stoppwörtern - hier z.B. reduzierte quanteda-Liste
  tokens_remove(stoppwörter) %>% 
  
  #Stemming
  tokens_wordstem() %>%
  
  #Ausgabe des ersten Textes
  head(1)

####### 2.2.2 Aufgabe 2 📌 #######

#Basis: Eine Liste mit 3-5 Stopwörtern erstellen und diese als Teil des Preprocessings zusätzlich entfernen?
  
#Fortgeschritten: Dafür sorgen, dass Namen von Städten (hier als Beispiel „New York“) als ein einzelnes Feature beibehalten werden?
  
##### 3. Text-as-Data-Repräsentation #####
  
###### 3.1 Erstellung einer DFM ######
  
#Wir erstellen eine Document-Feature matrix
dfm <- tokens %>%
  dfm()

#So sieht das Ergebnis aus
dfm

###### 3.2 Zusätzliche Normalisierung: Relative Pruning ######

#Anzahl Features vor relative pruning
dfm

#Anwendung des relative pruning
dfm  <- dfm  %>% 
  dfm_trim( min_docfreq = 0.005, 
            max_docfreq = 0.99, 
            docfreq_type = "prop", 
            verbose = TRUE) 

#Anzahl Features nach relative pruning
dfm
  
##### 4. Erste Analysen #####
  
###### 4.1 Top Features ######
  
topfeatures(dfm, 10) %>%
  
  #Umwandlung in einen "schöneren" Dataframe mit der Spalte "Häufigkeit"
  as.data.frame() %>%
  rename("Häufigkeit" = '.')

###### 4.2. Die berühmt-berüchtigte Word Cloud ######
textplot_wordcloud(dfm, max_words = 100)