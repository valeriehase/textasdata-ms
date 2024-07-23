#### 1. Pakete laden und Daten einlesen ####
  
#install.packages("tidyverse)
#install.packages("RCurl")
#install.packages("quanteda")
#install.packages("stm")
#install.packages("reshape2")

library("tidyverse")
library("RCurl")
library("quanteda")
library("stm")
library("reshape2")

# Sowie zusätzlich:
install.packages("stminsights")
library("stminsights")

# Daten laden
url <-  getURL("https://raw.githubusercontent.com/valeriehase/textasdata-ms/main/data/data_tvseries.csv")
data <- read.csv2(text = url)

# Preprocessing
tokens <- tokens(data$Description,
                 what = "word", #Tokenisierung, hier zu Wörtern als Analyseeinheit
                 remove_punct = TRUE, #Entfernung von Satzzeichen
                 remove_numbers = TRUE) %>% #Entfernung von Zahlen
  
  # Kleinschreibung
  tokens_tolower() %>% 
  
  # Entfernung von Stoppwörtern
  tokens_remove(stopwords("english")) %>% 
  
  # Stemming
  tokens_wordstem()

# Text-as-Data Repräsentation als Document-Feature-Matrix
dfm <- tokens %>% 
  dfm() %>% 
  
  # Relative pruning
  dfm_trim( min_docfreq = 0.005, 
            max_docfreq = 0.99, 
            docfreq_type = "prop", 
            verbose = TRUE) 

#### 2. Anzahl Themen K ####

# DFM in STM-Objekt umwandeln
dfm_stm <- convert(dfm, to = "stm")

##### 2.1 Statistischer Fit #####

# dfm_stm$documents: Welche Dokumente nutzen wir?
# dfm_stm$vocab: Welche Features nutzen wir?
stat_fit <- searchK(dfm_stm$documents, dfm_stm$vocab, K = c(4,6), verbose = TRUE)

# Wir speichern die Ergebnisse im Objekt "Plot" ab 
plot <- data.frame("K" = c(4, 6),
                   "Coherence" = unlist(stat_fit$results$semcoh),
                   "Perplexity" = unlist(stat_fit$results$heldout))

# Wir wandeln das Format zu einem "long format" um
plot <- melt(plot, id = c("K"))

# Plot erstellen
ggplot(plot, aes(K, value, color = variable)) +
  geom_line(linewidth = 1.5, show.legend = FALSE) +
  scale_x_continuous(breaks = c(4, 6)) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Anzahl Themen K",
       title = "Statistischer Fit für Modelle mit K = 4 und K = 6")

##### 2.2 Inhaltliche Interpretierbarkeit #####

# Model mit K = 4 berechnen
model_4K <- stm(documents = dfm_stm$documents,
                vocab = dfm_stm$vocab, 
                K = 4)

# Model mit K = 6 berechnen
model_6K <- stm(documents = dfm_stm$documents,
                vocab = dfm_stm$vocab, 
                K = 6)

###### 2.2.1 Top Features ######

# Top Features für K = 4
topics_4K <- labelTopics(model_4K, n = 10)

# Nur Top-10 Features nach Frex-Gewichtung, welche besser interpretierbar ist
# Gewichtet Features nach Kohärenz und Exklusivität
topics_4K <- data.frame("features" = t(topics_4K$frex))

# Benennung & Ausgabe
colnames(topics_4K) <- paste("Topics", c(1:4))
topics_4K

# Top Features für K = 6
topics_6K <- labelTopics(model_6K, n = 10)

# Nur Top-10 Features nach Frex-Gewichtung, welche besser interpretierbar ist
# Gewichtet Features nach Kohärenz und Exklusivität
topics_6 <- data.frame("features" = t(topics_6K$frex))

#Benennung & Ausgabe
colnames(topics_6) <- paste("Topics", c(1:6))
topics_6

###### 2.2.2 Top Documents ######
findThoughts(model_4K, data$Description, topics=1, n=3)

##### 2.3 Rank-1 Metrik #####

# Document-Topic Matrix extrahieren
theta_4K <- make.dt(model_4K)
theta_6K <- make.dt(model_6K)

# Schauen wir uns kurz beispielhaft die Matrix an:
theta_4K %>%
  head()

###### 2.3.1 Zuordnung der Hauptthemen ######

# Zuerst erstellen wir zwei leere Spalten in unserem Dataframe data
data <- data %>%
  
  # Leere Variable für Hauptthema, wird später "aufgefüllt"
  mutate(Rank1_K4 = NA,
         Rank1_K6 = NA)

# Berechnung von Rank-1 Metrik
for (i in 1:nrow(data)){ # Schleife: Für jede nachfolgende Zeile...
  
  # Bestimme Hauptthema für K = 4
  
  # Wähle alle Spalten der Document-Topic-Matrix aus (ohne die erste, die nur doc_id enthält)
  column <- theta_4K[i,-1]
  
  # Bestimmung des Hauptthemas (Spalte mit dem höchsten Wert)
  maintopic <- colnames(column)[which(column == max(column))] 
  
  # Zuweisung des Hauptthemas zur entsprechenden Zeile
  data$Rank1_K4[i] <- maintopic 
  rm(column, maintopic)
  
  # Bestimme Hauptthema für K = 6
  
  # Wähle alle Spalten der Document-Topic-Matrix aus (ohne die erste, die nur doc_id enthält)
  column <- theta_6K[i,-1] 
  
  # Bestimmung des Hauptthemas (Spalte mit dem höchsten Wert)
  maintopic <- colnames(column)[which(column == max(column))]
  
  # Zuweisung des Hauptthemas zur entsprechenden Zeile
  data$Rank1_K6[i] <- maintopic 
  rm(column, maintopic)
}

# Erzeugung einer Häufigkeitstabelle für Rank-1 Themen bei K = 4
data %>%
  
  # absolute Anzahl jedes Themas
  count(Rank1_K4) %>%
  
  # Ausgabe in Prozent (perc)
  mutate(perc = prop.table(n)*100,
         perc = round(perc, 2))

# Erzeugung einer Häufigkeitstabelle für Rank-1 Themen bei K = 6
data %>%
  
  # absolute Anzahl jedes Themas
  count(Rank1_K6) %>%
  
  # Ausgabe in Prozent (perc)
  mutate(perc = prop.table(n)*100,
         perc = round(perc, 2))

#### 3. Analyse ####

#### 3.1 Einfluss unabhängiger Variablen ####

data <- data %>%
  
  # Wir entfernen alle nicht-numerische Zeichen, um "-" zu entfernen
  mutate(Year_Start = gsub("[^0-9]", "", Year),
         
         # Wir beschränken uns nur auf die ersten 4 Jahre
         Year_Start = substr(Year_Start, 1, 4),
         
         # Wir verwandeln das ganze in eine numerische Variable
         Year_Start = as.numeric(Year_Start),
         
         #Wir ersetzen fehlende Werte mit dem Mittelwert (2010)
         Year_Start = replace(Year_Start,
                              is.na(Year_Start),
                              2010))

# Ausgabe der ersten Zeilen
data %>%
  
  # Reduktion auf weniger Variablen
  select(Title, Year, Year_Start)

# Wir lassen das angepasste Modell laufen
model_6K_year <- stm(documents = dfm_stm$documents,
                     vocab = dfm_stm$vocab, 
                     K = 6,
                     prevalence = ~ Year_Start,
                     data = data)

# Wir extrahieren den Effekt
effect <- estimateEffect(formula = ~ Year_Start, 
                         stmobj = model_6K_year, 
                         metadata = data)

# Welche Themen wollen wir vergleichen?
topics_6 %>%
  select(`Topics 2`, `Topics 4`)

# Plot
plot(effect, "Year_Start", 
     method = "continuous", 
     topics = c(2,4), 
     model = model_6K_year)

#### 3.2 Visualisierung des Topic Models ####

# Wir erstellen das Objekt out, in dem alle wichtigen Infos gespeichert sind
out <- list(documents = dfm_stm$documents,
            vocab = dfm_stm$vocab,
            meta = dfm_stm$meta)

# Wir lassen die Shiny App laufen
run_stminsights()

# Wir speichern das Environment ab, um es hochzuladen
save.image("Sitzung4.RDATA")

#### Aufgabe 1 📌 ####

##### Aufgabe 1.1 (Basis) #####
# Könnt ihr testen, wie sich das Modell verändert, wenn wir mit K = 10 Serien arbeiten?

##### Aufgabe 1.2 (Fortgeschritten) #####
# Könnt ihr mittels des Datensatzes zu Horoskopen testen, ob Zwillinge und Wassermänner andere Themen in ihren Horoskopen vorhergesagt kriegen?