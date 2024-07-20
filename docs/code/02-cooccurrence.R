#### 1. Pakete laden und Daten einlesen ####
  
#install.packages("tidyverse)
#install.packages("quanteda")
#install.packages("quanteda.textplots")
#install.packages("RCurl")
#install.packages("quanteda.textstats")
#install.packages("udpipe")

library("tidyverse")
library("quanteda")
library("quanteda.textplots")
library("RCurl")
library("quanteda.textstats")
library("udpipe")

#Plus neues rsyntax Package
install.packages("rsyntax")
library("rsyntax")

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
  
  #Relative pruning
  dfm_trim( min_docfreq = 0.005, 
            max_docfreq = 0.99, 
            docfreq_type = "prop", 
            verbose = TRUE) 

#### 2. Co-Occurrence Analysen ####

##### 2.1 N-grams identifizieren #####
tokens %>%
  
  # Umwandlung in bigrams
  tokens_ngrams(n = 2) %>%
  
  # Ausgabe für erstes Dokument
  head(1)

# Top Features: Welche ngrams sind häufig?
tokens %>%
  
  # Umwandlung in bigrams
  tokens_ngrams(n = 2) %>%
  
  # Umwandlung in dfm für topfeatures-Befehl
  dfm() %>%
  
  # Ausgabe der häufigsten Features
  topfeatures(10) %>%
  
  # Umwandlung in einen "schöneren" Dataframe mit der Spalte "Häufigkeit"
  as.data.frame() %>%
  rename("Häufigkeit" = '.')

# Ngrams zu einem Feature zusammenfassen
# Definition häufiger Ngrams auf Basis der vorherigen Ausgabe
ngrams <- c("los angel","new york citi", "serial killer", "high school", "best friend")

# Text-as-Data Repräsentation als Document-Feature-Matrix
dfm <- tokens %>% 
  
  # Zusätzlicher Schritt, um Ngrams als einzelnes Feature einzulesen
  tokens_compound(pattern = phrase(ngrams)) %>%
  
  # reguläre DFM, inkl. Relative Pruning
  dfm() %>% 
  dfm_trim( min_docfreq = 0.005, 
            max_docfreq = 0.99, 
            docfreq_type = "prop", 
            verbose = TRUE) 

# Beispiel: Wie wird das Feature "Los Angeles" eingelesen?
dfm %>%
  
  # Umwandlung zu Data-Frame
  convert(to = "data.frame") %>%
  
  # Reduktion auf Doc ID und Features, die mit "los" beginnen
  select(doc_id, starts_with("los")) %>%
  
  # Ausgabe ausgewählter Serien (Zeile 125 bis 130)
  slice(125:130)

##### 2.2 Keywords-in-Context (KWIC) #####
data$Description %>% 
  
  # Keywords-in-Context mit Window von 1 Wort vor und nach Schlüsselwort
  kwic(pattern = "hero", 
       window = 1) %>%
  
  # Ausgabe der ersten Zeilen
  head()

##### 2.3 Collocations #####
tokens %>%
  
  # Identifikation von Collocations, die mind. 10 Mal vorkommen
  textstat_collocations(min_count = 10) %>%
  
  # Sortierung nach lambda: Je grösser, 
  # desto wahrscheinlicher handelt es sich um nicht-zufällige Collocations
  arrange(-lambda) %>%
  
  # Ausgabe der häufigsten 10 Collocation
  head(10)

##### 2.4 Semantische Netzwerke auf Basis von Co-Occurrenzen #####
tokens %>%
  
  # Umwandlung in eine Feature-Co-Occurrence-Matrix
  fcm(context = "document") %>%
  
  # Ausgabe der ersten Zeilen
  head()

# Analyse von Geschlechterstereotypen
fcm <- tokens %>%
  
  # Erstellung einer FCM mit einem Window von 8
  fcm(window = 8) %>%
  
  # Reduktion auf ausgewähler Features
  fcm_select(pattern = c("fight", "man", 
                         "love", "young", "woman"), 
             selection = "keep")

# Plot des semantischen Netzwerks
textplot_network(fcm)

#### 3. Part-of-Speech Tagging ####
data_pos_tagged <- data$Description %>%
  
  # Format für das udpipe Paket anpassen
  as_tibble() %>%
  mutate(doc_id = paste0("text", 1:n())) %>% 
  rename(text = value) %>%
  
  # Part-of-speech tagging
  udpipe("english") %>% 
  
  # Wir reduzieren die Ausgabe auf relevante Variablen (z.B. Text-ID, Tag)
  select(doc_id, sentence_id, token_id, token, lemma, upos, head_token_id)

# Wir schauen uns die Ausgabe an
head(data_pos_tagged)

# Analyse von Adjektiven mit Bezug zu Family
data_pos_tagged %>%
  
  # Wir filtern den Datensatz nach dem Substantiv "Family"
  filter(upos == "NOUN" & lemma == "family") %>%
  
  # Für alle gefundenen Fälle suchen wir die zugehörigen Sätze im "vollen" Datensatz
  # Das Matching geschieht via doc_id (ID des Dokuments) und sentence_ic (ID des Satzes im Dokument)
  inner_join(data_pos_tagged, by = c("doc_id", "sentence_id")) %>%
  
  # Wir behalten mit filter nur Adjektive, die sich auf Familie beziehen
  # Nämlich solche, die bei "head_token" die "token_id" des Features "Family" haben
  filter(upos.y == "ADJ" & head_token_id.y == token_id.x) %>%
  
  # Wir benennen manche Variablen um, damit das Ganze besser verständlich ist
  rename(token_id = token_id.y,
         token = token.y) %>%
  
  # Wir wählen nur relevante Variablen aus
  select(doc_id, sentence_id, token_id, token) %>%
  
  # erste Zeilen ausgeben
  head()

#### 4. Dependency Parsing ####
data$Description %>%
  
  # Format für das udpipe Paket anpassen
  as_tibble() %>%
  mutate(doc_id = paste0("text", 1:n())) %>% 
  rename(text = value) %>%
  
  # Der Einfachheit halber machen wir diese Analyse nur für einen Text
  slice(1) %>%
  
  # dependency parsing
  udpipe("english") %>% 
  
  # relevanten Variablen auswählen
  select(doc_id, sentence_id, token_id, token, head_token_id, dep_rel) %>%
  
  # erste Zeilen ausgeben
  head(5)

# Visualisierung mit Rsyntax: #Beispielsatz in udpipe
udpipe("My only goal in life is to understand dependency parsing", "english") %>%
  
  # Umwandlung in Format für rsyntax-Paket
  as_tokenindex() %>%
  
  # Visualisierung
  plot_tree(., token, lemma, upos)

#### Aufgabe 1 📌 ####

##### Aufgabe 1.1 #####
# Lest den Horoskop-Datensatz ein und verschafft euch einen Überblick über die Daten. Welche Variablen sind dort vorhanden?

##### Aufgabe 1.2 #####
# Bereitet den Datensatz durch Preprocessing und das Umwandeln in eine DFM für die Analyse vor. Hinterfragt kritisch, welche Bereinigung- und Normalisierungsschritte ihr tatsächlich braucht.

##### Aufgabe 1.3 #####
#S chaut euch als erste Analyse an, welcher Ausdrück häufiger vorkommt: "**secret fear**" oder "**in love**"?

##### Aufgabe 1.4 #####
# Jetzt wollen wir wissen, bei welchem Sternzeichen es am mysteriösesten wird: Bei welchem Sternzeichen fällt am häufigsten das Stichwort "**secret**"?

##### Aufgabe 1.5 #####
# Visualisiert auf Basis eines semantischen Netzwerk, mit welchen Adjektiven die Sternzeichen 
# "Aquarius" (Wassermann) vs. "Gemini" (Zwilling) häufig in den Horoskopen assoziiert werden.