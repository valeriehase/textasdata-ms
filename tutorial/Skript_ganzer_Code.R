# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 
# Skript zum Workshop Automatisierte Inhaltsanalyse       #
#
# Methodenworkshop an der Universität Münster, July 2024  #
#
# Valerie Hase & Luisa Kutlar                             #
#
# # # # # # # # # #  # # # # # # # #  # # # # # # # # # # #

##### 1 Packete laden und Daten einlesen ----
# Packete installieren
#install.packages("RCurl")
#install.packages("quanteda")
#install.packages("tidyverse)
#install.packages("dplyr")
#install.packages("quanteda.textplots")
#install.packages("quanteda.textstats")
#install.packages("udpipe")

# Packete laden
library("RCurl")
library("quanteda")
library("tidyverse")
library("dplyr")
library("quanteda.textplots")
library("quanteda.textstats")
library("udpipe")

#Daten einlesen
url <-  getURL("https://raw.githubusercontent.com/valeriehase/Salamanca-CSS-SummerSchool/main/Processing%20text%20and%20text%20as%20data/data_tvseries.csv")
daten_df <-  read.csv2(text = url)

head(daten_df)
str(daten_df)
View(daten_df)

#### 2 Preprocessing ----
#### 2.1 Encoding Issues checken ------



