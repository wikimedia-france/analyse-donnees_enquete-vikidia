### Exploration des données


# Librairies
library(tidyverse)
library(summarytools)


# Import
enquete_vikidia <- read_delim("~/Downloads/enquete_sur_la_communaute_vikidienne.tsv", 
    delim = "\t", escape_double = FALSE, 
    trim_ws = TRUE, skip = 2)


# Noms des colonnes
names <- as.data.frame(names(enquete_vikidia))

# Stats générales
View(dfSummary(enquete_vikidia))
