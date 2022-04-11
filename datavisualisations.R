### Dataviz


# Librairies
packages = c("tidyverse", "wordcloud2", "tm","textstem")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)}})


# Import
enquete_vikidia <- read_delim("enquete_sur_la_communaute_vikidienne.tsv", 
    delim = "\t", escape_double = FALSE, 
    trim_ws = TRUE, skip = 2)

# Format des variables
str(enquete_vikidia)

# Suppression de doublons
enquete_vikidia <- enquete_vikidia %>% filter(Séquentiel != 474, Séquentiel != 475)




        ### TRAITEMENT DES VARIABLES


# CRÉATION : Le répondant est-il contributeur ?
enquete_vikidia <- enquete_vikidia %>% mutate(is_contrib = case_when(`Quel est votre degré de contribution ?` == "J'ai déjà contribué une fois ou deux" | 
                                                                 `Quel est votre degré de contribution ?` == "Je contribue une fois par semaine ou plus" | 
                                                                 `Quel est votre degré de contribution ?` == "Je contribue plusieurs fois par mois" | 
                                                                 `Quel est votre degré de contribution ?` == "Je contribue environ une fois par mois, ou moins" ~ 1,
                                                                 TRUE ~ 0))

# AGRÉGATION : activité + niveau d'étude
enquete_vikidia <- enquete_vikidia %>% mutate(activite = case_when(`Vous êtes ...` == "Étudiant, élève" & `Veuillez préciser votre niveau d'étude` == "Collège" ~ "Élève au collège",
                                                                   `Vous êtes ...` == "Étudiant, élève" & `Veuillez préciser votre niveau d'étude` == "École primaire (du CP au CM2)" ~ "Élève en école primaire",
                                                                   `Vous êtes ...` == "Étudiant, élève" & `Veuillez préciser votre niveau d'étude` == "Études supérieures" ~ "Étudiant",
                                                                   `Vous êtes ...` == "Étudiant, élève" & `Veuillez préciser votre niveau d'étude` == "Lycée" ~ "Élève au lycée",
                                                                   TRUE ~ `Vous êtes ...`))

# EXTRACTION : champ libre 'année'
annee <- as.data.frame(table(enquete_vikidia$`En quelle année avez-vous rejoint la communauté Vikidienne ?`))
enquete_vikidia <- enquete_vikidia %>% mutate(annee = str_extract(`En quelle année avez-vous rejoint la communauté Vikidienne ?`, "(1|2)\\d{3}"))


# LEMMATISATION / HARMONISATION : champ libre 'motivation à contribuer'
motivations_contrib <- as.data.frame(table(enquete_vikidia$`Qu'est ce qui vous a motivé à contribuer pour la première fois ?`))
enquete_vikidia <- enquete_vikidia %>% mutate(motivations_contrib = NA) #à compléter sur le drive


# LEMMATISATION / HARMONISATION : champ libre 'alternatives à Vikidia'
alternatives <- as.data.frame(table(enquete_vikidia$`Lorsque vous ne trouvez pas des informations sur Vikidia, où allez-vous en priorité ?`))
    # réponses sans accent et en minuscule
enquete_vikidia <- enquete_vikidia %>% mutate(alternatives = tolower(`Lorsque vous ne trouvez pas des informations sur Vikidia, où allez-vous en priorité ?`))
enquete_vikidia <- data.table::data.table(enquete_vikidia)
enquete_vikidia[, alternatives := stringi::stri_trans_general (str = alternatives, id = "Latin-ASCII")]
    # extraction des mots-clés
alternatives_df <- enquete_vikidia %>% 
    mutate(alternatives = str_match_all(alternatives, "wikipedia|internet|net|bet|google|moteur de recherche|fandom|dicoado.org|ecosia|opera mini|persee|kikourou|wikimini|universalis|youtube|livres|bibliotheque|larousse|dictionnaire|encyclopedie|depen|autre")) %>% 
    unnest(alternatives) %>% #quand plusieurs matchs les valeurs sont mises dans une liste donc unnest la liste
    mutate(alternatives = str_replace_all(alternatives, c("depen" = "ça depend",
                                        "larousse" = "encyclopedie",
                                        "net" = "internet", "interinternet" = "internet",
                                        "bibliotheque" = "livres")))  # harmonisation des réponses
    
    






        ### VISUALISATIONS DES DONNÉES


# -------------- Qui sont les vikidiens ?


# -------------- Qui sont les contributeurs ?


# -------------- Pourquoi contribuer ?


## --- Wordcloud 

  # préparation des données
data_wordcloud <- glimpse(enquete_vikidia)
corpus = Corpus(VectorSource(data_wordcloud$`Qu'est ce qui vous a motivé à contribuer pour la première fois ?`))
  # mise en forme des mots
corpus = tm_map(corpus, PlainTextDocument) #Conversion to Lowercase
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation) #Removing Punctuation
  # retrait des mots non désirés (pronoms, auxiliaires etc.)
#ajout_stopwords <- c("le", "et", "je", "jai", ",", "ça", "cest", "quand", "cul", "lol", "'")
remix_stopwords <- c(ajout_stopwords, stopwords("french"))  # adding own undesired words to stopwords
corpus = tm_map(corpus, removeWords, c("cloth", remix_stopwords)) #Remove stopwords
corpus = tm_map(corpus, stripWhitespace) # Eliminate white spaces
corpus[[1]][1] 
  # bon format du df
DTM <- TermDocumentMatrix(corpus)
mat <- as.matrix(DTM)
f <- sort(rowSums(mat),decreasing=TRUE) 
word_data <- data.frame(word = names(f),freq=f)
    # graphique pour voir
wordcloud2(data=word_data, size=1.9, col="grey", minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)




# -------------- Quels sont les types de contribution ?


# -------------- Comment arrive-t-on dans la communauté ?


# -------------- Pourquoi arrive-t-on dans la communauté ?


# -------------- Pourquoi reste-t-on dans la communauté ?


# -------------- Quelles sont les alternatives à Vikidia ?


# -------------- Ce qui questionne


# -------------- Comment se sent-on au sein de la communauté ?


# -------------- Quels outils pour animer la communauté ?


# -------------- Quelle confiance est accordée à Vikidia ?


