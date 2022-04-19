
### Dataviz


# Librairies
packages = c("tidyverse", "wordcloud2", "tm","textstem", "lemon", "echarts4r", "echarts4r.assets")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE) #remotes::install_github("JohnCoene/echarts4r.assets")
      library(x, character.only = TRUE)}})




                ###########
                ## RAW DATA
                ###########




# Import raw data
enquete_vikidia <- read_delim("data/raw_data.tsv", 
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
    mutate(alternatives = str_match_all(alternatives, "wikipedia|internet|net|google|moteur de recherche|fandom|dicoado.org|ecosia|opera mini|persee|kikourou|wikimini|universalis|youtube|livres|bibliotheque|larousse|dictionnaire|encyclopedie|depen|autre")) %>% 
    unnest(alternatives) %>% #quand plusieurs matchs les valeurs sont mises dans une liste donc unnest la liste
    mutate(alternatives = str_replace_all(alternatives, "depen", "ça depend")) %>% 
    select(Séquentiel, `Lorsque vous ne trouvez pas des informations sur Vikidia, où allez-vous en priorité ?`, alternatives) # harmonisation des réponses 

# ecosia, opera : moteur de recherche
# treemap par groupe (ressources numériques, ressources physiques, proches), puis détail :
  #- num : internet, moteur de recherche, wikipédia
  #- papier : livre, larousse, bibliothèque
  #- proches : mamie, parents, amis etc.
# regroupement : "larousse" = "encyclopedie", "net" = "internet", "interinternet" = "internet", "bibliotheque" = "livres"
    
    



                ###############
                ## PROCESS DATA
                ###############



# Import des 2 bases (complétées sur le drive)
process_annee <- read_csv("data/process_annee.csv")
process_motiv_contrib <- read_csv("data/process_motiv-contrib.csv")

# Jointure des bases 
enquete_vikidia <- left_join(process_annee, process_motiv_contrib[,c(1,4)], by = "Séquentiel")

# On ordonne la variable des âges
enquete_vikidia$`Quel âge avez-vous ?` <- factor(enquete_vikidia$`Quel âge avez-vous ?`, order = TRUE, levels = c('Moins de 8 ans', 'Entre 8 et 13 ans', 'Entre 14 et 18 ans', 'Entre 19 et 25 ans', 'Entre 26 et 35 ans', 'Entre 36 et 50 ans', 'Entre 51 et 60 ans', 'Plus de 60 ans', 'Je préfère ne pas le dire'))

# On ordonne la variable de l'activité
enquete_vikidia$activite <- factor(enquete_vikidia$activite, order = TRUE, levels = c('Élève en école primaire', 'Élève au collège', 'Élève au lycée', 'Étudiant', 'Étudiant, élève', "En recherche d'emploi", 'En activité', 'Retraité'))



        ### VISUALISATIONS DES DONNÉES


# -------------- Qui sont les vikidiens ?


### 1)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(`Êtes vous ...`) %>% summarise(Freq = n()) %>% rename(Sexe = `Êtes vous ...`)

  # Paramètres graphiques
table$fraction <- table$Freq / sum(table$Freq)  #percentages
table$ymax <- cumsum(table$fraction)  # cumulative percentages (top of each rectangle)
table$ymin <- c(0, head(table$ymax, n=-1))  #bottom of each rectangle
table$labelPosition <- (table$ymax + table$ymin) / 2  #label position

  # Plot
ggplot(table, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.7, fill = Sexe)) +
  geom_rect() +
  geom_text(x=4.7, aes(y=labelPosition, label = Freq, color = Sexe), size=7) +
  geom_text(aes(x = 0, y = 0, label = sum(Freq)), col = "#333333", alpha=0.8, size=17, fontface="bold", inherit.aes = FALSE) +
  scale_fill_manual(values = c('#82888d','#3a25ff','#d40356')) +
  scale_color_manual(values = c('#82888d','#3a25ff','#d40356')) +
  coord_polar(theta="y") +
  ggtitle("Sexe des répondants de l'enquête") +
  xlim(c(0, 4)) +   
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", hjust = 0.2, size = 18))


### 2)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% filter(`Êtes vous ...` != "Je préfère ne pas le dire") %>% group_by(`Quel âge avez-vous ?`, `Êtes vous ...`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% 
  mutate(percent = round(Freq / sum(Freq) *100, 0)) %>% 
  rename(Sexe = `Êtes vous ...`)

  # Plot
table %>% #filter(percent != 0) %>% 
  ggplot(mapping = aes(x = ifelse(test = Sexe == "Un homme", yes = -Freq, no = Freq), 
                     y = `Quel âge avez-vous ?`, fill = Sexe)) +
  geom_col() +
  scale_x_symmetric(labels = abs) +
  scale_colour_manual(values = c('#d40356', '#3a25ff'),
                      aesthetics = c("colour", "fill")) +
  labs(x = "Nombre de répondants", y = "", title = "Nombre de répondants selon l'âge et le sexe") +
  theme_classic() + theme(plot.title = element_text(size = 16, face = "bold"),
                          axis.title.y = element_blank(),
                          axis.line.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.y = element_text(face = "bold"),
                          axis.text.x = element_text(face = "bold"),
                          )


### 3)



  # Table de fréquences des réponses
table <- as.data.frame(table(enquete_vikidia$activite)) #%>% arrange(Freq)

  # Plot
table %>% 
  e_charts(Var1) %>% 
  e_pictorial(Freq, symbol = ea_icons("user"), 
              symbolRepeat = TRUE, z = -1,
              symbolSize = c(20, 20)) %>% 
  e_theme("westeros") %>%
  e_title(text = "Nombre de répondants selon l'activité", right = '30%') %>% 
  e_flip_coords() %>%
  # Hide Legend
  e_legend(show = FALSE) %>%
  # Remove Gridlines
  e_x_axis(splitLine=list(show = FALSE)) %>%
  e_y_axis(splitLine=list(show = FALSE)) %>%
  # Format Label
  e_labels(fontSize = 16, fontWeight ='bold', position = "right", offset=c(10, 0)) %>% 
  e_grid(left = "23%")


### 4)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% filter(annee > 2005 & annee < 2022) %>% group_by(annee) %>% 
  summarise(Freq = n()) %>% ungroup() 

  # Plot
ggplot(table, aes(x=annee, y=Freq)) +
  geom_line(color="#3a25ff", size=1.7, alpha=0.9, linetype=1) +
  geom_point(fill = "#3a25ff", colour="white", size=3.4, pch = 21, stroke = 1.8) +
  labs(x = "Année", y = "Nombre de contributeurs", title = "Nombre de répondants contributeurs ayant rejoint la communauté par année") +
  theme_classic()


### 5)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% select(21:26) %>% summarise_all(funs(sum(!is.na(.)))) %>% 
  t() %>% as.data.frame() %>% rownames_to_column()
table$rowname <- gsub("\\s*\\([^\\)]+\\)", "", as.character(table$rowname))
table$rowname <- table$rowname %>% str_replace_all(c("Autre...26" = "Autre",
                                                 "mon " = "le ",
                                                 "ma " = "la "))

  # Plot
table %>% mutate(rowname = fct_reorder(rowname, V1)) %>% 
  ggplot(aes(x=rowname, y=V1))+
    geom_bar(stat="identity", fill="#3a25ff", width=.6) +
    coord_flip() +
    ylab("Nombre de réponses") + xlab("") + ggtitle("Les contextes d'utilisation de Vikidia") +
    theme_bw()








# -------------- Qui sont les contributeurs ?


# Filtre sur les contributeurs
contributeurs <- enquete_vikidia %>% filter(is_contrib == 1)


### 1)


  # Table de fréquences des réponses
table <- contributeurs %>% group_by(`Êtes vous ...`) %>% summarise(Freq = n()) %>% rename(Sexe = `Êtes vous ...`)

  # Paramètres graphiques
table$fraction <- table$Freq / sum(table$Freq)  #percentages
table$ymax <- cumsum(table$fraction)  # cumulative percentages (top of each rectangle)
table$ymin <- c(0, head(table$ymax, n=-1))  #bottom of each rectangle
table$labelPosition <- (table$ymax + table$ymin) / 2  #label position

  # Plot
ggplot(table, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.7, fill = Sexe)) +
  geom_rect() +
  geom_text(x=4.7, aes(y=labelPosition, label = Freq, color = Sexe), size=7) +
  geom_text(aes(x = 0, y = 0, label = sum(Freq)), col = "#333333", alpha=0.8, size=17, fontface="bold", inherit.aes = FALSE) +
  scale_fill_manual(values = c('#82888d','#3a25ff','#d40356')) +
  scale_color_manual(values = c('#82888d','#3a25ff','#d40356')) +
  coord_polar(theta="y") +
  ggtitle("Sexe des répondants contributeurs de l'enquête") +
  xlim(c(0, 4)) +   
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", hjust = 0.2, size = 18))


### 2)


  # Table de fréquences des réponses
table <- contributeurs %>% filter(`Êtes vous ...` != "Je préfère ne pas le dire") %>% group_by(`Quel âge avez-vous ?`, `Êtes vous ...`) %>% 
  dplyr::summarise(Freq = n()) %>% ungroup() %>% 
  mutate(percent = round(Freq / sum(Freq) *100, 0)) %>% 
  dplyr::rename(Sexe = `Êtes vous ...`)

  # Plot
table %>% #filter(percent != 0) %>% 
  ggplot(mapping = aes(x = ifelse(test = Sexe == "Un homme", yes = -percent, no = percent), 
                     y = `Quel âge avez-vous ?`, fill = Sexe)) +
  geom_col() +
  scale_x_symmetric(labels = abs) +
  scale_colour_manual(values = c('#d40356', '#3a25ff'),
                      aesthetics = c("colour", "fill")) +
  labs(x = "Nombre de contributeurs", y = "", title = "Nombre de contributeurs selon l'âge et le sexe") +
  theme_classic() + theme(plot.title = element_text(size = 16, face = "bold"),
                          axis.title.y = element_blank(),
                          axis.line.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.y = element_text(face = "bold"),
                          axis.text.x = element_text(face = "bold"),
                          )


### 3)


  # Table de fréquences des réponses
table <- as.data.frame(table(contributeurs$activite)) %>% arrange(Freq)

  # Plot
table %>% 
  e_charts(Var1) %>% 
  e_pictorial(Freq, symbol = ea_icons("user"), 
              symbolRepeat = TRUE, z = -1,
              symbolSize = c(20, 20)) %>% 
  e_theme("westeros") %>%
  e_title(text = "Nombre de répondants selon l'activité", right = '30%') %>% 
  e_flip_coords() %>%
  # Hide Legend
  e_legend(show = FALSE) %>%
  # Remove Gridlines
  e_x_axis(splitLine=list(show = FALSE)) %>%
  e_y_axis(splitLine=list(show = FALSE)) %>%
  # Format Label
  e_labels(fontSize = 16, fontWeight ='bold', position = "right", offset=c(10, 0)) %>% 
  e_grid(left = "23%")


### 4)


  # Table de fréquences des réponses
table <- contributeurs %>% filter(annee > 2005 & annee < 2022, is_contrib == 1) %>% group_by(annee) %>% 
  dplyr::summarise(Freq = n()) %>% ungroup() 

  # Plot
ggplot(table, aes(x=annee, y=Freq)) +
  geom_line(color="#3a25ff", size=1.7, alpha=0.9, linetype=1) +
  geom_point(fill = "#3a25ff", colour="white", size=3.4, pch = 21, stroke = 1.8) +
  labs(x = "Année", y = "Nombre de contributeurs", title = "Nombre de répondants contributeurs ayant rejoint la communauté par année") +
  theme_classic()
# mettre is_contrib==1 et is_contrib==0 sur c graphique, 2 lignes


### 5)


  # Table de fréquences des réponses
table <- contributeurs %>% select(21:26) %>% summarise_all(funs(sum(!is.na(.)))) %>% 
  t() %>% as.data.frame() %>% rownames_to_column()
table$rowname <- gsub("\\s*\\([^\\)]+\\)", "", as.character(table$rowname))
table$rowname <- table$rowname %>% str_replace_all(c("Autre...26" = "Autre",
                                                 "mon " = "le ",
                                                 "ma " = "la "))

  # Plot
table %>% mutate(rowname = fct_reorder(rowname, V1)) %>% 
  ggplot(aes(x=rowname, y=V1))+
    geom_bar(stat="identity", fill="#3a25ff", width=.6) +
    coord_flip() +
    ylab("Nombre de réponses") + xlab("") + ggtitle("Les contextes d'utilisation de Vikidia") +
    theme_bw()





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


