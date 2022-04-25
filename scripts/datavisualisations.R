
### Dataviz


# Librairies
packages = c("tidyverse", "wordcloud2", "tm","textstem", "lemon", "echarts4r", "echarts4r.assets", "extrafont", "ggraph", "igraph")
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
    # extraction des lignes avec les réponses les plus courantes
alternatives_df <- enquete_vikidia %>% 
    mutate(alternatives = str_match_all(alternatives, "wikipedia|internet|net|google|moteur de recherche|fandom|dicoado.org|ecosia|opera mini|persee|kikourou|wikimini|universalis|youtube|livres|bibliotheque|larousse|dictionnaire|encyclopedie|depen|autre")) %>% 
    unnest(alternatives) %>% #quand plusieurs matchs les valeurs sont mises dans une liste donc unnest la liste
    mutate(alternatives = str_replace_all(alternatives, "depen", "ça depend")) %>% 
    select(Séquentiel, `Lorsque vous ne trouvez pas des informations sur Vikidia, où allez-vous en priorité ?`, alternatives) 
    # extraction des autres réponses
seq_alt <- alternatives_df %>% select(1) %>% unique()
seq_all <- enquete_vikidia %>% select(1) %>% unique()
manquant <- setdiff(seq_all, seq_alt)
manquant <- left_join(manquant, enquete_vikidia, by = "Séquentiel")
manquant <- manquant %>% select(1,59)

    



                ###############
                ## PROCESS DATA
                ###############



# Import des 2 bases (complétées sur le drive)
process_annee <- read_csv("data/process_annee.csv")
process_motiv_contrib <- read_csv("data/process_motiv-contrib.csv")
process_alternatives <- read_csv("data/process_alternatives.csv")

# Jointure des bases 
enquete_vikidia <- left_join(process_annee, process_motiv_contrib[,c(1,4)], by = "Séquentiel") %>% arrange(Séquentiel)

# Suppression des trolls
enquete_vikidia <- enquete_vikidia %>% filter(Séquentiel != 43, Séquentiel != 75, Séquentiel != 317, Séquentiel != 465, Séquentiel != 479, Séquentiel != 484, Séquentiel != 498, Séquentiel != 499, Séquentiel != 524, Séquentiel != 537, Séquentiel != 594, Séquentiel != 598, Séquentiel != 613, Séquentiel != 614, Séquentiel != 621, Séquentiel != 630, Séquentiel != 676, Séquentiel != 677, Séquentiel != 717, Séquentiel != 722, Séquentiel != 780, Séquentiel != 793)

# On ordonne la variable des âges
enquete_vikidia$`Quel âge avez-vous ?` <- factor(enquete_vikidia$`Quel âge avez-vous ?`, order = TRUE, levels = c('Moins de 8 ans', 'Entre 8 et 13 ans', 'Entre 14 et 18 ans', 'Entre 19 et 25 ans', 'Entre 26 et 35 ans', 'Entre 36 et 50 ans', 'Entre 51 et 60 ans', 'Plus de 60 ans', 'Je préfère ne pas le dire'))

# On ordonne la variable de l'activité
enquete_vikidia$activite <- factor(enquete_vikidia$activite, order = TRUE, levels = c('Élève en école primaire', 'Élève au collège', 'Élève au lycée', 'Étudiant', 'Étudiant, élève', "En recherche d'emploi", 'En activité', 'Retraité'))

# On remplace les valeurs prises par la variable du genre
enquete_vikidia <- enquete_vikidia %>% mutate(`Êtes vous ...` = str_replace_all(`Êtes vous ...`, c("Un homme" = "masculin",
                                                                                                   "Une femme" = "féminin",
                                                                                                   "Je préfère ne pas le dire" = "préfère ne pas le dire")))

# Formats
enquete_vikidia <- enquete_vikidia %>% mutate(is_contrib = as.factor(is_contrib))


        ### VISUALISATIONS DES DONNÉES


#remotes::install_version("Rttf2pt1", version = "1.3.8")
#font_import()



# -------------- Qui sont les vikidiens ?


### 1)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(`Êtes vous ...`) %>% summarise(Freq = n()) %>% rename(Genre = `Êtes vous ...`)

  # Paramètres graphiques
table$fraction <- table$Freq / sum(table$Freq)  #percentages
table$ymax <- cumsum(table$fraction)  # cumulative percentages (top of each rectangle)
table$ymin <- c(0, head(table$ymax, n=-1))  #bottom of each rectangle
table$labelPosition <- (table$ymax + table$ymin) / 2  #label position

  # Plot
ggplot(table, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.7, fill = Genre)) +
  geom_rect() +
  geom_text(x=4.7, aes(y=labelPosition, label = Freq, color = Genre), size=7) +
  geom_text(aes(x = 0, y = 0, label = sum(Freq)), col = "#333333", alpha=0.8, size=15, fontface="bold", inherit.aes = FALSE) +
  scale_fill_manual(values = c('#d40356','#3a25ff','#82888d')) +
  scale_color_manual(values = c('#d40356','#3a25ff','#82888d')) +
  coord_polar(theta="y") +
  ggtitle("Genre des répondants de l'enquête") +
  xlim(c(0, 4)) +   
  theme_void() +
  theme(legend.position = "right",
        text = element_text(family = "Monsterrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))


### 2)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% filter(`Êtes vous ...` != "préfère ne pas le dire") %>% group_by(`Quel âge avez-vous ?`, `Êtes vous ...`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% 
  mutate(percent = round(Freq / sum(Freq) *100, 0)) %>% 
  rename(Sexe = `Êtes vous ...`)

  # Plot
table %>% #filter(percent != 0) %>% 
  ggplot(mapping = aes(x = ifelse(test = Sexe == "masculin", yes = -Freq, no = Freq), 
                     y = `Quel âge avez-vous ?`, fill = Sexe)) +
  geom_col(col = "white") +
  scale_x_symmetric(labels = abs) +
  scale_colour_manual(values = c('#d40356', '#3a25ff'),
                      aesthetics = c("colour", "fill")) +
  labs(x = "Nombre de répondants", y = "", title = "Nombre de répondants selon l'âge et le genre") +
  theme_classic() + theme(legend.position = "right",
                          text = element_text(family = "Monsterrat", size = 12),
                          plot.title = element_text(face = "bold", size = 21),
                          axis.title.y = element_blank(),
                          axis.line.y = element_blank(),
                          axis.ticks.y = element_blank())


### 3)



  # Table de fréquences des réponses
table <- as.data.frame(table(enquete_vikidia$activite)) %>% arrange(Freq)

  # Plot
table %>% 
  e_charts(Var1) %>% 
  e_pictorial(Freq, symbol = ea_icons("user"), 
              symbolRepeat = TRUE, z = -1,
              symbolSize = c(20, 20)) %>% 
  e_theme("westeros") %>%
  e_theme_custom('{"color":["#3a25ff"]}') %>% 
  e_title(text = "Nombre de répondants selon l'activité") %>% 
  e_flip_coords() %>%
  # Hide Legend
  e_legend(show = FALSE) %>%
  # Remove Gridlines
  e_x_axis(splitLine=list(show = FALSE)) %>%
  e_y_axis(splitLine=list(show = FALSE)) %>%
  # Format Label
  e_labels(fontSize = 12, fontWeight ='bold', position = "right", offset=c(10, 0)) %>% 
  e_grid(left = "23%")


### 4)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% filter(annee > 2005 & annee < 2022) %>% group_by(annee, is_contrib) %>% 
  summarise(Freq = n()) %>% ungroup() %>% mutate(is_contrib = str_replace_all(is_contrib, c("0" = "non contributeur", "1" = "contributeur"))) %>% rename(` ` = is_contrib)

  # Plot
ggplot(table, aes(x = annee, y = Freq, group = ` `, colour = ` `)) +
  geom_line(size=1.7, alpha=0.9, linetype=1) +
  geom_point(colour="white", size = 2, pch = 21, stroke = 1.5) +
  scale_color_manual(values = c("#3a25ff", "#82888d")) +
  labs(x = "Année", y = "Nombre de répondants", title = "Nombre de répondants ayant rejoint la communauté par année") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))
  


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
    theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())








# -------------- Qui sont les contributeurs ?


# Filtre sur les contributeurs
contributeurs <- enquete_vikidia %>% filter(is_contrib == 1)



### 1)


  # Table de fréquences des réponses
table <- contributeurs %>% group_by(`Êtes vous ...`) %>% summarise(Freq = n()) %>% rename(Genre = `Êtes vous ...`)

  # Paramètres graphiques
table$fraction <- table$Freq / sum(table$Freq)  #percentages
table$ymax <- cumsum(table$fraction)  # cumulative percentages (top of each rectangle)
table$ymin <- c(0, head(table$ymax, n=-1))  #bottom of each rectangle
table$labelPosition <- (table$ymax + table$ymin) / 2  #label position

  # Plot
ggplot(table, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.7, fill = Genre)) +
  geom_rect() +
  geom_text(x=4.7, aes(y=labelPosition, label = Freq, color = Genre), size=7) +
  geom_text(aes(x = 0, y = 0, label = sum(Freq)), col = "#333333", alpha=0.8, size=15, fontface="bold", inherit.aes = FALSE) +
  scale_fill_manual(values = c('#d40356','#3a25ff','#82888d')) +
  scale_color_manual(values = c('#d40356','#3a25ff','#82888d')) +
  coord_polar(theta="y") +
  ggtitle("Genre des répondants contributeurs de l'enquête") +
  xlim(c(0, 4)) +   
  theme_void() +
  theme(legend.position = "right",
        text = element_text(family = "Monsterrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))


### 2)


  # Table de fréquences des réponses
table <- contributeurs %>% filter(`Êtes vous ...` != "préfère ne pas le dire") %>% group_by(`Quel âge avez-vous ?`, `Êtes vous ...`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% 
  mutate(percent = round(Freq / sum(Freq) *100, 0)) %>% 
  rename(Sexe = `Êtes vous ...`)

  # Plot
table %>% #filter(percent != 0) %>% 
  ggplot(mapping = aes(x = ifelse(test = Sexe == "masculin", yes = -Freq, no = Freq), 
                     y = `Quel âge avez-vous ?`, fill = Sexe)) +
  geom_col(col = "white") +
  scale_x_symmetric(labels = abs) +
  scale_colour_manual(values = c('#d40356', '#3a25ff'),
                      aesthetics = c("colour", "fill")) +
  labs(x = "Nombre de répondants", y = "", title = "Nombre de répondants contributeurs selon l'âge et le genre") +
  theme_classic() + theme(legend.position = "right",
                          text = element_text(family = "Monsterrat", size = 12),
                          plot.title = element_text(face = "bold", size = 21),
                          axis.title.y = element_blank(),
                          axis.line.y = element_blank(),
                          axis.ticks.y = element_blank())


### 3)



  # Table de fréquences des réponses
table <- as.data.frame(table(contributeurs$activite)) %>% arrange(Freq) %>% filter(Freq != 0)

  # Plot
table %>% 
  e_charts(Var1) %>% 
  e_pictorial(Freq, symbol = ea_icons("user"), 
              symbolRepeat = TRUE, z = -1,
              symbolSize = c(20, 20)) %>% 
  e_theme("westeros") %>%
  e_theme_custom('{"color":["#3a25ff"]}') %>% 
  e_title(text = "Nombre de répondants contributeurs selon l'activité") %>% 
  e_flip_coords() %>%
  # Hide Legend
  e_legend(show = FALSE) %>%
  # Remove Gridlines
  e_x_axis(splitLine=list(show = FALSE)) %>%
  e_y_axis(splitLine=list(show = FALSE)) %>%
  # Format Label
  e_labels(fontSize = 12, fontWeight ='bold', position = "right", offset=c(10, 0)) %>% 
  e_grid(left = "23%")



### 4)


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
    ylab("Nombre de réponses") + xlab("") + ggtitle("Les contextes d'utilisation de Vikidia chez les contributeurs") +
    theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())


### 5)



  # Table de fréquences des réponses
d1=data.frame(from="origin", to=enquete_vikidia$is_contrib)
d2=data.frame(from = enquete_vikidia$is_contrib, to = enquete_vikidia$`Quel est votre degré de contribution ?`)
edges=rbind(d1, d2) %>% group_by(from, to) %>% mutate(value = n()) %>% distinct()
  
name <- unique(c(as.character(edges$from), as.character(edges$to)))
vertices <- data.frame(
  name=name,
  group=c( rep(NA,3) ,  rep(1, 4), rep(0, 2)),
  cluster=sample(letters[1:4], length(name), replace=T),
  value = c(0, edges$value)
)

  # Plot
mygraph <- graph_from_data_frame(edges, vertices=vertices)
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal() +
  geom_node_text(aes( label=name, filter=leaf, color=as.factor(group)) , angle=0, hjust=0, nudge_y=0.1) +
  geom_node_point(aes(filter=leaf, size=value, color=as.factor(group)), alpha=1) +
  ggtitle("Degré de contribution des répondants") +
  coord_flip() + scale_y_reverse(expand=c(0.01, 0), limits = c(NA,-2)) +
  scale_colour_manual(values = rep(c('#82888d', '#3a25ff')), aesthetics = "colour") +
  theme(legend.position="none",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))






# -------------- Pourquoi contribuer ?


### 1)


  # préparation des données
data_wordcloud <- glimpse(contributeurs)
corpus = Corpus(VectorSource(data_wordcloud$motivations_contrib))
  # mise en forme des mots
corpus = tm_map(corpus, PlainTextDocument) #Conversion to Lowercase
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation) #Removing Punctuation
  # retrait des mots non désirés (pronoms, auxiliaires etc.)
corpus = tm_map(corpus, removeWords, c("cloth", stopwords("french"))) #Remove stopwords
corpus = tm_map(corpus, stripWhitespace) # Eliminate white spaces
corpus[[1]][1] 
  # bon format du df
DTM <- TermDocumentMatrix(corpus)
mat <- as.matrix(DTM)
f <- sort(rowSums(mat),decreasing=TRUE) 
word_data <- data.frame(word = names(f),freq=f)
    # graphique pour voir
wordcloud2(data=word_data, size = 1, minRotation = 0, maxRotation = 0, rotateRatio = 1, color=rep_len( c("#82888d","#3a25ff","#d40356"), nrow(word_data)))




### 2)


  # Table de fréquences des réponses
table <- contributeurs %>% select(30:34) %>% summarise_all(funs(sum(!is.na(.)))) %>% 
  t() %>% as.data.frame() %>% rownames_to_column()
table$rowname <- table$rowname %>% str_replace_all(c("Autre...34" = "Autre"))

  # Plot
table %>% mutate(rowname = fct_reorder(rowname, V1)) %>% 
  ggplot(aes(x=rowname, y=V1))+
    geom_bar(stat="identity", fill="#3a25ff", width=.6) +
    coord_flip() +
    ylab("Nombre de réponses") + xlab("") + ggtitle("Les raisons des multiples contributions") +
    theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())



 
# -------------- Quels sont les types de contribution ?


  # Table de fréquences des réponses
pat <- contributeurs %>% filter(!is.na(`Patrouilleur`)) %>% select(36:42) %>% summarise_all(funs(sum(!is.na(.)))) %>% rename(Autre = `Autre...42`) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(outil = "Patrouilleur")
adm <- contributeurs %>% filter(!is.na(`Administrateur`)) %>% select(36:42) %>% summarise_all(funs(sum(!is.na(.)))) %>% rename(Autre = `Autre...42`) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(outil = "Administrateur")
bur <- contributeurs %>% filter(!is.na(`Bureaucrate`)) %>% select(36:42) %>% summarise_all(funs(sum(!is.na(.)))) %>% rename(Autre = `Autre...42`) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(outil = "Bureaucrate")
ver <- contributeurs %>% filter(!is.na(`Vérificateur d'IP`)) %>% select(36:42) %>% summarise_all(funs(sum(!is.na(.)))) %>% rename(Autre = `Autre...42`) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(outil = "Vérificateur d'IP")
nuk <- contributeurs %>% filter(!is.na(`Nuker`)) %>% select(36:42) %>% summarise_all(funs(sum(!is.na(.)))) %>% rename(Autre = `Autre...42`) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(outil = "Nuker")
plu <- contributeurs %>% filter(!is.na(`Plus maintenant`)) %>% select(36:42) %>% summarise_all(funs(sum(!is.na(.)))) %>% rename(Autre = `Autre...42`) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(outil = "Plus maintenant")
    # merge
table <- rbind(pat, adm, bur, ver, nuk, plu) %>% rename(`Types de contributions` = rowname)

  # Plot
ggplot(table, aes(x = outil, y = V1)) +
  geom_col(aes(color = `Types de contributions`, fill = `Types de contributions`), position = position_dodge(0.8), width = 0.7) +
  scale_color_manual(values = c("#82888d","#82888d","#82888d","#82888d","#82888d","#82888d","#82888d"))+
  scale_fill_manual(values = c("#3a25ff", "#eeeaff", "#d40356", "#fdf3f8", "#82888d", "#f6f6f6", "#fffd33")) +
  labs(x = "Outils", y = "Nombre de réponses", "Nombre d'utilisations", title = "Contributions apportées à Vikidia selon les outils utilisés") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))




# -------------- Comment arrive-t-on dans la communauté ?


# -------------- Pourquoi arrive-t-on dans la communauté ?


# -------------- Pourquoi reste-t-on dans la communauté ?


# -------------- Quelles sont les alternatives à Vikidia ?


# -------------- Ce qui questionne


# -------------- Comment se sent-on au sein de la communauté ?


# -------------- Quels outils pour animer la communauté ?


# -------------- Quelle confiance est accordée à Vikidia ?


