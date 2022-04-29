
### Dataviz


# Librairies
packages = c("tidyverse", "wordcloud2", "tm","textstem", "lemon", "echarts4r", "echarts4r.assets", "extrafont", "ggraph", "igraph", "flexdashboard", "waffle", "hrbrthemes", "ggpubr", "fmsb", "glue", "treemap")
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
process_alternatives <- read_csv("data/process_alternatives.csv") %>% select(1,4,6)

# Jointure des bases 
enquete_vikidia <- left_join(process_annee, process_motiv_contrib[,c(1,4)], by = "Séquentiel") %>% arrange(Séquentiel)



    ### Nettoyage



# Suppression des trolls
enquete_vikidia <- enquete_vikidia %>% filter(Séquentiel != 43, Séquentiel != 75, Séquentiel != 317, Séquentiel != 465, Séquentiel != 479, Séquentiel != 484, Séquentiel != 498, Séquentiel != 499, Séquentiel != 524, Séquentiel != 537, Séquentiel != 594, Séquentiel != 598, Séquentiel != 613, Séquentiel != 614, Séquentiel != 621, Séquentiel != 630, Séquentiel != 676, Séquentiel != 677, Séquentiel != 717, Séquentiel != 722, Séquentiel != 780, Séquentiel != 793)
process_alternatives <- process_alternatives %>% filter(Séquentiel != 43, Séquentiel != 75, Séquentiel != 317, Séquentiel != 465, Séquentiel != 479, Séquentiel != 484, Séquentiel != 498, Séquentiel != 499, Séquentiel != 524, Séquentiel != 537, Séquentiel != 594, Séquentiel != 598, Séquentiel != 613, Séquentiel != 614, Séquentiel != 621, Séquentiel != 630, Séquentiel != 676, Séquentiel != 677, Séquentiel != 717, Séquentiel != 722, Séquentiel != 780, Séquentiel != 793)


# On attribue une activité aux 3 répondants 'Étudiant, élève' n'ayant pas précisé leur niveau d'étude
enquete_vikidia[92, 96] <- "Étudiant"
enquete_vikidia[254, 96] <- "Élève au collège"
enquete_vikidia[419, 96] <- "Élève au lycée"

# On "neutralise" les réponses
  # genre
enquete_vikidia <- enquete_vikidia %>% mutate(`Quel âge avez-vous ?` = str_replace_all(`Quel âge avez-vous ?`, 
                                                                                       c("Je préfère ne pas le dire" = "Préfère ne pas le dire")))
  # genre
enquete_vikidia <- enquete_vikidia %>% mutate(`Êtes vous ...` = str_replace_all(`Êtes vous ...`, c("Un homme" = "masculin",
                                                                                                   "Une femme" = "féminin",
                                                                                                   "Je préfère ne pas le dire" = "préfère ne pas le dire")))
  # degré de contribution
enquete_vikidia <- enquete_vikidia %>% mutate(`Quel est votre degré de contribution ?` = str_replace_all(`Quel est votre degré de contribution ?`, 
                                                                                                         c("C'est la première fois que je consulte Vikidia" = "Consulte Vikidia pour la première fois",
                                                                                                           "Je consulte Vikidia mais je n'ai jamais contribué" = "Consulte Vikidia mais n'y contribue pas",
                                                                                                           "J'ai déjà contribué une fois ou deux" = "A contribué une ou deux fois",
                                                                                                           "Je contribue environ une fois par mois, ou moins" = "Contribue une fois pas moins ou moins",
                                                                                                           "Je contribue plusieurs fois par mois" = "Contribue plusieurs fois par mois",
                                                                                                           "Je contribue une fois par semaine ou plus" = "Contribue une fois par semaine ou plus")))


# On ordonne les variables aux catégories ordonnées
  # âge
enquete_vikidia$`Quel âge avez-vous ?` <- factor(enquete_vikidia$`Quel âge avez-vous ?`, order = TRUE, 
                                                 levels = c('Moins de 8 ans', 'Entre 8 et 13 ans', 'Entre 14 et 18 ans', 'Entre 19 et 25 ans', 'Entre 26 et 35 ans', 'Entre 36 et 50 ans', 'Entre 51 et 60 ans', 'Plus de 60 ans', 'Préfère ne pas le dire'))
  # activité
enquete_vikidia$activite <- factor(enquete_vikidia$activite, order = TRUE, 
                                   levels = c('Élève en école primaire', 'Élève au collège', 'Élève au lycée', 'Étudiant', "En recherche d'emploi", 'En activité', 'Retraité'))
  # degré de contribution
enquete_vikidia$`Quel est votre degré de contribution ?` <- factor(enquete_vikidia$`Quel est votre degré de contribution ?`, order = TRUE, 
                                                                   levels = c("Consulte Vikidia pour la première fois", "Consulte Vikidia mais n'y contribue pas", "A contribué une ou deux fois", "Contribue une fois pas moins ou moins", "Contribue plusieurs fois par mois", "Contribue une fois par semaine ou plus"))


# Formats
enquete_vikidia <- enquete_vikidia %>% mutate(is_contrib = as.factor(is_contrib))


        ### VISUALISATIONS DES DONNÉES


#remotes::install_version("Rttf2pt1", version = "1.3.8")
#font_import()

# Fonction pour sauvegarder les graphs : format PNG pour les slides et format SVG pour les posters
saving_plot <- function(graph, name) {
  ggsave(file = glue("dataviz/SVG/{name}.svg"), plot=graph, width=12, height=8)
  ggsave(file = glue("dataviz/PNG/{name}.png"), plot=graph, width=12, height=8)
}



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
graph <- ggplot(table, aes(ymax = ymax, ymin = ymin, xmax = 3.3, xmin = 2, fill = Genre)) +
  geom_rect() +
  geom_text(x=4, aes(y=labelPosition, label = Freq), color = "#333333", size=7) +
  geom_text(aes(x = 0, y = 0, label = sum(Freq)), col = "#333333", alpha=0.8, size=14, fontface="bold", inherit.aes = FALSE) +
  scale_fill_manual(values = c('#fffd33','#3a25ff','#82888d')) +
  scale_color_manual(values = c('#fffd33','#3a25ff','#82888d')) +
  coord_polar(theta="y") +
  ggtitle("Genre des répondants de l'enquête") +
  xlim(c(0, 4)) +   
  theme_void() +
  theme(legend.position = "right",
        text = element_text(family = "Monsterrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))
graph
saving_plot(graph, "1_donut")


### 2)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% filter(`Êtes vous ...` != "préfère ne pas le dire") %>% group_by(`Quel âge avez-vous ?`, `Êtes vous ...`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% 
  mutate(percent = round(Freq / sum(Freq) *100, 0)) %>% 
  rename(Genre = `Êtes vous ...`)

  # Plot
graph <- table %>% #filter(percent != 0) %>% 
  ggplot(mapping = aes(x = ifelse(test = Genre == "masculin", yes = -Freq, no = Freq), 
                     y = `Quel âge avez-vous ?`, fill = Genre)) +
  geom_col() + #col = "black"
  scale_x_symmetric(labels = abs) +
  scale_colour_manual(values = c('#fffd33', '#3a25ff'),
                      aesthetics = c("colour", "fill")) +
  labs(x = "Nombre de répondants", y = "", title = "Nombre de répondants selon l'âge et le genre") +
  theme_classic() + theme(legend.position = "right",
                          text = element_text(family = "Monsterrat", size = 12),
                          plot.title = element_text(face = "bold", size = 21),
                          axis.title.y = element_blank(),
                          axis.line.y = element_blank(),
                          axis.ticks.y = element_blank())
graph
saving_plot(graph, "2_pyramid")



### 3)



  # Table de fréquences des réponses
table <- as.data.frame(table(enquete_vikidia$activite)) 

  # Plot
graph <- table %>% 
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
graph




### 4)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% filter(annee > 2005 & annee < 2022) %>% group_by(annee, is_contrib) %>% 
  summarise(Freq = n()) %>% ungroup() %>% mutate(is_contrib = str_replace_all(is_contrib, c("0" = "non contributeurs", "1" = "contributeurs"))) %>% rename(` ` = is_contrib)

  # Plot
graph <- ggplot(table, aes(x = annee, y = Freq, group = ` `, colour = ` `)) +
  geom_line(size=1.7, alpha=0.9, linetype=1) +
  geom_point(colour="white", size = 2, pch = 21, stroke = 1.5) +
  scale_color_manual(values = c("#3a25ff", "#82888d")) +
  labs(x = "Année", y = "Nombre de répondants", title = "Nombre de répondants ayant rejoint la communauté par année") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))
graph
saving_plot(graph, "4_timeline1")


  # Table de fréquences des réponses
table <- enquete_vikidia %>% filter(annee > 2005 & annee < 2022) %>% group_by(annee, `Vous êtes ...`) %>% 
  summarise(Freq = n()) %>% ungroup()

  # Plot
graph <- ggplot(table, aes(x = annee, y = Freq, group = `Vous êtes ...`, colour = `Vous êtes ...`)) +
  geom_line(size=1.7, alpha=0.9, linetype=1) +
  geom_point(colour="white", size = 2, pch = 21, stroke = 1.5) +
  scale_color_manual(values = c("#3a25ff", "#82888d", "#fffd33", "#48ffbc", "#f6f6f6")) +
  labs(x = "Année", y = "Nombre de répondants", title = "Nombre de répondants ayant rejoint la communauté par année") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))
graph
saving_plot(graph, "4_timeline2")


  # Table de fréquences des réponses
table <- enquete_vikidia %>% filter(annee > 2005 & annee < 2022) %>% group_by(annee, `Quel est votre degré de contribution ?`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% rename(`Degré de contribution` = `Quel est votre degré de contribution ?`)

  # Plot
graph <- ggplot(table, aes(x = annee, y = Freq, group = `Degré de contribution`, colour = `Degré de contribution`)) +
  geom_line(size=1.7, alpha=0.9, linetype=1) +
  geom_point(colour="white", size = 2, pch = 21, stroke = 1.5) +
  scale_color_manual(values = c("#3a25ff", "#82888d", "#eeeaff", "#fffd33", "#48ffbc", "#f6f6f6")) +
  labs(x = "Année", y = "Nombre de répondants", title = "Nombre de répondants ayant rejoint la communauté par année") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))
graph
saving_plot(graph, "4_timeline4")





### 5)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% select(21:26) %>% summarise_all(funs(sum(!is.na(.)))) %>% 
  t() %>% as.data.frame() %>% rownames_to_column()
table$rowname <- gsub("\\s*\\([^\\)]+\\)", "", as.character(table$rowname))
table$rowname <- table$rowname %>% str_replace_all(c("Autre...26" = "Autre",
                                                     "mon " = "le ",
                                                     "ma " = "la ",
                                                     "simple " = ""))

  # Plot
graph <- table %>% mutate(rowname = fct_reorder(rowname, V1)) %>% 
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
graph
saving_plot(graph, "5_barplot_contextes")




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
graph <- ggplot(table, aes(ymax = ymax, ymin = ymin, xmax = 3.3, xmin = 2, fill = Genre)) +
  geom_rect() +
  geom_text(x=4, aes(y=labelPosition, label = Freq), color = "#333333", size=7) +
  geom_text(aes(x = 0, y = 0, label = sum(Freq)), col = "#333333", alpha=0.8, size=14, fontface="bold", inherit.aes = FALSE) +
  scale_fill_manual(values = c('#fffd33','#3a25ff','#82888d')) +
  scale_color_manual(values = c('#fffd33','#3a25ff','#82888d')) +
  coord_polar(theta="y") +
  ggtitle("Genre des répondants contributeurs de l'enquête") +
  xlim(c(0, 4)) +   
  theme_void() +
  theme(legend.position = "right",
        text = element_text(family = "Monsterrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))
graph
saving_plot(graph, "6_donut_contrib")



### 2)


  # Table de fréquences des réponses
table <- contributeurs %>% filter(`Êtes vous ...` != "préfère ne pas le dire") %>% group_by(`Quel âge avez-vous ?`, `Êtes vous ...`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% 
  mutate(percent = round(Freq / sum(Freq) *100, 0)) %>% 
  rename(Genre = `Êtes vous ...`)

  # Plot
graph <- table %>% #filter(percent != 0) %>% 
  ggplot(mapping = aes(x = ifelse(test = Genre == "masculin", yes = -Freq, no = Freq), 
                     y = `Quel âge avez-vous ?`, fill = Genre)) +
  geom_col() + #col = "black"
  scale_x_symmetric(labels = abs) +
  scale_colour_manual(values = c('#fffd33', '#3a25ff'),
                      aesthetics = c("colour", "fill")) +
  labs(x = "Nombre de répondants contributeurs", y = "", title = "Nombre de contributeurs selon l'âge et le genre") +
  theme_classic() + theme(legend.position = "right",
                          text = element_text(family = "Monsterrat", size = 12),
                          plot.title = element_text(face = "bold", size = 21),
                          axis.title.y = element_blank(),
                          axis.line.y = element_blank(),
                          axis.ticks.y = element_blank())
graph
saving_plot(graph, "7_pyramid_contrib")



### 3)



  # Table de fréquences des réponses
table <- as.data.frame(table(contributeurs$activite)) 

  # Plot
graph <- table %>% 
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
graph



### 4)


  # Table de fréquences des réponses
table <- contributeurs %>% select(21:26) %>% summarise_all(funs(sum(!is.na(.)))) %>% 
  t() %>% as.data.frame() %>% rownames_to_column()
table$rowname <- gsub("\\s*\\([^\\)]+\\)", "", as.character(table$rowname))
table$rowname <- table$rowname %>% str_replace_all(c("Autre...26" = "Autre",
                                                     "mon " = "le ",
                                                     "ma " = "la "))

  # Plot
graph <- table %>% mutate(rowname = fct_reorder(rowname, V1)) %>% 
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
graph
saving_plot(graph, "9_barplot_contextes_contrib")



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
) %>% mutate(percent = paste(round(value / 774 * 100, 0), "% : ", name, sep = ""))

  # Plot
mygraph <- graph_from_data_frame(edges, vertices=vertices)
graph <- ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal() +
  geom_node_text(aes(label=percent, filter=leaf, color=as.factor(group)) , angle=0, hjust=0, nudge_y=0.1) +
  geom_node_point(aes(filter=leaf, size=value, color=as.factor(group)), alpha=1) +
  scale_size(range = c(2,10)) +
  ggtitle("Degré de contribution des répondants") +
  coord_flip() + scale_y_reverse(expand=c(0.01, 0), limits = c(NA,-2)) +
  scale_colour_manual(values = rep(c('#82888d', '#3a25ff')), aesthetics = "colour") +
  theme_void() +
  theme(legend.position="none",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))
graph
saving_plot(graph, "10_dendrogram")





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
graph <- wordcloud2(data=word_data, size = 1, minRotation = 0, maxRotation = 0, rotateRatio = 1, 
                    color=rep_len(c("#fffd33","#3a25ff","#48ffbc"), nrow(word_data)))
graph




### 2)


  # Table de fréquences des réponses
table <- contributeurs %>% select(30:34) %>% summarise_all(funs(sum(!is.na(.)))) %>% 
  t() %>% as.data.frame() %>% rownames_to_column()
table$rowname <- table$rowname %>% str_replace_all(c("Autre...34" = "Autre",
                                                     "J'ai des responsabilités" = "Avoir des responsabilités",
                                                     "J'aime contribuer au projet Vikidia" = "Contribuer au projet Vikidia",
                                                     "Je me plais au sein de la communauté" = "Se plaire dans la communauté",
                                                     "Les domaines abordés m'intéressent" = "Intérêt pour les domaines abordés"))

  # Plot
graph <- table %>% mutate(rowname = fct_reorder(rowname, V1)) %>% 
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
graph
saving_plot(graph, "12_bar_motiv-contrib")


 
# -------------- Quels sont les types de contribution ?



### 1)

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
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(outil = "Ancien responsable")
    # merge
table <- rbind(pat, adm, bur, ver, nuk, plu) %>% rename(`Types de contributions` = rowname)

  # Neutralisation des réponses
table$`Types de contributions` <- table$`Types de contributions` %>% 
          str_replace_all(c("De l'aide aux nouveaux utilisateurs" = "Aide aux nouveaux utilisateurs",
                            "De la patrouille" = "Patrouille",
                            "Des ajouts sur la partie technique" = "Ajouts sur la partie technique",
                            "Des corrections orthographiques ou typographiques" = "Corrections orthographiques ou typographiques",
                            "Des créations ou améliorations d'articles" = "Créations ou améliorations d'articles",
                            "Des travaux de catégorisation et d'ajout de portail" = "Travaux de catégorisation et d'ajout de portail"))

  # Ordonner les réponses
table$`Types de contributions` <- factor(table$`Types de contributions`, order = TRUE, 
                                            levels = c("Patrouille", 
                                                       "Aide aux nouveaux utilisateurs",
                                                       "Créations ou améliorations d'articles",
                                                       "Ajouts sur la partie technique",
                                                       "Corrections orthographiques ou typographiques",
                                                       "Travaux de catégorisation et d'ajout de portail",
                                                       "Autre"))

  # Plot
graph <- ggplot(table, aes(x = outil, y = V1)) +
  geom_col(aes(color = `Types de contributions`, fill = `Types de contributions`), position = position_dodge(0.8), width = 0.7) +
  scale_color_manual(values = c("#3a25ff","#48ffbc","#82888d","#3a25ff","#48ffbc","#82888d","#fffd33"))+
  scale_fill_manual(values = c("#3a25ff", "#eeeaff", "#82888d", "#f6f6f6", "#48ffbc", "#f6f6f6", "#fffd33")) +
  labs(x = "Responsabilités", y = "Nombre de réponses", "Nombre d'utilisations", title = "Contributions apportées à Vikidia selon les responsabilités") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))
graph
saving_plot(graph, "13_grouped_contrib")



### 2)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(is_contrib, `Avez-vous déjà fait un don à Vikidia ?`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% group_by(is_contrib) %>% 
  mutate(percent = round(Freq / sum(Freq) *100, 0)) %>% ungroup()
  rename(Sexe = `Êtes vous ...`)
num <- table[2,]$percent

  # Plot
gauge(table[2,]$percent, min = 0, max = 100, symbol = '%', label = "Non contributeurs", gaugeSectors(colors = '#82888d'))
gauge(table[4,]$percent, min = 0, max = 100, symbol = '%', label = "Contributeurs", gaugeSectors(colors = '#3a25ff'))



# -------------- Comment arrive-t-on dans la communauté ?



  # Table de fréquences des réponses
table <- contributeurs %>% select(14:19) %>% summarise_all(funs(sum(!is.na(.)))) %>% 
  t() %>% as.data.frame() %>% rownames_to_column()
table$rowname <- table$rowname %>% str_replace_all(c("Autre...19" = "Autre",
                                                     "Par hasard et au gré de vos navigations sur Internet" = "Par hasard, sur Internet",
                                                     "Par vos proches" = "Par des proches",
                                                     "Par votre milieu professionnel" = "Par le milieu professionnel",
                                                     "Par votre milieu scolaire, étudiant" = "Par le milieu scolaire, étudiant"))

  # Plot
graph <- table %>% mutate(rowname = fct_reorder(rowname, V1)) %>% 
  ggplot(aes(x=rowname, y=V1))+
    geom_bar(stat="identity", fill="#3a25ff", width=.6) +
    coord_flip() +
    ylab("Nombre de réponses") + xlab("") + ggtitle("Canaux par lesquels les répondants ont connu Vikidia") +
    theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())
graph
saving_plot(graph, "15_bar_connu-outil")



# -------------- Pourquoi arrive-t-on dans la communauté ?


  # Table de fréquences des réponses
non <- enquete_vikidia %>% filter(!is.na(`Non`))
non <- as.data.frame(table(non$`Selon vous, comment Vikidia est-elle perçue par le grand public ?`)) %>% mutate(partage = "Ne partage pas")
arr <- enquete_vikidia %>% filter(!is.na(`Il m'arrive de parler de mes découvertes sur l'encyclopédie`))
arr <- as.data.frame(table(arr$`Selon vous, comment Vikidia est-elle perçue par le grand public ?`)) %>% mutate(partage = "Partage de temps à autre")
rec <- enquete_vikidia %>% filter(!is.na(`Je recommande le site à mes proches`))
rec <- as.data.frame(table(rec$`Selon vous, comment Vikidia est-elle perçue par le grand public ?`)) %>% mutate(partage = "Recommande aux proches")
    # merge
table <- rbind(non, arr, rec)

  # Neutralisation des réponses
table$Var1 <- table$Var1 %>% 
          str_replace_all(c("Personne ne connaît Vikidia en dehors de la communauté" = "Inconnue en dehors de la communauté",
                            "Vikidia est peu connue, et le grand public en est méfiant" = "Peu connue et avec une certaine méfiance",
                            "Vikidia devrait gagner en popularité" = "Devrait gagner en popularité",
                            "Vikidia est de plus en plus connue et elle gagne en confiance" = "De plus en plus connue, gagne en confiance",
                            "Tout le monde connaît Vikidia !" = "Connue par tous"))

  # Ordonner les réponses
table$Var1 <- factor(table$Var1, order = TRUE, 
                                            levels = c("Autre",
                                                       "Inconnue en dehors de la communauté", 
                                                       "Peu connue et avec une certaine méfiance",
                                                       "Devrait gagner en popularité",
                                                       "De plus en plus connue, gagne en confiance",
                                                       "Connue par tous"
                                                       ))

  # Plot
graph <- ggplot(table, aes(fill = Var1, values = Freq, color = Var1)) +
  geom_waffle(size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~partage, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10,
                     expand = c(0,0)) +
  scale_fill_manual(values = c("#48ffbc", "#82888d", "#fffd33", "#3a25ff", "#eeeaff", "#f6f6f6")) +
  scale_color_manual(values = c("white", "white", "white", "white", "#3a25ff", "#82888d")) +
  #scale_color_manual(values = c("black","black","black","black","black","black")) +
  coord_equal() +
  labs(
    title = "Niveau de popularité estimé par le répondant, selon qu'il parle 
de Vikidia autour de lui",
    x = "Niveau de communication de l'outil",
    y = "Nombre de répondants",
  ) +
  theme_minimal(base_family = "Montserrat") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
        legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21)) +
  guides(fill = guide_legend(reverse = T, title = "Perception estimée de Vikidia par le grand public"),
         color = 'none')
graph
saving_plot(graph, "16_waffle")




# -------------- Pourquoi reste-t-on dans la communauté ?


  # Table de fréquences des réponses
filtre <- enquete_vikidia %>% filter(!is.na(`Les articles y sont plus faciles à comprendre`), `Êtes vous ...` != "préfère ne pas le dire")
fac <- as.data.frame(table(filtre$`Quel âge avez-vous ?`, filtre$`Êtes vous ...`)) %>% mutate(raisons = "Simplicité des articles")
filtre <- enquete_vikidia %>% filter(!is.na(`Vikidia est plus adaptée à mon niveau/âge`), `Êtes vous ...` != "préfère ne pas le dire")
age <- as.data.frame(table(filtre$`Quel âge avez-vous ?`, filtre$`Êtes vous ...`)) %>% mutate(raisons = "Outil adapté au niveau/âge")
filtre <- enquete_vikidia %>% filter(!is.na(`Je préfère l'ambiance au sein de la communauté vikidienne`), `Êtes vous ...` != "préfère ne pas le dire")
amb <- as.data.frame(table(filtre$`Quel âge avez-vous ?`, filtre$`Êtes vous ...`)) %>% mutate(raisons = "Ambiance favorable")
filtre <- enquete_vikidia %>% filter(!is.na(`La communauté vikidienne est de plus petite taille`), `Êtes vous ...` != "préfère ne pas le dire")
tai <- as.data.frame(table(filtre$`Quel âge avez-vous ?`, filtre$`Êtes vous ...`)) %>% mutate(raisons = "Communauté plus petite")
filtre <- enquete_vikidia %>% filter(!is.na(`Je consulte peu Vikidia, je suis surtout contributeur`), `Êtes vous ...` != "préfère ne pas le dire")
con <- as.data.frame(table(filtre$`Quel âge avez-vous ?`, filtre$`Êtes vous ...`)) %>% mutate(raisons = "Consulte peu, contribue pincipalement")
filtre <- enquete_vikidia %>% filter(!is.na(`Je préfère Wikipédia`), `Êtes vous ...` != "préfère ne pas le dire")
wik <- as.data.frame(table(filtre$`Quel âge avez-vous ?`, filtre$`Êtes vous ...`)) %>% mutate(raisons = "Préfèrence pour Wikipédia")
filtre <- enquete_vikidia %>% filter(!is.na(`Autre...57`), `Êtes vous ...` != "préfère ne pas le dire")
aut <- as.data.frame(table(filtre$`Quel âge avez-vous ?`, filtre$`Êtes vous ...`)) %>% mutate(raisons = "Autre")
    # merge
table <- rbind(fac,age,amb,tai,con,wik,aut)

  # Ordonner les réponses
table$raisons <- factor(table$raisons, order = TRUE, 
                                            levels = c("Autre",
                                                       "Préfèrence pour Wikipédia",
                                                       "Consulte peu, contribue pincipalement",
                                                       "Ambiance favorable",
                                                       "Communauté plus petite",
                                                       "Simplicité des articles", 
                                                       "Outil adapté au niveau/âge"
                                                       ))

  # Plot
graph <- ggballoonplot(table, x = "Var1", y = "raisons", size = "Freq", fill = "Var2", ggtheme = theme_bw()) + 
  labs(title = "Motivations à l'utilisation de Vikidia plutôt que Wikipédia") +
  facet_wrap(~Var2) +
 # scale_alpha("Freq") +
  scale_fill_manual(values = c("#fffd33", "#3a25ff")) +
  theme(text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))+
  guides(size = guide_legend(reverse = T, title = "Effectifs"),
         fill = "none")
graph
saving_plot(graph, "17_ballonplot")



# -------------- Quelles sont les alternatives à Vikidia ?


  # Table de fréquences des réponses
table <- process_alternatives %>% group_by(`Grandes catégories...6`, `Petites catégories`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% na.omit() %>% 
  mutate(`Moyennes catégories` = case_when(`Grandes catégories...6` == "ressources numériques" & grepl("wiki", `Petites catégories`) == TRUE ~ "Les wikis",
                                           TRUE ~ " "),
         `Grandes catégories...6` = toupper(`Grandes catégories...6`),
         colors = case_when(`Grandes catégories...6` == "RESSOURCES NUMÉRIQUES" ~ "#3a25ff",
                            `Grandes catégories...6` == "RESSOURCES PHYSIQUES" ~ "#82888d",
                            `Grandes catégories...6` == "RESSOURCES HUMAINES" ~ "#48ffbc"))

  # Plot
treemap(table, index= c("Grandes catégories...6", "Moyennes catégories", "Petites catégories"), vSize = "Freq", vColor="colors",
 
    type="color",                    
    palette = c("#fffd33", "#3a25ff", "#82888d"),
    drop.unused.levels = FALSE,
    title="Alternatives à Vikidia par type de ressources",   
    fontsize.title=21,                    
    fontsize.labels=c(18, 15, 12),              
    fontcolor.labels=c("white", "white", "white"),    # Color of labels
    #fontcolor.labels=c("black", "black", "black"),    # Color of labels
    fontface.labels=c(2, 4, 1),   
    fontfamily.title = "Montserrat",
    fontfamily.labels = "Montserrat",
    fontfamily.legend = "Montserrat",
    bg.labels=c("transparent"), 
    border.lwds=c(3,2,1),
    align.labels=list(
        c("left", "top"), 
        c("right", "bottom"),
        c("center", "center")),                                 
    inflate.labels = F,
    force.print.labels = T,
    
   # position.legend = "right",
    #reverse.legend = TRUE,
    #title.legend = "Type de ressources",
) 




# -------------- Ce qui questionne


### 1)

  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(`Quel âge avez-vous ?`, `Trouvez-vous les articles de Vikidia accessibles ?`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% rename(`Accessibilité des articles` = `Trouvez-vous les articles de Vikidia accessibles ?`)

  # Neutralisation des réponses
table$`Accessibilité des articles` <- table$`Accessibilité des articles` %>% 
          str_replace_all(c("Pas du tout accessibles" = "Inexistante",
                            "Peu accessibles" = "Insuffisante",  # faible / basse ?
                            "Moyennement accessibles" = "Moyenne",
                            "Plutôt accessibles" = "Correcte",
                            "Très accessibles" = "Élevée"))

  # Ordonner les réponses
table$`Accessibilité des articles` <- factor(table$`Accessibilité des articles`, order = TRUE, 
                                            levels = c("Élevée",
                                                       "Correcte",
                                                       "Moyenne",
                                                       "Insuffisante",
                                                       "Inexistante"
                                                       ))

  # Plot
graph <- ggplot(table, aes(x = `Quel âge avez-vous ?`, y = Freq)) +
  geom_col(aes(color = `Accessibilité des articles`, fill = `Accessibilité des articles`), position = "stack", width = 0.7) +
  scale_color_manual(values = c("white","white","white","white","white","white","white","white"))+
  scale_fill_manual(values = c("#3a25ff", "#eeeaff", "#fffd33", "#48ffbc", "#82888d", "#f6f6f6")) +
  labs(x = "Âge", y = "Nombre de répondants", title = "Évaluation de l'accessibilité des articles selon l'âge") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))
graph
ggsave(file = "dataviz/SVG/19_stacked_accessibilite.svg", plot=graph, width=14, height=8)
ggsave(file = "dataviz/PNG/19_stacked_accessibilite.png", plot=graph, width=14, height=8)




### 2)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(`Comment évaluez vous votre participation aux canaux de discussion ?`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% arrange(Freq)

  # Neutralisation des réponses
table$`Comment évaluez vous votre participation aux canaux de discussion ?` <- table$`Comment évaluez vous votre participation aux canaux de discussion ?` %>% 
          str_replace_all(c("[^[:alnum:]]" = " ",   #remove "??" characters
                            "Quels canaux de discussion" = "N'a pas connaissance des canaux",
                            "Je les connais mais je n y ai jamais participé" = "A connaissance sans y participer", 
                            "J ai déjà écrit un ou deux messages" = "Participe de temps à autre",
                            "Il m arrive de répondre aux messages" = "Participe de temps à autre",
                            "Je suis plutôt actif sur les canaux" = "Actif sur les canaux",
                            "Je suis très actif sur les canaux" = "Actif sur les canaux"
                            )) 

  # Ordonner les réponses
table$`Comment évaluez vous votre participation aux canaux de discussion ?` <- factor(table$`Comment évaluez vous votre participation aux canaux de discussion ?`, order = TRUE, levels = c("N'a pas connaissance des canaux   ", "A connaissance sans y participer", "Participe de temps à autre", "Actif sur les canaux"))

  # Regroupement des catégories : somme par groupe
table <- table %>% group_by(`Comment évaluez vous votre participation aux canaux de discussion ?`) %>% mutate(Freq = sum(Freq)) %>% distinct() %>% 
  mutate(`Comment évaluez vous votre participation aux canaux de discussion ?` = paste(Freq, "-", `Comment évaluez vous votre participation aux canaux de discussion ?`))

  # Plot
par(family = 'Monsterrat')
pie(table$Freq, labels = table$`Comment évaluez vous votre participation aux canaux de discussion ?`, border="white", col = c("#82888d", "#eeeaff", "#fffd33", "#3a25ff"), main = "Participation aux canaux de discussion", cex.main = 2)




### 3)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(`Que pensez-vous des pages d'aide ?`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% arrange(Freq)

  # Neutralisation des réponses
table$`Que pensez-vous des pages d'aide ?` <- table$`Que pensez-vous des pages d'aide ?` %>% 
          str_replace_all(c("[^[:alnum:]]" = " ",   #remove "??" characters
                            "Il existe des pages d aide" = "N'a pas connaissance des pages d'aide",
                            "Je n en ai jamais eu besoin" = "N'en a pas besoin",  
                            "Je ne les trouve pas utiles" = "N'en a pas besoin",
                            "Je pense qu elles pourraient être améliorées" = "Elles pourraient être améliorées",
                            "Elles sont trop complexes" = "Elles pourraient être améliorées",
                            "Elles sont suffisamment claires" = "Elles sont claires"
                            )) 

  # Ordonner les réponses
table$`Que pensez-vous des pages d'aide ?` <- factor(table$`Que pensez-vous des pages d'aide ?`, order = TRUE, levels = c("N'a pas connaissance des pages d'aide   ", "N'en a pas besoin", "Elles pourraient être améliorées", "Elles sont claires"))

  # Regroupement des catégories : somme par groupe
table <- table %>% group_by(`Que pensez-vous des pages d'aide ?`) %>% mutate(Freq = sum(Freq)) %>% distinct() %>% 
  mutate(`Que pensez-vous des pages d'aide ?` = paste(Freq, "-", `Que pensez-vous des pages d'aide ?`))

  # Plot
par(family = 'Monsterrat')
pie(table$Freq, labels = table$`Que pensez-vous des pages d'aide ?`, border="white", col = c("#82888d", "#eeeaff", "#fffd33", "#3a25ff"), main = "Avis sur les pages d'aide", cex.main = 2)




# -------------- Comment se sent-on au sein de la communauté ?



### 1)

  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(is_contrib, `Vous sentez-vous suffisamment intégré à la communauté vikidienne ?`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% arrange(Freq) %>% 
  group_by(is_contrib) %>% mutate(total = sum(Freq), percent = round(Freq / total *100, 0)) %>% 
  filter(`Vous sentez-vous suffisamment intégré à la communauté vikidienne ?` == "Oui") %>% ungroup() %>% 
  mutate(is_contrib = str_replace_all(is_contrib, c("1" = "Contributeur", "0" = "Non contributeur")))

  # Plot
graph <- ggplot(table, aes(x = is_contrib)) + 
  geom_bar(aes(y = total), width = 0.3, stat = "identity", fill = c("#eeeaff", "#f6f6f6"), col = c("#3a25ff", "#82888d"), size = 3) +
  geom_bar(aes(y = Freq), width = 0.3, stat = "identity", fill = c("#3a25ff", "#82888d"), col = c("#3a25ff", "#82888d")) +
  geom_text(aes(x = is_contrib, y = 70), label = paste(table$percent, "%"),
            colour = c("#f6f6f6","#eeeaff"), size = 8, fontface = "bold") +
  coord_flip() + labs(title = "Part des personnes se sentant intégrées à la communauté de Vikidia, 
selon s'ils contribuent ou pas à Vikidia", y = "Nombre de répondants", x = "") +
  theme_classic() +
  theme(text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))
graph   
saving_plot(graph, "21_gauge_integration")



### 2)



  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(`Comment avez-vous jugé votre accueil sur Vikidia ?`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% arrange(desc(Freq))

  # Ordonner les réponses
table$`Comment avez-vous jugé votre accueil sur Vikidia ?` <- factor(table$`Comment avez-vous jugé votre accueil sur Vikidia ?`, order = TRUE, 
                                                                     levels = c("Laisse à désirer",
                                                                                "Moyen",
                                                                                "Bon",
                                                                                "Excellent"))

  # Plot
graph <- table %>% #mutate(`Comment avez-vous jugé votre accueil sur Vikidia ?` = fct_reorder(`Comment avez-vous jugé votre accueil sur Vikidia ?`, Freq)) %>% 
  ggplot(aes(x=`Comment avez-vous jugé votre accueil sur Vikidia ?`, y=Freq))+
    geom_bar(stat="identity", fill="#3a25ff", width=.6) +
    coord_flip() +
    ylab("Nombre de répondants") + xlab("") + ggtitle("Évaluation de l'accueil sur Vikidia") +
    theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())
graph
saving_plot(graph, "22_bar_accueil")




# -------------- Quels outils pour animer la communauté ?



  # Table de fréquences des réponses
table <- enquete_vikidia %>% select(87:92) %>% filter(!grepl(c("ucun|jamais|rien|pas|nul|zero|sans"), `Veuillez préciser...92`)) %>% select(-6) %>%
  rename(`Autres canaux` = `Autres canaux de discussion`,
         `Bavardages` = `Les Bavardages`,
         `Pages de discussion` = `Les pages de discussion`) %>% 
  summarise_all(funs(sum(!is.na(.)))) %>% t() %>% as.data.frame() %>% rownames_to_column()

  # Plot
graph <- table %>% mutate(rowname = fct_reorder(rowname, desc(V1))) %>% 
  ggplot(aes(x=rowname, y=V1))+
    geom_bar(stat="identity", fill="#3a25ff", width=.6) +
    ylab("Nombre de répondants") + xlab("") + ggtitle("Canaux de discussions actifs") +
    theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())
graph
saving_plot(graph, "23_bar_canaux")




# -------------- Quelle confiance est accordée à Vikidia ?



### 1)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(`Êtes vous ...`) %>% select(63:68) %>% filter(`Êtes vous ...` != "préfère ne pas le dire") %>% 
  mutate_at(vars(`La qualité des articles`, `L'accessibilité des sujets traités`, `La qualité et la pertinence des illustrations`, `La diversité des sujets traités`, `La qualité synthaxique et typographique`, `Le projet Vikidia en général`), ~ str_replace_all(., c("Très bonne" = "5", "Plutôt bonne" = "4", "Moyenne" = "3", "Plutôt mauvaise" = "2", "Très mauvaise" = "1")))
table[,2:7] <- lapply(table[,2:7], as.numeric)
table <- table %>% ungroup() %>% summarise_all(funs(mean(.))) %>% select(-1)
table <- rbind(rep(5,5) , rep(1,5) , table)

  # Plot
radarchart(table, axistype=1 , 
    pcol="#3a25ff", plwd=3, 
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(1, 5 ,1), cglwd=0.8,
    vlabels = c("Qualité des articles", 
                "Accessibilité des sujets traités                    ", 
                "Qualité et pertinence des illustrations                                             ", 
                "Diversité des sujets traités", 
                "                                     Qualité synthaxique et typographique", 
                "                  Projet Vikidia en général"),
    vlcex=1.2,
    title = "Évaluation de la satisfaction sur différents sujets", 
    font.main = 2, family = "Montserrat", cex.main = 2,)
legend(1,0,
       legend=c("1 : Très mauvaise",
                "5 : Très bonne"),
       text.col = "#666666", text.font = 3,
       col="white", 
       lty=c(1,1), bty="n")



### 2)


  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(activite) %>% select(60) %>% na.omit() %>% 
  mutate(`Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?` = str_replace_all(`Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?`, c("Les articles de Vikidia sont tout à fait fiables" = "5", "Les articles de Vikidia sont plutôt fiables" = "4", "Les informations trouvées sur Vikidia ne sont pas toujours fiables" = "3", "Les informations trouvées sur Vikidia sont à prendre avec précaution" = "2", "Je me méfie des informations que je trouve sur Vikidia" = "1"))) %>% rename(Moy = `Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?`)
table$Moy <- as.numeric(table$Moy)
table <- table %>% group_by(activite) %>% summarise_all(funs(mean(.))) %>% ungroup() %>% arrange(desc(Moy))

  # Plot
graph <- table %>% 
  ggplot(aes(x=activite, y=Moy))+
    geom_segment(aes(xend=activite, yend=0), size = .7) +
    geom_point(size=6, color="#3a25ff") +
    coord_flip() +
    ylab("Nombre de répondants") + xlab("") + ggtitle("Évaluation de la fiabilité des articles selon l'activité") +
    lims(y = c(0, 5)) +
    theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())
graph
saving_plot(graph, "25_lollipop")



### 3)



  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(is_contrib,`Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?`) %>% summarise(Freq = n()) %>% rename(`Fiabilités des articles` = `Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?`) %>% 
  mutate(is_contrib = str_replace_all(is_contrib, c("0" = "Non contributeur", "1" = "Contributeur")))

  # Neutralisation des réponses
table$`Fiabilités des articles` <- table$`Fiabilités des articles` %>% 
          str_replace_all(c("Je me méfie des informations que je trouve sur Vikidia" = "Méfiance vis-à-vis du contenu", 
                            "Les articles de Vikidia sont plutôt fiables" = "Plutôt fiables",
                            "Les articles de Vikidia sont tout à fait fiables" = "Tout à fait fiables",  
                            "Les informations trouvées sur Vikidia ne sont pas toujours fiables" = "Pas toujours fiables",
                            "Les informations trouvées sur Vikidia sont à prendre avec précaution" = "À prendre avec précaution"
                            )) 

  # Ordonner les réponses
table$`Fiabilités des articles` <- factor(table$`Fiabilités des articles`, order = TRUE, levels = c("Méfiance vis-à-vis du contenu", "À prendre avec précaution", "Pas toujours fiables", "Plutôt fiables", "Tout à fait fiables"))

  # Pourcentage plutôt que fréquence
table <- table %>% group_by(is_contrib) %>% mutate(percent = round(Freq / sum(Freq), 2))

  # Plot
graph <- ggplot(table, aes(x = is_contrib, y = percent)) +
  geom_col(aes(color = `Fiabilités des articles`, fill = `Fiabilités des articles`), position = position_dodge(0.8), width = 0.7) +
  scale_color_manual(values = c("#82888d","#82888d","#82888d","#3a25ff","#3a25ff","#3a25ff","#82888d"))+
  scale_fill_manual(values = c("#82888d", "#f6f6f6", "#fffd33", "#3a25ff", "#eeeaff", "#3a25ff", "#fffd33")) +
  labs(x = "", y = "Pourcentage de réponses par groupe", title = "Évaluation de la fiabilité des articles selon la contribution") +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21))
graph
saving_plot(graph, "26_grouped_fiabilite")



