# Documentation des manipulations réalisées au vu des visualisations


### Doublons

Les réponses au séquentiel 473, 474 et 475 sont exactement les mêmes (cela est vérifiable notamment dans les réponses libres), nous supprimons donc les réponses des séquentiels 474 et 475 pour commencer.

<br>

### Réponses non fiables

Nous avons remarqué au fur et à mesure que nous nous penchions sur les données, que certaines réponses au questionnaire semblent très peu fiables, du fait de différentes remarques : 

- incohérence des réponses : âgé de 14 à 18 ans et 'retraité'
- contenu des champs libres : insultes aux réponses 'libres'
- constance des réponses extrêmes : les réponses fournies sont toujours les pires (ex: `Veuillez noter votre satisfaction sur les sujets suivants`, réponse "*Très mauvaise*" pour chaque sujet)

Les réponses peu fiables que nous avons constatées sont celles aux séquentiels n°479, 484, 498, 593 et 594. N^ous avons donc décidé de les supprimer pour ne pas biaiser les résultats. 

<br>

### Création de variables

#### is_contrib

La variable '*is_contrib*' a été créée pour savoir si le répondant est contributeur ou non. S'il l'est, la variable prend la valeur '1' et s'il ne l'est pas la variable prend la valeur '0'. 

<br>

### Traitement des variables existantes

#### En quelle année avez-vous rejoint la communauté Vikidienne ?

Extraire sous R les années (4 chiffres) contenue dans le texte de réponse à cette question. Traiter les réponses de type ('il y a 4 ans') à la main.

#### Qu'est ce qui vous a motivé à contribuer pour la première fois ?

Lemmatisation, harmonisation à la main sous Google Sheets. 

#### Lorsque vous ne trouvez pas des informations sur Vikidia, où allez-vous en priorité ?

**Première passe** d'harmonisation des réponses : retirer les accents et mettre en minuscule. Étudier les réponses possibles puis extraire des chaînes de caractères les réponses les plus courantes / les mots-clés. La liste des mots-clés extraits en **deuxième passe** est la suivante : wikipedia, internet, net, google, moteur de recherche, fandom, dicoado.org, ecosia, opera mini, persee, kikourou, wikimini, universalis, youtube, livres, bibliotheque, larousse, dictionnaire, encyclopedie, depen, autre.

Enfin, une **troisième passe** consiste à lemmatiser les réponses en les regroupant sou un même nom, par exemple "**livres**" est regroupé sous la catégorie '*bibliothèque*', "**larousse**" sous '*encyclopedie*' etc.


#### Vous êtes ... et Veuillez préciser votre niveau d'étude 

Agrégation des deux champs pour les représenter visuellement sur 1 même graphique, le détail de niveau d'étude est ainsi directement collé à l'activité du répondant (ex: "Élève en école primaire"). Seuls 3 répondants 'Étudiant/élève' n'ont pas précisé leur niveau d'étude, restant ainsi dans cette catégorie plus générale. 

<br>

