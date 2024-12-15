# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : 2024-12-09
# Pourquoi : pour l'ensemble du traitement des données de nappe phréatique 
# Structure :
# —— connectivite
#         |—— data
#                     |—— raw
#                     |—— clean
#                     |—— archive
#         |—— output
#                     |—— data
#                     |—— figures
#         |—— scripts
# NOTES : 

# créer des RData
# exemple de code pour ouvrir : 
# load("~/Documents/Maîtrise/DonnéesAnalyses/PLS/rt_sg_large.RData") # rt_sg_large
# load("rt_sg_large.RData") # rt_sg_large
# obtenu via manips sur script "nettoyage_spectres_lissage_correction.R" dans le document "scripts-NETTOYAGE"

###########################################################################-

setwd("Aliz/Documents/Doctorat/_R.&.Stats_PhD")

# Librairies ----
if (!require("conflicted")) install.packages("conflicted") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("readxl")) install.packages("readxl") # lire les excel
# if (!require("stringr")) install.packages("stringr") # gosser avec des suites de caractères
if (!require("dplyr")) install.packages("dplyr") # entre autres : left_join()
# if (!require("tidyselect")) install.packages("tidyselect") # entre autres : last_col()
# if (!require("tidyverse")) install.packages("tidyverse") # entre autres : last_col()
if (!require("tidyr")) install.packages("tidyr") # entre autres : extract_numeric() / extract_numeric() is deprecated: please use readr::parse_number() instead
# if (!require("readr")) install.packages("readr") #  parse_number()
# if (!require("WriteXLS")) install.packages("WriteXLS")

# fichiers de consigne de données ----
ll.pre <- list.files("connectivite/data/raw", pattern = "^4")
ll.clean <- list()

# boucle ----
# notes : modifications pour chaque fichier issus d'une période de mesures des level loggers
for (i in 1:length(ll.pre)) {
# import et ménage
  paste(i)
  ll.pre[i]
  # read.csv(paste0("connectivite/data/raw/",ll[i])) # fonctionne, mais ne peut lire les lignes bizarrement codées
  ll.pre.0 <- readLines(paste0("connectivite/data/raw/",ll.pre[i]))
  str(ll.pre.0)
  ll.pre.1 <- gsub(" ,", ",", ll.pre.0) # replace " ," by "," 
  str(ll.pre.1)
  ll.pre.2 <- gsub(" ", "", ll.pre.1) # enlever tous les espaces dans le subset de données
  str(ll.pre.2)
  
# création des subsets ----
# notes : les noms réfèrent à l'étape et non à une matrice en particulier, les objets seront remplacés au fil de la boucle. 
# l'info importante est consignée dans la liste ll.clean[i]
  ll.pre.2.metadata <-  ll.pre.2[c(1:9)] # inclue les anciens noms de colonnes, qui sont dans un format et un ordre bizzare
  ll.pre.2.data <- ll.pre.2[-c(1:9)]
  str(ll.pre.2.data)

# vérification : logger.serial.no == nom du fichier, sinon arrêter TOUT ! ----
 {# base R pour extraire juste les chiffres...
   # logger serial no
   text <- ll.pre.2.metadata[4]
   # Extract numbers using regular expressions
   numbers <- gregexpr("[0-9]+", text)
   result <- regmatches(text, numbers)
   # Convert to numeric
   numeric_result <- as.numeric(unlist(result))
   logger.serial.no <- print(numeric_result)
   # fichier
   text <- ll.pre[i]
   # Extract numbers using regular expressions
   numbers <- gregexpr("[0-9]+", text)
   result <- regmatches(text, numbers)
   # Convert to numeric
   numeric_result <- as.numeric(unlist(result))
   fichier <- print(numeric_result)
   # test
   if(!(logger.serial.no %in% fichier)) { # si FALSE = STOP et warning "blabla" // si TRUE = continuer la boucle (donc rien, donc IF statement)
     stop(paste0("Attention, le nom du fichier ne correspond pas au numéro de série du level logger. Fichier problématique : i = ", paste(i), "; ", ll.pre[i]))
   }
   # si problème : aller changer manuellement en utilisant le no de série (unique) inscrit dans le fichier et PAS son nom 
   # ** 1. créer copie -> archive; 2. s'assurer de changer partout ** : QGIS, fichier, onglet, data_site.id
 } 
  
# création du dataframe et ménage  ----
  ll.pre.2.data.1 <- read.csv(text = ll.pre.2.data, col.names = c("scan.id", "date.AAAA-MM-JJ", "time.HH:MM:SS",'raw.value',"calibrated.value"))
  # vérifications
  head(ll.pre.2.data.1)
  str(ll.pre.2.data.1)
  # si deux colonnes raw et calibrated sont exactement les mêmes, c'est qu'il n'y a pas eu de calibration; supprimer les valeurs doublons et/ou 
  # les remplacer par les bonnes valeur calibrées (calcul à faire)
  ll.pre.2.data.1$calibrated.value <- ifelse(ll.pre.2.data.1$raw.value == ll.pre.2.data.1$calibrated.value, yes = ll.pre.2.data.1$calibrated.value[rep("NA", times = length(ll.pre.2.data.1$calibrated.value))], no = ll.pre.2.data.1$calibrated.value)
  head(ll.pre.2.data.1)
  str(ll.pre.2.data.1)
  
  # À FAIRE
  # ENLEVER LES DONNÉES DE LA PREMIÈRE JOURNÉE (24H) CAR RABATTEMENT DE LA NAPPE ET 
  # DÉCOUPER LES DONNÉES DE LA DERNIÈRE JOURNÉES
  
  # création de la liste dans la liste [[i]]  ----
  # noted : <- le fichier du level logger correspondant à la position i; [1] : data (dataframe), [2] : metadata (character string)
  ll.clean[[i]] <- list("data" = ll.pre.2.data.1, "metadata" = ll.pre.2.metadata)
  
# calcul de calibration  ----
# notes : les longueurs doivent être en mm !
  # RENDUE LÀ : ouvrir et croiser les données créées dans ll.clean et les données de calibration issues du fichier "level.logger.calibration.all.xlsx".
  
  cal.data <- read_xlsx("connectivite/data/raw/level.logger.calibration.all.xlsx")
  head(cal.data) # tibble
  str(cal.data) # idée : 
                # si unité = cm -> convertir en mm, si mm-> continuer
  
# FORMULES pour la calibration ----
  # RES.NP.calibré = ((DATA.raw.value " - b.offset) / a.slope ) - longueur.fil
  # si Y = (a * X) + b,
  # X = ( Y - b ) / a, puis on enlève la longueur du fil à la mesure de NP
  # où 
  # y = raw.value aux longueurs 1 et 2 du test de calibration (p. ex. 200 mm et 800 mm ou 1400 mm, pour STH)
  # b.offset = ( y2 - y1 ) / ( x2 - x1 ), soit la proportion de changement de y pour chaque changement de x
  # a = longueur de fil (mm)
  # x2 = longueur fil test #2 (V = "cal.order"), x1 = longueur fil test #1 (V = "cal.order")
  # y2 = raw.value à du test #2 (V = "cal.value"), y1 = raw.value à du test #1 (V = "cal.value")
  
  for (i in 1:2) { # 2 fois, car deux mesures ?
    
  }
    
}
# vérifier que les erreurs sont tjrs la meme affaire inutile -> incomplete final line, tenté de régler le problème, mais sans succès; 
# et different length (ça le dit quand le "cal" est vide, et ça met des NA, ce qui est parfait)



