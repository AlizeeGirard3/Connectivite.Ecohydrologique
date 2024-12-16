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

setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

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
if (!require("sf")) install.packages("sf"); if (!require("lutz")) install.packages("lutz") # GIS in R
# if (!require("lubridate")) install.packages("lubridate")


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
    if(!(logger.serial.no %in% fichier)) { # si TRUE = STOP et warning "blabla" // si TRUE = continuer la boucle (donc rien, donc IF statement)
      stop(paste0("Attention, le nom du fichier ne correspond pas au numéro de série du level logger. Fichier problématique : i = ", paste(i), "; ", ll.pre[i]))
    }
    # si problème : aller changer manuellement en utilisant le no de série (unique) inscrit dans le fichier et PAS son nom 
    # ** 1. créer copie -> archive; 2. s'assurer de changer partout ** : QGIS, fichier, onglet, data_site.id
  } 
  
  # création du dataframe et ménage  ----
  ll.pre.2.data.1 <- read.csv(text = ll.pre.2.data, col.names = c("scan.id", "date.AAAA-MM-JJ", "time.HH:MM:SS",'raw.value.mm',"calibrated.value.mm"))
  # vérifications
  head(ll.pre.2.data.1)
  str(ll.pre.2.data.1)
  # si deux colonnes raw et calibrated sont exactement les mêmes, c'est qu'il n'y a pas eu de calibration; supprimer les valeurs doublons et/ou 
  # les remplacer par les bonnes valeur calibrées (calcul à faire)
  ll.pre.2.data.1$calibrated.value <- ifelse(ll.pre.2.data.1$raw.value == ll.pre.2.data.1$calibrated.value, yes = ll.pre.2.data.1$calibrated.value[rep("NA", times = length(ll.pre.2.data.1$calibrated.value))], no = ll.pre.2.data.1$calibrated.value)
  head(ll.pre.2.data.1)
  str(ll.pre.2.data.1)
  
  
  # _________________
  
  # trouver les UTC (time zones) ----  
  # extraction : nom du site pour trouver les coordonnées qui serviront à connaître le fuseau horaire ----
  site.name <- sub(".*,", "", ll.pre.2.metadata[1])
  
  # ouvrir données du shapefile pour accéder les zones
  zones <- read_sf("~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp")
  head(zones)
  str(zones)
  
  # extraire la bonne lat, long selon le nom du site
  coords <- c(zones$latitude[grep(site.name, zones, ignore.case = TRUE)], zones$longitude[grep(site.name, zones, ignore.case = TRUE)])
  
  # trouver le UTC selon la lat long
  head(test1)
  tz <- tz_lookup_coords(coords[1], coords[2], method = "fast", warn = FALSE) 
  # modifier mes colonnes pour avoir le format ISO (manque encore le UTC à ajouter à la fin)
  test1 <- ll.pre.2.data.1 %>% mutate(new = paste0(date.AAAA.MM.JJ," ", time.HH.MM.SS)) %>% select(!c("date.AAAA.MM.JJ", "time.HH.MM.SS"))
  colnames(test1)
  
  #  tr
  with_tz(dmy_hms(test1$new, tz = tz), tzone = "UTC") # les heures sont ainsi ramenées à UTC +0

  
  
  
  # _________________
  
  
  
  
  
  
  
  # changer pour un nom explicite
  ll.cal.pre <- ll.pre.2.data.1
  
  # calcul de calibration  ----
  # notes : les longueurs doivent être en mm !
  # ouvrir le fichier de données
  cal.data <- read_xlsx("connectivite/data/raw/level.logger.calibration.all.xlsx")
  head(cal.data) # tibble
  str(cal.data)
  # idée : 
  # si unité = cm -> convertir en mm, si mm-> continuer
  
  # trouver les lignes qui correspondent à la sonde à calibrer
  cal.probe.i <- cal.data %>% dplyr::filter(cal.data$probe.serial.no == logger.serial.no)
  cal.probe.i
  
  # test: si raw.value == vecteur de "NA", on peut procédéer à la calibration, sinon ça veut dire qu'on a la cal du programme de la sonde, garder ces données (créer autre colonne)
  if(FALSE %in% (!ll.cal.pre$calibrated.value %in% rep("NA", times = length(ll.cal.pre$calibrated.value)))) { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
    stop(paste0("Attention, la colonne calibrated.value n'est pas vide. Sonde problématique : i = ", paste(i), "; ", ll.pre[i]))
    # créer une autre colonne, le cas échéant (à faire)
  }
  
  # calcul des termes de la calibration ----
  # FORMULES
  # RES.NP.calibré = ((DATA.raw.value " - b.offset) / a.slope ) - longueur.fil
  # si Y = (a * X) + b,
  # X = ( Y - b ) / a, puis on enlève la longueur du fil à la mesure de NP
  # où 
  # y = raw.value aux longueurs 1 et 2 du test de calibration (p. ex. 200 mm et 800 mm ou 1400 mm, pour STH)
  # b.offset = y1 - a.slope * x1
  # a.slope = ( y2 - y1 ) / ( x2 - x1 ), soit la proportion de changement de y pour chaque changement de x
  # a.slope = longueur de fil (mm)
  # x2 = longueur fil test #2 (V = "cal.order"), x1 = longueur fil test #1 (V = "cal.order")
  # y2 = raw.value à du test #2 (V = "cal.value"), y1 = raw.value à du test #1 (V = "cal.value")
  {longueur.fil = cal.probe.i$cal.length.cm[cal.probe.i$cal.order==1]*10
  x2 = cal.probe.i$cal.length.cm[cal.probe.i$cal.order==2]*10
  x1 = cal.probe.i$cal.length.cm[cal.probe.i$cal.order==1]*10
  y2 = cal.probe.i$cal.value[cal.probe.i$cal.order==2]
  y1 = cal.probe.i$cal.value[cal.probe.i$cal.order==1]
  a.slope = ( y2 - y1 ) / ( x2 - x1 )
  b.offset = y1 - (a.slope * x1)}
  
  # changer la colonne calibrated pour les données corrigées
  ll.cal.pre$calibrated.value = ((ll.cal.pre$raw.value - b.offset) / a.slope ) - longueur.fil
  
  head(ll.cal.pre)
  
  
  
  # CHANTIER
  
  # rendue là *******
  
  # idée : selon l'emplacement (à ajouter dans level.logger data), trouver le UTC et tout conveetir au UTC 0
  
  # À FAIRE
  # ENLEVER LES DONNÉES DE LA PREMIÈRE JOURNÉE (24H) CAR RABATTEMENT DE LA NAPPE ET 
  # DÉCOUPER LES DONNÉES DE LA DERNIÈRE JOURNÉES
  
  # format : ISO date AAAA-MM-JJTHH:MM:SS,ss-/+FF:ff, voir https://fr.wikipedia.org/wiki/ISO_8601
  # Heure : « Z » à la fin lorsqu’il s’agit de l’heure UTC. (« Z » pour méridien zéro, aussi connu sous le nom « Zulu » dans l’alphabet radio international).
  ll.cal.pre$date.AAAA.MM.JJ <- as.Date(ll.cal.pre$date.AAAA.MM.JJ, "%d/%m/%y") # s'assurer que les jours et mois sont dans le bon ordre // tz = time zone; si utile
  head(ll.cal.pre)
  
  # paste0(min(ll.cal.pre$date.AAAA.MM.JJ),"_",min(ll.cal.pre$time.HH.MM.SS))
  # paste0(min(ll.cal.pre$date.AAAA.MM.JJ),"_",min(ll.cal.pre$time.HH.MM.SS))
  # 
  # 
  
  ll.cal <- ll.cal.pre # ceci est donc le format final, à intégrer dans la liste ll.clean
  
  # création de la liste dans la liste [[i]]  ----
  # noted : <- le fichier du level logger correspondant à la position i; [1] : data (dataframe), [2] : metadata (character string)
  ll.clean[[i]] <- list("data" = ll.cal, "metadata" = ll.pre.2.metadata)
} 
# vérifier que les erreurs sont tjrs la meme affaire inutile -> incomplete final line, tenté de régler le problème, mais sans succès; 
# et different length (ça le dit quand le "cal" est vide, et ça met des NA, ce qui est parfait)

if("ll.clean.RData" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }
