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
if (!require("stringr")) install.packages("stringr") # gosser avec des suites de caractères, str_replace, [...]
if (!require("dplyr")) install.packages("dplyr") # entre autres : left_join()
if (!require("tidyr")) install.packages("tidyr") # entre autres : extract_numeric() / extract_numeric() is deprecated: please use readr::parse_number() instead
if (!require("sf")) install.packages("sf"); if (!require("lutz")) install.packages("lutz") # GIS in R
if (!require("lubridate")) install.packages("lubridate")

# Boucle principale ----
# fonction : modifications automatisées pour chaque fichier issus d'une période de mesures des level loggers

# fichiers de consigne de données ----
ll.pre <- list.files("connectivite/data/raw", pattern = "^4")
ll.clean <- list()

for (i in 1:length(ll.pre)) {
# import et ménage
  paste(i)
  ll.pre[i]
  ll.pre.0 <- readLines(paste0("connectivite/data/raw/",ll.pre[i])); str(ll.pre.0) # lire en format texte
  # Warning message:
  #   In readLines(paste0("connectivite/data/raw/", ll.pre[i])) :
  #   incomplete final line found on 'connectivite/data/raw/[...].csv'
  # c'est chill, je n'ai pas réussi à arranger ça, mais vérifié √ pas de problème
  ll.pre.1 <- gsub(" ,", ",", ll.pre.0); str(ll.pre.1) # replace " ," by "," 
  ll.pre.2 <- gsub(" ", "", ll.pre.1); str(ll.pre.2) # enlever tous les espaces dans le subset de données
  
  # création des subsets data & metadata ----
  # notes : les noms réfèrent à l'étape et non à une matrice en particulier, les objets seront remplacés au fil de la boucle. 
  # l'info importante est consignée dans la liste ll.clean[i], à la fin
  ll.pre.2.metadata <-  ll.pre.2[c(1:9)] # inclus les anciens noms de colonnes, qui sont dans un format et un ordre bizzare
  ll.pre.2.data <- ll.pre.2[-c(1:9)]
  str(ll.pre.2.data) # chr
  
# vérification du fichier level logger brut : logger.serial.no == nom du fichier, sinon arrêter TOUT ! ----
  {
    # probe.serial.no.i dans les metadata
    texte <- ll.pre.2.metadata[4] # logger serial nom, en base R
    numbers <- gregexpr("[0-9]+", texte)
    result <- regmatches(texte, numbers)
    numeric_result <- as.numeric(unlist(result))
    probe.serial.no.i <- print(numeric_result)
    # no du level logger dans le nom du fichier brut (.csv), correspond à l'item "i" de la présente boucle
    texte <- ll.pre[i]
    numbers <- gregexpr("[0-9]+", texte)
    result <- regmatches(texte, numbers)
    numeric_result <- as.numeric(unlist(result))
    fichier <- print(numeric_result)
    # test logger.serial.no == nom du fichier
    if(!(probe.serial.no.i %in% fichier)) { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc "else" statement)
      stop(paste0("Attention, le nom du fichier ne correspond pas au numéro de série du level logger. Fichier problématique : i = ", paste(i), "; ", ll.pre[i]))
    }
    # si problème : aller changer manuellement en utilisant le no de série (unique) inscrit dans le fichier et PAS son nom 
    # ** 1. créer copie -> archive; 2. s'assurer de changer partout ** : QGIS, fichier, onglet, data_site.id
  } 
  
  # création du dataframe level legger (ll) contenant données de nappe phréatique (NP) et ménage  ----
  ll.pre.2.data.1 <- read.csv(text = ll.pre.2.data, col.names = c("scan.id", "date.AAAA-MM-JJ", "time.HH:MM:SS",'raw.value.mm',"calibrated.value.mm")) # text = argument de read.csv qui lit la valeur contenue dans l'objet
    # vérifications
  head(ll.pre.2.data.1)
  str(ll.pre.2.data.1)
  # si deux colonnes raw et calibrated sont exactement les mêmes, c'est qu'il n'y a pas eu de calibration; supprimer les valeurs doublons et/ou 
  # les remplacer par les bonnes valeur calibrées (calcul à faire)
  ll.pre.2.data.1$calibrated.value.mm <- ifelse(ll.pre.2.data.1$raw.value.mm == ll.pre.2.data.1$calibrated.value.mm, yes = ll.pre.2.data.1$calibrated.value.mm[rep("NA", times = length(ll.pre.2.data.1$calibrated.value))], no = ll.pre.2.data.1$calibrated.value)
  head(ll.pre.2.data.1)
  str(ll.pre.2.data.1)
  
 {
# Date et heure : format ISO date AAAA-MM-JJTHH:MM:SS,ss-/+FF:ff, voir https://fr.wikipedia.org/wiki/ISO_8601 ----
   # heure : « Z » à la fin lorsqu’il s’agit de l’heure UTC. (« Z » pour méridien zéro, aussi connu sous le nom « Zulu » dans l’alphabet radio international).
   # extraction : nom du site pour trouver les coordonnées qui serviront à connaître le fuseau horaire

   
   
   # CA FONCTIONNE PLUS !!
   # L'OSTI DE CSV qui décide s'il met des , ou è pour séparer les données !!! ÇA CHIE MA BOUCLE !!
     site.name <- sub(".*,", "", ll.pre.2.metadata[1])
   # 20 déc. j'ai écrit à Maryann pour qu'elle me transfère le fichier brut, attente
  
  # ouvrir données du shapefile pour accéder les zones
  zones <- read_sf("~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp")
  head(zones)
  str(zones)
  
  # CA FONCTIONNE PLUS !!
  
  
  # extraire la bonne lat, long selon le nom du site
  # coords <- c(zones$latitude[grep(site.name, zones, ignore.case = TRUE)], zones$longitude[grep(site.name, zones, ignore.case = TRUE)])
  coords <- c(zones$latitude[grep(site.name, zones, ignore.case = TRUE)], zones$longitude[grep(site.name, zones, ignore.case = TRUE)])
  
  # trouver le UTC selon la lat long
  tz <- tz_lookup_coords(coords[1], coords[2], method = "fast", warn = FALSE) 
  # modifier mes colonnes pour avoir le format ISO (manque encore le UTC à ajouter à la fin)
  ll.pre.2.data.2 <- ll.pre.2.data.1 %>% mutate(date.time.UTC.0pre = paste0(date.AAAA.MM.JJ," ", time.HH.MM.SS)) %>% select(!c("date.AAAA.MM.JJ", "time.HH.MM.SS"))
  ll.pre.2.data.2$date.time.UTC.0pre <- gsub("00:00", "00:01", ll.pre.2.data.2$date.time.UTC.0pre) # sinon, les données 00:00:00 étaient effacées !

  #  transformer en format ISO 8601
  ll.pre.2.data.2 <- ll.pre.2.data.2 %>% 
    mutate(date.time.UTC.0pre.1 = with_tz(dmy_hms(ll.pre.2.data.2$date.time.UTC.0pre, tz = tz), tzone = "UTC")) # les heures sont ainsi ramenées à UTC +0
  head(ll.pre.2.data.2) # différence de 4 heures
  
  # début et fin inscrits dans "level.logger.calibration.all.csv" (début = installation + 24h de rabattement de la NP, fin = heure de retrait)
  # note : données de date en format xlsx ça lit TOUT CROCHE, transformé en csv fonctionne bien
  cal.data <- read.csv("connectivite/data/raw/level.logger.calibration.all.csv", sep = ";")
  cal.data$day.begining.aaaa.mm.dd.hh.00.01 <- as.POSIXct(cal.data$day.begining.aaaa.mm.dd.hh.00.01)
  cal.data$day.end.aaaa.mm.dd.hh.00.01 <- as.POSIXct(cal.data$day.end.aaaa.mm.dd.hh.00.01)
  head(cal.data) # tibble
  str(cal.data)
  
  
  # CHANTIER

  # transformer en format ISO 8601, UTC +0
  cal.data <- cal.data %>%
    mutate(day.begining.aaaa.mm.dd.hh.00.01 = with_tz(ymd_hms(cal.data$day.begining.aaaa.mm.dd.hh.00.01, tz = tz), tzone = "UTC")) %>% # les heures sont ainsi ramenées à UTC +0 / ceci écrase la colonne du mm nom
    mutate(day.end.aaaa.mm.dd.hh.00.01 = with_tz(ymd_hms(cal.data$day.end.aaaa.mm.dd.hh.00.01, tz = tz), tzone = "UTC")) # les heures sont ainsi ramenées à UTC +0 / ceci écrase la colonne du mm nom
  head(cal.data)
  str(cal.data)
  # trouver les dates de départ et de fin - vérification
  # ll.pre.2.data.3 <- ll.pre.2.data.2 %>% dplyr::filter(date.time.UTC.0pre.1 >= cal.data$day.begining.aaaa.mm.dd.hh.00.01[cal.data$probe.serial.no == probe.serial.no.i][1]) # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data, trouvé dans la ligne dont la ll.pre.2.data.2$probe.serial.no == à la cal.data$probe.serial.no.i
  # nrow(ll.pre.2.data.2) - nrow(ll.pre.2.data.3) # différence de ~ 27  lignes, soit 27 heures (pourquoi pas 24h ? mais bon, pas grave Maryann me disait qu'elle retranche jusqu'à 48h)
  # ll.pre.2.data.4 <- ll.pre.2.data.3 %>% dplyr::filter(date.time.UTC.0pre.1 <= cal.data$day.end.aaaa.mm.dd.hh.00.01[cal.data$probe.serial.no == probe.serial.no.i][1]) # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data, trouvé dans la ligne dont la ll.pre.2.data.2$probe.serial.no == à la cal.data$probe.serial.no.i
  # nrow(ll.pre.2.data.3) - nrow(ll.pre.2.data.4) # différence de ~ 27  lignes, soit 27 heures (pourquoi pas 24h ? mais bon, pas grave Maryann me disait qu'elle retranche jusqu'à 48h)
  ll.pre.2.data.3 <- ll.pre.2.data.2 %>% 
    dplyr::filter(date.time.UTC.0pre.1 >= # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data, trouvé dans la ligne dont la ll.pre.2.data.2$probe.serial.no == à la cal.data$probe.serial.no.i
                    cal.data$day.begining.aaaa.mm.dd.hh.00.01[cal.data$probe.serial.no == probe.serial.no.i][1]) %>% 
  dplyr::filter(date.time.UTC.0pre.1 <= # <= date de mesure de NP plus petite ou égale à la date end dans cal.data [...], recoupe tous les jours entre la récupération des sondes et leur mise en arrêt
                  cal.data$day.end.aaaa.mm.dd.hh.00.01[cal.data$probe.serial.no == probe.serial.no.i][1]) 

  
  ll.pre.2.data.3 <- ll.pre.2.data.3 %>%   # enlever l'espace entre date et heure (ISO 8601)
    mutate(date.time.UTC.0pre.2 = str_replace(ll.pre.2.data.3$date.time.UTC.0pre.1, " ", "T"))
  ll.pre.2.data.3$date.time.UTC.0 <- str_replace_all(ll.pre.2.data.3$date.time.UTC.0pre.2, "00:01","00:01Z") # ajouter le Z à la fin (ISO 8601)
  ll.pre.2.data.3 <- ll.pre.2.data.3 %>% select(!c("date.time.UTC.0pre","date.time.UTC.0pre.1", "date.time.UTC.0pre.2")) # enlever les vielles colonnes
  head(ll.pre.2.data.3)
  colnames(ll.pre.2.data.3)
  }
  
# changer pour un nom explicite
  ll.cal.pre <- ll.pre.2.data.2
  
# calcul de calibration  ----
  # notes : les longueurs doivent être en mm ; idée pour cal.data : 
  # si unité = cm -> convertir en mm, si mm-> continuer
  
  # trouver les lignes qui correspondent à la sonde à calibrer
  cal.probe.i <- cal.data %>% dplyr::filter(cal.data$probe.serial.no == probe.serial.no.i)
  cal.probe.i
  
  # test: si raw.value == vecteur de "NA", on peut procédéer à la calibration, sinon ça veut dire qu'on a la cal du programme de la sonde, garder ces données (créer autre colonne)
  if(FALSE %in% (!ll.cal.pre$calibrated.value.mm %in% rep("NA", times = length(ll.cal.pre$calibrated.value.mm)))) { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
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
  {
    longueur.fil = cal.probe.i$cal.length.cm[cal.probe.i$cal.order==1]*10
    x2 = cal.probe.i$cal.length.cm[cal.probe.i$cal.order==2]*10
    x1 = cal.probe.i$cal.length.cm[cal.probe.i$cal.order==1]*10
    y2 = cal.probe.i$cal.value[cal.probe.i$cal.order==2]
    y1 = cal.probe.i$cal.value[cal.probe.i$cal.order==1]
    a.slope = ( y2 - y1 ) / ( x2 - x1 )
    b.offset = y1 - (a.slope * x1)
  }
  
  # changer la colonne calibrated pour les données corrigées
  ll.cal.pre$calibrated.value.mm = ((ll.cal.pre$raw.value.mm - b.offset) / a.slope ) - longueur.fil
  colnames(ll.cal.pre)
  head(ll.cal.pre)

# format final -> nom final
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

