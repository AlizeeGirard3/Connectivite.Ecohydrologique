#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                 Water table, data extraction from raw probe files
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : 2024-12-09
# Pourquoi : pour l'ensemble du traitement des données de nappe phréatique 
# Structure :
# —— connectivite
#         |—— archive
#         |—— data
#                     |—— raw
#                     |—— clean
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
##########################################################################-

# .rs.restartR()
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

# Librairies ----
if (!require("conflicted")) install.packages("conflicted") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("readxl")) install.packages("readxl") # lire les excel
if (!require("stringr")) install.packages("stringr") # gosser avec des suites de caractères, str_replace, [...]
if (!require("dplyr")) install.packages("dplyr") # entre autres : left_join()
if (!require("tidyr")) install.packages("tidyr") # entre autres : extract_numeric() / extract_numeric() is deprecated: please use readr::parse_number() instead
if (!require("sf")) install.packages("sf"); if (!require("lutz")) install.packages("lutz") # GIS in R
if (!require("lubridate")) install.packages("lubridate")
options(lubridate.verbose = TRUE) # pour expliciter ce que les fonctions font
# librairies de weathercan
if (!require("weathercan")) install.packages("weathercan") # Integrating data from weathercan (ECCC/CCCS), Gouvernement du Canada
# if (!require("naniar")) install.packages("naniar") # Checking data completeness
# if (!require("mapview")) install.packages("mapview") ## Spatial analyses
if (!require("parsedate")) install.packages("parsedate") # lire les excel


# A Donnée issues du Capacitance Water Level Logger Odyssey® ----
## A.1 nettoyage ----
# fonction : modifications automatisées pour chaque fichier issus d'une période de mesures des level loggers

# fichiers de consigne de données
ll.pre <- list.files("connectivite/data/raw", pattern = "^4")
ll.clean <- list()

for (i in 1:length(ll.pre)) {
  # import et ménage
  print(i)
  ll.pre[i]
  ll.pre.0 <- readLines(paste0("connectivite/data/raw/",ll.pre[i])); str(ll.pre.0) # lire en format texte
  # Warning message:
  #   In readLines(paste0("connectivite/data/raw/", ll.pre[i])) :
  #   incomplete final line found on 'connectivite/data/raw/[...].csv'
  # c'est chill, je n'ai pas réussi à arranger ça, mais vérifié √ pas de problème
  ll.pre.1 <- gsub(" ,", ",", ll.pre.0); str(ll.pre.1) # replace " ," by "," 
  ll.pre.2 <- gsub(" ", "", ll.pre.1); str(ll.pre.2) # enlever tous les espaces dans le subset de données
  
  ### création des subsets data & metadata ----
  # notes : les noms réfèrent à l'étape et non à une matrice en particulier, les objets seront remplacés au fil de la boucle. 
  # l'info importante est consignée dans la liste ll.clean[i], à la fin
  ll.pre.2.metadata <-  ll.pre.2[c(1:9)] # inclus les anciens noms de colonnes, qui sont dans un format et un ordre bizzare
  ll.pre.2.data <- ll.pre.2[-c(1:9)]
  str(ll.pre.2.data) # chr
  
  ### vérification du fichier level logger brut : logger.serial.no == nom du fichier, sinon arrêter TOUT ! ----
  {
    # trouver le probe.uid.i (== probe.uid, logger serial no) dans les metadata
    texte <- ll.pre.2.metadata[4] # logger serial no, en base R
    numbers <- gregexpr("[0-9]+", texte)
    result <- regmatches(texte, numbers)
    probe.uid.i <- as.numeric(unlist(result))
    # no du level logger dans le nom du fichier brut (.csv), correspond à l'item "i" de la présente boucle
    texte <- ll.pre[i]
    numbers <- gregexpr("[0-9]+", texte)
    result <- regmatches(texte, numbers)
    fichier <- as.numeric(unlist(result))
    # test logger.serial.no == nom du fichier
    if(!(probe.uid.i %in% fichier)) { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc "else" statement)
      stop(paste0("Attention, le nom du fichier ne correspond pas au numéro de série du level logger. Fichier problématique : i = ", paste(i), "; ", ll.pre[i]))
    }
    # si problème : aller changer manuellement en utilisant le no de série (unique) inscrit dans le fichier et PAS son nom 
    # ** 1. créer copie -> archive; 2. s'assurer de changer partout ** : QGIS, fichier, onglet, data_site.id
  }

    ### création du dataframe level legger (ll) contenant données de nappe phréatique (NP) et ménage  ----
  ll.pre.2.data.1 <- read.csv(text = ll.pre.2.data, col.names = c("scan.id", "date.JJ.MM.AAAA", "time.HH.MM.SS",'raw.value.mm',"calibrated.value.mm")) # text = argument de read.csv qui lit la valeur contenue dans l'objet / DATE mauvais format
    # vérifications
  head(ll.pre.2.data.1)
  str(ll.pre.2.data.1)
  # si deux colonnes raw et calibrated sont exactement les mêmes, c'est qu'il n'y a pas eu de calibration; supprimer les valeurs doublons et/ou 
  # les remplacer par les bonnes valeur calibrées (calcul à faire)
  ll.pre.2.data.1$calibrated.value.mm <- ifelse(ll.pre.2.data.1$raw.value.mm == ll.pre.2.data.1$calibrated.value.mm, yes = ll.pre.2.data.1$calibrated.value.mm[rep("NA", times = length(ll.pre.2.data.1$calibrated.value))], no = ll.pre.2.data.1$calibrated.value)
  head(ll.pre.2.data.1)
  str(ll.pre.2.data.1)
  
 {
### date et heure : format ISO date AAAA-MM-JJTHH:MM:SS,ss-/+FF:ff, voir https://fr.wikipedia.org/wiki/ISO_8601 ----
   # heure : « Z » à la fin lorsqu’il s’agit de l’heure UTC. (« Z » pour méridien zéro, aussi connu sous le nom « Zulu » dans l’alphabet radio international).
   # extraction : nom du site pour trouver les coordonnées qui serviront à connaître le fuseau horaire
   site.name.pre <- sub("SiteName","",ll.pre.2.metadata[1])
   site.name <- stringr::str_to_title(gsub(",", "", site.name.pre))

  # ouvrir données du shapefile pour accéder les zones
   zones <- read_sf("~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp")
   zones <- as.data.frame(zones)
   head(zones)
   str(zones)
  
  # extraire la bonne lat, long selon le nom du site
   coords <- c(zones$latitude[zones$site==site.name], zones$longitude[zones$site==site.name])

  # trouver le UTC selon la lat long
   tz <- tz_lookup_coords(coords[1], coords[2], method = "fast", warn = FALSE) 
  # modifier mes colonnes pour avoir le format ISO (manque encore le UTC à ajouter à la fin)
   # garder date.AAAA-MM-JJ"
   ll.pre.2.data.2 <- ll.pre.2.data.1 %>% dplyr::mutate(date.time.UTC.0pre = paste0(date.JJ.MM.AAAA," ", time.HH.MM.SS)) # %>% select(!"time.HH.MM.SS") # conserver l'heure aussi ?
   nrow(ll.pre.2.data.2)
   
   
   
   
   # ICI, SI EN POSIX et en UTC+0, utiliser (exemple)
   # ll.pre.0.data.2$date.time.UTC.0pre.1 <- format_iso_8601(ll.pre.0.data.2$date.time.UTC.0pre) # pour ensuite éviter l'étape de mettre T et Z dans l'énoncé, supprimer plus loin
   # ll.pre.0.data.2$date.time.UTC.0 <- gsub("[+]00:00", "Z",  ll.pre.0.data.2$date.time.UTC.0pre.1)
   
   
   
   
   ll.pre.2.data.2$date.time.UTC.0pre <- gsub("00:00", "00:01", ll.pre.2.data.2$date.time.UTC.0pre) # sinon, les données 00:00:00 étaient effacées !

  #  transformer en format ISO 8601
  # garder date.AAAA-MM-JJ
     ll.pre.2.data.2 <- ll.pre.2.data.2 %>% 
       mutate(`date.AAAA-MM-JJ` = dmy(ll.pre.2.data.2$date.JJ.MM.AAAA, tz = tz)) %>%
       mutate(`date.time.tz.orig` = dmy_hms(paste0(ll.pre.2.data.2$date.JJ.MM.AAAA," ", ll.pre.2.data.2$time.HH.MM.SS))) %>% 
       mutate(date.time.UTC.0pre.1 = with_tz(dmy_hms(ll.pre.2.data.2$date.time.UTC.0pre, tz = tz), tzone = "UTC"))  # les heures sont ainsi ramenées à UTC +0
     # erreur ici, causée par les fichiers vides
     # There was 1 warning in `mutate()`.
     # ℹ In argument: `date.time.tz.orig = dmy_hms(paste0(ll.pre.2.data.2$date.JJ.MM.AAAA, " ", ll.pre.2.data.2$time.HH.MM.SS))`.
     # Caused by warning:
     # ! All formats failed to parse. No formats found. 
   head(ll.pre.2.data.2) # différence de X heures, vérifier
  
  # début et fin inscrits dans "level.logger.calibration.all.csv" (début = installation + 24h de rabattement de la NP, fin = heure de retrait)
  # note : données de date en format xlsx ça lit TOUT CROCHE, transformé en csv fonctionne bien
  cal.data <- read.csv("connectivite/data/raw/level.logger.calibration.all.csv", sep = ";")
  cal.data$day.begining.aaaa.mm.dd.hh.00.01 <- as.POSIXct(cal.data$day.begining.aaaa.mm.dd.hh.00.01)
  cal.data$day.end.aaaa.mm.dd.hh.00.01 <- as.POSIXct(cal.data$day.end.aaaa.mm.dd.hh.00.01)
  head(cal.data) # tibble
  str(cal.data)
  
  # transformer en format ISO 8601, UTC +0
  cal.data <- cal.data %>%
    mutate(day.begining.aaaa.mm.dd.hh.00.01 = with_tz(ymd_hms(cal.data$day.begining.aaaa.mm.dd.hh.00.01, tz = tz), tzone = "UTC")) %>% # les heures sont ainsi ramenées à UTC +0 / ceci écrase la colonne du mm nom
    mutate(day.end.aaaa.mm.dd.hh.00.01 = with_tz(ymd_hms(cal.data$day.end.aaaa.mm.dd.hh.00.01, tz = tz), tzone = "UTC")) # les heures sont ainsi ramenées à UTC +0 / ceci écrase la colonne du mm nom
  head(cal.data)
  str(cal.data)
  # trouver les dates de départ et de fin - vérification
  # ll.pre.2.data.3 <- ll.pre.2.data.2 %>% dplyr::filter(date.time.UTC.0pre.1 >= cal.data$day.begining.aaaa.mm.dd.hh.00.01[cal.data$probe.uid == probe.uid.i][1]) # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data, trouvé dans la ligne dont la ll.pre.2.data.2$probe.uid == à la cal.data$probe.uid.i
  # nrow(ll.pre.2.data.2) - nrow(ll.pre.2.data.3) # différence de ~ 27  lignes, soit 27 heures (pourquoi pas 24h ? mais bon, pas grave Maryann me disait qu'elle retranche jusqu'à 48h)
  # ll.pre.2.data.4 <- ll.pre.2.data.3 %>% dplyr::filter(date.time.UTC.0pre.1 <= cal.data$day.end.aaaa.mm.dd.hh.00.01[cal.data$probe.uid == probe.uid.i][1]) # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data, trouvé dans la ligne dont la ll.pre.2.data.2$probe.uid == à la cal.data$probe.uid.i
  # nrow(ll.pre.2.data.3) - nrow(ll.pre.2.data.4) # différence de ~ 27  lignes, soit 27 heures (pourquoi pas 24h ? mais bon, pas grave Maryann me disait qu'elle retranche jusqu'à 48h)
  ll.pre.2.data.3 <- ll.pre.2.data.2 %>% 
    dplyr::filter(date.time.UTC.0pre.1 >= # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data, trouvé dans la ligne dont la ll.pre.2.data.2$probe.uid == à la cal.data$probe.uid.i
                  unique(na.omit(cal.data$day.begining.aaaa.mm.dd.hh.00.01[cal.data$probe.uid == probe.uid.i]))) %>% 
    dplyr::filter(date.time.UTC.0pre.1 <= # <= date de mesure de NP plus petite ou égale à la date end dans cal.data [...], recoupe tous les jours entre la récupération des sondes et leur mise en arrêt
                    unique(na.omit(cal.data$day.end.aaaa.mm.dd.hh.00.01[cal.data$probe.uid == probe.uid.i])))

  ll.pre.2.data.3 <- ll.pre.2.data.3 %>%   # enlever l'espace entre date et heure (ISO 8601)
    mutate(date.time.UTC.0pre.2 = str_replace(ll.pre.2.data.3$date.time.UTC.0pre.1, " ", "T"))
  ll.pre.2.data.3$date.time.UTC.0 <- str_replace_all(ll.pre.2.data.3$date.time.UTC.0pre.2, "00:01","00:01Z") # ajouter le Z à la fin (ISO 8601)
  ll.pre.2.data.3 <- ll.pre.2.data.3 %>% select(!c("date.JJ.MM.AAAA", "date.time.UTC.0pre.1", "date.time.UTC.0pre.2")) %>% # enlever les vielles colonnes
    select("scan.id", "raw.value.mm", "calibrated.value.mm", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0") # date et time sans "UTC.0" sont dans le fuseau horaire d'origine (tz trouvé en croisant les coordonnées "coords")
  head(ll.pre.2.data.3); colnames(ll.pre.2.data.3)
  }
  
  # changer pour un nom explicite
  ll.cal.pre <- ll.pre.2.data.3
  
  ### calcul de calibration  ----
  # notes : les longueurs doivent être en mm ; idée pour cal.data : 
  # si unité = cm -> convertir en mm, si mm-> continuer
  
  # trouver les lignes qui correspondent à la sonde à calibrer
  cal.probe.i <- cal.data %>% dplyr::filter(cal.data$probe.uid == probe.uid.i)
  cal.probe.i
  
  # test: si raw.value == vecteur de "NA", on peut procédéer à la calibration, sinon ça veut dire qu'on a la cal du programme de la sonde, garder ces données (créer autre colonne)
  if(FALSE %in% (!ll.cal.pre$calibrated.value.mm %in% rep("NA", times = length(ll.cal.pre$calibrated.value.mm)))) { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
    stop(paste0("Attention, la colonne calibrated.value n'est pas vide. Sonde problématique : i = ", paste(i), "; ", ll.pre[i]))
    # créer une autre colonne, le cas échéant (à faire)
  }
  
  ### calcul des termes de la calibration ----
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
  
  ### création de la liste dans la liste [[i]]  ----
  # noted : <- le fichier du level logger correspondant à la position i; [1] : data (dataframe), [2] : metadata (character string)
  ll.clean[[i]] <- list("data" = ll.cal, "metadata" = ll.pre.2.metadata)
} 
# vérifier que les erreurs sont tjrs la meme affaire inutile -> incomplete final line, tenté de régler le problème, mais sans succès; 
# et different length (ça le dit quand le "cal" est vide, et ça met des NA, ce qui est parfait)

if("ll.clean.RData" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { saveRDS(ll.clean, file = "connectivite/data/clean/ll.clean.RDS") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }


## A.2 examination des données ----
for (j in 1:length(ll.clean)) {
  print(j)
  
  # extraire no de sonde
  metadata.line <- ll.pre.2.metadata[4] # logger serial nom, en base R
  numbers <- gregexpr("[0-9]+", metadata.line)
  sonde <- regmatches(metadata.line, numbers)
  
  # données à visualiser
  data <- ll.clean[[j]]$data
  if (nrow(data) > 0) {
    hist(data$calibrated.value.mm/10, warn.unused = F, 
         main = paste("Histogram des données de sonde no ", paste(sonde,"\n"))) # en cm
  } 
  # les hauteurs de nappe phréatique calibrées devraient toutes être négatives ou presque !
}




# CHANTIER ICI 






## A.3 données de vérification/calibration avec bulleur ----
# créé le 23 déc. pour vérifier données des Odyssey de St-Henri 2024

# bulleur.pre <- list.files("connectivite/data/raw", pattern = "^data_") # "^" = "starts with"
# for (i in 1:length(bulleur.pre)) {
#   print(i);  print(bulleur.pre[i])
#   bulleur.pre.i <- read_excel(paste0("connectivite/data/raw/", bulleur.pre[i]),
#                               sheet = "well")
#   bulleur.pre.i <- as.data.frame(bulleur.pre.i)





# bon ci dessous
# TRANSFÉRÉ DANS RMarkdown le 7 janvier 2025

# création de level.logger.calibration.clean ----
# hauteur de nappe avec le bulleur
ll.bulleur <- read.csv("connectivite/data/raw/level.logger.calibration.all.csv", sep = ";", dec = ",")
str(ll.bulleur)

# calcul du OUT
ll.bulleur$`out.long.tuyau-sol.cm` <- round(ll.bulleur$pt.haut.cm - ((ll.bulleur$pt.bas1.cm + ll.bulleur$pt.bas2.cm + ll.bulleur$pt.bas3.cm)/3), digits = 2)

# calcul de la profondeur de la nappe phréatiquedu (OUT - IN, où IN = mesure lue sur le bulleur)
ll.bulleur$water.table.depth.cm <- round(ll.bulleur$bulleur.cm - ll.bulleur$`out.long.tuyau-sol.cm`, digits = 2)
# ici, PROFONDEUR de nappe, donc quand c'est -5cm par exemple, nappe au DESSUS du sol

# À FAIRE 
# vérifier comment on présente typiquement ces données
# enlever colonnes inutiles
# save le csv dans clean

ll.clean <- readRDS("connectivite/data/clean/ll.clean.RDS")
# water.table.verif <- list()
water.table.verif <- data.frame() #<- réfléchir à comment lui indiquer à quelle ligne consigner les données

# pas sous forme de liste, mais de dataframe !*

for (j in 1:length(ll.clean)) {

  # j <- 3

  print(j)
  ll.clean.j <- ll.clean[[j]]
  
  # extraire no de sonde et bon format
  metadata.line.wt <- ll.clean.j$metadata[4] # logger serial nom, en base R
  numbers <- gregexpr("[0-9]+", metadata.line.wt)
  sonde.wt <- data.frame("probe.no" = unlist(regmatches(metadata.line.wt, numbers)), stringsAsFactors = F)
  probe.uid.j <- as.numeric(sonde.wt$probe.no)
  
  # réaliser l'exercice si fichier ll.clean[[j]]$data n'est pas vide
  if (nrow(ll.clean[[j]]$data)!=0) {
    
    # hauteur de nappe capté avec la sonde au moment de la mesure avec le bulleur (dernière mesure)
    last.probe.measure <- tail(ll.clean[[j]]$data, n = 1) 
    water.table.verif[j,1:3] <- data.frame("probe.uid" = probe.uid.j,
                                         "last.probe.measure.cm" = last.probe.measure$calibrated.value.mm/10,
                                         "bulleur.mesure.cm" = unique(ll.bulleur$water.table.depth.cm[ll.bulleur$probe.uid==probe.uid.j]))
    water.table.verif[j,1:3]
    # différence d'une dizaine ?
  } else { # si ll.clean[[j]]$data est vide, mettre NA dans le dataframe
    water.table.verif[j,1:3] <- data.frame("probe.uid" = probe.uid.j,
                                         "last.probe.measure.cm" = NA,
                                         "bulleur.mesure.cm" = NA)
    water.table.verif[j,1:3]
    # différence d'une dizaine ? 
  }
  
}
water.table.verif

# .rs.restartR()





# gossage à  effacer après
# temp.cal <- as.data.frame(read.csv("connectivite/data/raw/level.logger.calibration.all.csv", sep = ";"))
# 
# temp.bull <- as.data.frame(read_excel("~/Desktop/TEMP_level.logger.calibration.all.xlsx",
#           sheet = "Feuil1"))
# str(temp.bull)
# temp.all <- full_join(temp.cal, temp.bull, by = join_by(site.id, lab.probe.id,  probe.uid, well.id), relationship = "many-to-many")
# write_xlsx(temp.all, "~/Desktop/temp.all.xlsx")





## B !INTÉGRER DANS A (QUAND TOUT EST BEAU) !!Donnée issues du HOBO® ----
# B.1 nettoyage
# fonction : modifications automatisées pour chaque fichier issus d'une période de mesures des level loggers

# important de supprimer les objets en mémoire // 
# ou combiner le traitement de données dans la boucle 
# avec des if/else pour traiter différement les HOBO des ODYSSEY
rm(list=ls())

# importer si nécessaire :
# ll.clean <- readRDS("connectivite/data/clean/ll.clean.RDS")
# # obtenu via le script "/scripts/data_water.table.all.R"
# # importer le graphique que topographie
# # fichiers de consigne de données

ll.pre <- list.files("connectivite/data/raw", pattern = "hobo")
ll.clean.k.hobo <- list()

# for (k in 1:length(ll.pre)) {
k<-3
  # import et ménage
  print(k)
  ll.pre[k]
  # ll.pre.0 <- read.csv(paste0("connectivite/data/raw/", ll.pre[k]), sep = "','")
  ll.pre.0 <- readLines(paste0("connectivite/data/raw/", ll.pre[k])); str(ll.pre.0) # lire en format texte
  # ** tz orig mentionnée dans la colonne ll.pre.0.metadata[2], coder pour l'obtenir au besoin
  # Warning message:
  #   In readLines(paste0("connectivite/data/raw/", ll.pre[k])) :
  #   incomplete final line found on 'connectivite/data/raw/20853328_INK_20250106_hobo.csv'
  
  ### création des subsets data & metadata ----
  # notes : les noms réfèrent à l'étape et non à une matrice en particulier, les objets seront remplacés au fil de la boucle. 
  # l'info importante est consignée dans la liste ll.clean.k.hobo[i], à la fin
  ll.pre.0.metadata <-  ll.pre.0[c(1:2)] # inclus les anciens noms de colonnes, qui sont dans un format et un ordre bizzare
  ll.pre.0.data <- ll.pre.0[-c(1:2)]
  str(ll.pre.0.data) # chr

  #### vérification du fichier level logger brut : logger.serial.no == nom du fichier, sinon arrêter TOUT ! ----
  {
    # trouver le probe.uid.i (== probe.uid, logger serial no) dans les metadata
    # texte <- ll.pre.0.metadata[4] # logger serial no, en base R
    # numbers <- grepl("*S/N", ll.pre.0.metadata)
    
    # str_match(with STR1 (.*?) STR2
    texte <- as.data.frame(str_match(ll.pre.0.metadata, "(?s)LGR S/N: \\s*(.*?)\\s*,")) # extraire tout ce qui se trouve
    # entre "LGR S/N: " et la "," directement subséquente, sans savoir s'il y a des sauts de ligne et peu importe les 
    # espaces dans l'énoncé.
    probe.uid.k <- as.numeric(texte[2,2])
    # no du level logger dans le nom du fichier brut (.csv), correspond à l'item "k" de la présente boucle
    texte <- ll.pre[k]
    nombres <- gregexpr("[0-9]+", texte)
    resultat <- regmatches(texte, nombres)
    fichier <- as.numeric(unlist(resultat)[1])
    # test logger.serial.no == nom du fichier
    if(!(probe.uid.k %in% fichier)) { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc "else" statement)
      stop(paste0("Attention, le nom du fichier ne correspond pas au numéro de série du level logger. Fichier problématique : i = ", paste(i), "; ", ll.pre[i]))
    }
    # si problème : aller changer manuellement en utilisant le no de série (unique) inscrit dans le fichier et PAS son nom 
    # ** 1. créer copie -> archive; 2. s'assurer de changer partout ** : QGIS, fichier, onglet, data_site.id
  }
  
  #### création du dataframe level legger (ll) contenant données de nappe phréatique (NP) et ménage  ----
  ll.pre.0.data.0 <- read.csv(text = ll.pre.0.data, header = F, col.names = c("scan.id", "date.JJ.MM.AAAA_time.HH.MM.SS",	"raw.value.kPa_pres.abs",	"temperature_dC", "Coupleur détaché", "Coupleur attaché", 'Hôte connecté',	"Arrêté", "Fin de fichier")) # text = argument de read.csv qui lit la valeur contenue dans l'objet / DATE mauvais format
  ll.pre.0.data.1 <- ll.pre.0.data.0[1:4] # garder seules les colonnes pertinentes
  
  #### date et heure : format ISO date AAAA-MM-JJTHH:MM:SS,ss-/+FF:ff, voir https://fr.wikipedia.org/wiki/ISO_8601 ----
  # heure : « Z » à la fin lorsqu’il s’agit de l’heure UTC. (« Z » pour méridien zéro, aussi connu sous le nom « Zulu » dans l’alphabet radio international).
  # extraction : nom du site pour trouver les coordonnées qui serviront à connaître le fuseau horaire
  site.name.pre <- sub("Titre de tracé : ","",ll.pre.0.metadata[1])
  site.name <- stringr::str_to_title(gsub(",", "", site.name.pre))
  
  # ouvrir données du shapefile pour accéder les zones
  zones <- read_sf("~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp")
  zones <- as.data.frame(zones)
  head(zones); str(zones)
  
  # extraire la bonne lat, long selon le nom du site
  coords <- c(zones$latitude[zones$site==site.name], zones$longitude[zones$site==site.name])
  
  # trouver le UTC selon la lat long
  (tz <- tz_lookup_coords(coords[1], coords[2], method = "fast", warn = FALSE))
  
  #### ménage de la date et heure  ----
  # coller le tz dans la colonne "date.JJ.MM.AAAA_time.pre.HH.MM.SS"
  ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS_tz <- paste0(ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS, " ", tz)
  ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS_tz <- gsub("00:00", "00:01", ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS_tz) # sinon, les données 00:00:00 étaient effacées !
  ll.pre.0.data.1$date.time.tz.orig <- readr::parse_datetime(ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS_tz, format = '%m/%d/%y %I:%M:%S %p %Z', locale = readr::locale(tz = tz)) # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
  ll.pre.0.data.2 <- data.frame(separate_wider_position(ll.pre.0.data.1, # date et time en deux colonnes (idem à ODYSSEY)
                                                        widths = c("date.AAAA.MM.JJ" = 11, "time.HH.MM.SS" = 8),
                                                        cols = date.time.tz.orig, cols_remove = F)) 
  ll.pre.0.data.2$`date.AAAA-MM-JJ` = ymd(ll.pre.0.data.2$date.AAAA.MM.JJ, tz = tz)
  ll.pre.0.data.2$date.time.UTC.0pre <- with_tz(ll.pre.0.data.2$date.time.tz.orig, tz = "UTC") # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
  tz(ll.pre.0.data.2$date.time.UTC.0pre) # GMT = UTC
  ll.pre.0.data.2$date.time.UTC.0pre.1 <- format_iso_8601(ll.pre.0.data.2$date.time.UTC.0pre)
  ll.pre.0.data.2$date.time.UTC.0 <- gsub("[+]00:00", "Z",  ll.pre.0.data.2$date.time.UTC.0pre.1)
  
  # ajouter colonne vide "calibrated value" à l'instar de ODYSSEY, où sera inséré la valeur finale de nappe phréatique
  ll.pre.0.data.2$"calibrated.value.mm" <- rep(NA, times = nrow(ll.pre.0.data.2))
  colnames(ll.pre.0.data.2); head(ll.pre.0.data.2); str(ll.pre.0.data.2)
  
  # vérifications
  colnames(ll.pre.0.data.2); head(ll.pre.0.data.2); str(ll.pre.0.data.2) # date et heure ne sont pas sous forme POSIX -> changer dans la section "### date et heure"
  # nouveau nom préliminaire (et retirer colonnes inutiles)
  ll.pre.0.data.3 <- ll.pre.0.data.2 %>% select(!c(date.JJ.MM.AAAA_time.HH.MM.SS, date.AAAA.MM.JJ,  "date.time.UTC.0pre", "date.time.UTC.0pre.1")) %>% 
    select("scan.id", "date.JJ.MM.AAAA_time.HH.MM.SS_tz", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0", 
           "raw.value.kPa_pres.abs", "temperature_dC", "calibrated.value.mm")
  head(ll.pre.0.data.3); str(ll.pre.0.data.3)
  # suite :
  # si calibration intégrée avec le hobo, QUE FAIRE ? coder ici, voir procédure avec ODYSSEY
  
  #### début et fin des mesures ----
  # inscrits dans "level.logger.calibration.all.csv" (début (généralement) = installation + 24h de rabattement de la NP 
  # (ou non, si puits intallé d'avance, dans quel cas inscrire début officiel - 24h), fin = heure de retrait)
  # note : données de date en format xlsx ça lit TOUT CROCHE, transformé en csv fonctionne bien
  cal.data <- read.csv("connectivite/data/raw/level.logger.calibration.all.csv", sep = ";")
  # transformer en format ISO 8601, UTC +0 pour comparaison
  cal.data$day.begining.aaaa.mm.dd.hh.00.01 <- gsub("[+]00:00", "Z",  format_iso_8601(parse_iso_8601(cal.data$day.begining.aaaa.mm.dd.hh.00.01, default_tz = tz)))
  cal.data$day.end.aaaa.mm.dd.hh.00.01 <-  gsub("[+]00:00", "Z",  format_iso_8601(parse_iso_8601(cal.data$day.end.aaaa.mm.dd.hh.00.01, default_tz = tz)))
  head(cal.data); tail(cal.data); str(cal.data)
  
  
  # idée 1.
  # entrer dans un ifelse, 
  # si 1 -> code ci-dessous
  # si >1 -> coder pour dire que
  # cal.data$day.begining.aaaa.mm.dd.hh.00.01[cal.data$probe.uid == probe.uid.k][1] avec
  # cal.data$day.end.aaaa.mm.dd.hh.00.01[cal.data$probe.uid == probe.uid.k][1]
  # toujours séquentiellement, car dans l'ordre de lecture des lignes du fichier original
  
  # idée 2
  # loop -> de 1 : nombre de fois ou probe est répété dans le df 
  # de toute façon, tout y sera entré au fil du temps, les mm NO seront constamment réutilisés, 
  #  l'important, c'est que ce soit évalué LIGNE PAR LIGNE
  # loop où j'extrait le df de UNE ligne séquentiellement
  # où begingin et end sont évalués tel que ci-dessous
  
  # PROBLÈME ICI CAR +DE 1 PROBE UID POUR CERTAINS
  
  
  
  
  # recoupage de ll.pre.data selon cal.data selon début et fin des mesures et retrait de colonnes
  ll.pre.0.data.4 <- ll.pre.0.data.3 %>% 
    dplyr::filter(date.time.UTC.0 >= # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data, trouvé dans la ligne dont la ll.pre.2.data.2$probe.uid == à la cal.data$probe.uid.i
                    unique(na.omit(cal.data$day.begining.aaaa.mm.dd.hh.00.01[cal.data$probe.uid == probe.uid.k]))) %>% 
    dplyr::filter(date.time.UTC.0 <= # <= date de mesure de NP plus petite ou égale à la date end dans cal.data [...], recoupe tous les jours entre la récupération des sondes et leur mise en arrêt
                    unique(na.omit(cal.data$day.end.aaaa.mm.dd.hh.00.01[cal.data$probe.uid == probe.uid.k]))) %>% # enlever les vielles colonnes
    select("scan.id", "raw.value.kPa_pres.abs", "calibrated.value.mm",  "temperature_dC", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0") # date et time sans "UTC.0" sont dans le fuseau horaire d'origine (tz trouvé en croisant les coordonnées "coords")
  # vérifications
  head(ll.pre.0.data.4); colnames(ll.pre.0.data.4)
  
  # changer pour un nom explicite, fichier encore à calibrer (d'où "pre")
  ll.cal.pre.k <- ll.pre.0.data.4
  
  ### calcul de calibration  ----
  # * avec HOBO, calibration est faite selon une station météorologique *
  # Référence : Jutras et Bourgault, 2024, Version 2.0, section 7 (/Users/Aliz/Documents/Doctorat/_Connectivité/Protocoles (dossiers copiés du serveur A'24)/Leveloggers & Hauteur nappe phréatique/_HOBO_Protocole de mesure de nappe_2024-11-01_NE PAS DIFFUSER.docx)

  
  
  
  
  
  # RENDUE ICI
  # RENDUE ICI
  
  # quoi faire quand j'ai plusieurs début et fins pour le mm hobo ?
  
  
  
  
  
  #### extraction des données de ECCC/CCCS et ménage ----
  # transformer eccc.data avec le mm format de colonne que ll.cal.pre.k 0$date.time.tz.orig
  station_ids <- unique(cal.data$cal.station_id[cal.data$probe.uid == probe.uid.k]) # pas grave ici si plusieurs probe.uid
  
  
  
  
  
  # PROBLÈME ICI CAR +DE 1 PROBE UID
  
  
  
  
  eccc.data.pre <- weather_dl(station_ids, start = cal.data$day.begining.aaaa.mm.dd.hh.00.01[cal.data$probe.uid == probe.uid.k], end = cal.data$day.end.aaaa.mm.dd.hh.00.01[cal.data$probe.uid == probe.uid.k])
  colnames(eccc.data.pre) <- paste0(colnames(eccc.data.pre), ".wc") # ajout de ".wc" pour identifier les colonnes issues de WeatherCan

  ##### inscrire le time zone (tz) dans la colonne time (équivalent à "date.time.tz.orig.pre") ----
  # trouver la station météorologique canadienne à moins de 25km de distance (inscrire manuellement dans "data/raw/ll.calibration.all.csv")
  station_tz.pre <- stations_search(coords = c(zones$latitude[zones$site == site.name],
                                               zones$longitude[zones$site == site.name]), dist = 25)
  station_ids <- cal.data$cal.station_id[cal.data$probe.uid == probe.uid.k]
  station_tz.pre.1 <- stations_search(unique(station_tz.pre$station_name[station_tz.pre$station_id == station_ids]))
  station_tz <- unique(station_tz.pre.1$tz) # OlsonNames() compatible
  
  # à l'aide de la tz, ajouter une seconde (idem aux infos temporelles dans ll.cal.pre.k)
  eccc.data.pre$date.time.tz.orig.wc <- force_tz(eccc.data.pre$time.wc, tz = station_tz) + 1 # ajout d'une seconde, sinon, les données 00:00:00 étaient effacées !
  # eccc.data.pre$date.time.tz.orig <- force_tz(eccc.data.pre$time, tz = "America/Moncton") + 1
  
  eccc.data.pre <- eccc.data.pre %>% select(date.time.tz.orig.wc, time.wc, everything())
  
  # convertir au bon format de date et manip de colonnes (idem aux infos temporelles dans ll.cal.pre.k) / date.time.UTC selon norme iso
  eccc.data.pre.1 <- eccc.data.pre %>%
    mutate(date.time.UTC.0.pre = with_tz(ymd_hms(eccc.data.pre$date.time.tz.orig.wc, tz = tz), tzone = "GMT")) # les heures sont ainsi ramenées à UTC +0 / ceci écrase la colonne du mm nom
  head(eccc.data.pre.1$date.time.UTC.0.pre) # ok ici
  
  
  eccc.data.pre.2 <- eccc.data.pre.1 %>%  # enlever l'espace entre date et heure (ISO 8601)
    mutate(date.time.UTC.0.pre.1 = str_replace(eccc.data.pre.1$date.time.UTC.0.pre, " ", "T")) %>% 
    select(date.time.tz.orig.wc, time.wc, date.time.UTC.0.pre, date.time.UTC.0.pre.1, everything())
  head(eccc.data.pre.2$date.time.UTC.0.pre.1) # ok ici
  # head(eccc.data.pre$time.wc)
  # head(eccc.data.pre$date.time.tz.orig.wc) # alors que ici, 
  
  eccc.data.pre.2$date.time.UTC.0 <- str_replace_all(eccc.data.pre.2$date.time.UTC.0.pre.1, "00:01","00:01Z") # ajouter le Z à la fin (ISO 8601)
  eccc.data <- eccc.data.pre.2 %>% select(`date.AAAA-MM-JJ` = "date.wc", time.wc, date.time.tz.orig.wc, date.time.UTC.0, everything()) %>% select(!c(date.time.UTC.0.pre, date.time.UTC.0.pre.1))
  eccc.data <- as.data.frame(eccc.data)
  head(eccc.data); str(eccc.data); class(eccc.data)
  
  # # # vérif pour le join, il faut que la sytaxe soit exactement la mm entre les deux df
  # c(class(eccc.data[60,]$date.time.UTC.0), class(ll.cal.pre.k[50,]$date.time.UTC.0))
  # c(as.character(eccc.data[60,]$date.time.UTC.0), as.character(ll.cal.pre.k[50,]$date.time.UTC.0))
  # c(eccc.data[60,]$date.time.UTC.0, ll.cal.pre.k[50,]$date.time.UTC.0)
  # eccc.data[60,]$date.time.UTC.0 == ll.cal.pre.k[50,]$date.time.UTC.0 # -> doit renvoyer T

  #### assembler données du HOBO et données de ECCC/CCCS selon la date et l'heure ----
  eccc.cal.data <- left_join(ll.cal.pre.k, eccc.data, by = join_by(date.time.UTC.0)) %>% 
    select("scan.id", "date.time.UTC.0","raw.value.kPa_pres.abs", "temperature_dC", "calibrated.value.mm",
           `date.AAAA-MM-JJ` = "date.AAAA-MM-JJ.x", "time.HH.MM.SS", `date.time.tz.orig`,
           "date.time.tz.orig.wc", "station_name.wc", pressure.kPa.wc = "pressure.wc", everything()) %>% 
    select(!c(`date.AAAA-MM-JJ.y`, "time.wc"))
  colnames(eccc.cal.data)
  colnames(left_join(ll.cal.pre.k, eccc.data, by = join_by(date.time.UTC.0)) )
  
  
  
  
  
  # SUITE ICI

  
  }




# SUPPRIMER APRÈS (?) 
# modifier mes colonnes pour avoir le format ISO 
head(ll.pre.0.data.3)
class(ll.pre.0.data.3$`date.AAAA-MM-JJ`)

# [1] "POSIXct" "POSIXt" 
head(ll.clean[[1]]$data, n = 1) # format à atteindre
# scan.id raw.value.mm calibrated.value.mm date.AAAA-MM-JJ time.HH.MM.SS   date.time.tz.orig      date.time.UTC.0
# 25         2884        544.4089            2024-06-12      15:00:00     2024-06-12 15:00:00   2024-06-12T19:00:01Z
class(ll.clean[[1]]$data$date.time.tz.orig)
# [1] "POSIXct" "POSIXt" 
colnames(ll.clean[[1]]$data)
# "scan.id", "raw.value.mm", "calibrated.value.mm", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0"
