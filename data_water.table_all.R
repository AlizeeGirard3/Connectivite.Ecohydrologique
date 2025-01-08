#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                 Water table, data extraction from raw probe files
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
###########################################################################
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

###########################################################################-

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


# 1. Donnée issues du Capacitance Water Level Logger Odyssey® ----


# CHANTIER : il se pourrait que les données du HOBO ou autre nécessitent un autre code

# Boucle principale ----
# fonction : modifications automatisées pour chaque fichier issus d'une période de mesures des level loggers

# fichiers de consigne de données ----
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
    probe.serial.no.i <- as.numeric(unlist(result))
    # no du level logger dans le nom du fichier brut (.csv), correspond à l'item "i" de la présente boucle
    texte <- ll.pre[i]
    numbers <- gregexpr("[0-9]+", texte)
    result <- regmatches(texte, numbers)
    fichier <- as.numeric(unlist(result))
    # test logger.serial.no == nom du fichier
    if(!(probe.serial.no.i %in% fichier)) { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc "else" statement)
      stop(paste0("Attention, le nom du fichier ne correspond pas au numéro de série du level logger. Fichier problématique : i = ", paste(i), "; ", ll.pre[i]))
    }
    # si problème : aller changer manuellement en utilisant le no de série (unique) inscrit dans le fichier et PAS son nom 
    # ** 1. créer copie -> archive; 2. s'assurer de changer partout ** : QGIS, fichier, onglet, data_site.id
  }

    # création du dataframe level legger (ll) contenant données de nappe phréatique (NP) et ménage  ----
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
# Date et heure : format ISO date AAAA-MM-JJTHH:MM:SS,ss-/+FF:ff, voir https://fr.wikipedia.org/wiki/ISO_8601 ----
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
   head(ll.pre.2.data.2) # différence de 4 heures
  
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
  # ll.pre.2.data.3 <- ll.pre.2.data.2 %>% dplyr::filter(date.time.UTC.0pre.1 >= cal.data$day.begining.aaaa.mm.dd.hh.00.01[cal.data$probe.serial.no == probe.serial.no.i][1]) # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data, trouvé dans la ligne dont la ll.pre.2.data.2$probe.serial.no == à la cal.data$probe.serial.no.i
  # nrow(ll.pre.2.data.2) - nrow(ll.pre.2.data.3) # différence de ~ 27  lignes, soit 27 heures (pourquoi pas 24h ? mais bon, pas grave Maryann me disait qu'elle retranche jusqu'à 48h)
  # ll.pre.2.data.4 <- ll.pre.2.data.3 %>% dplyr::filter(date.time.UTC.0pre.1 <= cal.data$day.end.aaaa.mm.dd.hh.00.01[cal.data$probe.serial.no == probe.serial.no.i][1]) # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data, trouvé dans la ligne dont la ll.pre.2.data.2$probe.serial.no == à la cal.data$probe.serial.no.i
  # nrow(ll.pre.2.data.3) - nrow(ll.pre.2.data.4) # différence de ~ 27  lignes, soit 27 heures (pourquoi pas 24h ? mais bon, pas grave Maryann me disait qu'elle retranche jusqu'à 48h)
  ll.pre.2.data.3 <- ll.pre.2.data.2 %>% 
    dplyr::filter(date.time.UTC.0pre.1 >= # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data, trouvé dans la ligne dont la ll.pre.2.data.2$probe.serial.no == à la cal.data$probe.serial.no.i
                  unique(na.omit(cal.data$day.begining.aaaa.mm.dd.hh.00.01[cal.data$probe.serial.no == probe.serial.no.i]))) %>% 
    dplyr::filter(date.time.UTC.0pre.1 <= # <= date de mesure de NP plus petite ou égale à la date end dans cal.data [...], recoupe tous les jours entre la récupération des sondes et leur mise en arrêt
                    unique(na.omit(cal.data$day.end.aaaa.mm.dd.hh.00.01[cal.data$probe.serial.no == probe.serial.no.i])))
                            
    #  SUPPRIMER
  # CI DESSOUS : [1] pas bon, si NA ça enlève toutes les données
  # plutôt demander : lequel n'égale pas NA
  #   # archive
  #   dplyr::filter(date.time.UTC.0pre.1 >= # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data, trouvé dans la ligne dont la ll.pre.2.data.2$probe.serial.no == à la cal.data$probe.serial.no.i
  #                   cal.data$day.begining.aaaa.mm.dd.hh.00.01[cal.data$probe.serial.no == probe.serial.no.i][1]) %>% 
  # dplyr::filter(date.time.UTC.0pre.1 <= # <= date de mesure de NP plus petite ou égale à la date end dans cal.data [...], recoupe tous les jours entre la récupération des sondes et leur mise en arrêt
  #                 cal.data$day.end.aaaa.mm.dd.hh.00.01[cal.data$probe.serial.no == probe.serial.no.i][1]) 

  ll.pre.2.data.3 <- ll.pre.2.data.3 %>%   # enlever l'espace entre date et heure (ISO 8601)
    mutate(date.time.UTC.0pre.2 = str_replace(ll.pre.2.data.3$date.time.UTC.0pre.1, " ", "T"))
  ll.pre.2.data.3$date.time.UTC.0 <- str_replace_all(ll.pre.2.data.3$date.time.UTC.0pre.2, "00:01","00:01Z") # ajouter le Z à la fin (ISO 8601)
  ll.pre.2.data.3 <- ll.pre.2.data.3 %>% select(!c("date.JJ.MM.AAAA", "date.time.UTC.0pre.1", "date.time.UTC.0pre.2")) %>% # enlever les vielles colonnes
    select("scan.id", "raw.value.mm", "calibrated.value.mm", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0") # date et time sans "UTC.0" sont dans le fuseau horaire d'origine (tz trouvé en croisant les coordonnées "coords")
  head(ll.pre.2.data.3)
  colnames(ll.pre.2.data.3)
  }
  
# changer pour un nom explicite
  ll.cal.pre <- ll.pre.2.data.3
  
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
} else { saveRDS(ll.clean, file = "connectivite/data/clean/ll.clean.RDS") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }

## 1.1 ----
# Examination des données
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






# 2. Données de vérification/calibration avec bulleur ----
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
  probe.serial.no <- as.numeric(sonde.wt$probe.no)
  
  # réaliser l'exercice si fichier ll.clean[[j]]$data n'est pas vide
  if (nrow(ll.clean[[j]]$data)!=0) {
    
    # hauteur de nappe capté avec la sonde au moment de la mesure avec le bulleur (dernière mesure)
    last.probe.measure <- tail(ll.clean[[j]]$data, n = 1) 
    water.table.verif[j,1:3] <- data.frame("probe.serial.no" = probe.serial.no,
                                         "last.probe.measure.cm" = last.probe.measure$calibrated.value.mm/10,
                                         "bulleur.mesure.cm" = unique(ll.bulleur$water.table.depth.cm[ll.bulleur$probe.serial.no==probe.serial.no]))
    water.table.verif[j,1:3]
    # différence d'une dizaine ?
  } else { # si ll.clean[[j]]$data est vide, mettre NA dans le dataframe
    water.table.verif[j,1:3] <- data.frame("probe.serial.no" = probe.serial.no,
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
# temp.all <- full_join(temp.cal, temp.bull, by = join_by(site.id, lab.probe.id,  probe.serial.no, well.id), relationship = "many-to-many")
# write_xlsx(temp.all, "~/Desktop/temp.all.xlsx")

