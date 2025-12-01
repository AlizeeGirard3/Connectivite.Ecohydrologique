#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                 Water table, data extraction from raw probe files
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2024-12-09
# Date mise à jour : 2025-03-27
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
# version avec la calibration corrigée

# LEXIQUE :
# SNH : sonde de niveau hydrostatique / synonymes : LL : level logger; sonde, probe
# NP : Nappe phréatique / synonymes : water table
# ECCC/CSSS : Environnement and Climate Change Canada / Canadian Centre for Climate Services 
# tz : time zone, syn. fuseau horaire
# lettres de l'alphabet : i, j, k, l -> boucles / a, b, x et y -> équations mathématiques
# 15 janvier : lettres de boucle UTILISÉES : i, j, k, l, m et n (en désordre), où 
# i,k et l dans A.1; j dans A.2; m et n dans A.3
# cal.data et ll.bulleur, syn. connectivite/data/raw/level_logger_calibration_all.csv
# pattern universel d'appellation des fichiers de SNH : probe.uid_site.uid_datedextraction_probe.brand.csv
##########################################################################-

# fichiers "R data serialized" (RDS) à charger directement
# ll.clean<-readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/ll.clean.RDS") # issu de section A.1 du code ci-présent

# .rs.restartR()
source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/general.scripts/scripts/fonctions_generales.R")
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

# Librairies ----
if (!require("conflicted")) install.packages("conflicted") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("readxl")) install.packages("readxl") # lire les excel
if (!require("openxlsx")) install.packages("openxlsx") # lire les excel
if (!require("stringr")) install.packages("stringr") # gosser avec des suites de caractères, str_replace, [...]
if (!require("tidyverse")) install.packages("tidyverse") # gosser avec des suites de caractères, str_replace, [...]
# if (!require("dplyr")) install.packages("dplyr") # entre autres : left_join()
# if (!require("tidyr")) install.packages("tidyr") # entre autres : extract_numeric() / extract_numeric() is deprecated: please use readr::parse_number() instead
# contient purr aussi
if (!require("sf")) install.packages("sf"); if (!require("lutz")) install.packages("lutz") # GIS in R
if (!require("lubridate")) install.packages("lubridate")
options(lubridate.verbose = TRUE) # pour expliciter ce que les fonctions font
# if (!require("naniar")) install.packages("naniar") # Checking data completeness
# if (!require("mapview")) install.packages("mapview") ## Spatial analyses
if (!require("parsedate")) install.packages("parsedate") # lire les excel
# option d'arrêter le code si message d'erreur (source fonctions.R)
# options(error=pause)
# options(error=NULL) # annuler
# archives supprimer quand ça fonctionne : 
  # librairies de weathercan
  # if (!require("weathercan")) install.packages("weathercan") # Integrating data from weathercan (ECCC/CCCS), Gouvernement du Canada
  # stations_dl()
  # stations_meta()

# A  Données issues des sonde de niveau hydrostatique ----
SNH <- as.vector(c("_odyssey", "_hobo"), mode = "character") # liste des types de SNH avec lesquelles j'ai pris des données; chaque "marque" est traitée de façon différente

# A.1 nettoyage et enregistrement en RDS ----
# fonction : modifications automatisées pour chaque fichier issus d'une période de mesures des level loggers

# fichiers de consigne de données
ll.pre <- list.files("connectivite/data/raw", pattern = "_odyssey|_hobo") # mettre dans "pattern" tous les ID de SNH listés dans l'objet SNH
tidy.WTD.data <- list() # équivalent à ll.clean (ancien)
fichier.uid.df <- data.frame(fichier.uid = NA, file.name = NA, probe.uid = NA, "extraction.donnees.aaaammjj" = NA, "tz_orig" = NA) # pour stocker les fichier.uid (aussi première colonne de cal.data) et autres données intérimaires
odyssey_offset_archives <- data.frame(fichier.uid = NA, offset_cm_date = NA, a.slope_excel = NA,	b.verticalIntercept = NA) #, `prof_nappe_bulleur_cm_plus.out` = NA, pre_prof_nappe_odyssey_mm_to_cm = NA,	`prof_nappe_odyssey_cm_plus.out` = NA)
for (i in 1:length(ll.pre)) {
  # i<-13 9 10 11 12 13 14 15 16# 41362(27 mars 2025)
  print(i)
  ll.pre[i] # début de la loop pour les ODYSSEY (if() prochaine ligne)
  if (grepl(SNH[1], ll.pre[i])) {  # début de la loop pour les ODYSSEY
    # import et ménage
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
    # création du fichier.uid.i, nom unique du FICHIER qui ne pourra JAMAIS être dupliqué (utila dans seciton début et fin des mesures par périodes, pour un mm FICHIER)
    fichier.uid.i <- paste0(unlist(result)[1], "_", unlist(result)[2]) # ceci sera écrasé à la prochaine itération
    fichier.uid.df[i,1:4] <- c(paste0(unlist(result)[1], "_", unlist(result)[2]), ll.pre[i], probe.uid.i, as.numeric(unlist(result)[2])) # ceci sera gardé en mémoire (doit être identique à la colonne fichier.uid dans cal.data)
    # ajouts aux métadonnées des fichiers
    ll.pre.2.metadata[10:13] <- c(paste0("fichier.uid : ", unlist(result)[1], "_", unlist(result)[2]), paste0('file.name : ', "`", ll.pre[i], "`"), 
                                  paste0("probe.uid : ", probe.uid.i), paste0("date d'extraction des données : ", as.numeric(unlist(result)[2])))
    class(ll.pre.2.metadata)
    ### création du dataframe level legger (ll) contenant données de nappe phréatique (NP) et ménage  ----
    ll.pre.2.data.1 <- read.csv(text = ll.pre.2.data, col.names = c("scan.id", "date.JJ.MM.AAAA", "time.HH.MM.SS",'raw.value.mm',"calibrated.value.cm")) # text = argument de read.csv qui lit la valeur contenue dans l'objet / DATE mauvais format
    
    # vérifications
    head(ll.pre.2.data.1, n=20); str(ll.pre.2.data.1)
        # si deux colonnes raw et calibrated sont exactement les mêmes, c'est qu'il n'y a pas eu de calibration; supprimer les valeurs doublons et/ou 
    # les remplacer par les bonnes valeur calibrées (calcul à faire)
    ll.pre.2.data.1$calibrated.value.cm <- ifelse(ll.pre.2.data.1$raw.value.mm == ll.pre.2.data.1$calibrated.value.cm, yes = ll.pre.2.data.1$calibrated.value.cm[rep("NA", times = length(ll.pre.2.data.1$calibrated.value.cm))], no = ll.pre.2.data.1$calibrated.value.cm)
    head(ll.pre.2.data.1, n=20) ; str(ll.pre.2.data.1)
    
    ### date et heure : format ISO date AAAA-MM-JJTHH:MM:SS,ss-/+FF:ff, voir https://fr.wikipedia.org/wiki/ISO_8601 ----
    # heure : « Z » à la fin lorsqu’il s’agit de l’heure UTC. (« Z » pour méridien zéro, aussi connu sous le nom « Zulu » dans l’alphabet radio international).
    # extraction : nom du site pour trouver les coordonnées qui serviront à connaître le fuseau horaire
    site.name.pre <- sub("SiteName","",ll.pre.2.metadata[1])
    site.name <- stringr::str_to_title(gsub(",", "", site.name.pre))
    
    # ouvrir données du shapefile pour accéder les zones
    zones <- read_sf("~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp")
    zones <- as.data.frame(zones)
    head(zones); str(zones)
    
    # extraire la bonne lat, long selon le nom du site
    # coords <- c(zones$latitude[zones$site==site.name], zones$longitude[zones$site==site.name])
    coords <- c(zones$latitude[zones$site==site.name][1], zones$longitude[zones$site==site.name][1])
    
    # trouver le UTC selon la lat long
    (tz <- tz_lookup_coords(coords[1], coords[2], method = "fast", warn = FALSE))
    # ajouts aux métadonnées des fichiers
    fichier.uid.df[i,5] <- tz
    ll.pre.2.metadata[14] <- paste0("original time zone : ", tz)
    
    #### ménage de la date et heure  ----
    # modifier mes colonnes pour avoir le format ISO (manque encore le UTC à ajouter à la fin)
    # garder date.AAAA-MM-JJ"
    ll.pre.2.data.2 <- ll.pre.2.data.1 %>% dplyr::mutate(date.JJ.MM.AAAA_time.HH.MM.SS_tz = paste0(date.JJ.MM.AAAA," ", time.HH.MM.SS, " ", tz)) %>% 
      dplyr::select(!c(date.JJ.MM.AAAA, time.HH.MM.SS)) # supprimer ces colonnes en format character, recréer bientôt en POSIX
    ll.pre.2.data.2$date.JJ.MM.AAAA_time.HH.MM.SS_tz <- gsub("00:00", "00:01", ll.pre.2.data.2$date.JJ.MM.AAAA_time.HH.MM.SS_tz) # sinon, les données 00:00:00 étaient effacées !
    ll.pre.2.data.2$date.JJ.MM.AAAA_time.HH.MM.SS_tz <- gsub("24:00:", "00:00:", ll.pre.2.data.2$date.JJ.MM.AAAA_time.HH.MM.SS_tz)
    ll.pre.2.data.2$date.time.tz.orig <- readr::parse_datetime(ll.pre.2.data.2$date.JJ.MM.AAAA_time.HH.MM.SS_tz, format = '%d/%m/%Y %H:%M:%S %Z', locale = readr::locale(tz = tz)) # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
    ll.pre.2.data.3 <- data.frame(separate_wider_position(ll.pre.2.data.2, # date et time en deux colonnes (idem à ODYSSEY)
                                                          widths = c("date.AAAA.MM.JJ" = 11, "time.HH.MM.SS" = 8),
                                                          cols = date.time.tz.orig, cols_remove = F)) 
    # ll.pre.2.data.2$date.JJ.MM.AAAA_time.HH.MM.SS_tz <- gsub("24:00:", "00:00:", ll.pre.2.data.2$date.JJ.MM.AAAA_time.HH.MM.SS_tz)
    ll.pre.2.data.3$`date.AAAA-MM-JJ` = ymd(ll.pre.2.data.3$date.AAAA.MM.JJ, tz = tz)
    ll.pre.2.data.3$date.time.UTC.0pre <- with_tz(ll.pre.2.data.3$date.time.tz.orig, tz = "UTC") # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
    tz(ll.pre.2.data.3$date.time.UTC.0pre) # UTC
    ll.pre.2.data.3$date.time.UTC.0pre.1 <- format_iso_8601(ll.pre.2.data.3$date.time.UTC.0pre)
    ll.pre.2.data.3$date.time.UTC.0 <- gsub("[+]00:00", "Z",  ll.pre.2.data.3$date.time.UTC.0pre.1)
    ll.pre.2.data.3$date.time.tz.orig <- gsub("00:00:01", "24:00:01", ll.pre.2.data.3$date.time.tz.orig) # rechanger les 00:00:01 dans date.time.tz.orig pour ne pas perdre des lignes (7 avril 2025)
    # tel que codé actuellement, il peut y avoir un décalage de +/- une heure à cause que TZ prend l'heure basée sur Sys.timezone, qui dépend de l'heure d'été ou d'hiver
    # ARRANGER UN JOUR (langage C++ pour plus de complications) 
    # ou alors setter cette date manuellement (voir à chaque année la date de changement d'heure)
    # Sys.timezone(location = F) essayé, n'aide pas
    
    # vérifications
    colnames(ll.pre.2.data.3); head(ll.pre.2.data.3, n=35); str(ll.pre.2.data.3) # date et heure ne sont pas sous forme POSIX -> changer dans la section "### date et heure"
    # nouveau nom préliminaire (et retirer colonnes inutiles)
    ll.pre.2.data.4 <- ll.pre.2.data.3 %>% dplyr::select(!c(date.AAAA.MM.JJ,  "date.time.UTC.0pre", "date.time.UTC.0pre.1")) %>% 
      dplyr::select("scan.id", "raw.value.mm", "calibrated.value.cm", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", date.time.UTC.0) # date et time sans "UTC.0" sont dans le fuseau horaire d'origine (tz trouvé en croisant les coordonnées "coords")
    head(ll.pre.2.data.4); str(ll.pre.2.data.4)
    
    #### début et fin des mesures par fichier.uid.i ----
    # inscrits dans "level_logger_calibration_all.csv"
    # début (généralement) = installation + 48h de rabattement de la NP / ou non, si puits intallé d'avance, dans quel cas inscrire début officiel - 48h)
    # fin = heure de retrait
    # note : données de date en format xlsx ça lit TOUT CROCHE, transformé en csv fonctionne bien
    
    ##### import et nettoyage ----
    cal.data.pre <- read.csv("connectivite/data/raw/level_logger_calibration_all.csv", sep = ";", dec = ",")
    str(cal.data.pre)
    # cal.data <- cal.data.pre %>% mutate_at(c("pre_prof_nappe_odyssey_mm_to_cm", "prof_nappe_bulleur_cm_plus.out", "prof_nappe_odyssey_cm_plus.out",
    #                                                  "last_offset_cm"), as.numeric) %>% mutate_at("probe.uid", as.character)
    cal.data <- cal.data.pre %>% mutate_at("probe.uid", as.character)
    
    # cal.data$last_offset_date <- as.numeric(cal.data$last_offset_date)
    colnames(cal.data); str(cal.data)
    
    # out = (pt haut - moyenne pt bas)
    cal.data$out.R = round(cal.data$pt.haut.cm - ((cal.data$pt.bas1.cm+cal.data$pt.bas2.cm+cal.data$pt.bas3.cm)/3), digits = 1)
    # long négative en mm = cal.length.cm*-10
    cal.data$long_negative_cal.length_mm_y <- cal.data$cal.length.cm*-10 # longueur de fil nécessaire : en mm et au négatif / les NA seront calculé prochainement
    
    cal.data <- cal.data %>% dplyr::select("fichier.uid","measure_type", "measure_status", "site.uid", "well.uid", "trmnt.uid", "lab.probe.id", "probe.uid", "probe.brand", 
                                    "cal.length.cm", "cal.order", "long_negative_cal.length_mm_y", "cal.value_x", "comment", 
                                    "day.begining.aaaa.mm.dd.hh.mm", "day.end.aaaa.mm.dd.hh.mm", "distance.m", "out.R", "out.long.tuyau.sol.cm", everything()) #, -"caduque.long.fil.cm")
    cal.data$period.fichier.uid <- paste0(cal.data$day.begining.aaaa.mm.dd.hh.mm, "--", cal.data$day.end.aaaa.mm.dd.hh.mm, ".",cal.data$fichier.uid)
    
    # cal.data$out.R[1] <- 2 # tester si une valeur FALSE, if ci-dessous devrait donner un avertissement
    # vérification de valeurs OUT
    cal.data$out.R
    round(cal.data$out.long.tuyau.sol.cm, digits = 1)
    if(all(cal.data$out.R == round(cal.data$out.long.tuyau.sol.cm, digits = 1), na.rm = T))  { # si TOUS TRUE (fonction any()) = changer nom de out.R et supprimer la mesure entrée manuellement // si FALSE = avertissement
      cal.data$out.long.tuyau.sol.cm <- cal.data$out.R
      cal.data <- cal.data %>% dplyr::select(!out.R) # out.R DISPARAÎT ! NE PLUS LA CHERCHER !
    } else { stop("Attention, le out entré dans cal.data (syn. level_logger_calibration_all.csv) n'est pas identique à la moyenne des points bas soustraite du point haut du puits.") }
    # format POSIX begining et end
    cal.data$day.begining.aaaa.mm.dd.hh.mm <- ymd_hm(cal.data$day.begining.aaaa.mm.dd.hh.mm, tz = tz)
    cal.data$day.end.aaaa.mm.dd.hh.mm <- ymd_hm(cal.data$day.end.aaaa.mm.dd.hh.mm, tz = tz)
    
    head(cal.data); tail(cal.data); str(cal.data)
    
    ##### boucle de concaténation des données (fichier.uid ensemble, sinon autre calibration et graphique distinct) ----
    # raison de l'étape : si sonde retirée et remise, sans écraser les données contenues (continuation des mesures), retirer la période 
    # de données invalides (quelques heures, période de rééquilibrage) et recoller les lignes ensemble pour former le fichier d'heures valides
    ll.cal.pre.i.l <- list()
    for (l in 1:length(unique(cal.data$period.fichier.uid[which(grepl(fichier.uid.i, cal.data$fichier.uid))]))) { # si mm fichier.uid.i, coller les périodes ensemble (ainsi, retirer et remettre ne demande pas plus de manipulations et surtout ps des manipulations individuelles)
      print(l)
      cal.data.i.l <- unique(cal.data[which(grepl(fichier.uid.i, cal.data$fichier.uid)),
                                      c("fichier.uid", "site.uid", "well.uid", "trmnt.uid", 'lab.probe.id', 'probe.uid', 'probe.brand',
                                        "day.begining.aaaa.mm.dd.hh.mm", 'day.end.aaaa.mm.dd.hh.mm', "period.fichier.uid")])[l,] # cal.data.i.l = les infos dont j'ai besoin pour recouper selon la période l du fichier i
      # recoupage de ll.pre.data selon cal.data selon début et fin des mesures et retrait de colonnes
      ll.pre.2.data.4.l <- ll.pre.2.data.4 %>%
        dplyr::filter(date.time.tz.orig >= cal.data.i.l$day.begining.aaaa.mm.dd.hh.mm) %>% # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data.i.l
        dplyr::filter(date.time.tz.orig <= cal.data.i.l$day.end.aaaa.mm.dd.hh.mm) %>% # <= date de mesure de NP plus petite ou égale à la date end dans cal.data.i.l 
        dplyr::select("scan.id", "raw.value.mm", "calibrated.value.cm", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0") # %>%  # date et time sans "UTC.0" sont dans le fuseau horaire d'origine (tz trouvé en croisant les coordonnées "coords")
      # vérifications
      head(ll.pre.2.data.4.l); colnames(ll.pre.2.data.4.l); nrow(ll.pre.2.data.4.l)
      # changer pour un nom explicite, fichier encore à calibrer (d'où "pre")
      ll.cal.pre.i.l[[l]] <- ll.pre.2.data.4.l
      nrow(ll.cal.pre.i.l[[l]])
    }
    
    # coller toutes les données de la sonde k ensemble (différentes mesures temporelles, mm puits.trmnt.année) ----
    ll.cal.pre.i <- do.call(rbind, ll.cal.pre.i.l) # row bind -> on colle deux df de structure identique (les l nombre de ll.cal.pre.i.l) de différents k.l, associées à différents temps de la période de mesure de la sonde k
    # explications de cette loop ----
    # mm fichier.uid (loop extrait séquentiellement toutes les lignes de chaque # de SNH, qui peuvent être uniques ou multiples pour un SNH donné);
    # la loop teste si toutes les lignes de ce # de SNH ont le même fichier.uid (i), dans quel cas, si les périodes sont différentes, 
    # la boucle coupe le fichier pour chaque période différente (l), et ensuite réassemble le fichier avec seules les périodes à conserver
    
    ### calcul de calibration  ----
    # * avec ODYSSEY, calibration est faite selon les colonnes "cal." du fichier cal.data et la donnée de bulleur (qui donne le offset**)
    # ** le offset doit être ensuite appliqué à toute les données
    
    # extraire les données de calibration pour le fichier.uid (3 lignes min.)
    cal.probe.i <- cal.data %>% dplyr::filter(cal.data$fichier.uid == fichier.uid.i) %>% dplyr::mutate_at("cal.order", ~replace(., is.na(.), 0)) # remplacer les NA dans cal.order par 0, sinon inclus dans les résultats
    # test: si raw.value == vecteur de "NA", on peut procédéer à la calibration, sinon ça veut dire qu'on a la cal du programme de la sonde, garder ces données (créer autre colonne)
    if(nrow(cal.probe.i) > 3){
      paste0("Attention, ça ne fonctionne pas parce qu'il y a 6 lignes. Lui dire de n'en sélectionner que 3...")
      next
    }
    
    # test: si raw.value == vecteur de "NA", on peut procédéer à la calibration, sinon ça veut dire qu'on a la cal du programme de la sonde, garder ces données (créer autre colonne)
    if(FALSE %in% (!ll.cal.pre.i$calibrated.value.cm %in% rep("NA", times = length(ll.cal.pre.i$calibrated.value.cm)))) { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
      stop(paste0("Attention, la colonne calibrated.value n'est pas vide. Sonde problématique : i = ", paste(i), "; ", ll.pre[i]))
      # créer une autre colonne, le cas échéant (à faire)
    }
    ### calibration ----
    # PRÉALABLE : utiliser la valeur NÉGATIVE de longueur de fil à la calibration
    #### étape 1 : si y=ax+b, calcul des termes a et b  ----
    # FORMULES
    # a.slope = ( y2 - y1 ) / ( x2 - x1 ), soit la proportion de changement de y pour chaque changement de x
    # où
    # y = raw.value aux longueurs 1 et 2 du test de calibration (p. ex. 200 mm et 800 mm ou 1400 mm, pour STH)
    # x2 = longueur fil test où "cal.order"=2, x1 = longueur fil test où "cal.order"=1
    # et finalement
    # b.verticalIntercept = y1 - a.slope * x1
    {
      # long_negative_cal.length_mm_y.R déjà calculé ci-haut// ou sinon  = cal.probe.i$cal.length.cm[cal.probe.i$cal.order==1]*-10 # en cm et au négatif
      y2 = cal.probe.i$long_negative_cal.length_mm_y[cal.probe.i$cal.order==2]
      y1 = cal.probe.i$long_negative_cal.length_mm_y[cal.probe.i$cal.order==1]
      x2 = cal.probe.i$cal.value_x[cal.probe.i$cal.order==2]
      x1 = cal.probe.i$cal.value_x[cal.probe.i$cal.order==1]
      a.slope = ( y2 - y1 ) / ( x2 - x1 )
      b.verticalIntercept = y1 - (a.slope * x1)
    }

    #### étape 2 : appliquer a et b pour trouver le offsets à appliquer aux données ----
    # pour les lignes de measure_type == offset_measurement :
    # avec les valeurs a.slope et b.offset, calcluler la "longueur équivalente" (long_negative_cal.length_mm_y) de fil, avec la donnée de ll au moment de la mesure de bulleur = pre_prof_nappe_odyssey_mm_to_cm
    # prof_nappe_odyssey_cm_plus.out.R = pre_prof_nappe_odyssey_mm_to_cm.R + out.long.tuyau.sol.cm # NOTER L'ADDITION DU OUT
    # prof_nappe_bulleur_cm_plus.out.R = prof_nappe_bulleur_cm_+out.R + out.long.tuyau.sol.cm # NOTER L'ADDITION DU OUT
    # offset_cm = prof_nappe_odyssey_cm_plus.out.R - prof_nappe_bulleur_cm_plus.out.R # Noter : SOUSTRACTION et ordre
    # placer la valeur de offset_cm dans un dataframe de consigne de toutes les valeurs de offset obtenus + autres métadonnées importantes
      long_negative_cal.length_mm_y <- (cal.probe.i$cal.value_x[cal.probe.i$measure_type=="offset_measurement"]*a.slope)+b.verticalIntercept
      pre_prof_nappe_odyssey_mm_to_cm <- long_negative_cal.length_mm_y/10 #  sensé donner NA (mais actuellement remplis, à écraser avec calcul automatisé), on va remplir cette donnée avec les nouvelles valeurs -> longueur fictive em mm transformée en cm
      prof_nappe_odyssey_cm_plus.out <- pre_prof_nappe_odyssey_mm_to_cm + cal.probe.i$out.long.tuyau.sol.cm[cal.probe.i$measure_type=="offset_measurement"]
      prof_nappe_bulleur_cm_plus.out <- cal.probe.i$`in.bulleur1.rel.to.surface.cm`[cal.probe.i$measure_type=="offset_measurement"] + cal.probe.i$out.long.tuyau.sol.cm[cal.probe.i$measure_type=="offset_measurement"]
      offset_cm <- prof_nappe_odyssey_cm_plus.out - prof_nappe_bulleur_cm_plus.out

      # FAIRE DU MÉNAGE caduque
      # cal.probe.i$long_negative_cal.length_mm_y[cal.probe.i$measure_type=="offset_measurement"] # sensé donner NA, on va remplir cette donnée avec la formule obtenue y=ax+b
      # cal.probe.i$long_negative_cal.length_mm_y[cal.probe.i$measure_type=="offset_measurement"] <- (cal.probe.i$cal.value_x[cal.probe.i$measure_type=="offset_measurement"]*a.slope)+b.verticalIntercept
      # cal.probe.i$long_negative_cal.length_mm_y[cal.probe.i$measure_type=="offset_measurement"] # vérification de la valeur
      
      # cal.probe.i$pre_prof_nappe_odyssey_mm_to_cm[cal.probe.i$measure_type=="offset_measurement"] #  sensé donner NA (mais actuellement remplis, à écraser avec calcul automatisé), on va remplir cette donnée avec les nouvelles valeurs
      # cal.probe.i$pre_prof_nappe_odyssey_mm_to_cm[cal.probe.i$measure_type=="offset_measurement"] <- cal.probe.i$long_negative_cal.length_mm_y[cal.probe.i$measure_type=="offset_measurement"]/10 #  sensé donner NA (mais actuellement remplis, à écraser avec calcul automatisé), on va remplir cette donnée avec les nouvelles valeurs -> longueur fictive em mm transformée en cm
      # cal.probe.i$pre_prof_nappe_odyssey_mm_to_cm[cal.probe.i$measure_type=="offset_measurement"] #  vérification de la valeur
      
      # cal.probe.i$prof_nappe_odyssey_cm_plus.out[cal.probe.i$measure_type=="offset_measurement"]
      # cal.probe.i$prof_nappe_odyssey_cm_plus.out[cal.probe.i$measure_type=="offset_measurement"] <- cal.probe.i$pre_prof_nappe_odyssey_mm_to_cm[cal.probe.i$measure_type=="offset_measurement"] + cal.probe.i$out.long.tuyau.sol.cm[cal.probe.i$measure_type=="offset_measurement"]
      # cal.probe.i$prof_nappe_odyssey_cm_plus.out[cal.probe.i$measure_type=="offset_measurement"]
      
      # cal.probe.i$prof_nappe_bulleur_cm_plus.out[cal.probe.i$measure_type=="offset_measurement"]
      # cal.probe.i$prof_nappe_bulleur_cm_plus.out[cal.probe.i$measure_type=="offset_measurement"] <- cal.probe.i$`bulleur.1.rel.to.surface.cm`[cal.probe.i$measure_type=="offset_measurement"] + cal.probe.i$out.long.tuyau.sol.cm[cal.probe.i$measure_type=="offset_measurement"]
      # cal.probe.i$prof_nappe_bulleur_cm_plus.out[cal.probe.i$measure_type=="offset_measurement"]
    
    # Consigne du offset par date et par fichier.uid dans un format d'archivage (non-écrasable)
    # NOTE : je suis encore dans la loop par fichier.uid (période, site, probe.uid); la calibration est valide et doit être appliquée à tout le fichier
    odyssey_offset_archives[i, 1] <- fichier.uid.i
    odyssey_offset_archives[i, 2] <- paste0(round(prof_nappe_odyssey_cm_plus.out - prof_nappe_bulleur_cm_plus.out, 3), "-", Sys.Date())
    odyssey_offset_archives[i, 3] <- paste0(round(a.slope, 3), "-", Sys.Date())
    odyssey_offset_archives[i, 4] <- paste0(round(b.verticalIntercept, 3), "-", Sys.Date())
    
    # changer la colonne calibrated pour les données corrigées
    ## tests 9 avril
    # ll.cal.pre.i$slope <-((ll.cal.pre.i$raw.value.mm*a.slope) + b.verticalIntercept)/10
    # ll.cal.pre.i$pre.offset <- ll.cal.pre.i$slope+cal.probe.i$out.long.tuyau.sol.cm[cal.probe.i$measure_type=="offset_measurement"] 
    # ll.cal.pre.i$offset <-  ll.cal.pre.i$pre.offset-offset_cm
    ll.cal.pre.i$calibrated.value.cm = (((ll.cal.pre.i$raw.value.mm*a.slope) + b.verticalIntercept)/10) + cal.probe.i$out.long.tuyau.sol.cm[cal.probe.i$measure_type=="offset_measurement"] - offset_cm
    colnames(ll.cal.pre.i); head(ll.cal.pre.i); tail(ll.cal.pre.i)
    
    # format final -> nom final
    ll.cal <- ll.cal.pre.i # ceci est donc le format final, à intégrer dans la liste ll.clean
    
    ### création de la liste dans la liste [[i]]  ----
    # noted : <- le fichier du level logger correspondant à la position i; [1] : data (dataframe), [2] : metadata (character string)
    tidy.WTD.data[[i]] <- list("data" = ll.cal, "metadata" = ll.pre.2.metadata) 
    
    ### création de fichiers excels propres  ----
    # À FAIRE : excel [[i]] avec un onglet metadata et onglet data
    # # noted : <- le fichier du level logger correspondant à la position i; [1] : data (dataframe), [2] : metadata (character string)
    # ll.clean[[i]] <- list("data" = ll.cal, "metadata" = ll.pre.2.metadata) 
    # À FAIRE
    # format excel des ll.clean -> arranger pr que ça fonctionne
    # if("ll.clean.xlsx" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
    #   stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
    # } else { write.xlsx(ll.clean, file = "connectivite/data/clean/ll.clean.xlsx") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }
    # 
  } # fin de la loop pour les ODYSSEY / # début de la loop pour les HOBO (else if() prochaine ligne)
  
  else if (grepl(SNH[2], ll.pre[i])) { # début de la loop pour les HOBO
    # import et ménage
    k <- i
    # i<-5
    print(k)
    ll.pre[k]
    # ll.pre.0 <- read.csv(paste0("connectivite/data/raw/", ll.pre[k]), sep = "','")
    ll.pre.0 <- readLines(paste0("connectivite/data/raw/", ll.pre[k])); str(ll.pre.0) # lire en format texte
    # ** tz orig mentionnée dans la colonne ll.pre.0.metadata[2], coder pour l'obtenir au besoin
    # Warning message:
    #   In readLines(paste0("connectivite/data/raw/", ll.pre[k])) :
    #   incomplete final line found on 'connectivite/data/raw/..._hobo.csv'
    
    ### création des subsets data & metadata ----
    # notes : les noms réfèrent à l'étape et non à une matrice en particulier, les objets seront remplacés au fil de la boucle. 
    # l'info importante est consignée dans la liste ll.clean.k.hobo[i], à la fin
    ll.pre.0.metadata <-  ll.pre.0[c(1:2)] # inclus les anciens noms de colonnes, qui sont dans un format et un ordre bizzare
    ll.pre.0.data <- ll.pre.0[-c(1:2)]
    str(ll.pre.0.data) # chr
    
    #### vérification du fichier level logger brut : logger.serial.no == nom du fichier, sinon arrêter TOUT ! ----
    {
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
    # création du fichier.uid.i, nom unique du FICHIER qui ne pourra JAMAIS être dupliqué (utila dans seciton début et fin des mesures par périodes, pour un mm FICHIER)
    fichier.uid.i <- paste0(unlist(resultat)[1], "_", unlist(resultat)[2]) # ceci sera écrasé à la prochaine itération
    fichier.uid.df[i,1:4] <- c(paste0(unlist(resultat)[1], "_", unlist(resultat)[2]), ll.pre[i], probe.uid.k, as.numeric(unlist(resultat)[2])) # ceci sera gardé en mémoire (doit être identique à la colonne fichier.uid dans cal.data)
    # ajouts aux métadonnées des fichiers
    ll.pre.0.metadata[3:6] <-c(paste0("fichier.uid : ", unlist(resultat)[1], "_", unlist(resultat)[2]), paste0('file.name : ', "`", ll.pre[i], "`"), 
                               paste0("probe.uid : ", probe.uid.k), paste0("date d'extraction des données : ", as.numeric(unlist(resultat)[2])))
    
    #### création du dataframe level legger (ll) contenant données de nappe phréatique (NP) et ménage  ----
    ll.pre.0.data.0 <- read.csv(text = ll.pre.0.data, header = F, col.names = c("scan.id", "date.JJ.MM.AAAA_time.HH.MM.SS",	"raw.value.kPa_pres.abs",	"temperature_dC", "Coupleur détaché", "Coupleur attaché", 'Hôte connecté',	"Arrêté", "Fin de fichier")) # text = argument de read.csv qui lit la valeur contenue dans l'objet / DATE mauvais format
    ll.pre.0.data.1 <- ll.pre.0.data.0[1:4] # garder seules les colonnes pertinentes
    
    #### date et heure : format ISO date AAAA-MM-JJTHH:MM:SS,ss-/+FF:ff, voir https://fr.wikipedia.org/wiki/ISO_8601 ----
    # heure : « Z » à la fin lorsqu’il s’agit de l’heure UTC. (« Z » pour méridien zéro, aussi connu sous le nom « Zulu » dans l’alphabet radio international).
    # extraction : nom du site pour trouver les coordonnées qui serviront à connaître le fuseau horaire
    site.0 <- gsub("\\\"", '', ll.pre.0.metadata)[1] # extraire nom de site fichier origine
    site <- sub("Titre de tracé : ","",site.0)
    
    # ouvrir données du shapefile pour accéder les zones
    zones <- read_sf("~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp")
    zones <- as.data.frame(zones)
    head(zones); str(zones)
    
    # extraire la bonne lat, long selon le nom du site
    coords <- c(zones$latitude[zones$site==site][1], zones$longitude[zones$site==site][1])
    
    # trouver le UTC selon la lat long
    (tz <- tz_lookup_coords(coords[1], coords[2], method = "fast", warn = FALSE))
    # ajouts aux métadonnées des fichiers
    fichier.uid.df[i,5] <- tz
    ll.pre.0.metadata[7] <- paste0("original time zone : ", tz)
    
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
    ll.pre.0.data.2$"calibrated.value.cm" <- rep(NA, times = nrow(ll.pre.0.data.2))
  
    # vérifications
    colnames(ll.pre.0.data.2); head(ll.pre.0.data.2); str(ll.pre.0.data.2) # date et heure ne sont pas sous forme POSIX -> changer dans la section "### date et heure"
    # nouveau nom préliminaire (et retirer colonnes inutiles)
    ll.pre.0.data.3 <- ll.pre.0.data.2 %>% select(!c(date.JJ.MM.AAAA_time.HH.MM.SS, date.AAAA.MM.JJ,  "date.time.UTC.0pre", "date.time.UTC.0pre.1")) %>% 
      select("scan.id", "date.JJ.MM.AAAA_time.HH.MM.SS_tz", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0", 
             "raw.value.kPa_pres.abs", "temperature_dC", "calibrated.value.cm")
    head(ll.pre.0.data.3); str(ll.pre.0.data.3)
    # suite :
    # si calibration intégrée avec le hobo, QUE FAIRE ? coder ici, voir procédure avec ODYSSEY
    
    #### début et fin des mesures par PROBE.WELL.UID ----
    # inscrits dans "level_logger_calibration_all.csv"
    # début (généralement) = installation + 24h de rabattement de la NP / ou non, si puits intallé d'avance, dans quel cas inscrire début officiel - 24h)
    # fin = heure de retrait
    # note : données de date en format xlsx ça lit TOUT CROCHE, transformé en csv fonctionne bien
    
    ##### import et nettoyage ----
    cal.data <- read.csv("connectivite/data/raw/level_logger_calibration_all.csv", sep = ";", dec = ",")
    # out = (pt haut - moyenne pt bas)
    cal.data$out.R = round(cal.data$pt.haut.cm - ((cal.data$pt.bas1.cm+cal.data$pt.bas2.cm+cal.data$pt.bas3.cm)/3), digits = 1)
    colnames(cal.data)
    cal.data <- cal.data %>% select("site.uid", "well.uid", "trmnt.uid", "lab.probe.id", "probe.uid", "probe.brand", 
                                    "cal.length.cm", "cal.order", "cal.value_x", "comment", 
                                    "day.begining.aaaa.mm.dd.hh.mm", "day.end.aaaa.mm.dd.hh.mm", "distance.m", "out.R", "out.long.tuyau.sol.cm", everything())
    # vérification de valeurs OUT
    if(all(cal.data$out.R == round(cal.data$out.long.tuyau.sol.cm, digits = 1), na.rm =T))  { # si TOUS TRUE (fonction any()) = changer nom de out.R et supprimer la mesure entrée manuellement // si FALSE = avertissement
      cal.data$out.long.tuyau.sol.cm <- cal.data$out.R
      cal.data <- cal.data %>% select(!out.R)
    } else { stop("Attention, le out entré dans cal.data (syn. level_logger_calibration_all.csv) n'est pas identique à la moyenne des points bas soustraite du point haut du puits.") } 
    # création d'une colonne unique
    
    
    # exclure les lignes qui n'ont pas de day beggining / rejeter ces lignes et filtrer avec la fonction ( à écrire )
    cal.data$period.fichier.uid <- paste0(cal.data$day.begining.aaaa.mm.dd.hh.mm, "--", cal.data$day.end.aaaa.mm.dd.hh.mm, ".",cal.data$fichier.uid)
    # format POSIX begining et end
    cal.data$day.begining.aaaa.mm.dd.hh.mm <- ymd_hm(cal.data$day.begining.aaaa.mm.dd.hh.mm, tz = tz)
    cal.data$day.end.aaaa.mm.dd.hh.mm <- ymd_hm(cal.data$day.end.aaaa.mm.dd.hh.mm, tz = tz)
    head(cal.data); tail(cal.data); str(cal.data)
    
    ##### boucle de concaténation des données (fichier.uid ensemble, sinon autre calibration et graphique disctinct) ----
    ll.cal.pre.i.l <- list()  # rappel : i == k, changer un jour au besoin, vestige
    for (l in 1:length(unique(cal.data$period.fichier.uid[which(grepl(fichier.uid.i, cal.data$fichier.uid))]))) { # si mm fichier.uid.i, coller les périodes ensemble (ainsi, retirer et remettre ne demande pas plus de manipulations et surtout ps des manipulations incividuelles)
      if (length(unique(cal.data$period.fichier.uid[which(grepl(fichier.uid.i, cal.data$fichier.uid))])) != 0) {
        ll.pre[[i]]; print(l)
        cal.data.i.l <- unique(cal.data[which(grepl(fichier.uid.i, cal.data$fichier.uid)),
                                        c("fichier.uid", "site.uid", "well.uid", "trmnt.uid", 'lab.probe.id', 'probe.uid', 'probe.brand',
                                          "day.begining.aaaa.mm.dd.hh.mm", 'day.end.aaaa.mm.dd.hh.mm', "period.fichier.uid")])[l,] # cal.data.i.l = les infos dont j'ai besoin pour recouper selon la période l du fichier i
        period.fichier.uid.l <- cal.data.i.l$period.fichier.uid
        # recoupage de ll.pre.data selon cal.data selon début et fin des mesures et retrait de colonnes
        ll.pre.0.data.4.l.pre <- ll.pre.0.data.3 %>%
          dplyr::filter(date.time.tz.orig >= cal.data.i.l$day.begining.aaaa.mm.dd.hh.mm) %>% # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data.i.l
          dplyr::filter(date.time.tz.orig <= cal.data.i.l$day.end.aaaa.mm.dd.hh.mm) %>% # <= date de mesure de NP plus petite ou égale à la date end dans cal.data.i.l 
          select("scan.id", "raw.value.kPa_pres.abs", "calibrated.value.cm",  "temperature_dC", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0") # %>%  # date et time sans "UTC.0" sont dans le fuseau horaire d'origine (tz trouvé en croisant les coordonnées "coords")
        # répliquer les données cal.data.k.l à chaque ligne de ll.pre.0.data.4.l.pre
        cal.data.i.l.all <- cal.data[cal.data$period.fichier.uid == period.fichier.uid.l,]
        rownames(cal.data.i.l.all) <- NULL
        cal.data.i.l.rep <- cbind(cal.data.i.l.all, rep(row.names(cal.data.i.l.all), each = nrow(ll.pre.0.data.4.l.pre)))
        colnames(cal.data.i.l.rep)
        # assembler les colonnes
        ll.pre.0.data.4.l <- bind_cols(ll.pre.0.data.4.l.pre, cal.data.i.l.rep)
        ll.pre.0.data.4.l <- ll.pre.0.data.4.l %>% select(!"rep(row.names(cal.data.i.l.all), each = nrow(ll.pre.0.data.4.l.pre))")
        # chaque cal.data.k = une section de mesures de la sonde k, durant l'été, associée ou non à une mesure au bulleur et à une longueur de fil
        # vérifications
        head(ll.pre.0.data.4.l); colnames(ll.pre.0.data.4.l); nrow(ll.pre.0.data.4.l)
        # changer pour un nom explicite, fichier encore à calibrer (d'où "pre")
        ll.cal.pre.i.l[[l]] <- ll.pre.0.data.4.l
      }
      # coller toutes les données de la sonde k ensemble (différentes mesures temporelles, mm puits.trmnt.année) ----
      ll.cal.pre.i <- do.call(rbind, ll.cal.pre.i.l) # row bind -> on colle deux df de structure identique (les ll.cal.pre.i) de différents k.l, associées à différents temps de la période de mesure de la sonde k
      # explications de cette loop ----
      # mm fichier.uid (loop extrait séquentiellement toutes les lignes de chaque # de SNH, qui peuvent être uniques ou multiples pour un SNH donné);
      # la loop teste si toutes les lignes de ce # de SNH ont le même fichier.uid (i), dans quel cas, si les périodes sont différentes, 
      # la boucle coupe le fichier pour chaque période différente (l), et ensuite réassemble le fichier avec seules les périodes à conserver
    }
    
    ### calcul de calibration  ----
    # * avec HOBO, calibration est faite selon une station météorologique *
    # Référence : Jutras et Bourgault, 2024, Version 2.0, section 7 (/Users/Aliz/Documents/Doctorat/_Connectivité/Protocoles (dossiers copiés du serveur A'24)/Leveloggers & Hauteur nappe phréatique/_HOBO_Protocole de mesure de nappe_2024-11-01_NE PAS DIFFUSER.docx)
    
    #### extraction des données de METEOSTAT //[auparavant : ECCC/CCCS] et ménage ----
    meteoStat.data.pre.0 <- read.csv(paste0("connectivite/data/raw/", list.files(path = "connectivite/data/raw", pattern = site)))
    meteoStat.data.pre.1 <- meteoStat.data.pre.0 %>% mutate(date.time = paste(year, month, day, hour)) %>% mutate(pressure.kPa = pres * 0.1) # pression donnée en hPa (hectopascal). 1 hPa = 0,1 kPa. Example: convert 15 hPa to kPa: 15 hPa = 15 × 0.1 kPa = 1.5 kPa
    meteoStat.data.pre.1$date.time <- ymd_h(meteoStat.data.pre.1$date.time, tz = tz) + 1
    meteoStat.data.pre.1 <- meteoStat.data.pre.1 %>%  select(date.time, everything(), -c("year", month, day, hour, X, pres, "wdir","wdir_source","wspd","wspd_source","cldc","cldc_source","coco","coco_source")) # ajuster la date et l'heure et ajout d'une seconde, sinon, les données 00:00:00 étaient effacées !
    
    # changement de nom pour identifier quelles colonnes du futur cal.meteoStat.data proviennent de meteoStat
    colnames(meteoStat.data.pre.1) <- paste0(colnames(meteoStat.data.pre.1), ".ms") # ajout de ".ms" pour identifier les colonnes issues de MeteoStat
    
    # convertir au bon format de date et manip de colonnes (idem aux infos temporelles de fichier de sonde) / date.time.UTC selon norme iso
    meteoStat.data.pre.2 <- meteoStat.data.pre.1 %>%
      mutate(date.time.UTC.0.pre = with_tz(ymd_hms(meteoStat.data.pre.1$date.time.ms, tz = tz), tzone = "GMT")) # les heures sont ainsi ramenées à UTC +0 / ceci écrase la colonne du mm nom
    head(meteoStat.data.pre.2$date.time.UTC.0.pre) # ok ici
    
    meteoStat.data.pre.3 <- meteoStat.data.pre.2 %>%  # enlever l'espace entre date et heure (ISO 8601)
      mutate(date.time.UTC.0.pre.1 = str_replace(meteoStat.data.pre.2$date.time.UTC.0.pre, " ", "T")) %>% 
      select(date.time.ms, date.time.UTC.0.pre, date.time.UTC.0.pre.1, everything())
    head(meteoStat.data.pre.3$date.time.UTC.0.pre.1) # ok ici
    
    meteoStat.data.pre.3$date.time.UTC.0 <- str_replace_all(meteoStat.data.pre.3$date.time.UTC.0.pre.1, "00:01","00:01Z") # ajouter le Z à la fin (ISO 8601)
    meteoStat.data <- meteoStat.data.pre.3 %>% select(date.time.ms, date.time.UTC.0, everything()) %>% select(!c(date.time.UTC.0.pre, date.time.UTC.0.pre.1))
    head(meteoStat.data); str(meteoStat.data); class(meteoStat.data)
    
    # # vérif pour le join, il faut que la sytaxe soit exactement la mm entre les deux df
    # c(class(meteoStat.data[4523,]$date.time.UTC.0), class(ll.cal.pre.i[1,]$date.time.UTC.0))
    # c(as.character(meteoStat.data[4523,]$date.time.UTC.0), as.character(ll.cal.pre.i[1,]$date.time.UTC.0))
    # c(meteoStat.data[4523,]$date.time.UTC.0, ll.cal.pre.i[1,]$date.time.UTC.0)
    # meteoStat.data[4523,]$date.time.UTC.0 == ll.cal.pre.i[1,]$date.time.UTC.0 # -> doit renvoyer T
    
    #### assembler données du HOBO et données de ECCC/CCCS selon la date et l'heure ----
    # Jutras&Bourgault V2.0, 2024; étape a) Associer par dates et par heures les données mesurées par les sondes de niveau hydrostatique et la pression atmosphérique
    cal.meteoStat.data <- left_join(ll.cal.pre.i, meteoStat.data, by = join_by(date.time.UTC.0)) %>% 
      select("scan.id", "date.time.UTC.0","raw.value.kPa_pres.abs", "temperature_dC", "calibrated.value.cm",
             `date.AAAA-MM-JJ`, "time.HH.MM.SS", `date.time.tz.orig`, "date.time.ms", pressure.kPa.ms, everything(), -x.archive.well.uid) # enlever les nombreuses colonnes qui n'ont pas rapport dans ces démarches
    colnames(cal.meteoStat.data)

    
    # À faire : VÉRIFIER SI TOUT EST OK NIVEAU TIME ZONES... 
    ##### inscrire le time zone (tz) dans la colonne time (équivalent à "date.time.tz.orig.pre") ----
    # json_data <- fromJSON(file ="connectivite/data/raw/full.json") # time zone inscrite dans ce fichier
    # trouver ma station
     # ??? et le bon UTC...
    
    
    # à faire
    # REMETTRE FICHIERS BRNTC dans dossier principal
    # SI MESSAGE D'ERREUR contient les caractères suivants, UTILISER LES DONNÉES DE LA STATION MÉTÉO LOCALE
    # "There are no data for station 6128 for this interval (hour)"
    # PRO : JE N'AI PAS DE DONNÉES HORAIRES de pression atmosphérique

    
    
    # Jutras&Bourgault V2.0, 2024; étape b)	Calculer la hauteur d’eau au-dessus de la sonde par la soustraction de la pression atmosphérique, convertie en cm d’eau, à la pression mesurée par la sonde
    # Jutras&Bourgault V2.0, 2024; étape b.i)	La conversion de kPa en cm d’eau est : 1 kPa = 10,1972 cm d’eau 
    cal.meteoStat.data$pression.eau.kPa <- cal.meteoStat.data$raw.value.kPa_pres.abs - cal.meteoStat.data$pressure.kPa.ms
    cal.meteoStat.data$hauteur.eau.cm.pre <- cal.meteoStat.data$pression.eau.kPa * 10.197162129779 # règle de trois
    cal.meteoStat.data$hauteur.eau.cm <- cal.meteoStat.data$hauteur.eau.cm.pre # dépend de la façon dont les mesures de longueurs en cm sont prises
    cal.meteoStat.data <- cal.meteoStat.data %>% select("scan.id", "date.time.UTC.0","raw.value.kPa_pres.abs", pression.eau.kPa, hauteur.eau.cm, everything()) 
    
    # Jutras&Bourgault V2.0, 2024; étape c)	Convertir la hauteur d’eau au-dessus de la sonde en profondeur de la nappe phréatique par rapport à la surface du sol
                                         # c.bis) création d'un vecteur de longueur CDS à ajouter à la longueur du fil (protocole pour éviter l'erreur humaine)
    # Jutras&Bourgault V2.0, 2024; étape c.i)	La profondeur de la nappe phréatique par rapport à la surface du sol = 
    # ((La longueur du fil + La constante CDS) – La longueur du puits d’observation qui dépasse la surface du sol) – La hauteur d’eau au-dessus de la sonde
    
    # c.bis
    # D'abord, constante de distance à la sonde en fonction de l'appareil de mesure, à ajouter à la longueur de fil
    CDS <- data.frame(type = c("U20", "U20L", "odyssey"), # Hobo seulement : mesure longueur du fil tel que dans protocole; à la limite de la boîte de sonde. Les constantes de longueur de boîte de sonde à la sonde à l'interface intérieur de la sonde sont ajoutées à cette étape-ci.
                      constante = c("12.93", "13.3", "0")) %>% 
      mutate_at('constante', as.numeric) # liste des types de SNH avec lesquelles j'ai pris des données; chaque "marque/modèle" (type) est traitée de façon différente 
    str(CDS)
    
    # vérifications des types de chaque variable
    str(cal.meteoStat.data$long.fil.cm); str(cal.meteoStat.data$out.long.tuyau.sol.cm); str(cal.meteoStat.data$hauteur.eau.cm)  # numeric
    # calcul de la profondeur
    cal.meteoStat.data$calibrated.value.cm <-  cal.meteoStat.data$long.fil.cm - cal.meteoStat.data$out.long.tuyau.sol.cm - cal.meteoStat.data$hauteur.eau.cm # avec le moins, ça donne 20 de profondeur
    # cal.eccc.data$calibrated.value.cm <- cal.eccc.data$long.fil.cm - cal.eccc.data$out.long.tuyau.sol.cm + cal.eccc.data$hauteur.eau.cm
    head(cal.meteoStat.data)$calibrated.value.cm
    
    # format final -> nom final
    ll.cal.k <- cal.meteoStat.data %>%  # ceci est donc le format final, à intégrer dans la liste ll.clean
      select(scan.id, raw.value.kPa_pres.abs, calibrated.value.cm, `date.AAAA-MM-JJ`, time.HH.MM.SS, date.time.tz.orig, # retirer des colonnes intermédiaires et mm format que ll.clean[[i]]$data
             date.time.UTC.0)

        ### création de la liste dans la liste [[i]]  ----
    # noted : <- le fichier du level logger correspondant à la position i; [1] : data (dataframe), [2] : metadata (character string)
    
    tidy.WTD.data[[i]] <- list("data" = ll.cal.k, "metadata" = ll.pre.0.metadata) 
  } # fin de la loop pour les HOBO
  # else {
  #  stop("ERREUR : CODER ICI") # si nécessaire, ajouter 3e type de traitement de SNH
  # }
}  
# vérifier que les erreurs sont tjrs la meme affaire inutile -> incomplete final line, tenté de régler le problème, mais sans succès; 
# et different length (ça le dit quand le "cal" est vide, et ça met des NA, ce qui est parfait)

# enregistrer le tableau des métadonnées de fichier
if("metadata_SNH_fichiers.csv" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { write.csv(fichier.uid.df, file = "connectivite/data/clean/metadata_SNH_fichiers.csv") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }

# format R des ll.clean
if("tidy.WTD.data.RDS" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { saveRDS(tidy.WTD.data, file = "connectivite/data/clean/tidy.WTD.data.RDS") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }

# supprimer ?
# # Joindre les lignes de offset de l'objet "odyssey_offset_archives" dans le fichier "level.logger_offset_archive.csv"
# level.logger_offset_archive <- read_excel("connectivite/data/raw/level.logger_offset_archive.xlsx")#, col_types = c(rep("text", times = 2)))
# str(level.logger_offset_archive)
# level.logger_offset_archive$fichier.uid <- as.character(level.logger_offset_archive$fichier.uid)
# level.logger_offset_archive$offset_cm_date <- as.character(level.logger_offset_archive$offset_cm_date)
# level.logger_offset_archive$a.slope_excel <- as.character(level.logger_offset_archive$a.slope_excel)
# level.logger_offset_archive$b.verticalIntercept <- as.character(level.logger_offset_archive$b.verticalIntercept)
# level.logger_offset_archive <- full_join(odyssey_offset_archives, level.logger_offset_archive)# %>% na.omit
# 
# if("level.logger_offset_archive.xlsx" %in% list.files("connectivite/data/raw"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
#   stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
# } else { write.xlsx(level.logger_offset_archive, file = "connectivite/data/raw/level.logger_offset_archive.xlsx") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }




# A.2 examination des données ----
tidy.WTD.data <- readRDS("connectivite/data/clean/tidy.WTD.data.RDS")
SNH <- as.vector(c("_odyssey", "_hobo"), mode = "character") # liste des types de SNH avec lesquelles j'ai pris des données; chaque "marque" est traitée de façon différente
for (j in 1:length(tidy.WTD.data)) {
  # j<-1
  print(j)
  ll.clean.j <- tidy.WTD.data[[j]]
  
  # ODYSSEY
  if (grepl(SNH[1], tidy.WTD.data.j$metadata[11])) {
    # où trouver no de sonde dans ODYSSEY
    metadata.line <- tidy.WTD.data.j$metadata[12] # probe.uid
    numbers <- gregexpr("[0-9]+", metadata.line)
    sonde <- regmatches(metadata.line, numbers)
  } else if (grepl(SNH[2], tidy.WTD.data.j$metadata[4])) {
    # où trouver no de sonde dans HOBO
    metadata.line <- tidy.WTD.data.j$metadata[5] # probe.uid
    numbers <- gregexpr("[0-9]+", metadata.line)
    sonde <- regmatches(metadata.line, numbers)
  }
  # données à visualiser
  data <- tidy.WTD.data[[j]]$data
  if (nrow(data) > 0) {
    hist(data$calibrated.value.mm/10, warn.unused = F, 
         main = paste("Histogram des données de sonde no ", paste(sonde,"\n"))) # en cm
  } 
  # les hauteurs de nappe phréatique calibrées devraient toutes être négatives ou presque !
  # ou alors est-ce que les ODYSSEY donnent en + ?
}












# A.3 caduque




# CADUQUE OU CHANGER .MM EN .CM ET AUTRES MODIFICATIONS NON JOUR NOMS CHANGÉS
# A.3 données de vérification/calibration avec bulleur ----
# créé le 23 déc. pour vérifier données des Odyssey de St-Henri 2024
## import et ménage ----
### sondes de niveau hydrostatique (rappel : SNH) ----
SNH <- as.vector(c("_odyssey", "_hobo"), mode = "character") # liste des types de SNH avec lesquelles j'ai pris des données; chaque "marque" est traitée de façon différente
tidy.WTD.data <- readRDS("connectivite/data/clean/tidy.WTD.data.RDS") # fichiers SNH clean

### données de calibration (rappel : level_logger_calibration_all.csv) ) ----
ll.bulleur <- read.csv("connectivite/data/raw/level_logger_calibration_all.csv", sep = ";", dec = ","); str(ll.bulleur)
# out = (pt haut - moyenne pt bas)
ll.bulleur$out.R = round(ll.bulleur$pt.haut.cm - ((ll.bulleur$pt.bas1.cm+ll.bulleur$pt.bas2.cm+ll.bulleur$pt.bas3.cm)/3), digits = 1)
ll.bulleur <- ll.bulleur %>% select("site.uid", "well.uid", "trmnt.uid", "lab.probe.id", "probe.uid", "probe.brand",
                                    "cal.length.cm", "cal.length.mm", "cal.order", "cal.value", "comment",
                                    "day.begining.aaaa.mm.dd.hh.mm", "day.end.aaaa.mm.dd.hh.mm", "distance.m", "out.R", "out.long.tuyau.sol.cm", everything())
# vérification de valeurs OUT
if(all(ll.bulleur$out.R == round(ll.bulleur$out.long.tuyau.sol.cm, digits = 1)))  { # si TOUS TRUE (fonction any()) = changer nom de out.R et supprimer la mesure entrée manuellement // si FALSE = avertissement
  ll.bulleur$out.long.tuyau.sol.cm <- ll.bulleur$out.R
  ll.bulleur <- ll.bulleur %>% select(!out.R)
} else { stop("Attention, le out entré dans cal.data (syn. level_logger_calibration_all.csv) n'est pas identique à la moyenne des points bas soustraite du point haut du puits.") } 

# calcul de la profondeur de la nappe phréatiquedu (OUT - IN, où IN = mesure lue sur le bulleur)
ll.bulleur$water.table.depth.cm <- round(ll.bulleur$in.bulleur.1.cm - ll.bulleur$out.long.tuyau.sol.cm, digits = 2)
# ici, PROFONDEUR de nappe, donc quand c'est -5cm par exemple, nappe au DESSUS du sol





# REFAIRE LL.BULLEUR
if("ll.bulleur.csv" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { write.csv(ll.bulleur, "connectivite/data/clean/ll.bulleur.csv") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }

# à partir d'ici : TRANSFÉRÉ DANS RMarkdown le 7 janvier 2025 / retravaillé 17 janvier et retransféré (en cours)
## boucle de vérification au bulleur pour chaque SNH (de ll.clean) ----
# fichiers de consigne de données
water.table.verif <- data.frame()
for (m in 1:length(ll.clean)) {
  print(m)
  ll.clean.m <- ll.clean[[m]]; ll.clean[[m]]
  # extraire # sonde des différentes marques de SNH
  if (grepl(SNH[1], ll.clean.m$metadata[11])) { # ODYSSEY
    # où trouver no de sonde dans ODYSSEY metadata
    metadata.line <- ll.clean.m$metadata[12] # probe.uid
    numbers <- gregexpr("[0-9]+", metadata.line)
    sonde.m <- regmatches(metadata.line, numbers)
    # où trouver la date d'extraction dans ODYSSEY metadata
    date.line <- ll.clean.m$metadata[13] # probe.uid
    date.numbers <- gregexpr("[0-9]+", date.line)
    date.m <- unlist(regmatches(date.line, date.numbers))
  } else if (grepl(SNH[2], ll.clean.m$metadata[4])) { # HOBO
    # où trouver no de sonde dans HOBO metadata
    metadata.line <- ll.clean.m$metadata[5] # probe.uid
    numbers <- gregexpr("[0-9]+", metadata.line)
    sonde.m <- unlist(regmatches(metadata.line, numbers))
    # où trouver la date d'extraction dans HOBO metadata
    date.line <- ll.clean.m$metadata[6]
    date.numbers <- gregexpr("[0-9]+", date.line)
    date.m <- unlist(regmatches(date.line, date.numbers))
  }
  # création du dataframe pour chaque vérification au bulleur pour chaque SNH
  if (nrow(ll.clean[[m]]$data) != 0) { # si le fichier SNH n'est pas vide
    water.table.verif.n <- data.frame()
    ll.bulleur.m <- ll.bulleur[ll.bulleur$probe.uid == sonde.m,] # filtrer ll.bulleur par no de sonde "m"
    for (n in 1:nrow(ll.bulleur.m)) {
      print(n)
      ll.bulleur.m.n <- ll.bulleur.m[n,] # filtrer ll.bulleur (level_logger_calibration_all.csv) par le ligne "n" (vérification n au bulleur)
      ll.clean.m.n <- ll.clean.m$data[ll.clean.m$data$date.time.UTC.0 == # fitlrer les données du fichier SNH par la période (unique) de la ligne n = vérification au bulleur
                                        ll.bulleur.m$bulleur.1.date.time.UTC.0,]
      water.table.verif.n[n, 1:4] <- data.frame("probe.uid" = sonde.m, # créer le dataframe de vérification pour les lignes "n" de la SNH "m"
                                                "file.extraction.date" = date.m,
                                                "probe.measure.cm" = ll.clean.m.n$calibrated.value.mm/10,
                                                "bulleur.mesure.cm" = ll.bulleur.m.n$in.bulleur.1.cm)
    } 
    water.table.verif[nrow(water.table.verif) + 1:nrow(water.table.verif.n), 1:4] <- water.table.verif.n # inscrire les données dans le dataframe final, à la dernière ligne
  } else if (nrow(ll.clean[[m]]$data) == 0)  {
    water.table.verif[nrow(water.table.verif) + 1, 1:4] <- data.frame("probe.uid" = sonde.m, # si ll.clean[[j]]$data est vide, mettre NA dans le dataframe
                                                                      "file.extraction.date" = date.m,
                                                                      "probe.measure.cm" = NA,
                                                                      "bulleur.mesure.cm" = NA)
  } 
} 
water.table.verif
water.table.verif <- water.table.verif[!duplicated(water.table.verif),]
rownames(water.table.verif) <- NULL
# .rs.restartR()



# À FAIRE 
# vérifier comment on présente typiquement ces données
# enlever colonnes inutiles
# save le csv dans clean (mais apparaît aussi dans le RMarkdown)
# vérifier les IN et OUT des ODYSSEY

