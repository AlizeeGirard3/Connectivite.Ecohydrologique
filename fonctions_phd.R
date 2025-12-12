# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : 2025-10-28
# Pourquoi : 
# Pour charger les fonctions utiles à traiter données de SHN
# NOTES : 
#   SNH : sondes de niveau hydrostatique
# La fonction s'applique à un fichier brut, traite, corrige et nettoye, et produit un fichier de données aux composantes identiques, peu importe le type de sonde.
# Produit un dataframe rectangulaire. Les métadonnées sont à part (ou créer liste avec données + métadonnées).
#   Merci à Francis Lessard pour ses idées.
# référer aux fonctions en copiant-collant seulement la ligne suivante
# source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd.R")

# ============================================================================= /
#  Libraries ----
# ============================================================================= /
if (!require("tidyverse")) install.packages("tidyverse") # “meta”-package
if (!require("data.table")) install.packages("data.table") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")

# ============================================================================= /
#  Data selection ----
# ============================================================================= /

# dplyr::filter(!measure.status == "rejected")

# données [nb arbres, DHP] j'à NA -> nombre -> nouvelle colonne : total d'individus ***

read_excel_allsheets <- function(filename, tibble = FALSE) {  # for tidyverse tibbles (the default with read_excel): tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  # lapply = applique la fonction suivante décrite à tous les éléments contenus dans l'objet "sheets" et crée une liste avec le résultat
  if(!tibble) x <- lapply(x, as.data.frame) # le résultat de la fonction appliquée n'est pas une tibble, fait en un dataframe
  names(x) <- sheets
  x
}

# filename <- raw.env.data[i]
read_excel_sheets <- function(filename, tibble = FALSE) {  # for tidyverse tibbles (the default with read_excel): tibble = TRUE
  sheets.pre <- readxl::excel_sheets(filename)
  sheets <- subset(sheets.pre,!grepl(pattern = "À FAIRE|sp_code|validation|READ ME|cad.", sheets.pre)) # keeps any other sheet
  
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  # lapply = applique la fonction suivante décrite à tous les éléments contenus dans l'objet "sheets" et crée une liste avec le résultat
  lapply(x, as.data.frame) # fait en un dataframe
  names(x) <- sheets
  x
}

cat_lists <- function(list1, list2) {   # concatener le contenu de listes aux noms identiques
  keys <- unique(c(names(list1), names(list2)))
  map2(list1[keys], list2[keys], c) %>% 
    set_names(keys)  
}

# ============================================================================= /
#  Data download and overwrite ----
# ============================================================================= /
# 1ier décembre 2025 fonctionne
# station_id.phd <- read.csv("connectivite/data/raw/station_id.phd.csv") # issu du script "Recherche_station_meteo_ID_v2.0.r"
# year <- (2024:2025) # ajouter 2026 en 2026 et dans bind_rows aussi
# list.data.format <- c("hourly", "daily", "monthly", "normals") # ajouter boucle pour données d'autres type au besoin
# meteoStat.site.year <- list()
# for(n in 1:nrow(station_id.phd)) {
#   for (i in 1:length(year)) {
#     # n<-1
#     URL <- paste0("https://data.meteostat.net/", list.data.format[1], "/", year[i],"/", station_id.phd$station_id_MeteoStat[n],".csv.gz")
#     temp <- tempfile()
#     download.file(url = URL, temp)
#     meteoStat.site.year[[i]] <- fread(temp)
#   }
#   aggr.meteoStat.site <- bind_rows(meteoStat.site.year[[1]], meteoStat.site.year[[2]]) # ajouter 3e année et + (2026, +) ou coder différemment
#   write.csv(aggr.meteoStat.site,  paste0("connectivite/data/raw/meteoStat.data.", station_id.phd$phd.site.name[n], ".csv"))
# }


# ============================================================================= /
#  Logger serial data import and cleaning ----
# ============================================================================= /

## data.metadata ----
# path <- "connectivite/data/raw/22224413_INK_20250721_hobo.csv" # lien pour tester fonction, mais dans le code, il se réfère aux lignes précédantes
# path <- "connectivite/data/raw/41387_STH_20241125_odyssey.CSV"
# data.metadata(raw.ll.files[i])
data.metadata <- function(path) { # type = odyssey vs  EST DANS MON PATH pas besoin de l'argument je vais lui dire
  if (grepl("odyssey", path)) { # début de la loop pour les ODYSSEY
    raw.ll.files.0 <- readLines(path) # lire en format texte
    # Warning message: [...] incomplete final line found on 'connectivite/data/raw/..._hobo.csv' -> OK
    raw.ll.files.1 <- gsub(" ,", ",", raw.ll.files.0) # enlever espaces inutiles
    raw.ll.files.2 <- gsub(" ", "", raw.ll.files.1) # enlever tous les espaces dans le subset de données
    ### création des subsets data & metadata ----
    raw.ll.files.2.metadata <-  raw.ll.files.2[c(1:9)] # inclus les anciens noms de colonnes, qui sont dans un format et un ordre bizzare
    raw.ll.files.2.data <- raw.ll.files.2[-c(1:9)]
    raw.ll.files.i <- list(raw.ll.files.2.data, raw.ll.files.2.metadata)
    return(list(raw.ll.files.2.data, raw.ll.files.2.metadata))
  }
  if (grepl("hobo", path)) { # début de la loop pour les ODYSSEY
    # k <- i
    raw.ll.files.init <- readLines(path) # lire en format texte
    # ** tz orig mentionnée dans la colonne ll.pre.0.metadata[2], coder pour l'obtenir au besoin
    # Warning message: [...] incomplete final line found on 'connectivite/data/raw/..._hobo.csv' -> OK
    ### création des subsets data & metadata ----
    raw.ll.files.metadata <-  raw.ll.files.init[c(1:2)] # inclus les anciens noms de colonnes, qui sont dans un format et un ordre bizzare
    raw.ll.files.data <- raw.ll.files.init[-c(1:2)]
    return(list(raw.ll.files.data, raw.ll.files.metadata)) }}

## metadata ----
#### vérification du fichier level logger brut : logger.serial.no == nom du fichier, sinon arrêter TOUT ! ----
# x <- raw.ll.files.i.init
# i <- 86 # odyssey
# i <- 12 # hobo
metadata <- function(x) {
  if (grepl("odyssey", raw.ll.files[i])) {
    texte <- x[[2]][4] # logger serial no, en base R
    numbers <- gregexpr("[0-9]+", texte)
    result <- regmatches(texte, numbers)
    probe.uid.i <- as.numeric(unlist(result))
    # no du level logger dans le nom du fichier brut (.csv), correspond à l'item "i" de la présente boucle
    texte <- raw.ll.files[i]
    numbers <- gregexpr("[0-9]+", texte)
    result <- regmatches(texte, numbers)
    fichier <- as.numeric(unlist(result))
    # test logger.serial.no == nom du fichier
    if(!(probe.uid.i %in% fichier)) { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc "else" statement)
      stop(paste0("Attention, le nom du fichier ne correspond pas au numéro de série du level logger. Fichier problématique : i = ", paste(i), "; ", raw.ll.files[i]))
    }
    # ajouts aux métadonnées des fichiers
    raw.ll.files.i.init[[2]][10:13] <- c(paste0("file.uid : ", unlist(result)[1], "_", unlist(result)[2]), paste0('file.name : ', "`", raw.ll.files[i], "`"), 
                                    paste0("probe.uid : ", probe.uid.i), paste0("date d'extraction des données : ", as.numeric(unlist(result)[2])))
    return(raw.ll.files.i.init)
  }
  if (grepl("hobo", raw.ll.files[i])) {
    texte <- as.data.frame(str_match(x[[2]], "(?s)LGR S/N: \\s*(.*?)\\s*,")) # extraire tout ce qui se trouve
    # entre "LGR S/N: " et la "," directement subséquente, sans savoir s'il y a des sauts de ligne et peu importe les espaces dans l'énoncé.
    probe.uid.i <- as.numeric(texte[2,2])
    # no du level logger dans le nom du fichier brut (.csv), correspond à l'item "k" de la présente boucle
    texte <- raw.ll.files[i]
    nombres <- gregexpr("[0-9]+", texte)
    resultat <- regmatches(texte, nombres)
    fichier <- as.numeric(unlist(resultat)[1])
    # test logger.serial.no == nom du fichier
    if(!(probe.uid.i %in% fichier)) { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc "else" statement)
      stop(paste0("Attention, le nom du fichier ne correspond pas au numéro de série du level logger. Fichier problématique : i = ", paste(i), "; ", raw.ll.files.i[i]))
    } # si problème : aller changer manuellement en utilisant le no de série (unique) inscrit dans le fichier et PAS son nom // ** 1. créer copie -> archive; 2. s'assurer de changer partout ** : QGIS, fichier, onglet, data_site.id // création du fichier.uid.i, nom unique du FICHIER qui ne pourra JAMAIS être dupliqué (utila dans seciton début et fin des mesures par périodes, pour un mm FICHIER)
    # ajouts aux métadonnées des fichiers
    raw.ll.files.i.init[[2]][3:6] <- c(paste0("file.uid : ", unlist(resultat)[1], "_", unlist(resultat)[2]), paste0('file.name : ', "`", raw.ll.files[i], "`"), 
                               paste0("probe.uid : ", probe.uid.i), paste0("date d'extraction des données : ", as.numeric(unlist(resultat)[2])))
    return(raw.ll.files.i.init)}}

## files.uids ----
# x <- raw.ll.files.i.init
files.uids <- function(x) { # création du fichier.uid.i, nom unique du FICHIER qui ne pourra JAMAIS être dupliqué (utile dans section début et fin des mesures par périodes, pour un mm FICHIER)
  if (grepl("odyssey", raw.ll.files[i])) {
    texte <- x[[2]][4] # logger serial no, en base R
    numbers <- gregexpr("[0-9]+", texte)
    result <- regmatches(texte, numbers)
    probe.uid.i <- as.numeric(unlist(result))
    texte <- raw.ll.files[i]
    numbers <- gregexpr("[0-9]+", texte)
    result <- regmatches(texte, numbers)
    fichier <- as.numeric(unlist(result))
    file.uid.df[i,1:4] <- c(paste0(unlist(result)[1], "_", unlist(result)[2]), raw.ll.files[i], probe.uid.i, as.numeric(unlist(result)[2])) # ceci sera gardé en mémoire (doit être identique à la colonne fichier.uid dans cal.data)
    return(file.uid.df)
  }
  if (grepl("hobo", raw.ll.files[i])) {
    texte <- as.data.frame(str_match(x[[2]], "(?s)LGR S/N: \\s*(.*?)\\s*,")) # extraire tout ce qui se trouve
    # entre "LGR S/N: " et la "," directement subséquente, sans savoir s'il y a des sauts de ligne et peu importe les espaces dans l'énoncé.
    probe.uid.i <- as.numeric(texte[2,2])
    # no du level logger dans le nom du fichier brut (.csv), correspond à l'item "k" de la présente boucle
    texte <- raw.ll.files[i]
    nombres <- gregexpr("[0-9]+", texte)
    resultat <- regmatches(texte, nombres)
    fichier <- as.numeric(unlist(resultat)[1])
    file.uid.df[i,1:4] <- c(paste0(unlist(resultat)[1], "_", unlist(resultat)[2]), raw.ll.files[i], probe.uid.i, as.numeric(unlist(resultat)[2])) # ceci sera gardé en mémoire (doit être identique à la colonne fichier.uid dans cal.data)
    return(file.uid.df)  }}

# raw_to_clean.ll ----
# file.i.raw.data <- raw.ll.files.i[[1]]
raw_to_clean.ll <- function(file.i.raw.data) { # ne calibre pas encore les données
  if (grepl("odyssey", raw.ll.files[i])) {
    raw.ll.data <- read.csv(text = raw.ll.files.i[[1]], # création du dataframe contenant données de nappe phréatique et ménage  ----
                            col.names = c("scan.id", "date.JJ.MM.AAAA", "time.HH.MM.SS",'raw.value.mm',"calibrated.value.cm")) 
    raw.ll.data$calibrated.value.cm <- ifelse(raw.ll.data$raw.value.mm == raw.ll.data$calibrated.value.cm, yes = raw.ll.data$calibrated.value.cm[rep("NA", times = length(raw.ll.data$calibrated.value.cm))], no = raw.ll.data$calibrated.value.cm)
    
    ### date et heure : format ISO date AAAA-MM-JJTHH:MM:SS,ss-/+FF:ff, voir https://fr.wikipedia.org/wiki/ISO_8601 ----
    # heure : « Z » à la fin lorsqu’il s’agit de l’heure UTC. (« Z » pour méridien zéro, aussi connu sous le nom « Zulu » dans l’alphabet radio international).
    # extraction : nom du site pour trouver les coordonnées qui serviront à connaître le fuseau horaire
    site.name.pre <- sub("SiteName","",raw.ll.files.i[[2]][1])
    site.name <- stringr::str_to_title(gsub(",", "", site.name.pre))
    tz <- zones("~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp")
    # ajouts aux métadonnées des fichiers
    file.uid.df[i,5] <- tz
    raw.ll.files.i[[2]][14] <- paste0("original time zone : ", tz)
    #### ménage de la date et heure  ----
    # modifier mes colonnes pour avoir le format ISO (manque encore le UTC à ajouter à la fin)
    # garder date.AAAA-MM-JJ"
    ll.pre.2.data.2 <- raw.ll.data %>% dplyr::mutate(date.JJ.MM.AAAA_time.HH.MM.SS_tz = paste0(date.JJ.MM.AAAA," ", time.HH.MM.SS, " ", tz)) %>% 
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
    return(ll.pre.2.data.4)
  }
  if (grepl("hobo", raw.ll.files[i])) {
    raw.ll.data <- read.csv(text = raw.ll.files.i[[1]], header = F, col.names = c("scan.id", "date.JJ.MM.AAAA_time.HH.MM.SS",	"raw.value.kPa_pres.abs",	"temperature_dC", "Coupleur détaché", "Coupleur attaché", 'Hôte connecté',	"Arrêté", "Fin de fichier")) # text = argument de read.csv qui lit la valeur contenue dans l'objet / DATE mauvais format
    ll.pre.0.data.1 <- raw.ll.data[1:4] # garder seules les colonnes pertinentes
    
    #### date et heure : format ISO date AAAA-MM-JJTHH:MM:SS,ss-/+FF:ff, voir https://fr.wikipedia.org/wiki/ISO_8601 ----
    # heure : « Z » à la fin lorsqu’il s’agit de l’heure UTC. (« Z » pour méridien zéro, aussi connu sous le nom « Zulu » dans l’alphabet radio international).
    # extraction : nom du site pour trouver les coordonnées qui serviront à connaître le fuseau horaire
    site.0 <- gsub("\\\"", '', raw.ll.files.i[[2]])[1] # extraire nom de site fichier origine
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
    file.uid.df[i,5] <- tz
    raw.ll.files.i[[2]][7] <- paste0("original time zone : ", tz)
    
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
    return(ll.pre.0.data.3)
    }
    }

# ============================================================================= /
#  EN CHANTIER ----
# ============================================================================= /
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

# read_hobo <- function(path){
#   read.csv(path, sep = "\t") %>% 
#     slice(-(1:5)) %>% # enlever métadonnées, traitées à part
#     separate(1, into = c("scan_no", "date", "hour", "raw", "calibrated"), sep = ",") %>% # ligne 1 = nom des colonnes
#     # nettoyer données dates et heure
#     mutate(date = gsub("\\s+", "", date)) %>% # "\\s+" = enlever les espaces
#     mutate(hour = gsub("\\s+", "", hour)) %>% 
#     mutate(hour = gsub(":", "/", hour)) %>% 
#     mutate(date = paste0(date, "/", hour)) %>% 
#     mutate(date = as.POSIXct(date, format = "%d/%m/%Y/%H/%M/%OS")) %>% # combiner date et heure
#     dplyr::select(-hour) -> tidy.data # enelver vielle colonne heure inutile
#   
#   return(tidy.data)
# }


# tests
# # read_hobo <- function(path){
# read.csv("connectivite/data/raw//10279777_INK_20250106_hobo.csv", sep = "\t") %>%  # 
# # read.csv("connectivite/data/raw//20573974_INK_20250106_hobo.csv", sep = "\t") %>%  # 
# # read.csv("connectivite/data/raw//10279769_INK_20250106_hobo.csv", sep = "\t") %>%  # 
#   #,"Date Heure, GMT-04:00","Pres. abs., kPa (LGR S/N: 10279777, SEN S/N: 10279777)","Temp., °C (LGR S/N: 10279777, SEN S/N: 10279777)","Coupleur détaché (LGR S/N: 10279777)","Coupleur attaché (LGR S/N: 10279777)","Hôte connecté (LGR S/N: 10279777)","Arrêté (LGR S/N: 10279777)","Fin de fichier (LGR S/N: 10279777)"  # slice(-(1)) %>% # enlever métadonnées, traitées à part (slice) 
#   slice(-1) %>% # ligne 1 = nom des colonnes
#   separate(1, into = c("scan_no", "date.hour", , "raw","notes.1","notes.2","notes.3","notes.4","notes.5", "notes.6"), sep = ",") %>% 
#   tail() #head()
#   # nettoyer données dates et heure
#   mutate(date = gsub("\\s+", "", date)) %>% # "\\s+" = enlever les espaces
#   mutate(hour = gsub("\\s+", "", hour)) %>% 
#   mutate(hour = gsub(":", "/", hour)) %>% 
#   mutate(date = paste0(date, "/", hour)) %>% 
#   mutate(date = as.POSIXct(date, format = "%d/%m/%Y/%H/%M/%OS")) %>% # combiner date et heure
#   dplyr::select(-hour) -> data # enelver vielle colonne heure inutile
#   return(data)
# # }
# head(data)

# UTILISATION DANS DATA_WATER.TABLE_ALL
SNH <- as.vector(c("_odyssey", "_hobo"), mode = "character") # liste des types de SNH avec lesquelles j'ai pris des données; chaque "marque" est traitée de façon différente
# # raw.ll.files <- list.files(path = "connectivite/data/raw/", pattern = "_odyssey|_hobo", full.names = T) # equivalent à ll.clean (ancien)
# for (i in 1:length(raw.files)) {
#   i <- 2
#   # ajouter condition : si hobo vs odyssey <- dans une fonction ?
#   if (grepl(SNH[1], raw.ll.files[i])) {
#     NULL
#     # data.i -> tidy.WTD.data[[i]]
#   }
#   else if (grepl(SNH[2], raw.ll.files[i]))
#   data.i <- read_hobo(raw.ll.files[i])
#   data.i <- tidy.WTD.data[[i]] # ne fonctionne pas
# }
# head(data.i)

cat_lists <- function(list1, list2) {   # concatener le contenu de listes aux noms identiques
  keys <- unique(c(names(list1), names(list2)))
  map2(list1[keys], list2[keys], c) %>% 
    set_names(keys)  
}




# lapply(raw.files, read_hobo) -> tidy.WTD.data # ne fonctionne pas

# x <- raw.files[[1]]
# purrr::map(raw.files, # gérer des données en liste, renvoyer une liste (équivalent à lapply dans base R)
#            function(x){
#              read_hobo(x) %>% 
#                mutate(file = x) %>% # ajouter une colonne avec le nom de fichier
#            }) -> data
# 
# purrr::map_dfr(files,  # gérer des données en liste, renvoyer un dataframe aux lignes concatennées (lapply ne fait pas ça)
#                function(x){
#                  read_hobo(x) %>% 
#                    mutate(file = x) %>% 
#                    mutate(hour = hour(date))
#                }) -> data
# data %>% 
#   filter(hour == 20) # filtre à traver l'ENSEMBLE DES DONNÉES !




# ============================================================================= /
#  HOBO ----
# ============================================================================= /
## read_hobo ----

# NONÀJOURread_hobo <- function(path){
#   read.csv(path, sep = "\t") %>% 
#     slice(-(1:5)) %>% # enlever métadonnées, traitées à part
#     separate(1, into = c("scan_no", "date", "hour", "raw", "calibrated"), sep = ",") %>% # ligne 1 = nom des colonnes
#     # nettoyer données dates et heure
#     mutate(date = gsub("\\s+", "", date)) %>% # "\\s+" = enlever les espaces
#     mutate(hour = gsub("\\s+", "", hour)) %>% 
#     mutate(hour = gsub(":", "/", hour)) %>% 
#     mutate(date = paste0(date, "/", hour)) %>% 
#     mutate(date = as.POSIXct(date, format = "%d/%m/%Y/%H/%M/%OS")) %>% # combiner date et heure
#     dplyr::select(-hour) -> data # enelver vielle colonne heure inutile
#   
#   return(data)
# }

## read_hobo_header ----
NONÀJOURread_hobo_header <- function(path){
  read.csv(path, sep = "\t") %>% 
    slice((1:5)) -> data
  return(data)
  
}

## data.metadata.hobo ----
# path <- "connectivite/data/raw/22224413_INK_20250721_hobo.csv"
# data.metadata.hobo(path)
# data.metadata <- function(path, SNH) {
#   
#   grepl(SNH[2], raw.ll.files[i])
#   if (grepl(SNH[2], raw.ll.files[i])) {
#     raw.ll.files.0 <- readLines(path) # lire en format texte
#     # Warning message:
#     #   In readLines(paste0("connectivite/data/raw/", ll.pre[i])) :
#     #   incomplete final line found on 'connectivite/data/raw/[...].csv'
#     # c'est chill, je n'ai pas réussi à arranger ça, mais vérifié √ pas de problème
#     # enlever espaces inutiles
#     
#     ### création des subsets data & metadata ----
#     raw.ll.files.1.metadata <-  raw.ll.files.0[c(1:2)] # inclus les anciens noms de colonnes, qui sont dans un format et un ordre bizzare
#     raw.ll.files.1.data <- raw.ll.files.0[-c(1:2)]
#     
#     raw.ll.files.i <- list(raw.ll.files.1.data, raw.ll.files.1.metadata)
#     return(raw.ll.files.i)}  
# }  
# metadata.verif.hobo(raw.ll.files.i)

  

## metadata.verif ---- 
#### vérification du fichier level logger brut : logger.serial.no == nom du fichier, sinon arrêter TOUT ! ----
# x <- raw.ll.files.i

  metadata.verif.hobo <- function(x) {
    texte <- as.data.frame(str_match(x[[2]], "(?s)LGR S/N: \\s*(.*?)\\s*,")) # extraire tout ce qui se trouve
    # entre "LGR S/N: " et la "," directement subséquente, sans savoir s'il y a des sauts de ligne et peu importe les 
    # espaces dans l'énoncé.
    probe.uid.i <- as.numeric(texte[2,2])
    # no du level logger dans le nom du fichier brut (.csv), correspond à l'item "k" de la présente boucle
    texte <- raw.ll.files[i]
    nombres <- gregexpr("[0-9]+", texte)
    resultat <- regmatches(texte, nombres)
    fichier <- as.numeric(unlist(resultat)[1])
    # test logger.serial.no == nom du fichier
    if(!(probe.uid.i %in% fichier)) { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc "else" statement)
      stop(paste0("Attention, le nom du fichier ne correspond pas au numéro de série du level logger. Fichier problématique : i = ", paste(i), "; ", raw.ll.files.i[i]))
    }
    # si problème : aller changer manuellement en utilisant le no de série (unique) inscrit dans le fichier et PAS son nom 
    # ** 1. créer copie -> archive; 2. s'assurer de changer partout ** : QGIS, fichier, onglet, data_site.id
  }
  








# ============================================================================= /
#  ODYSSEY ----
# ============================================================================= /

## read_odyssey ----
# comprend des sous-fonctions qui se trouvent ensuite, en ordre
read_odyssey <- function(path){
  
  data.metadata.odyssey(path) # sous fonction, ci-dessous  √ OK 20102025
  file.name.verif(raw.ll.files.i) # objet raw.ll.files.i créé dans fonction précédante √ OK 20102025
  # [...]
  return(tidy.WTD.data.i) # retourne la liste des objets nettoyés, composée de data + metadata
}

## data.metadata.odyssey ----
# séparer données et métadonnées
# créé une pour odyssey + hobo

## metadata.verif.hobo ----
# vérification : probe.uid dans les métadonnées == nom du fichier
metadata.verif.odyssey <- function(x) {
  # trouver le probe.uid.i (== probe.uid, logger serial no) dans les metadata
  texte <- x[[2]][4] # logger serial no, en base R
  numbers <- gregexpr("[0-9]+", texte)
  result <- regmatches(texte, numbers)
  probe.uid.i <- as.numeric(unlist(result))
  # no du level logger dans le nom du fichier brut (.csv), correspond à l'item "i" de la présente boucle
  texte <- raw.ll.files[i]
  numbers <- gregexpr("[0-9]+", texte)
  result <- regmatches(texte, numbers)
  fichier <- as.numeric(unlist(result))
  # test logger.serial.no == nom du fichier
  if(!(probe.uid.i %in% fichier)) { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc "else" statement)
    stop(paste0("Attention, le nom du fichier ne correspond pas au numéro de série du level logger. Fichier problématique : i = ", paste(i), "; ", raw.ll.files[i]))
  }
}


## data.tidying.odyssey ----
#



# ____Rendue là_____

  
# tests










## read_odyssey ----
# (fonction qui met en action les fonctions précédantes)

## read_odyssey_header ----

# ============================================================================= /
#  Georeferenced data ----
# ============================================================================= /
# trouver le fuseau horaire des sites
# zone.shp <- "~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp"
zones <- function(zone.shp) {
  zones <- read_sf(zone.shp) %>% # ouvrir données du shapefile pour accéder les zones
    as.data.frame(zones)
  coords <- c(zones$latitude[zones$site==site.name][1], zones$longitude[zones$site==site.name][1]) # extraire la bonne lat, long selon le nom du site
  (tz <- tz_lookup_coords(coords[1], coords[2], method = "fast", warn = FALSE)) # trouver le UTC selon la lat long
  
  return(tz)
}


# ============================================================================= /
#  Calibration data ----
# ============================================================================= /
# données de bulleur, emplacement des puits, nom de fichier, long. fil, etc.
cal.data <- function(cal.data.path) {
  cal.data <- read.csv(cal.data.path, sep = ";", dec = ",") %>%
    mutate_at("probe.uid", as.character)
  cal.data$out.R = round(cal.data$pt.haut.cm - ((cal.data$pt.bas1.cm+cal.data$pt.bas2.cm+cal.data$pt.bas3.cm)/3), digits = 1) # out = (pt haut - moyenne pt bas)
  cal.data$long_negative_cal.length_mm_y <- cal.data$cal.length.cm*-10 # longueur de fil nécessaire : en mm et au négatif / les NA seront calculé prochainement / 
  cal.data <- cal.data %>% dplyr::select("fichier.uid","measure_type", "measure_status", "site.uid", "well.uid", "trmnt.uid", "lab.probe.id", "probe.uid", "probe.brand", 
                                         "cal.length.cm", "cal.order", "long_negative_cal.length_mm_y", "cal.value_x", "comment", 
                                         "day.begining.aaaa.mm.dd.hh.mm", "day.end.aaaa.mm.dd.hh.mm", "distance.m", "out.R", "out.long.tuyau.sol.cm", everything()) #, -"caduque.long.fil.cm")
  cal.data$period.fichier.uid <- paste0(cal.data$day.begining.aaaa.mm.dd.hh.mm, "--", cal.data$day.end.aaaa.mm.dd.hh.mm, ".",cal.data$fichier.uid)
  
  # vérifier si moyenne des valeurs OUT du puits concordent ou si doivent être mises à jour
  round(cal.data$out.long.tuyau.sol.cm, digits = 1)
  if(all(cal.data$out.R == round(cal.data$out.long.tuyau.sol.cm, digits = 1), na.rm = T))  { # si TOUS TRUE (fonction any()) = changer nom de out.R et supprimer la mesure entrée manuellement // si FALSE = avertissement
    cal.data$out.long.tuyau.sol.cm <- cal.data$out.R
    cal.data <- cal.data %>% dplyr::select(!out.R) # out.R DISPARAÎT ! NE PLUS LA CHERCHER !
  } else { stop("Attention, le out entré dans cal.data (syn. level_logger_calibration_all.csv) n'est pas identique à la moyenne des points bas soustraite du point haut du puits.") }
  # format POSIX begining et end
  cal.data$day.begining.aaaa.mm.dd.hh.mm <- ymd_hm(cal.data$day.begining.aaaa.mm.dd.hh.mm, tz = tz)
  cal.data$day.end.aaaa.mm.dd.hh.mm <- ymd_hm(cal.data$day.end.aaaa.mm.dd.hh.mm, tz = tz)
  
  return(cal.data)
}  




# ============================================================================= /
#  À appliquer sur tidy.WTD.data ----
# ============================================================================= /
# x <- raw.files[[1]]
# purrr::map(tidy.WTD.data, # gérer des données en liste, renvoyer une liste (équivalent à lapply dans base R)
#            function(x){
#              mutate(file.name = x) %>% # ajouter une colonne avec le nom de fichier
#            }) -> data
# 
# purrr::map_dfr(tidy.WTD.data,  # gérer des données en liste, renvoyer un dataframe aux lignes concatennées (lapply ne fait pas ça)
#                function(x){
#                  read_hobo(x) %>% # ajuster; seulement si je 
#                    mutate(file.name = x) %>% # ajouter une colonne avec le nom de fichier
#                }) -> data
# data %>% 
#   filter(hour == 20) # filtre à travers l'ENSEMBLE DES DONNÉES !
# # il faudra surement enlever les métadonnées...

