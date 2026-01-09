# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : 2025-10-28
# Date mise à jour : 2026-01-08
# Pourquoi : 
# Pour charger les fonctions utiles à traiter données de SHN
# NOTES : 
# SNH : sondes de niveau hydrostatique

# La fonction s'applique à un fichier brut, traite, corrige et nettoye, et produit un fichier de données aux composantes identiques, peu importe le type de sonde.
# Produit un dataframe rectangulaire. Les métadonnées sont à part (liste avec données + métadonnées + données de vérification).
# Merci à Francis Lessard pour ses idées.
# référer aux fonctions en sourçant ce script (ligne suivante)
# source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd_v2.0.R")

# ============================================================================= /
#  Libraries ----
# ============================================================================= /
if (!require("tidyverse")) install.packages("tidyverse") # méta package // gosser avec des suites de caractères, str_replace, [...]
# if (!require("dplyr")) install.packages("dplyr") # entre autres : left_join()
# if (!require("tidyr")) install.packages("tidyr") # entre autres : extract_numeric() / extract_numeric() is deprecated: please use readr::parse_number() instead
# contient purr aussi
# 3 packages inutiles après avoir refomaté le code en tidyverse
if (!require("data.table")) install.packages("data.table") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("sf")) install.packages("sf"); if (!require("lutz")) install.packages("lutz") # GIS in R
if (!require("readxl")) install.packages("readxl") # lire les excel
if (!require("openxlsx")) install.packages("openxlsx") # lire les excel
if (!require("conflicted")) install.packages("conflicted") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("stringr")) install.packages("stringr") # gosser avec des suites de caractères, str_replace, [...]
if (!require("lubridate")) install.packages("lubridate")
options(lubridate.verbose = TRUE) # pour expliciter ce que les fonctions font
if (!require("parsedate")) install.packages("parsedate") # lire les excel
# if (!require("naniar")) install.packages("naniar") # Checking data completeness
# if (!require("mapview")) install.packages("mapview") ## Spatial analyses
# option d'arrêter le code si message d'erreur (source fonctions.R)
# options(error=pause)
# options(error=NULL) # annuler

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

# select.raw.ll.files
# vector.to.filter <- raw.ll.files.pre
select.raw.ll.files <- function(vector.to.filter) { # ne calibre pas encore les données
  cal.data.0 <- read.csv("connectivite/data/raw/level_logger_calibration_all.csv", sep = ";", dec = ",") 
  filter.out <- cal.data.0$file.uid[grep("rejected", cal.data.0$measure_status)]
  str_split <- str_split(filter.out, "_")
  filter.out.df <- data.frame(do.call(rbind, str_split)) # colnames = c("probe.uid", "extr.date"))
  exclude.lines <- vector()
  for(exclude in 1:nrow(filter.out.df)) {
    exclude.lines[exclude] <- which(grepl(filter.out.df[exclude,1], vector.to.filter) & grepl(filter.out.df[exclude,2], vector.to.filter))
  }
  raw.ll.files <- vector.to.filter[-exclude.lines]
  return(raw.ll.files)
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

## data.metadata
# path <- "connectivite/data/raw/22224403_INK_20251202_hobo.csv"
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

## metadata
# x <- raw.ll.files.i.init
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
    return(raw.ll.files.i.init)}
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

## files.uids
# x <- raw.ll.files.i.init
files.uid.df <- data.frame(file.uid = NA, file.name = NA, probe.uid = NA, "extraction.data.aaaammjj" = NA, "tz_orig" = NA, probe.brand = NA) # pour stocker les fichier.uid (aussi première colonne de cal.data) et autres données intérimaires
files.uid <- function(x) { # création du fichier.uid.i, nom unique du FICHIER qui ne pourra JAMAIS être dupliqué (utile dans section début et fin des mesures par périodes, pour un mm FICHIER)
  if (grepl("odyssey", raw.ll.files[i])) {
    texte <- x[[2]][4] # logger serial no, en base R
    numbers <- gregexpr("[0-9]+", texte)
    result <- regmatches(texte, numbers)
    probe.uid.i <- as.numeric(unlist(result))
    texte <- raw.ll.files[i]
    numbers <- gregexpr("[0-9]+", texte)
    result <- regmatches(texte, numbers)
    fichier <- as.numeric(unlist(result))
    files.uid.df[i,1:4] <- c(paste0(unlist(result)[1], "_", unlist(result)[2]), raw.ll.files[i], probe.uid.i, as.numeric(unlist(result)[2])) # ceci sera gardé en mémoire (doit être identique à la colonne fichier.uid dans cal.data)
    return(files.uid.df)}
  if (grepl("hobo", raw.ll.files[i])) {
    texte <- as.data.frame(str_match(x[[2]], "(?s)LGR S/N: \\s*(.*?)\\s*,")) # extraire tout ce qui se trouve
    # entre "LGR S/N: " et la "," directement subséquente, sans savoir s'il y a des sauts de ligne et peu importe les espaces dans l'énoncé.
    probe.uid.i <- as.numeric(texte[2,2])
    # no du level logger dans le nom du fichier brut (.csv), correspond à l'item "k" de la présente boucle
    texte <- raw.ll.files[i]
    nombres <- gregexpr("[0-9]+", texte)
    resultat <- regmatches(texte, nombres)
    fichier <- as.numeric(unlist(resultat)[1])
    files.uid.df[i,1:4] <- c(paste0(unlist(resultat)[1], "_", unlist(resultat)[2]), raw.ll.files[i], probe.uid.i, as.numeric(unlist(resultat)[2])) # ceci sera gardé en mémoire (doit être identique à la colonne fichier.uid dans cal.data)
    return(files.uid.df)  }}

## raw.to.clean_ll
# file.i.raw.data <- raw.ll.files.i[[1]]
raw.to.clean_ll <- function(file.i.raw.data) { # ne calibre pas encore les données
  if (grepl("odyssey", raw.ll.files[i])) {
    raw.ll.data <- read.csv(text = raw.ll.files.i[[1]], # création du dataframe contenant données de nappe phréatique et ménage  ----
                            col.names = c("scan.id", "date.JJ.MM.AAAA", "time.HH.MM.SS",'raw.value.mm',"calibrated.value.cm")) 
    raw.ll.data$calibrated.value.cm <- ifelse(raw.ll.data$raw.value.mm == raw.ll.data$calibrated.value.cm, yes = raw.ll.data$calibrated.value.cm[rep("NA", times = length(raw.ll.data$calibrated.value.cm))], no = raw.ll.data$calibrated.value.cm)
    
    ### date et heure : format ISO date AAAA-MM-JJTHH:MM:SS,ss-/+FF:ff, voir https://fr.wikipedia.org/wiki/ISO_8601 ----
    # heure : « Z » à la fin lorsqu’il s’agit de l’heure UTC. (« Z » pour méridien zéro, aussi connu sous le nom « Zulu » dans l’alphabet radio international).
    ll.pre.2.data.2 <- raw.ll.data %>% dplyr::mutate(date.JJ.MM.AAAA_time.HH.MM.SS = paste0(date.JJ.MM.AAAA," ", time.HH.MM.SS))
    ll.pre.2.data.2$date.time.tz.orig <- readr::parse_datetime(ll.pre.2.data.2$date.JJ.MM.AAAA_time.HH.MM.SS, format = '%d/%m/%Y %H:%M:%S') #, locale = readr::locale(tz = tz)) # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
    ll.pre.2.data.2 <- ll.pre.2.data.2 %>% 
      mutate(date.time.tz.orig.roundd.pre = round_date(date.time.tz.orig, unit = "hours") + seconds(1)) 
    ll.pre.2.data.2$date.time.tz.orig.roundd <- gsub("24:00:", "00:00:", ll.pre.2.data.2$date.time.tz.orig.roundd.pre)
    
    ll.pre.2.data.2 <- ll.pre.2.data.2 %>% mutate(date.JJ.MM.AAAA_time.HH.MM.SS_tz =  paste0(date.time.tz.orig.roundd, " ", tz)) %>% 
      dplyr::select(!c(date.JJ.MM.AAAA, time.HH.MM.SS)) # supprimer ces colonnes en format character, recréer bientôt en POSIX
    ll.pre.2.data.2$date.time.tz.orig <- readr::parse_datetime(ll.pre.2.data.2$date.JJ.MM.AAAA_time.HH.MM.SS_tz, format = '%Y-%m-%d %H:%M:%S %Z', locale = readr::locale(tz = tz)) # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
    ll.pre.2.data.3 <- data.frame(separate_wider_position(ll.pre.2.data.2, # date et time en deux colonnes (idem à ODYSSEY)
                                                          widths = c("date.AAAA.MM.JJ" = 11, "time.HH.MM.SS" = 8),
                                                          cols = date.time.tz.orig, cols_remove = F)) 
    ll.pre.2.data.3$`date.AAAA-MM-JJ` = ymd(ll.pre.2.data.3$date.AAAA.MM.JJ, tz = tz)
    ll.pre.2.data.3$date.time.UTC.0pre <- with_tz(ll.pre.2.data.3$date.time.tz.orig, tz = "UTC") # pour convertir AM/PM en décimales (0-24h), élément %p voir documentation
    tz(ll.pre.2.data.3$date.time.UTC.0pre) # UTC
    ll.pre.2.data.3$date.time.UTC.0pre.1 <- format_iso_8601(ll.pre.2.data.3$date.time.UTC.0pre)
    ll.pre.2.data.3$date.time.UTC.0 <- gsub("[+]00:00", "Z",  ll.pre.2.data.3$date.time.UTC.0pre.1)
    # ll.pre.2.data.3$date.time.tz.orig <- gsub("00:00:01", "24:00:01", ll.pre.2.data.3$date.time.tz.orig) # rechanger les 00:00:01 dans date.time.tz.orig pour ne pas perdre des lignes (7 avril 2025)
    # tel que codé actuellement, il peut y avoir un décalage de +/- une heure à cause que TZ prend l'heure basée sur Sys.timezone, qui dépend de l'heure d'été ou d'hiver
    # ARRANGER UN JOUR (langage C++ pour plus de complications) # ou alors setter cette date manuellement (voir à chaque année la date de changement d'heure) # Sys.timezone(location = F) essayé, n'aide pas
    # nom final (et retirer colonnes inutiles)
    ll.clean <- ll.pre.2.data.3 %>% dplyr::select(!c(date.AAAA.MM.JJ,  "date.time.UTC.0pre", "date.time.UTC.0pre.1")) %>% 
      dplyr::select("scan.id", "raw.value.mm", "calibrated.value.cm", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", date.time.UTC.0) # date et time sans "UTC.0" sont dans le fuseau horaire d'origine (tz trouvé en croisant les coordonnées "coords")
    return(ll.clean)}
  if (grepl("hobo", raw.ll.files[i])) {
    raw.ll.data <- read.csv(text = raw.ll.files.i[[1]], header = F, col.names = c("scan.id", "date.JJ.MM.AAAA_time.HH.MM.SS",	"raw.value.kPa_pres.abs",	"temperature_dC", "Coupleur détaché", "Coupleur attaché", 'Hôte connecté',	"Arrêté", "Fin de fichier")) # text = argument de read.csv qui lit la valeur contenue dans l'objet / DATE mauvais format
    ll.pre.0.data.1 <- raw.ll.data[1:4] # garder seules les colonnes pertinentes
    #### date et heure : format ISO date AAAA-MM-JJTHH:MM:SS,ss-/+FF:ff, voir https://fr.wikipedia.org/wiki/ISO_8601 ----
    # heure : « Z » à la fin lorsqu’il s’agit de l’heure UTC. (« Z » pour méridien zéro, aussi connu sous le nom « Zulu » dans l’alphabet radio international).
    # ajouts aux métadonnées des fichiers
    files.uid.df[i,5] <- tz
    raw.ll.files.i[[2]][7] <- paste0("original time zone : ", tz)
    #### ménage de la date et heure  ----
    # EXAMPLE ICI FONCTION DE TRANSFORMATION DATE HEURE RÉUTILISABLE
    # format_datetime <- function(data, col.date, col.time, col.datetime)
    # sortir la fonction d'ici et y référer
    # coller le tz dans la colonne "date.JJ.MM.AAAA_time.pre.HH.MM.SS"
    ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS_tz <- paste0(ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS, " ", tz)
    ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS_tz <- gsub("00:00", "00:01", ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS_tz) # sinon, les données 00:00:00 étaient effacées !
    ll.pre.0.data.1$date.time.tz.orig <- readr::parse_datetime(ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS_tz, format = '%m/%d/%y %I:%M:%S %p %Z', locale = readr::locale(tz = tz)) # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
    ll.pre.0.data.1.rdd <- ll.pre.0.data.1 %>% mutate(date.time.tz.orig.roundd = round_date(date.time.tz.orig, unit = "hours") + seconds(1)) 
    ll.pre.0.data.2 <- data.frame(separate_wider_position(ll.pre.0.data.1.rdd, # date et time en deux colonnes (idem à ODYSSEY)
                                                          widths = c("date.AAAA.MM.JJ" = 11, "time.HH.MM.SS" = 8),
                                                          cols = date.time.tz.orig.roundd, cols_remove = F)) 
    ll.pre.0.data.2$`date.AAAA-MM-JJ` = ymd(ll.pre.0.data.2$date.AAAA.MM.JJ, tz = tz)
    ll.pre.0.data.2$date.time.UTC.0pre <- with_tz(ll.pre.0.data.2$date.time.tz.orig.roundd, tz = "UTC") # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
    tz(ll.pre.0.data.2$date.time.UTC.0pre) # GMT = UTC
    ll.pre.0.data.2$date.time.UTC.0pre.1 <- format_iso_8601(ll.pre.0.data.2$date.time.UTC.0pre)
    ll.pre.0.data.2$date.time.UTC.0 <- gsub("[+]00:00", "Z",  ll.pre.0.data.2$date.time.UTC.0pre.1)
    # ajouter colonne vide "calibrated value" à l'instar de ODYSSEY, où sera inséré la valeur finale de nappe phréatique
    ll.pre.0.data.2$"calibrated.value.cm" <- rep(NA, times = nrow(ll.pre.0.data.2))
    # nom final (et retirer colonnes inutiles)
    ll.clean <- ll.pre.0.data.2 %>% select(!c(date.JJ.MM.AAAA_time.HH.MM.SS, date.AAAA.MM.JJ,  "date.time.UTC.0pre", "date.time.UTC.0pre.1")) %>% 
      select("scan.id", "date.JJ.MM.AAAA_time.HH.MM.SS_tz", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0", 
             "raw.value.kPa_pres.abs", "temperature_dC", "calibrated.value.cm")
    # suite : si calibration intégrée avec le hobo, QUE FAIRE ? coder ici, voir procédure avec ODYSSEY
    return(ll.clean)}}

# concatenate.ll
{ # explications boucle de concaténation
# boucle de concaténation des données (fichier.uid ensemble, sinon autre calibration et graphique distinct)
# raison de l'étape : si sonde retirée et remise, sans écraser les données contenues (continuation des mesures), retirer la période 
# de données invalides (quelques heures, période de rééquilibrage) et recoller les lignes ensemble pour former le fichier d'heures valide
# explications de cette loop
# mm fichier.uid (loop extrait séquentiellement toutes les lignes de chaque # de SNH, qui peuvent être uniques ou multiples pour un SNH donné);
# la loop teste si toutes les lignes de ce # de SNH ont le même fichier.uid (i), dans quel cas, si les périodes sont différentes, 
# la boucle coupe le fichier pour chaque période différente (l), et ensuite réassemble le fichier avec seules les périodes à conserver
  }
# file.to.concat <- ll.clean
concatenate.ll <- function(file.to.concat) {
  ll.cal.pre.i.l <- list()
  if (grepl("odyssey", raw.ll.files[i])) {
    if (length(unique(cal.data$period.file.uid[which(grepl(files.uid.df[i,1], cal.data$file.uid))]))>0) { # pour contourner erreur si aucun fichier dans les lignes de cal.data (p. ex. si étiquettées "rejected", comme '41361_20241125')
      for (l in 1:length(unique(cal.data$period.file.uid[which(grepl(files.uid.df[i,1], cal.data$file.uid))]))) { print(l) # si mm fichier.uid.i, coller les périodes ensemble (ainsi, retirer et remettre ne demande pas plus de manipulations et surtout ps des manipulations individuelles)
        # l<-1
        cal.data.i.l <- unique(cal.data[which(grepl(files.uid.df[i,1], cal.data$file.uid)),
                                            c("file.uid", "site.uid", "well.uid", "trmnt.uid", 'lab.probe.id', 'probe.uid', 'probe.brand',
                                              "day.begining.aaaa.mm.dd.hh.mm", 'day.end.aaaa.mm.dd.hh.mm', "period.file.uid")])[l,] # cal.data.i.l = les infos dont j'ai besoin pour recouper selon la période l du fichier i
        # recoupage de ll.pre.data selon cal.data selon début et fin des mesures et retrait de colonnes
        ll.clean.l <- ll.clean %>%
          dplyr::filter(date.time.tz.orig >= cal.data.i.l$day.begining.aaaa.mm.dd.hh.mm) %>% # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data.i.l
          dplyr::filter(date.time.tz.orig <= cal.data.i.l$day.end.aaaa.mm.dd.hh.mm) %>% # <= date de mesure de NP plus petite ou égale à la date end dans cal.data.i.l 
          dplyr::select("scan.id", "raw.value.mm", "calibrated.value.cm", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0") # %>%  # date et time sans "UTC.0" sont dans le fuseau horaire d'origine (tz trouvé en croisant les coordonnées "coords")
        # changer pour un nom explicite, fichier encore à calibrer (d'où "pre")
        ll.cal.pre.i.l[[l]] <- ll.clean.l
      }
      # coller toutes les données de la sonde i ensemble (différentes mesures temporelles, mm puits.trmnt.année) ----
      ll.cal.pre.i <- do.call(rbind, ll.cal.pre.i.l)
    } else { # si aucune données, produire ll.cal.pre.i quand même, structure pareille à ll.clean
      ll.cal.pre.i <- ll.clean
    }
    return(ll.cal.pre.i) 
    } # row bind -> on colle deux df de structure identique (les l nombre de ll.cal.pre.i.l) de différents k.l, associées à différents temps de la période de mesure de la sonde k 
  if (grepl("hobo", raw.ll.files[i])) {
    ##### boucle de concaténation des données (fichier.uid ensemble, sinon autre calibration et graphique disctinct) ----
    if (length(unique(cal.data$period.file.uid[which(grepl(files.uid.df[i,1], cal.data$file.uid))])) != 0) { # si mm fichier.uid.i, coller les périodes ensemble (ainsi, retirer et remettre ne demande pas plus de manipulations et surtout ps des manipulations incividuelles)
      for (l in 1:length(unique(cal.data$period.file.uid[which(grepl(files.uid.df[i,1], cal.data$file.uid))]))) { print(l)
        cal.data.i.l <- unique(cal.data[which(grepl(files.uid.df[i,1], cal.data$file.uid)),
                                            c("file.uid", "site.uid", "well.uid", "trmnt.uid", 'lab.probe.id', 'probe.uid', 'probe.brand',
                                              "day.begining.aaaa.mm.dd.hh.mm", 'day.end.aaaa.mm.dd.hh.mm', "period.file.uid")])[l,] # cal.data.i.l = les infos dont j'ai besoin pour recouper selon la période l du fichier i
        period.file.uid.l <- cal.data.i.l$period.file.uid
        # recoupage de ll.pre.data selon cal.data selon début et fin des mesures et retrait de colonnes
        ll.clean.l <- ll.clean %>%
        # ll.clean.l.pre <- ll.clean %>%
          dplyr::filter(date.time.tz.orig >= cal.data.i.l$day.begining.aaaa.mm.dd.hh.mm) %>% # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data.i.l
          dplyr::filter(date.time.tz.orig <= cal.data.i.l$day.end.aaaa.mm.dd.hh.mm) %>% # <= date de mesure de NP plus petite ou égale à la date end dans cal.data.i.l 
          select("scan.id", "raw.value.kPa_pres.abs", "calibrated.value.cm",  "temperature_dC", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0") # %>%  # date et time sans "UTC.0" sont dans le fuseau horaire d'origine (tz trouvé en croisant les coordonnées "coords")
        # insérer les données de longueur de fil et de out.long.tuyau.sol.cm de cal.data
        long.fil.cm <- cal.data$long.fil.cm[cal.data$period.file.uid == period.file.uid.l]
        ll.clean.l$long.fil.cm <- rep(long.fil.cm, times = nrow(ll.clean.l))
        out.long.tuyau.sol.cm <- cal.data$out.long.tuyau.sol.cm[cal.data$period.file.uid == period.file.uid.l]
        ll.clean.l$out.long.tuyau.sol.cm <- rep(out.long.tuyau.sol.cm, times = nrow(ll.clean.l))
        
        # caduque
        # # répliquer les données cal.data.k.l à chaque ligne de ll.pre.0.data.4.l.pre
        # cal.data.i.l.all <- cal.data[cal.data$period.file.uid == period.file.uid.l,]
        # rownames(cal.data.i.l.all) <- NULL
        # cal.data.i.l.rep <- cbind(cal.data.i.l.all, rep(row.names(cal.data.i.l.all), each = nrow(ll.clean.l.pre)))
        # colnames(cal.data.i.l.rep)
        # # assembler les colonnes
        # ll.clean.l <- bind_cols(ll.clean.l.pre, cal.data.i.l.rep)
        # ll.clean.l <- ll.clean.l %>% select(!"rep(row.names(cal.data.i.l.all), each = nrow(ll.clean.l.pre))")
        # # chaque cal.data.k = une section de mesures de la sonde k, durant l'été, associée ou non à une mesure au bulleur et à une longueur de fil
        # # vérifications
        # head(ll.clean.l); colnames(ll.clean.l); nrow(ll.clean.l)
        
        # changer pour un nom explicite, fichier encore à calibrer (d'où "pre")
        ll.cal.pre.i.l[[l]] <- ll.clean.l
      }
      ll.cal.pre.i <- do.call(rbind, ll.cal.pre.i.l) # row bind -> on colle deux df de structure identique (les ll.cal.pre.i) de différents i.l, associées à différents temps de la période de mesure de la sonde i
    } 
    else { # si aucune données, produire ll.cal.pre.i quand même, structure pareille à ll.clean
      ll.clean$long.fil.cm <- 0
      ll.clean$out.long.tuyau.sol.cm <- 0
      ll.cal.pre.i <- ll.clean
    }
  return(ll.cal.pre.i) 
}
}

# clean.to.calibrated_ll
# file.to.calibrate <- ll.cal.pre.i
clean.to.calibrated_ll <- function(file.to.calibrate) {
  # constante de distance de la sonde (CDS) : distance entre mesure de fil et emplacement exact de la mesure de pression ou de mS/cm par la sonde
  CDS <- data.frame(type = c("HOBO U20", "HOBO U20L", "ODYSSEY", "other"), # Hobo seulement : mesure longueur du fil tel que dans protocole; à la limite de la boîte de sonde. Les constantes de longueur de boîte de sonde à la sonde à l'interface intérieur de la sonde sont ajoutées à cette étape-ci.
                    constante = c("12.93", "13.3", "0", "0")) %>%
    mutate_at('constante', as.numeric) # liste des types de SNH avec lesquelles j'ai pris des données; chaque "marque/modèle" (type) est traitée de façon différente
  # boucles
  if(grepl("odyssey", raw.ll.files[i])) {     # i<-84 # exemple avec plusieurs mesures de bulleur 
    step <- c("cal", "bulleur") # utilisé pour créer des colonnes dans la "boucle large-to-long maison"
    cal.bulleur.list.appendd <- list()
    if(nrow(unique(cal.data[which(grepl(files.uid.df[i,1], cal.data$file.uid)),])) > 0) {
      if(FALSE %in% (!file.to.calibrate$calibrated.value.cm %in% rep("NA", times = length(file.to.calibrate$calibrated.value.cm)))) { # si TRUE = STOP et warning (les données ont été calibrées avec le programme-mère, vérifier que j'obtiens les mêmes) // si FALSE = continuer la boucle (donc rien, donc IF statement)
        stop(paste0("Attention, la colonne calibrated.value n'est pas vide. Sonde problématique : i = ", paste(i), "; ", ll.pre[i]))
      } # créer une autre colonne, le cas échéant (à faire)
      odyssey.data.pre <- unique(cal.data[which(grepl(files.uid.df[i,1], cal.data$file.uid)),]) # plusieurs périodes valides pour un fichier: toutes les coller ensemble (si plusieurs ligne dans la recherche : cal.data$period.file.uid[which(grepl(files.uid.df[k,1], cal.data$file.uid))]) toutes colonnes conservées
      # créer deux tableurs distincts, dans une liste : l'un pour les données de calibration, l'un pour les données de bulleur
      odyssey.data.cal <- odyssey.data.pre %>% select(!contains("in.bulleur"))
      odyssey.data.bulleur <- odyssey.data.pre %>% select(!contains("cal."))
      odyssey.data.list <- list(odyssey.data.cal, odyssey.data.bulleur)
      {
        # extraire les chiffres des colonnes bulleur/cal
        # pour les colonnes contenant des chiffres sauf "pt.bas", 
        # groupper par "in.bulleur" ou "cal."
        # pour chque groupe
        # faire ces étapes de :
        # transformer les colonnes en lignes aux informations répétées (large ton long)
      } # explications de la boucle large-to-long
      for(j in 1:length(odyssey.data.list)) {
        odyssey.cols <- colnames(odyssey.data.list[[j]])
        numbers <- regexpr("[0-9]+", odyssey.cols)
        nstep <- as.numeric(regmatches(odyssey.cols, numbers)) # pour chaque objet de la liste, trouver nom de colonnes extraire leur chiffre associé, le cas échéant, puis enlever le chiffre et refaire le df
        odyssey.data <- list()
        for (k in unique(nstep)) {
          remove <- paste(setdiff(unique(nstep), k), collapse = "|")
          odyssey.data.j.k <- odyssey.data.list[[j]] %>% select(!matches(remove)) %>% # sélect si contient j dans les noms de colonne; j'obtiens les colonnes de chiffre (step) k, je crée un df avec juste ces colonnes; j'ajoute une colonne avec le chiffre
            mutate(!!paste0(step[j],".no") := rep(k, nrow(odyssey.data.pre)),
                   period.file.uid = odyssey.data.pre$period.file.uid,
                   row.uid = odyssey.data.pre$row.uid)
          colnames(odyssey.data.j.k) <- sub('[[:digit:]]+', '', colnames(odyssey.data.j.k)) # nom colonne sans chiffre
          odyssey.data[[k]] <- odyssey.data.j.k
        }
        if(j == 1) {  cal.bulleur.list.appendd[[1]] <- do.call(rbind, odyssey.data) } else { # rbind les lignes des j df
          cal.bulleur.list.appendd[[2]] <- do.call(rbind, odyssey.data) %>% drop_na(., in.bulleur.prof.cm)
        }
        } 
      rm(odyssey.data.pre); rm(odyssey.data.bulleur); rm(odyssey.data.cal); rm(odyssey.data); rm(odyssey.data.j.k); rm(j); rm(k) # supprimer vieux objets (fait automatiquement dans une fonction)
      # préparation de la date-heure en prévision de la comparaison de date-heure entre tableaux
      tidy.bulleur.data.pre.0 <- cal.bulleur.list.appendd[[2]] %>% mutate(date.JJ.MM.AAAA_time.HH.MM.SS_tz = paste0(in.bulleur.date.aaaammdd, " ", in.bulleur.time.tz.orig, " ", tz)) # tz original /       # normal qu'il y ait des NA dans le df bulleur, les enlever (dépend du nombre de données de bulleur prises)
      tidy.bulleur.data.pre.0$date.time.tz.orig <- readr::parse_datetime(tidy.bulleur.data.pre.0$date.JJ.MM.AAAA_time.HH.MM.SS_tz, format = '%Y-%m-%d %H:%M:%S %Z', locale = readr::locale(tz = tz)) # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
      tidy.bulleur.data.pre <- tidy.bulleur.data.pre.0 %>% mutate(date.time.roundd.pre = round_date(tidy.bulleur.data.pre.0$date.time.tz.orig, "hours"))
      tidy.bulleur.data.pre$date.time.roundd <- gsub("00:00", "00:01", tidy.bulleur.data.pre$date.time.roundd.pre)
      tidy.bulleur.data <- tidy.bulleur.data.pre %>%
        mutate(date.time.roundd = readr::parse_datetime(date.time.roundd, locale = readr::locale(tz = tz))) %>% # remise de date.time.roundd en classe POSIX
        select(!c(date.JJ.MM.AAAA_time.HH.MM.SS_tz, date.time.tz.orig, date.time.roundd.pre))
      # colonne date.time.UTC.0
      tidy.bulleur.data$date.time.UTC.0pre <- with_tz(tidy.bulleur.data$date.time.roundd, tz = "UTC") # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
      tidy.bulleur.data$date.time.UTC.0pre.1 <- format_iso_8601(tidy.bulleur.data$date.time.UTC.0pre)
      tidy.bulleur.data$date.time.UTC.0 <- gsub("[+]00:00", "Z",  tidy.bulleur.data$date.time.UTC.0pre.1)
      tidy.bulleur.data <- tidy.bulleur.data %>%
        mutate(in.bulleur.prof.mm = in.bulleur.prof.cm * 10) %>% # données de bulleur en mm pour correspondre aux cal.val
        mutate(in.bulleur.rel.to.surface.mm = in.bulleur.rel.to.surface.cm * 10) %>% # données de bulleur en mm pour correspondre aux cal.val
        select(!c(date.time.UTC.0pre, date.time.UTC.0pre.1, date.time.roundd, in.bulleur.prof.cm, in.bulleur.rel.to.surface.cm))
      # joindre par la colonne en commun "date.time.UTC.0"
      tidy.cal.bulleur.data.pre.0 <- left_join(tidy.bulleur.data, file.to.calibrate)  # comparaions aux données (raw.val, en (UNITÉS?) de sonde (i) au même moment que chaque mesure (ligne) de tidy.bulleur.data // selon Wikipedia, il y aurait des mSiemens/mm qqpart
      tidy.cal.bulleur.data.pre <- full_join(tidy.cal.bulleur.data.pre.0, cal.bulleur.list.appendd[[1]], relationship = "many-to-many")
      
      # coller la valeur enregistrée (raw.value.mm) au moment du bulleur dans cal.value où cal.no == 3
      tidy.cal.bulleur.data.pre.1 <- tidy.cal.bulleur.data.pre %>%
        mutate(cal.value = ifelse(cal.no == "3", paste(raw.value.mm), cal.value),
               cal.value = as.numeric(cal.value), 
               cal.neg.length_mm = as.numeric(cal.neg.length_mm))
      rm(tidy.cal.bulleur.data.pre); rm(tidy.bulleur.data.pre.0); rm(tidy.bulleur.data.pre) # supprimer vieux objets (fait automatiquement dans une fonction)
      { # calibration
        # PRÉALABLE : utiliser la valeur NÉGATIVE de longueur de fil à la calibration
        # si y=ax+b, calcul des termes a et b
        # FORMULES
        # a.slope = ( y2 - y1 ) / ( x2 - x1 ), soit la proportion de changement de y pour chaque changement de x
        # où
        # y = raw.value aux longueurs 1 et 2 du test de calibration (p. ex. 200 mm et 800 mm ou 1400 mm, pour STH)
        # x2 = longueur fil test où "cal.order"=2, x1 = longueur fil test où "cal.order"=1
        # et finalement
        # b.verticalIntercept = y1 - a.slope * x1
        y2 = unique(tidy.cal.bulleur.data.pre.1$cal.neg.length_mm[tidy.cal.bulleur.data.pre.1$cal.no=="2"]) # en cm et au négatif
        y1 = unique(tidy.cal.bulleur.data.pre.1$cal.neg.length_mm[tidy.cal.bulleur.data.pre.1$cal.no=="1"]) # en cm et au négatif
        x2 = unique(tidy.cal.bulleur.data.pre.1$cal.value[tidy.cal.bulleur.data.pre.1$cal.no =="2"]) + CDS$constante[CDS$type == brand.i] # pour les ODYSSEY, valeur CDS = 0
        x1 = unique(tidy.cal.bulleur.data.pre.1$cal.value[tidy.cal.bulleur.data.pre.1$cal.no =="1"])
        a.slope = ( y2 - y1 ) / ( x2 - x1 ) # sans unité
        b.verticalIntercept = y1 - (a.slope * x1) # unité : ??
      }
      tidy.cal.bulleur.data.pre.1$cal.neg.length_mm[tidy.cal.bulleur.data.pre.1$cal.no == "3"] <- (tidy.cal.bulleur.data.pre.1$cal.value[tidy.cal.bulleur.data.pre.1$cal.no == "3"]*a.slope)+b.verticalIntercept # contourner le bug dû aux NA par du base R
      tidy.cal.bulleur.data <- tidy.cal.bulleur.data.pre.1 %>% 
        mutate(prof_nappe_odyssey_cm_plus.out = cal.neg.length_mm/10 + tidy.cal.bulleur.data.pre.1$out.long.tuyau.sol.cm,
               prof_nappe_bulleur_cm_plus.out = `in.bulleur.rel.to.surface.mm`/10 + out.long.tuyau.sol.cm,
               offset_cm = prof_nappe_odyssey_cm_plus.out - prof_nappe_bulleur_cm_plus.out)
      tidy.cal.bulleur.data$mean_offset_cm <- mean(tidy.cal.bulleur.data$offset_cm[tidy.cal.bulleur.data$cal.no == "3"], na.rm = TRUE) # ok vérif : sum(tidy.cal.bulleur.data$offset_cm[tidy.cal.bulleur.data$cal.no == "3"])/length(which(tidy.cal.bulleur.data$cal.no == "3"))
      tidy.cal.bulleur.data <- tidy.cal.bulleur.data %>% select("file.uid", "lat.garmin.dms", "long.garmin.dms", "measure_status", "chapitre", "site.uid", "well.uid", "trmnt.uid", 
                                                                "lab.probe.id", "probe.uid", "probe.brand", "long.fil.cm", "comment", "day.begining.aaaa.mm.dd.hh.mm", "day.end.aaaa.mm.dd.hh.mm", 
                                                                "distance.m", "out.long.tuyau.sol.cm", "bulleur.no",  "in.bulleur.prof.mm", "in.bulleur.rel.to.surface.mm", 
                                                                "in.bulleur.date.time.UTC.0" = "date.time.UTC.0", "in.bulleur.date.aaaammdd", "in.bulleur.time.tz.orig", "in.bulleur.obs", 
                                                                "period.file.uid", "row.uid", "scan.id", raw.value = "raw.value.mm", "calibrated.value.cm", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig",
                                                                "cal.neg.length_mm", "cal.value", "cal.no", "prof_nappe_odyssey_cm_plus.out", "prof_nappe_bulleur_cm_plus.out", "offset_cm", "mean_offset_cm")
      file.to.calibrate$calibrated.value.cm = (((file.to.calibrate$raw.value.mm*a.slope) + b.verticalIntercept)/10) + unique(tidy.cal.bulleur.data$out.long.tuyau.sol.cm - tidy.cal.bulleur.data$mean_offset_cm)
      file.to.calibrate <- file.to.calibrate %>% rename(raw.value = raw.value.mm)
      file.to.calibrate$file.uid <- rep(files.uid.df$file.uid[i], times = nrow(file.to.calibrate))
    } else {
      tidy.cal.bulleur.data <- data.frame(NA) # si cal.data rejetée, tidy.cal.bulleur.data = NA pour débugger
      file.to.calibrate <- file.to.calibrate %>% rename(raw.value = raw.value.mm) %>% 
        mutate(file.uid = rep(files.uid.df$file.uid[i], times = nrow(file.to.calibrate)))
}
    ll.cal <- file.to.calibrate # ceci est donc le format final, à intégrer dans la liste tidy.WTD.data
    ### création de la liste dans la liste [[i]]  ----
    tidy.WTD.data.i <- list("data" = ll.cal, "metadata" = raw.ll.files.i[[2]], "verif.data" = tidy.cal.bulleur.data)
    } # le fichier du level logger correspondant à la position i; [1] : data (dataframe), [2] : metadata (character string)
  if (grepl("hobo", raw.ll.files[i])) {
    # Référence : Jutras et Bourgault, 2024, Version 2.0, section 7 (/Users/Aliz/Documents/Doctorat/_Connectivité/Protocoles (dossiers copiés du serveur A'24)/Leveloggers & Hauteur nappe phréatique/_HOBO_Protocole de mesure de nappe_2024-11-01_NE PAS DIFFUSER.docx)
    #### extraction des données de METEOSTAT //[auparavant : ECCC/CCCS] et ménage ----
    site.name.pre <- gsub("\\\"", '', raw.ll.files.i[[2]])[1] # extraire nom de site fichier origine
    site.name <- sub("Titre de tracé : ","",site.name.pre)
    meteoStat.data.pre.0 <- read.csv(paste0("connectivite/data/raw/", list.files(path = "connectivite/data/raw", pattern = site.name)))
    meteoStat.data.pre.1 <- meteoStat.data.pre.0 %>% mutate(date.time = paste(year, month, day, hour)) %>% # mutate(pressure.kPa = pres * 0.1) # FAUX !! d'où je tenais ça ? -> pression donnée en hPa (hectopascal). 1 hPa = 0,1 kPa. Example: convert 15 hPa to kPa: 15 hPa = 15 × 0.1 kPa = 1.5 kPa
    meteoStat.data.pre.1$date.time <- ymd_h(meteoStat.data.pre.1$date.time, tz = tz) + 1
    meteoStat.data.pre.1 <- meteoStat.data.pre.1 %>%  select(date.time, everything(), -c("year", month, day, hour, X, pres, "wdir","wdir_source","wspd","wspd_source","cldc","cldc_source","coco","coco_source")) # ajuster la date et l'heure et ajout d'une seconde, sinon, les données 00:00:00 étaient effacées !
    # changement de nom pour identifier quelles colonnes du futur cal.meteoStat.data proviennent de meteoStat
    colnames(meteoStat.data.pre.1) <- paste0(colnames(meteoStat.data.pre.1), ".ms") # ajout de ".ms" pour identifier les colonnes issues de MeteoStat
    # convertir au bon format de date et manip de colonnes (idem aux infos temporelles de fichier de sonde) / date.time.UTC selon norme iso
    meteoStat.data.pre.2 <- meteoStat.data.pre.1 %>%
      mutate(date.time.UTC.0.pre = with_tz(ymd_hms(meteoStat.data.pre.1$date.time.ms, tz = tz), tzone = "GMT")) # les heures sont ainsi ramenées à UTC +0 / ceci écrase la colonne du mm nom
    meteoStat.data.pre.3 <- meteoStat.data.pre.2 %>%  # enlever l'espace entre date et heure (ISO 8601)
      mutate(date.time.UTC.0.pre.1 = str_replace(meteoStat.data.pre.2$date.time.UTC.0.pre, " ", "T")) %>%
      select(date.time.ms, date.time.UTC.0.pre, date.time.UTC.0.pre.1, everything())
    meteoStat.data.pre.3$date.time.UTC.0 <- str_replace_all(meteoStat.data.pre.3$date.time.UTC.0.pre.1, "00:01","00:01Z") # ajouter le Z à la fin (ISO 8601)
    meteoStat.data <- meteoStat.data.pre.3 %>% select(date.time.ms, date.time.UTC.0, everything()) %>% select(!c(date.time.UTC.0.pre, date.time.UTC.0.pre.1))
    
    #### assembler données du HOBO et données de MeteoStat selon la date et l'heure ----
    # Jutras&Bourgault V2.0, 2024; étape a) Associer par dates et par heures les données mesurées par les sondes de niveau hydrostatique et la pression atmosphérique
    cal.meteoStat.data <- left_join(file.to.calibrate, meteoStat.data, by = join_by(date.time.UTC.0)) %>%
      select("scan.id", "date.time.UTC.0","raw.value.kPa_pres.abs", "temperature_dC", "calibrated.value.cm",
             `date.AAAA-MM-JJ`, "time.HH.MM.SS", `date.time.tz.orig`, "date.time.ms", pressure.kPa.ms, everything()) # enlever les nombreuses colonnes qui n'ont pas rapport dans ces démarches
{    # À faire : VÉRIFIER SI TOUT EST OK NIVEAU TIME ZONES...
    ##### inscrire le time zone (tz) dans la colonne time (équivalent à "date.time.tz.orig.pre") ----
    # json_data <- fromJSON(file ="connectivite/data/raw/full.json") # time zone inscrite dans ce fichier
    # trouver ma station
    # ??? et le bon UTC...
    
    
    # à faire
    # REMETTRE FICHIERS BRNTC dans dossier principal
    # SI MESSAGE D'ERREUR contient les caractères suivants, UTILISER LES DONNÉES DE LA STATION MÉTÉO LOCALE
}     # À FAIRE
    
    # Jutras&Bourgault V2.0, 2024; étape b)	Calculer la hauteur d’eau au-dessus de la sonde par la soustraction de la pression atmosphérique, convertie en cm d’eau, à la pression mesurée par la sonde
    # Jutras&Bourgault V2.0, 2024; étape b.i)	La conversion de kPa en cm d’eau est : 1 kPa = 10,1972 cm d’eau
    cal.meteoStat.data$pression.eau.kPa <- cal.meteoStat.data$raw.value.kPa_pres.abs - cal.meteoStat.data$pressure.kPa.ms
    cal.meteoStat.data$hauteur.eau.cm.pre <- cal.meteoStat.data$pression.eau.kPa * 10.197162129779 # règle de trois
    cal.meteoStat.data$hauteur.eau.cm <- cal.meteoStat.data$hauteur.eau.cm.pre # dépend de la façon dont les mesures de longueurs en cm sont prises
    cal.meteoStat.data <- cal.meteoStat.data %>% select("scan.id", "date.time.UTC.0","raw.value.kPa_pres.abs", pression.eau.kPa, hauteur.eau.cm, everything())
    
    # Jutras&Bourgault V2.0, 2024; étape c)
    # (c.bis) création d'un vecteur de longueur CDS à ajouter à la longueur du fil 
    # constante de distance à la sonde en fonction de l'appareil de mesure, à ajouter à la longueur de fil
    cal.meteoStat.data$long.fil.cm <- cal.meteoStat.data$long.fil.cm + CDS$constante[CDS$type == brand.i]
    # Jutras&Bourgault V2.0, 2024; étape c.i)	La profondeur de la nappe phréatique par rapport à la surface du sol =
    # ((La longueur du fil + La constante CDS) – La longueur du puits d’observation qui dépasse la surface du sol) – La hauteur d’eau au-dessus de la sonde
    # c. Convertir la hauteur d’eau au-dessus de la sonde en profondeur de la nappe phréatique par rapport à la surface du sol
    cal.meteoStat.data$calibrated.value.cm <- cal.meteoStat.data$long.fil.cm - cal.meteoStat.data$out.long.tuyau.sol.cm - cal.meteoStat.data$hauteur.eau.cm
    # format final -> nom final
    ll.cal <- cal.meteoStat.data %>%  # ceci est donc le format final, à intégrer dans la liste ll.clean
      select(scan.id, raw.value = raw.value.kPa_pres.abs, calibrated.value.cm, `date.AAAA-MM-JJ`, time.HH.MM.SS, date.time.tz.orig, # retirer des colonnes intermédiaires et mm format que ll.clean[[i]]$data
             date.time.UTC.0)
    ll.cal$file.uid <- rep(files.uid.df$file.uid[i], times = nrow(ll.cal))
    
    # élaboration des verif.data : données de bulleur vs sonde
    step <- c("cal", "bulleur") # utilisé pour créer des colonnes dans la "boucle large-to-long maison"
    cal.bulleur.list.appendd <- list()
    if(nrow(unique(cal.data[which(grepl(files.uid.df[i,1], cal.data$file.uid)),])) > 0) {
      if(FALSE %in% (!file.to.calibrate$calibrated.value.cm %in% rep("NA", times = length(file.to.calibrate$calibrated.value.cm)))) { # si TRUE = STOP et warning (les données ont été calibrées avec le programme-mère, vérifier que j'obtiens les mêmes) // si FALSE = continuer la boucle (donc rien, donc IF statement)
        stop(paste0("Attention, la colonne calibrated.value n'est pas vide. Sonde : i = ", paste(i), "; ", ll.pre[i]))
      } # créer une autre colonne, le cas échéant (à faire)
      odyssey.data.pre <- unique(cal.data[which(grepl(files.uid.df[i,1], cal.data$file.uid)),]) # plusieurs périodes valides pour un fichier: toutes les coller ensemble (si plusieurs ligne dans la recherche : cal.data$period.file.uid[which(grepl(files.uid.df[k,1], cal.data$file.uid))]) toutes colonnes conservées
      # créer deux tableurs distincts, dans une liste : l'un pour les données de calibration, l'un pour les données de bulleur
      odyssey.data.cal <- odyssey.data.pre %>% select(!contains("in.bulleur"))
      odyssey.data.bulleur <- odyssey.data.pre %>% select(!contains("cal."))
      odyssey.data.list <- list(odyssey.data.cal, odyssey.data.bulleur)
      {
        # extraire les chiffres des colonnes bulleur/cal
        # pour les colonnes contenant des chiffres sauf "pt.bas", 
        # groupper par "in.bulleur" ou "cal."
        # pour chaque groupe
        # faire ces étapes de :
        # transformer les colonnes en lignes aux informations répétées (large ton long)
      } # explications de la boucle large-to-long
      for(j in 1:length(odyssey.data.list)) {
        odyssey.cols <- colnames(odyssey.data.list[[j]])
        numbers <- regexpr("[0-9]+", odyssey.cols)
        nstep <- as.numeric(regmatches(odyssey.cols, numbers)) # pour chaque objet de la liste, trouver nom de colonnes extraire leur chiffre associé, le cas échéant, puis enlever le chiffre et refaire le df
        odyssey.data <- list()
        for (k in unique(nstep)) {
          remove <- paste(setdiff(unique(nstep), k), collapse = "|")
          odyssey.data.j.k <- odyssey.data.list[[j]] %>% select(!matches(remove)) %>% # sélect si contient j dans les noms de colonne; j'obtiens les colonnes de chiffre (step) k, je crée un df avec juste ces colonnes; j'ajoute une colonne avec le chiffre
            mutate(!!paste0(step[j],".no") := rep(k, nrow(odyssey.data.pre)),
                   period.file.uid = odyssey.data.pre$period.file.uid,
                   row.uid = odyssey.data.pre$row.uid)
          colnames(odyssey.data.j.k) <- sub('[[:digit:]]+', '', colnames(odyssey.data.j.k)) # nom colonne sans chiffre
          odyssey.data[[k]] <- odyssey.data.j.k
        }
        if(j == 1) {  cal.bulleur.list.appendd[[1]] <- do.call(rbind, odyssey.data) 
        } else { # rbind les lignes des j df
          cal.bulleur.list.appendd[[2]] <- do.call(rbind, odyssey.data) %>% drop_na(., in.bulleur.prof.cm)
        }} 
      rm(odyssey.data.pre); rm(odyssey.data.bulleur); rm(odyssey.data.cal); rm(odyssey.data); rm(odyssey.data.j.k); rm(j); rm(k) # supprimer vieux objets (fait automatiquement dans une fonction)
      # préparation de la date-heure en prévision de la comparaison de date-heure entre tableaux
      tidy.bulleur.data.pre.0 <- cal.bulleur.list.appendd[[2]] %>% mutate(date.JJ.MM.AAAA_time.HH.MM.SS_tz = paste0(in.bulleur.date.aaaammdd, " ", in.bulleur.time.tz.orig, " ", tz)) # tz original /       # normal qu'il y ait des NA dans le df bulleur, les enlever (dépend du nombre de données de bulleur prises)
      tidy.bulleur.data.pre.0$date.time.tz.orig <- readr::parse_datetime(tidy.bulleur.data.pre.0$date.JJ.MM.AAAA_time.HH.MM.SS_tz, format = '%Y-%m-%d %H:%M:%S %Z', locale = readr::locale(tz = tz)) # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
      tidy.bulleur.data.pre <- tidy.bulleur.data.pre.0 %>% mutate(date.time.roundd.pre = round_date(tidy.bulleur.data.pre.0$date.time.tz.orig, "hours"))
      tidy.bulleur.data.pre$date.time.roundd <- gsub("00:00", "00:01", tidy.bulleur.data.pre$date.time.roundd.pre)
      tidy.bulleur.data <- tidy.bulleur.data.pre %>%
        mutate(date.time.roundd = readr::parse_datetime(date.time.roundd, locale = readr::locale(tz = tz))) %>% # remise de date.time.roundd en classe POSIX
        select(!c(date.JJ.MM.AAAA_time.HH.MM.SS_tz, date.time.tz.orig, date.time.roundd.pre))
      # colonne date.time.UTC.0
      tidy.bulleur.data$date.time.UTC.0pre <- with_tz(tidy.bulleur.data$date.time.roundd, tz = "UTC") # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
      tidy.bulleur.data$date.time.UTC.0pre.1 <- format_iso_8601(tidy.bulleur.data$date.time.UTC.0pre)
      tidy.bulleur.data$date.time.UTC.0 <- gsub("[+]00:00", "Z",  tidy.bulleur.data$date.time.UTC.0pre.1)
      tidy.bulleur.data <- tidy.bulleur.data %>%
        mutate(in.bulleur.prof.mm = in.bulleur.prof.cm * 10) %>% # données de bulleur en mm pour correspondre aux cal.val
        mutate(in.bulleur.rel.to.surface.mm = in.bulleur.rel.to.surface.cm * 10) %>% # données de bulleur en mm pour correspondre aux cal.val
        select(!c(date.time.UTC.0pre, date.time.UTC.0pre.1, date.time.roundd, in.bulleur.prof.cm, in.bulleur.rel.to.surface.cm))
      # joindre par la colonne en commun "date.time.UTC.0"
      tidy.cal.bulleur.data.pre.0 <- left_join(tidy.bulleur.data, file.to.calibrate)  # comparaions aux données (raw.val, en (UNITÉS?) de sonde (i) au même moment que chaque mesure (ligne) de tidy.bulleur.data // selon Wikipedia, il y aurait des mSiemens/mm qqpart
      tidy.cal.bulleur.data.pre <- full_join(tidy.cal.bulleur.data.pre.0, cal.bulleur.list.appendd[[1]], relationship = "many-to-many")
            # créer mm colonnes que pour les Odyssey en prévision du rbind
      tidy.cal.bulleur.data <- tidy.cal.bulleur.data.pre %>% 
        mutate(prof_nappe_odyssey_cm_plus.out = NA, prof_nappe_bulleur_cm_plus.out = NA, offset_cm = NA, mean_offset_cm = NA)
      tidy.cal.bulleur.data <- tidy.cal.bulleur.data %>% select("file.uid", "lat.garmin.dms", "long.garmin.dms", "measure_status", "chapitre", "site.uid", "well.uid", "trmnt.uid", 
                                                                "lab.probe.id", "probe.uid", "probe.brand", "long.fil.cm", "comment", "day.begining.aaaa.mm.dd.hh.mm", "day.end.aaaa.mm.dd.hh.mm", 
                                                                "distance.m", "out.long.tuyau.sol.cm", "bulleur.no",  "in.bulleur.prof.mm", "in.bulleur.rel.to.surface.mm", 
                                                                "in.bulleur.date.time.UTC.0" = "date.time.UTC.0", "in.bulleur.date.aaaammdd", "in.bulleur.time.tz.orig", "in.bulleur.obs", 
                                                                "period.file.uid", "row.uid", "scan.id", raw.value = "raw.value.kPa_pres.abs", "calibrated.value.cm", "date.AAAA-MM-JJ", "time.HH.MM.SS", 
                                                                "date.time.tz.orig","cal.neg.length_mm", "cal.value", "cal.no", "prof_nappe_odyssey_cm_plus.out", "prof_nappe_bulleur_cm_plus.out", 
                                                                "offset_cm", "mean_offset_cm")    
      } else {
          tidy.cal.bulleur.data <- data.frame(NA) # si cal.data rejetée, tidy.cal.bulleur.data = NA pour débugger
          file.to.calibrate <- file.to.calibrate %>% rename(raw.value = raw.value.kPa_pres.abs) %>% 
            mutate(file.uid = rep(files.uid.df$file.uid[i], times = nrow(file.to.calibrate)))
        }
    ### création de la liste dans la liste [[i]]  ----
    tidy.WTD.data.i <- list("data" = ll.cal, "metadata" = raw.ll.files.i[[2]], "verif.data" = tidy.cal.bulleur.data) 
  } # le fichier du level logger correspondant à la position i; [1] : data (dataframe), [2] : metadata (character string)
  return(tidy.WTD.data.i)
}

# ============================================================================= /
#  Calibration data ----
# ============================================================================= /
# clean.to.calibrated_ll
# données de bulleur, emplacement des puits, nom de fichier, long. fil, etc.
# cal.data.path <- "connectivite/data/raw/level_logger_calibration_all.csv" # lien pour tester fonction, mais dans le code, il se réfère aux lignes précédantes
raw.to.clean_cal.data <- function(cal.data.path) { # ne calibre pas encore les données
  cal.data.0 <- read.csv(cal.data.path, sep = ";", dec = ",") %>% 
    dplyr::filter(!measure_status == "rejected") %>% 
    select(!contains("x.archive"))
  cal.data.1 <- cal.data.0 %>% mutate(out.R = round(cal.data.0$pt.haut.cm - ((cal.data.0$pt.bas1.cm+cal.data.0$pt.bas2.cm+cal.data.0$pt.bas3.cm)/3), digits = 1)) # out = (pt haut - moyenne pt bas)
  # vérification de valeurs OUT
  if(all(cal.data.1$out.R == round(cal.data.1$out.long.tuyau.sol.cm, digits = 1), na.rm =T))  { # si TOUS TRUE (fonction any()) = changer nom de out.R et supprimer la mesure entrée manuellement // si FALSE = avertissement
    print("ok")# cal.data.1$out.long.tuyau.sol.cm <- cal.data.1$out.R
  } else { stop("Attention, le out entré dans cal.data.1 (syn. level_logger_calibration_all.csv) n'est pas identique à la moyenne des points bas soustraite du point haut du puits.") } 
  cal.data <- cal.data.1 %>% select(!c(starts_with("pt."), "out.R")) #"site.uid", "well.uid", "trmnt.uid", "lab.probe.id", "probe.uid", "probe.brand", contains("cal."), "comment", 
  # "day.begining.aaaa.mm.dd.hh.mm", "day.end.aaaa.mm.dd.hh.mm", "distance.m", "out.long.tuyau.sol.cm", 
  # création de colonnes à identifiant unique 
  #### À FAIRE  # exclure les lignes qui n'ont pas de day beggining / rejeter ces lignes et filtrer avec la fonction ( à écrire )
  cal.data$period.file.uid <- paste0(cal.data$day.begining.aaaa.mm.dd.hh.mm, "--", cal.data$day.end.aaaa.mm.dd.hh.mm, ".",cal.data$file.uid)
  # format POSIX begining et end
  cal.data$day.begining.aaaa.mm.dd.hh.mm <- ymd_hm(cal.data$day.begining.aaaa.mm.dd.hh.mm, tz = tz)
  # ICI LE TZ CHANGE EN FONCTION DU I, MAIS LE DERNIER TZ SEULEMENT.... (?) 7 jnavier j'ai tout bugg. en esseyant, et vérif = aucune différence d'utiliser le dernier...
  cal.data$day.end.aaaa.mm.dd.hh.mm <- ymd_hm(cal.data$day.end.aaaa.mm.dd.hh.mm, tz = tz)
  cal.data <- cal.data %>% mutate(row.uid = rownames(cal.data))
  return(cal.data)}


# ============================================================================= /
#  Georeferenced data ----
# ============================================================================= /
# zone.tz
# trouver le fuseau horaire des sites
# zone.shp <- "~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp"
zone.tz <- function(zone.shp) {
  zones <- read_sf(zone.shp) %>% as.data.frame(zones) # ouvrir données du shapefile pour accéder les zones
  if(grepl("odyssey", raw.ll.files[i])) {
    site.name.pre <- sub("SiteName","",raw.ll.files.i[[2]][1])
    site.name <- stringr::str_to_title(gsub(",", "", site.name.pre))
    coords <- c(zones$latitude[zones$site==site.name][1], zones$longitude[zones$site==site.name][1]) # extraire la bonne lat, long selon le nom du site
    tz <- tz_lookup_coords(coords[1], coords[2], method = "fast", warn = FALSE) # trouver le UTC selon la lat long
    return(tz)
  }
  if (grepl("hobo", raw.ll.files[i])) {
    site.name.pre <- gsub("\\\"", '', raw.ll.files.i[[2]])[1] # extraire nom de site fichier origine
    site.name <- sub("Titre de tracé : ","",site.name.pre)
    coords <- c(zones$latitude[zones$site==site.name][1], zones$longitude[zones$site==site.name][1]) # extraire la bonne lat, long selon le nom du site
    tz <- tz_lookup_coords(coords[1], coords[2], method = "fast", warn = FALSE) # trouver le UTC selon la lat long
    return(tz)
  }
}

# ============================================================================= /
#  Date-time manipulation ----
# ============================================================================= /

#### ménage de la date et heure
# data <- raw.ll.data # arranger pour me donner un exemple, mais que ce soit aussi versatile

date.time_manips <- function(data, date.col, time.col) {} 
# ABANDON 30 déc 2025




# ============================================================================= /
#  EN CHANTIER ----
# ============================================================================= /
# voir version archivée : "fonction_phd.r"