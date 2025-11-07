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

read_excel_sheets <- function(filename, tibble = FALSE) {  # for tidyverse tibbles (the default with read_excel): tibble = TRUE
  sheets <- readxl::excel_sheets(filename) %>% 
    subset(.,!grepl(pattern = "À FAIRE|sp_code|validation|READ ME|cad.", sheets)) # keeps any other sheet
  
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  # lapply = applique la fonction suivante décrite à tous les éléments contenus dans l'objet "sheets" et crée une liste avec le résultat
  if(!tibble) x <- lapply(x, as.data.frame)# le résultat de la fonction appliquée n'est pas une tibble, fait en un dataframe
  names(x) <- sheets
  x
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

NONÀJOURread_hobo <- function(path){
  read.csv(path, sep = "\t") %>% 
    slice(-(1:5)) %>% # enlever métadonnées, traitées à part
    separate(1, into = c("scan_no", "date", "hour", "raw", "calibrated"), sep = ",") %>% # ligne 1 = nom des colonnes
    # nettoyer données dates et heure
    mutate(date = gsub("\\s+", "", date)) %>% # "\\s+" = enlever les espaces
    mutate(hour = gsub("\\s+", "", hour)) %>% 
    mutate(hour = gsub(":", "/", hour)) %>% 
    mutate(date = paste0(date, "/", hour)) %>% 
    mutate(date = as.POSIXct(date, format = "%d/%m/%Y/%H/%M/%OS")) %>% # combiner date et heure
    dplyr::select(-hour) -> data # enelver vielle colonne heure inutile
  
  return(data)
}

## read_hobo_header ----
NONÀJOURread_hobo_header <- function(path){
  read.csv(path, sep = "\t") %>% 
    slice((1:5)) -> data
  return(data)
  
}

## data.metadata.hobo ----
path <- "connectivite/data/raw/22224413_INK_20250721_hobo.csv"
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
data.metadata.odyssey <- function(path) {
  raw.ll.files.0 <- readLines(path) # lire en format texte
  # Warning message:
  #   In readLines(paste0("connectivite/data/raw/", ll.pre[i])) :
  #   incomplete final line found on 'connectivite/data/raw/[...].csv'
  # c'est chill, je n'ai pas réussi à arranger ça, mais vérifié √ pas de problème
  # enlever espaces inutiles
  raw.ll.files.1 <- gsub(" ,", ",", raw.ll.files.0)
  raw.ll.files.2 <- gsub(" ", "", raw.ll.files.1) # enlever tous les espaces dans le subset de données
  
  ### création des subsets data & metadata ----
  # notes : les noms réfèrent à l'étape et non à une matrice en particulier, les objets seront remplacés au fil de la boucle. 
  # l'info importante est consignée dans la liste ll.clean[i], à la fin
  raw.ll.files.2.metadata <-  raw.ll.files.2[c(1:9)] # inclus les anciens noms de colonnes, qui sont dans un format et un ordre bizzare
  raw.ll.files.2.data <- raw.ll.files.2[-c(1:9)]
  raw.ll.files.i <- list(raw.ll.files.2.data, raw.ll.files.2.metadata)
  return(raw.ll.files.i)
}

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

