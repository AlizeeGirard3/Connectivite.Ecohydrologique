#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                           Fonctions rédigées pour mon Ph.D.
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : 2025-01-09
# Date mise à jour : 2026-04-14
# Pourquoi : 
# Pour charger les fonctions utiles à traiter données de SHN
# NOTES : 
# V3.1 ajout de la calibration avec SONDES BAROMÉTRIQUES dans FONCTIONS V3.1
# V3.2 nettoyage des données de MeteoStat HORS de ce script-ci, fait dans daily_weather_v2.0.R

# SNH : sondes de niveau hydrostatique

# La fonction s'applique à un fichier brut, traite, corrige et nettoye, et produit un fichier de données aux composantes identiques, peu importe le type de sonde.
# Produit un dataframe rectangulaire. Les métadonnées sont à part (liste avec données + métadonnées + données de vérification).
# Merci à Francis Lessard pour ses idées.
# référer aux fonctions en sourçant ce script (ligne suivante)
# source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd_v2.0.R")

# life hack : options(warn = 2) # wrning converted to error (arrête la boucle au moment où l'avertissement arrive)

# # LEXIQUE : 
# ms : MeteoStat
# bs : barométrique
###########################################################################-

message("Importation de paquet et chargement de fonctions pour le projet")

# ============================================================================= /
#  Initialisation ----
# ============================================================================= /
if (!require("tidyverse")) install.packages("tidyverse") # méta package // dplyr, tidyr, purrr, stringr, lubridate, ect
if (!require("data.table")) install.packages("data.table") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("sf")) install.packages("sf"); if (!require("lutz")) install.packages("lutz") # GIS in R
if (!require("readxl")) install.packages("readxl") # lire les excel
if (!require("openxlsx")) install.packages("openxlsx") # lire les excel
if (!require("conflicted")) install.packages("conflicted") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
options(lubridate.verbose = F) # pour expliciter ce que les fonctions font
if (!require("parsedate")) install.packages("parsedate") # lire les excel
# if (!require("withr")) install.packages("withr") # T'o Québec icitte (date-time en français)

# ============================================================================= /
#  Data selection ----
# ============================================================================= /
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
  return(x)
}

cat_lists <- function(list1, list2) {   # concatener le contenu de listes aux noms identiques
  keys <- unique(c(names(list1), names(list2)))
  map2(list1[keys], list2[keys], c) %>% 
    set_names(keys)  
}

# order.list
# pour commencer par nettoyer les fichiers de sonde barométriques, utilisées ultérieurement pour calibrer (en plus des données de MeteoStat)
# path <- "connectivite/data/raw"
# pattern <- "_odyssey|_hobo|barometric.station"
order.list <- function(path, pattern) {
  files.pre <- list.files(path = path, pattern = pattern, full.names = T)
  files <- files.pre[!grepl("\\.RDS$", files.pre)]
  barometric.station_T.F <- grepl("^connectivite/data/raw/barometric.station", files) # trouver les files avec le terme 'barometric.station', sauf ceux qui contiennent ".RDS" 
  sorted_files <- c(sort(files[barometric.station_T.F]), sort(files[!barometric.station_T.F])) # et les mettre en premier dans la liste
  barometric.station <- files.pre[barometric.station_T.F]
  return(list(sorted_files, barometric.station))
}

# filter.raw.file
# object.to.filter <- raw.ll.files.pre
# path.filtering.object <- "connectivite/data/raw/level_logger_calibration_all.csv"
# object.to.filter <- ele.profiles
# object.to.filter <- env.data.n
filter.raw.file <- function(object.to.filter = NULL, path.filtering.object = NULL, type = NULL) {
  if(c(is.null(path.filtering.object) & !is.null(object.to.filter) & is.null(type))) { # fournir juste un objet à filtrer, sans cal data path
    object.to.filter.filtrd <- object.to.filter %>% 
      dplyr::filter(!grepl("rejected", object.to.filter$measure.status), 
                    !if_all(everything(), is.na))
  } 
  if(c(!is.null(path.filtering.object) & !is.null(object.to.filter))) { # fournir l'objet à filtrer par les informations fournies dans le cal data path à filtrer
    filtering.object <- read.csv(path.filtering.object, , sep = ";", dec = ",")
    filter.out <- filtering.object$file.uid[grep("rejected", filtering.object$measure_status)]
    str_split <- str_split(filter.out, "_")
    filter.out.df <- data.frame(do.call(rbind, str_split)) # colnames = c("probe.uid", "extr.date"))
    exclude.lines <- vector()
    for(exclude in 1:nrow(filter.out.df)) { 
      exclude.lines[exclude] <- which(grepl(filter.out.df[exclude,1], object.to.filter) & grepl(filter.out.df[exclude,2], object.to.filter))
    }
    object.to.filter.filtrd <- object.to.filter[-exclude.lines]
  } 
  if(c(!is.null(path.filtering.object) & is.null(object.to.filter))) { # fournir juste le cal data path à filtrer
    object.to.filter.filtrd <- read.csv(path.filtering.object, sep = ";", dec = ",") %>% 
      dplyr::filter(!measure_status == "rejected") %>% 
      select(!contains("x.archive"))
  }
  if(c(!is.null(object.to.filter) && identical(type, "MeteoStat"))) {
    # caduque, metno_forecast n'est pas de la prédiction, ça vient d'une source qui s'appelle comme ça... # https://dev.meteostat.net/faq.html
    # object.to.filter.filtrd.pre <- names(object.to.filter) %>%
    #   reduce(function(df, col_name) {
    #     nom_source <- paste0(col_name, "_source")  # Condition d'exclusion : on vérifie si la colonne source existe dans le dataframe
    #     if (nom_source %in% colnames(df)) {
    #         df <- df %>% mutate(
    #         !!col_name := if_else(df[[nom_source]] == "metno_forecast", NA, df[[col_name]]))
    #         return(df)
    #     } else {
    #       return(df)
    #     }}, .init = object.to.filter)
    object.to.filter.filtrd <- object.to.filter %>%
      select(!"X")
  } # pour meteoStat
  return(object.to.filter.filtrd)
}

# uid.to.columns
# ele.profiles <- readRDS(file = "~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/elevation.profiles.RDS")
# file.to.restructure <- ele.profiles
# vegetation_lower.str <- read.xlsx("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/extracted_raw/vegetation_lower.str.xlsx")
# file.to.restructure <- vegetation_lower.str # ok
# vegetation_trees.shr <- read.xlsx("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/extracted_raw/vegetation_trees.shr.xlsx")
# file.to.restructure <- vegetation_trees.shr # ok
# canopy.peat.fauna <- read.xlsx("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/extracted_raw/canopy.peat.fauna.xlsx")
# file.to.restructure <- canopy.peat.fauna # ok
# path <- "~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/raw/level_logger_calibration_all.csv"; file.to.restructure = NULL
uid.to.columns <- function(file.to.restructure = NULL, type = NULL, path = NULL) { # other ou cal.data
  {
    col.sequence <- list() # idée : nom des colonnes à mettre dans col.sequence... y référer dans la boucle
    # mettre à jour les métadonnées de temps en temps (ok janv.2026)
    col.sequence$trmnt.uid.aaaa <- c("site.uid", "chapter", "type", "year", "no")
    col.sequence$trmnt.uid <- c("site.uid", "chapter", "type")
    col.sequence$trmnt.uid.orient.NO.aaaa <- c("site.uid", "chapter", "type", "orientation", "transect.replicate", "year")
    # col.sequence$perm.plot.uid.NO.aaaa <- c("site.uid", "chapter", "type", "perm.plot.replicate", "perm.plot.type", "year")
    col.sequence$perm.plot.uid.NO.quadrat.aaaa <- c("site.uid", "chapter", "type", "perm.plot.replicate", "perm.plot.type", "quadrat.letter", "year")
    col.sequence$trmnt.uid.rel.dist.aaaa <- c("site.uid", "chapter", "type", "relative.distance", "year")
    col.sequence$trmnt.uid.rel.dist.quadrat.aaaa <- c("site.uid", "chapter", "type", "relative.distance", "quadrat.letter", "year")
    col.sequence$trmnt.uid.ch3.position.aaaa <- c("site.uid", "chapter", "type", "well.no", "year")
    # cas spécifique de cal.data : retirer colonne probe.uid et la remettre après les manips
    col.sequence$well.uid <- list()
    col.sequence$well.uid$chap1 <- c("site.uid", "chapter", "type", "year") # traiter stations barométriques (lorsque chapitre == NA) mm façon que les sonde du chapitre 1
    col.sequence$well.uid$chap2 <- c("site.uid", "chapter", "type", "relative.distance", "year")
    col.sequence$well.uid$chap3 <- c("site.uid", "chapter", "type", "relative.distance", "year")
  }
  if(type == "other") {
    file <- file.to.restructure %>% 
      select(!c(site.uid, 
                grep("carotte.uid", colnames(file)), # ajouter des grep des colonnes à exclure de la restructuration, dans grep évite l'erreur "cannot remove col that doesn't exist
                # exclure ces dernier car info contenue n'est pas aggrégée (pas de points dans l'UID)
                grep("peat.samples_LOI_LAB.UID.1", colnames(file)), 
                grep("peat.samples_LOI_LAB.UID.2", colnames(file)), 
                grep("probe.uid", colnames(file)))) %>% 
      mutate(ID = as.character(sample(unique(abs(rnorm(n = nrow(file))))))) # créer une colonne d'ID unique par lequel joindre après la boucle
    cols <- grep("uid", colnames(file), ignore.case = T) # colonnes avec uid à séparer en plusieurs colonnes
    cols.list <- list()
    for(col in seq_along(cols)) {
      # col <-2
      col.no <- cols[col]
      file.2 <- file %>% 
        separate_wider_delim(colnames(file)[col.no], delim = ".", names = c(col.sequence[[match(colnames(file)[col.no], names(col.sequence))]]), cols_remove = F, too_few = "debug", too_many = "debug")
      file.2$type <- str_replace(file.2$type, "C", "control")
      cols.list[[col]] <- file.2
    }
    cols.df <- cols.list %>%
      reduce(full_join) %>%
      select(!c("ID"))  } 
  if(is.null(file.to.restructure) | c(is.null(path) & type == "cal.data")) {
    # file.0.pre <- filter.raw.file(path.filtering.object = path)
    file.0.pre <- file.to.restructure %>%
      mutate(ID = as.character(sample(unique(abs(rnorm(n = nrow(file.to.restructure))))))) #créer une colonne d'ID unique par lequel joindre après la boucle
    # rm(file)
    probe.df <- data.frame("probe.uid" = file.0.pre$probe.uid, "ID" = file.0.pre$ID, file.uid = file.0.pre$file.uid)
    file.0 <- file.0.pre %>% 
      select(!c(file.uid, site.uid, probe.uid)) # ignorer les colonnes à ne pas restructurer
    cols <- grep("uid", colnames(file.0), ignore.case = T) # colonnes avec uid à séparer en plusieurs colonnes
    cols.list <- list()
    for(col in seq_along(cols)) {
      # col<-2
      col.no <- cols[col]    
      if(colnames(file.0)[col.no] == "well.uid") { # colonne well.uid, cas spécial (diviser le df en 3 sets de lignes, recoller les lignes, poursuivre)
        uid.X.list <- list()
        for(chap in 1:length(col.sequence$well.uid)) {
          # chap <-1
          file.1.pre <- file.0[grep(paste0("ch", chap), file.0$well.uid), ]
          if(chap == 1) {
            NAchap.df <- file.0[grep("NA", file.0$well.uid), ] # file.0 oui parce que file.1.pre est un subset alors que je veux aller chercher un subset complémentaire (traiter les chapitre == NA pour les stations barométriques)
            file.1 <- rbind(NAchap.df, file.1.pre)
          } else {
            file.1 <- file.1.pre
          }
          file.2 <- file.1 %>%
            separate_wider_delim(colnames(file.1)[col.no], delim = ".", names = c(col.sequence$well.uid[[chap]]), cols_remove = F, too_few = "align_start") #, too_few = "debug", too_many = "debug")
          # file.2$type <- str_replace(file.2$type, "^C", "control") # où ^ = "pattern situé au début"
          uid.X.list[[chap]] <- file.2
        }
        col.lines <- do.call(bind_rows, uid.X.list) # row bind -> on colle deux df de structure identique (les ll.cal.pre.i) de différents i.l, associées à différents temps de la période de mesure de la sonde
        cols.list[[col]] <- col.lines
        rm(file.1); rm(file.1.pre); rm(file.2)
      }
      if(!colnames(file.0)[col.no] == "well.uid") {
        file.2 <- file.0 %>%
          separate_wider_delim(colnames(file.0)[col.no], delim = ".", names = c(col.sequence[[match(colnames(file.0)[col.no], names(col.sequence))]]), cols_remove = F) #, too_few = "debug", too_many = "debug")
        cols.list[[col]] <- file.2
      }
      # coller les colonnes ensemble en joignant par l'identifiant unique
      cols.df.pre <- cols.list %>%
        map(~ .x %>% mutate(across(everything(), as.character))) %>% # d'abord, tout en caractères, car classe des NA en arrière plan posait problème
        reduce(full_join, na_matches = "na") # précision de la gestion des NA pour débugger (voir code débuggage ci-dessous), cela ajoutait 13 lignes autrement; merci à GoogleIA pour l'aide au débuggage
      {
        # test <- reduce(cols.list, anti_join)
        # test <- test[, -c(15:45)]
        # complet <- do.call(bind_rows, cols.list)
        # subset.ID <- complet[complet$ID %in% test$ID,-c(15:45)]
        # test.binded <- bind_rows(test, subset.ID)
        # test.binded[13,] == test.binded[26,]
        } # preuve que les lignes sont essentiellement identiques -> puisque je veux utiliser uniquement la clé "ID" pour joindre les df, le join n'était plus sûr de comment gérer les NA
      cols.df <- left_join(cols.df.pre, probe.df) %>% 
        select(!c("ID"))
    }
  } # soit je fournis le path de cal.data, soit je fournis le fichier filtré (sortie de la fonction select.raw.ll.files() ci-dessus, exemple, utilisé dans la fonction raw.to.clean_cal.data() ci-dessous)
  return(cols.df)
}
# cols.df <- uid.to.columns(path = "~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/raw/level_logger_calibration_all.csv")

# ============================================================================= /
#  MeteoStat data (download and overwrite) ----
# ============================================================================= /
### tableau avec les station ID de chaque site (utilisé ci-dessous)
# source : meteoStat
#### MANUELLEMENT : trouvé la station ID (canada+(lat, long) et la distance du site de recherche et trouver le station ID sur MeteoStat[-> sur le site de MétéoStat])
# station_id.phd <- data.frame("phd.site.UID" = NA, "phd.site.name"= NA,"station_name" = NA, "station_id_canada" = NA, "station_id_MeteoStat" = NA,
#                              "lat.station" = NA, "long.station" = NA, "dist_from_zone" = NA, "start.hourly" = NA, "end.hourly" = NA) # start et end à jour : 1ier décembre
# station_id.phd[1,1:10] <- c("STH", "St-Henri","BEAUPORT",27803,71578,46.8,-71.2,18.14627, "2003", "2025-11-22")
# station_id.phd[2,1:10] <- c("INK", "Inkerman","TRACADIE",6205,71719,48.01,-64.49, 49.50673, "1977", "2025-04-27") # MISCOU ISLAND (AUT)
# station_id.phd[3,1:10] <- c("BRNTC", "Burnt Church","MIRAMICHI RCS", 10808,"AOYMS",47.01,-65.47,27.63049, "2020", "2022-12-14")
# station_id.phd[4,1:10] <- c("PRO", iconv("Président-Ouest", to = "UTF-8-MAC"),"RIVIERE-DU-LOUP",8539,71578,47.81,-69.55,3.021966, "2003", "2025-11-22") # merci google IA pourm'aider à traiter mes noms de site avec un accent francophone...
# station_id.phd[5,1:10] <- c("GPB", iconv("Grande Plée Bleue", to = "UTF-8-MAC"), "BEAUPORT",27803,71578,46.8,-71.2,12.499890, "2003", "2025-11-22") # merci google IA pourm'aider à traiter mes noms de site avec un accent francophone...
# write.csv(station_id.phd, file = "connectivite/data/raw/station_id.phd.csv", row.names = FALSE)
# ok (1ier déc. 2025), ajouter des sites au besoin

# télécharger données horaires (1ier décembre 2025 fonctionne)
# if (!require("data.table")) install.packages("data.table") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
# if (!require("tidyverse")) install.packages("tidyverse") # méta package // gosser avec des suites de caractères, str_replace, [...]
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
#   write.csv(aggr.meteoStat.site,  paste0("connectivite/data/raw/meteoStat.data.hourly", station_id.phd$phd.site.name[n], ".csv"), row.names = FALSE)
# }

## CADUQUE : télécharger données quotidiennes (22 janvier 2026) 
## if (!require("data.table")) install.packages("data.table") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
## if (!require("tidyverse")) install.packages("tidyverse") # méta package // gosser avec des suites de caractères, str_replace, [...]
## station_id.phd <- read.csv("connectivite/data/raw/station_id.phd.csv") # issu du script "Recherche_station_meteo_ID_v2.0.r"
## year <- (2024:2025) # ajouter 2026 en 2026 et dans bind_rows aussi
## list.data.format <- c("hourly", "daily", "monthly", "normals") # ajouter boucle pour données d'autres type au besoin
## meteoStat.site.year <- list()
## for(n in 1:nrow(station_id.phd)) {
##   for (i in 1:length(year)) {
##     # n<-1
##     URL <- paste0("https://data.meteostat.net/", list.data.format[2], "/", year[i],"/", station_id.phd$station_id_MeteoStat[n],".csv.gz")
##     temp <- tempfile()
##     download.file(url = URL, temp)
##     meteoStat.site.year[[i]] <- fread(temp)
##   }
##   aggr.meteoStat.site.daily <- bind_rows(meteoStat.site.year[[1]], meteoStat.site.year[[2]]) # ajouter 3e année et + (2026, +) ou coder différemment
##   write.csv(aggr.meteoStat.site.daily,  paste0("connectivite/data/raw/meteoStat.data.daily.", station_id.phd$phd.site.name[n], ".csv"), row.names = FALSE)
## }

# ============================================================================= /
#  Logger serial data import and cleaning ----
# ============================================================================= /
## data.metadata
# path <- "connectivite/data/raw/10279769_INK_20250106_hobo.csv"
# path <- "connectivite/data/raw/barometric.station.22063156_20251202.csv"
# path <- raw.ll.files[i]
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
  if (grepl("hobo|barometric.station", path)) { # début de la loop pour les ODYSSEY
    # k <- i
    raw.ll.files.init <- readLines(path) # lire en format texte
    raw.ll.files.init[1] <- gsub('"', '', raw.ll.files.init[1])
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
  if (grepl("hobo|barometric.station", raw.ll.files[i])) {
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
files.uid.df <- data.frame(file.uid = NA, file.name = NA, probe.uid = NA, "extraction.data.aaaammjj" = NA, 
                           "tz_orig" = NA, site.uid = NA,  probe.brand = NA, well.uid = NA) # pour stocker les fichier.uid (aussi première colonne de cal.data) et autres données intérimaires
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
    site.uid.pre <- sub("SiteName,","", raw.ll.files.i[[2]][1])
    site.uid <- substr(gsub("[^[:alnum:]]", "", site.uid.pre) , 1, 3)
    files.uid.df[i,1:4] <- c(paste0(unlist(result)[1], "_", unlist(result)[2]), raw.ll.files[i], probe.uid.i, as.numeric(unlist(result)[2])) # ceci sera gardé en mémoire (doit être identique à la colonne fichier.uid dans cal.data)
    files.uid.df[i,6] <- site.uid
    return(files.uid.df)}
  if (grepl("hobo|barometric.station", raw.ll.files[i])) {
    texte <- as.data.frame(str_match(x[[2]], "(?s)LGR S/N: \\s*(.*?)\\s*,")) # extraire tout ce qui se trouve après LGR S/N:... / "x" objet mis dans la fonction
    # entre "LGR S/N: " et la "," directement subséquente, sans savoir s'il y a des sauts de ligne et peu importe les espaces dans l'énoncé.
    probe.uid.i <- as.numeric(texte[2,2])
    # no du level logger dans le nom du fichier brut (.csv), correspond à l'item "k" de la présente boucle
    texte <- raw.ll.files[i]
    nombres <- gregexpr("[0-9]+", texte)
    resultat <- regmatches(texte, nombres)
    fichier <- as.numeric(unlist(resultat)[1])
    files.uid.df[i,1:4] <- c(paste0(unlist(resultat)[1], "_", unlist(resultat)[2]), raw.ll.files[i], probe.uid.i, as.numeric(unlist(resultat)[2])) # ceci sera gardé en mémoire (doit être identique à la colonne fichier.uid dans cal.data)
    # site.uid
    site.uid.pre <- gsub("\\\"", '', raw.ll.files.i[[2]])[4] # extraire nom de site fichier origine
    site.uid <- str_extract(site.uid.pre, "(?<=_)(.+?)(?=_)")
    files.uid.df$site.uid[i] <- site.uid # complétion des métadonnées
    return(files.uid.df)  }}

## raw.to.clean_ll
# file.i.raw.data <- raw.ll.files.i[[1]]
raw.to.clean_ll <- function(file.i.raw.data) { # ne calibre pas encore les données
  if (grepl("odyssey", raw.ll.files[i])) {
    raw.ll.data <- read.csv(text = raw.ll.files.i[[1]], # création du dataframe contenant données de nappe phréatique et ménage  ----
                            col.names = c("scan.id", "date.JJ.MM.AAAA", "time.HH.MM.SS",'raw.value.mm',"calibrated.value.cm.lin")) 
    if (all(is.na(raw.ll.data$calibrated.value.cm.lin)) == TRUE) {
      message("NA partout dans la colonne calibrated.value.cm.lin des données bruttes.
Signifie que PAS CALIBRÉ, les NA seront écrasés par les calculs suivants.")
    } else if (unique(raw.ll.data$raw.value.mm %in% raw.ll.data$calibrated.value.cm.lin) == TRUE) {
      message("Même valeur entre raw.value.mm et calibrated.value.cm.lin dans les données bruttes.
Signifie que PAS CALIBRÉ, donc remplacer calibrated.value.cm.lin par des NA qui seront écrasés par les calculs suivants.")
      raw.ll.data$calibrated.value.cm.lin <- rep("NA", times = length(raw.ll.data$calibrated.value.cm.lin)) 
    } else {
      stop("Pas la même valeur entre raw.value.mm et calibrated.value.cm.lin dans les données bruttes.
Signifie que calibrated.value.cm.lin fut calibré initialement avec le logiciel, conserver les valeurs et tout arrêter (à coder).")
    } 
    # colonnes utiles au JOIN FINAL
    # calibrated.value.cm.lin calibration initiale ~ relation linéaire de conductivité (Odyssey seulement)
    raw.ll.data$"calibrated.value.cm.blo" <- rep(NA, times = nrow(raw.ll.data)) # NA pour l'instant, sera rempli après CALCUL DES OFFSETS ** (Odyssey seulement); pour les Hobo, rempli avec mesure manuelle (voir section Calibration data ----)
    raw.ll.data$"calibrated.value.cm.ms" <- rep(NA, times = nrow(raw.ll.data)) # pour calibration via MeteoStat (pour les Odyssey)
    raw.ll.data$"calibrated.value.cm.bs" <- rep(NA, times = nrow(raw.ll.data)) # pour calibration via station barométrique (bs)
    
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
      dplyr::select("scan.id", raw.value.mm, contains("calibrated.value.cm"), date.time.UTC.0, date.time.tz.orig)
    # enlevé aussi : `date.AAAA-MM-JJ`, time.HH.MM.SS, date.time.tz.orig, long.fil.CDS.cm, out.mean.cm, hauteur.eau.cm 
    
    return(ll.clean)}
  if (grepl("hobo|barometric.station", raw.ll.files[i])) {
    raw.ll.data <- read.csv(text = raw.ll.files.i[[1]], header = F, col.names = c("scan.id", "date.JJ.MM.AAAA_time.HH.MM.SS",	
                                                                                  "raw.value.kPa_pres.abs",	"temperature_dC", "Coupleur détaché", 
                                                                                  "Coupleur attaché", 'Hôte connecté',	"Arrêté", "Fin de fichier")) # text = argument de read.csv qui lit la valeur contenue dans l'objet / DATE mauvais format
    # suite : si calibration intégrée avec le hobo, QUE FAIRE ? coder ici, voir procédure avec ODYSSEY
    
    ll.pre.0.data.1 <- raw.ll.data[1:4] # garder seules les colonnes pertinentes
    #### date et heure : format ISO date AAAA-MM-JJTHH:MM:SS,ss-/+FF:ff, voir https://fr.wikipedia.org/wiki/ISO_8601 ----
    # heure : « Z » à la fin lorsqu’il s’agit de l’heure UTC. (« Z » pour méridien zéro, aussi connu sous le nom « Zulu » dans l’alphabet radio international).
    # ajouts aux métadonnées des fichiers
    files.uid.df$tz_orig[i] <- tz
    raw.ll.files.i[[2]][7] <- paste0("original time zone : ", tz)
    #### ménage de la date et heure  ----
    # EXAMPLE ICI FONCTION DE TRANSFORMATION DATE HEURE RÉUTILISABLE
    # format_datetime <- function(data, col.date, col.time, col.datetime)
    # sortir la fonction d'ici et y référer
    # coller le tz dans la colonne "date.JJ.MM.AAAA_time.pre.HH.MM.SS"
    ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS_tz <- paste0(ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS, " ", tz)
    ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS_tz <- gsub("00:00", "00:01", ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS_tz) # sinon, les données 00:00:00 étaient effacées !
    ll.pre.0.data.1$date.time.tz.orig <- readr::parse_datetime(ll.pre.0.data.1$date.JJ.MM.AAAA_time.HH.MM.SS_tz, format = '%m/%d/%y %I:%M:%S %p %Z', 
                                                               locale = readr::locale(tz = tz)) # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
    ll.pre.0.data.1.rdd <- ll.pre.0.data.1 %>% mutate(date.time.tz.orig.roundd = round_date(date.time.tz.orig, unit = "hours") + seconds(1)) 
    ll.pre.0.data.2 <- data.frame(separate_wider_position(ll.pre.0.data.1.rdd, # date et time en deux colonnes (idem à ODYSSEY)
                                                          widths = c("date.AAAA.MM.JJ" = 11, "time.HH.MM.SS" = 8),
                                                          cols = date.time.tz.orig.roundd, cols_remove = F)) 
    ll.pre.0.data.2$`date.AAAA-MM-JJ` = ymd(ll.pre.0.data.2$date.AAAA.MM.JJ, tz = tz)
    ll.pre.0.data.2$date.time.UTC.0pre <- with_tz(ll.pre.0.data.2$date.time.tz.orig.roundd, tz = "UTC") # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
    ll.pre.0.data.2$date.time.UTC.0pre.1 <- format_iso_8601(ll.pre.0.data.2$date.time.UTC.0pre)
    # exemple format de la colonne formatée en ISO : "2024-08-14T15:00:01+00:00"
    # la ligne suivante lui dit que à chaque rencontre des caractères +00:00 (milisecondes), il remplace par un "Z" simplement
    ll.pre.0.data.2$date.time.UTC.0 <- gsub("[+]00:00", "Z",  ll.pre.0.data.2$date.time.UTC.0pre.1) 
    
    if (grepl("hobo", raw.ll.files[i])) {
      # colonnes utiles au JOIN FINAL
      ll.pre.0.data.2$calibrated.value.cm.lin <- rep(NA, times = nrow(ll.pre.0.data.2)) # NA (Odyssey seulement; calibration initiale ~ relation linéaire de conductivité)
      ll.pre.0.data.2$"calibrated.value.cm.blo" <- rep(NA, times = nrow(ll.pre.0.data.2)) # pour calibration via le bulleur (blowing pipe, blo)
      ll.pre.0.data.2$"calibrated.value.cm.ms" <- rep(NA, times = nrow(ll.pre.0.data.2)) # pour calibration via MeteoStat
      ll.pre.0.data.2$"calibrated.value.cm.bs" <- rep(NA, times = nrow(ll.pre.0.data.2)) # pour calibration via station barométrique (bs)
      
      # nom final (et retirer colonnes inutiles)
      ll.clean <- ll.pre.0.data.2 %>% 
        select(!c(date.JJ.MM.AAAA_time.HH.MM.SS, date.AAAA.MM.JJ,  "date.time.UTC.0pre", "date.time.UTC.0pre.1")) %>% 
        select("scan.id", "date.JJ.MM.AAAA_time.HH.MM.SS_tz", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0", 
               "raw.value.kPa_pres.abs", "temperature_dC", contains("calibrated.value.cm"))
    } else { # sondes barométriques : seules différences...
      # [...]
      # ici différence avec version pour sonde données hydrostatique; pas besoin de colonne "calibrated.value.cm"
      # autre différence : le fichier est maintenant propre et prêt à calibrer les sonde données hydrostatiques -> enregistrement
      # nom final (et retirer colonnes inutiles)
      ll.clean <- ll.pre.0.data.2 %>% 
        select(!c(scan.id, date.JJ.MM.AAAA_time.HH.MM.SS, date.AAAA.MM.JJ,  "date.time.UTC.0pre", "date.time.UTC.0pre.1", 
                  "date.JJ.MM.AAAA_time.HH.MM.SS_tz", date.time.tz.orig)) %>% # date.time.tz.orig et autres font DUPLIQUER le JOIN parce le moment exact n'est pas le mm
        select("date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.UTC.0", "pressure.kPa.bs" = raw.value.kPa_pres.abs, "temperature_bs"= temperature_dC) 
      # enlevé calibrated.value.cm.ms et ...bs, enlevé scan.id (info inutile, SURTOUT nuisible dans le JOIN)
      
      saveRDS(ll.clean, file = paste0("connectivite/data/raw/barometric.station.", files.uid.df$site.uid[i], ".RDS"))
    } # si sonde barométrique, enregistrer le ll.clean en .RDS
  }
  return(ll.clean)
}

# concatenate.ll
{ # explications boucle de concaténation
  # boucle de concaténation des données (fichier.uid ensemble, sinon autre calibration et graphique distinct)
  # raison de l'étape : si sonde retirée et remise, sans écraser les données contenues (continuation des mesures), retirer la période 
  # de données invalides (quelques heures, période de rééquilibrage) et recoller les lignes ensemble pour former le fichier d'heures valide
  # mm fichier.uid (loop extrait séquentiellement toutes les lignes de chaque # de SNH, qui peuvent être uniques ou multiples pour un SNH donné);
  # la loop teste si toutes les lignes de ce # de SNH ont le même fichier.uid (i), dans quel cas, si les périodes sont différentes, 
  # la boucle coupe le fichier pour chaque période différente (l), et ensuite réassemble le fichier avec seules les périodes à conserver
  # au 11 mars 2026, cette fonction n'est pas adaptée pour les sondes barometriques
}
# file.to.concat <- ll.clean
concatenate.ll <- function(file.to.concat) {
  ll.cal.pre.i.l <- list()
  if (grepl("odyssey", raw.ll.files[i])) {
    # if (length(unique(cal.data$period.file.uid[which(grepl(files.uid.df[i,1], cal.data$file.uid))]))>0) { # pour contourner erreur si aucun fichier dans les lignes de cal.data (p. ex. si étiquettées "rejected", comme '41361_20241125')
    cal.data <- cal.bulleur.list.appendd[[1]]
    for (l in 1:length(unique(cal.data$period.file.uid[which(grepl(files.uid.df[i,1], cal.data$file.uid))]))) { print(l) # si mm fichier.uid.i, coller les périodes ensemble (ainsi, retirer et remettre ne demande pas plus de manipulations et surtout ps des manipulations individuelles)
      cal.data.i.l <- unique(cal.data[which(grepl(files.uid.df[i,1], cal.data$file.uid)),
                                      c("file.uid", "site.uid", "well.uid", "trmnt.uid", 'lab.probe.id', 'probe.uid', 'probe.brand',
                                        "day.begining.aaaa.mm.dd.hh.mm", 'day.end.aaaa.mm.dd.hh.mm', "period.file.uid")])[l,] # cal.data.i.l = les infos dont j'ai besoin pour recouper selon la période l du fichier i
      # recoupage de ll.pre.data selon cal.data selon début et fin des mesures et retrait de colonnes
      ll.clean.l <- ll.clean %>%
        dplyr::filter(date.time.tz.orig >= cal.data.i.l$day.begining.aaaa.mm.dd.hh.mm) %>% # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data.i.l
        dplyr::filter(date.time.tz.orig <= cal.data.i.l$day.end.aaaa.mm.dd.hh.mm) %>% # <= date de mesure de NP plus petite ou égale à la date end dans cal.data.i.l 
        dplyr::select("scan.id", "raw.value.mm", contains("calibrated.value.cm"), "date.time.UTC.0", "date.time.tz.orig") 
      # changer pour un nom explicite, fichier encore à calibrer (d'où "pre")
      ll.cal.pre.i.l[[l]] <- ll.clean.l
    }
    # coller toutes les données de la sonde i ensemble (différentes mesures temporelles, mm puits.trmnt.année) ----
    ll.cal.pre.i <- do.call(rbind, ll.cal.pre.i.l)
    return(ll.cal.pre.i) 
  } # row bind -> on colle deux df de structure identique (les l nombre de ll.cal.pre.i.l) de différents k.l, associées à différents temps de la période de mesure de la sonde k 
  if (grepl("hobo", raw.ll.files[i])) {
    ##### boucle de concaténation des données (fichier.uid ensemble, sinon autre calibration et graphique disctinct) ----
    # if (length(unique(cal.data$period.file.uid[which(grepl(files.uid.df[i,1], cal.data$file.uid))])) != 0) { # si mm fichier.uid.i, coller les périodes ensemble (ainsi, retirer et remettre ne demande pas plus de manipulations et surtout ps des manipulations incividuelles)
    cal.data <- cal.bulleur.list.appendd[[2]]
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
        select("scan.id", "raw.value.kPa_pres.abs", contains("calibrated.value.cm"),  "temperature_dC", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0") # %>%  # date et time sans "UTC.0" sont dans le fuseau horaire d'origine (tz trouvé en croisant les coordonnées "coords")
      # insérer les données de longueur de fil(où ajouté CDS, voir raw.to.clean_cal.data) et de out.mean.cm de cal.data
      long.fil.CDS.cm <- unique(cal.data$long.fil.CDS.cm[cal.data$period.file.uid == period.file.uid.l])
      ll.clean.l$long.fil.CDS.cm <- rep(long.fil.CDS.cm, times = nrow(ll.clean.l))
      out.mean.cm <- unique(cal.data$out.mean.cm[cal.data$period.file.uid == period.file.uid.l])
      ll.clean.l$out.mean.cm <- rep(out.mean.cm, times = nrow(ll.clean.l))
      
      # changer pour un nom explicite, fichier encore à calibrer (d'où "pre")
      ll.cal.pre.i.l[[l]] <- ll.clean.l
    }
    ll.cal.pre.i <- do.call(rbind, ll.cal.pre.i.l) # row bind -> on colle deux df de structure identique (les ll.cal.pre.i) de différents i.l, associées à différents temps de la période de mesure de la sonde i
    return(ll.cal.pre.i) 
  }
}

# clean.to.calibrated_ll
# file.to.calibrate <- ll.cal.pre.i
offset.all <- tibble(offsets = list(), file.uid = character(), time = list())
clean.to.calibrated_ll <- function(file.to.calibrate) {
  # boucles
  if(grepl("odyssey", raw.ll.files[i])) {     # i<-84 # exemple avec plusieurs mesures de bulleur 
    if(FALSE %in% (!file.to.calibrate$calibrated.value.cm %in% rep("NA", times = length(file.to.calibrate$calibrated.value.cm)))) { # si TRUE = STOP et warning (les données ont été calibrées avec le programme-mère, vérifier que j'obtiens les mêmes) // si FALSE = continuer la boucle (donc rien, donc IF statement)
      stop(paste0("Attention, la colonne calibrated.value n'est pas vide. Sonde problématique : i = ", paste(i), "; ", ll.pre[i]))
    } # créer une autre colonne, le cas échéant (à faire)
    
    # joindre les données avec celles du fichier de calibration
    # joindre données de bulleur par la colonne en commun "date.time.UTC.0"
    tidy.cal.bulleur.data.pre.0 <- left_join(cal.bulleur.list.appendd[[2]], file.to.calibrate)  # comparaions aux données (raw.val, en (UNITÉS?) de sonde (i) au même moment que chaque mesure (ligne) de tidy.bulleur.data // selon Wikipedia, il y aurait des mSiemens/mm qqpart
    tidy.cal.bulleur.data.pre <- full_join(tidy.cal.bulleur.data.pre.0, cal.bulleur.list.appendd[[1]], relationship = "many-to-many")
    
    # coller la valeur enregistrée (raw.value.mm) au moment du bulleur dans cal.value où cal.no == 3
    tidy.cal.bulleur.data.pre.1 <- tidy.cal.bulleur.data.pre %>%
      mutate(cal.value = ifelse(cal.no == "3", paste(raw.value.mm), cal.value),
             cal.value = as.numeric(cal.value), 
             cal.neg.length_mm = as.numeric(cal.neg.length_mm))
    rm(tidy.cal.bulleur.data.pre) # supprimer vieux objets
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
      x2 = unique(tidy.cal.bulleur.data.pre.1$cal.value[tidy.cal.bulleur.data.pre.1$cal.no =="2"])
      x1 = unique(tidy.cal.bulleur.data.pre.1$cal.value[tidy.cal.bulleur.data.pre.1$cal.no =="1"])
      a.slope = ( y2 - y1 ) / ( x2 - x1 ) # sans unité
      b.verticalIntercept = y1 - (a.slope * x1) # unité : ??
    }
    tidy.cal.bulleur.data.pre.1$cal.neg.length_mm[tidy.cal.bulleur.data.pre.1$cal.no == "3"] <- (tidy.cal.bulleur.data.pre.1$cal.value[tidy.cal.bulleur.data.pre.1$cal.no == "3"]*a.slope)+b.verticalIntercept # contourner le bug dû aux NA par du base R
    tidy.cal.bulleur.data <- tidy.cal.bulleur.data.pre.1 %>%
      mutate(prof_nappe_odyssey_cm_plus.out = cal.neg.length_mm/10 + tidy.cal.bulleur.data.pre.1$out.mean.cm, # ok
             prof_nappe_bulleur_cm = (bulleur.rel.to.surface.mm/10), # en cm // ok -> le out déjà retiré dans raw.to.clean_cal.data, c'est pourquoi on a bulleur au lieu de in.bulleur (valeur brutte)
             offset_cm = prof_nappe_odyssey_cm_plus.out - prof_nappe_bulleur_cm)
    
    # vérfication des offsets
    nouvelle.ligne <- tibble(
        offsets = list(tidy.cal.bulleur.data$offset_cm[tidy.cal.bulleur.data$cal.no == "3"]), # & abs(tidy.cal.bulleur.data$offset_cm) <= 5]
        file.uid = files.uid.df$file.uid[i],
        time = list(tidy.cal.bulleur.data$date.time.UTC.0[tidy.cal.bulleur.data$cal.no == "3"]))
    offset.all <- bind_rows(offset.all, nouvelle.ligne)
    # si j'élimine des données, transformer cal.data : dupliquer ligne, celle avec valeur erronnée = measure.status == rejected, puis
    # ligne dupliquée enlever la valeur aberrante (manière la plus simple)   
    # calcul du mean_offset // CADUQUE (16 avril 2026) : en enlevant les outliers + de 4 cm d'écart (vérifier avec Sylvain)
    offset.all <- tidy.cal.bulleur.data$offset_cm[tidy.cal.bulleur.data$cal.no == "3"] # & abs(tidy.cal.bulleur.data$offset_cm) <= 5]    tidy.cal.bulleur.data$mean_offset_cm <- mean(offset.all) # ici ça devrait faire la moyenne sur les données, mais le tableur filtré pour conserver ligne approuvées (measure.status !== rejected)
    tidy.cal.bulleur.data$mean_offset_cm <- mean(offset.all)
    
    # tidy.cal.bulleur.data, pour les autres calibrations 
    tidy.cal.bulleur.data <- tidy.cal.bulleur.data %>% 
      select("file.uid", "lat.garmin.dms", "long.garmin.dms", "measure_status", "site.uid", "chapter", "type", "relative.distance", "year", "well.uid", "trmnt.uid", 
             "lab.probe.id", "probe.uid", "probe.brand", "comment", "day.begining.aaaa.mm.dd.hh.mm", "day.end.aaaa.mm.dd.hh.mm", 
             "out.mean.cm", "bulleur.no",  "bulleur.prof.mm", "bulleur.rel.to.surface.mm", 
             "in.bulleur.date.time.UTC.0" = "date.time.UTC.0", "date.time.tz.orig", "in.bulleur.date.aaaammdd", "in.bulleur.time.tz.orig", "in.bulleur.obs", 
             "period.file.uid", "scan.id", raw.value = "raw.value.mm", contains("calibrated.value.cm"), 
             "cal.neg.length_mm", "cal.value", "cal.no", "prof_nappe_odyssey_cm_plus.out", 
             "prof_nappe_bulleur_cm", "offset_cm", "mean_offset_cm", long.fil.CDS.cm)
    
    { # Calibration linéaire (Odyssey seulement)
      file.to.calibrate$calibrated.value.cm.lin = round( x = (((file.to.calibrate$raw.value.mm*a.slope) + b.verticalIntercept)/10) + unique(tidy.cal.bulleur.data$out.mean.cm - tidy.cal.bulleur.data$mean_offset_cm), digits = 2)
      } # Calibration linéaire (Odyssey seulement)
    
    { # Calibration avec le offset moyen à partir du bulleur (Odyssey seulement)
      file.to.calibrate$calibrated.value.cm.blo <- round(unique(tidy.cal.bulleur.data$mean_offset_cm) + file.to.calibrate$calibrated.value.cm.lin, 2)
    } # Calibration avec le bulleur (offset; Odyssey seulement)
    
    #### ajout précipitations (MeteoStat) ----
    pattern <- paste0("hourly*.res.", files.uid.df$site.uid[i]) # **
    # ** donnée de météo, avec seule modification = pression atm résiduelle après régression linéaire ~ température et effet aléatoire de station météo
    # voir script daily_weather_v2.0.R, créé vers avril 2026
    tidy.weather.data.res <- read.csv(paste0("connectivite/data/clean/", list.files(path = "connectivite/data/clean", pattern = pattern)))
    
    #### assembler données de sonde et données de MeteoStat selon la date et l'heure ----
    file.to.calibrate.meteo <- left_join(file.to.calibrate, tidy.weather.data.res, by = join_by(date.time.UTC.0))
    
    # format final -> nom final et ajout de métadonnées
    files.uid.df$well.uid[i] <- unique(tidy.cal.bulleur.data$well.uid)
    file.to.calibrate.meteo$well.uid <- rep(files.uid.df$well.uid[i], times = nrow(file.to.calibrate.meteo))
    site.pre <- sub("SiteName,","", raw.ll.files.i[[2]][1])
    file.to.calibrate.meteo$site <- str_to_title(site.pre)
    file.to.calibrate.meteo <- file.to.calibrate.meteo %>% rename(raw.value = raw.value.mm)
    file.to.calibrate.meteo$file.uid <- rep(files.uid.df$file.uid[i], times = nrow(file.to.calibrate.meteo))
    file.to.calibrate.meteo$probe.brand <- files.uid.df$probe.brand[i]
    ll.cal <- file.to.calibrate.meteo %>% # ceci est donc le format final, à intégrer dans la liste ll.clean
      select(scan.id, raw.value, contains("calibrated.value.cm"), date.time.UTC.0, `date.time.tz.orig`,
             "prcp.ms", well.uid, site, file.uid, probe.brand) # retirer des colonnes intermédiaires et mm format que ll.clean[[i]]$data
    
    ### création de la liste dans la liste [[i]]  ----
    tidy.WTD.data.i <- list("data" = ll.cal, "metadata" = raw.ll.files.i[[2]], 
                            "verif.data" = tidy.cal.bulleur.data, "odyssey.mean" = offset.all) 
  } # le fichier du level logger correspondant à la position i; [1] : data (dataframe), [2] : metadata (character string)
  if (grepl("hobo", raw.ll.files[i])) {
    # D'abord, tidy.cal.bulleur.data, utilisé dans certaines calibrations 
    # joindre les données avec celles du fichier de calibration
    # joindre données de bulleur par la colonne en commun "date.time.UTC.0"
    tidy.cal.bulleur.data.pre.0 <- left_join(cal.bulleur.list.appendd[[2]], file.to.calibrate)  # comparaions aux données (raw.val, en (UNITÉS?) de sonde (i) au même moment que chaque mesure (ligne) de tidy.bulleur.data // selon Wikipedia, il y aurait des mSiemens/mm qqpart
    tidy.cal.bulleur.data.pre <- full_join(tidy.cal.bulleur.data.pre.0, cal.bulleur.list.appendd[[1]], relationship = "many-to-many")
    # créer mm colonnes que pour les Odyssey en prévision du rbind
    tidy.cal.bulleur.data <- tidy.cal.bulleur.data.pre %>% 
      mutate(prof_nappe_odyssey_cm_plus.out = NA, prof_nappe_bulleur_cm = NA, offset_cm = NA, mean_offset_cm = NA)
    tidy.cal.bulleur.data <- tidy.cal.bulleur.data %>% select("file.uid", "lat.garmin.dms", "long.garmin.dms", "measure_status", "site.uid", "chapter", "type", "relative.distance", "year", "well.uid", "trmnt.uid", 
                                                              "lab.probe.id", "probe.uid", "probe.brand", "comment", "day.begining.aaaa.mm.dd.hh.mm", "day.end.aaaa.mm.dd.hh.mm", 
                                                              "out.mean.cm", "bulleur.no",  "bulleur.prof.mm", "bulleur.rel.to.surface.mm", 
                                                              "in.bulleur.date.time.UTC.0" = "date.time.UTC.0", "in.bulleur.date.aaaammdd", "in.bulleur.time.tz.orig", "in.bulleur.obs", 
                                                              "period.file.uid", "scan.id", raw.value = "raw.value.kPa_pres.abs", contains("calibrated.value.cm"), "date.time.tz.orig", # "date.AAAA-MM-JJ", "time.HH.MM.SS" # répétition de date.time.tz.orig
                                                              "cal.neg.length_mm", "cal.value", "cal.no", "prof_nappe_odyssey_cm_plus.out", "prof_nappe_bulleur_cm", "offset_cm", "mean_offset_cm", long.fil.CDS.cm)
    
    # Référence : Jutras et Bourgault, 2024, Version 2.0, section 7 (/Users/Aliz/Documents/Doctorat/_Connectivité/Protocoles (dossiers copiés du serveur A'24)/Leveloggers & Hauteur nappe phréatique/_HOBO_Protocole de mesure de nappe_2024-11-01_NE PAS DIFFUSER.docx)
    { # Calibration MeteoStat
      #### extraction des données de METEOSTAT //[auparavant : ECCC/CCCS] et ménage ----
      pattern <- paste0("hourly*.res.", files.uid.df$site.uid[i]) # **
      # ** donnée de météo, avec seule modification = pression atm résiduelle après régression linéaire ~ température et effet aléatoire de station météo
      # voir script daily_weather_v2.0.R, créé vers avril 2026
      meteoStat.data.hourly.res.site <- read.csv(paste0("connectivite/data/clean/", list.files(path = "connectivite/data/clean", pattern = pattern)))

      #### assembler données du HOBO et données de MeteoStat selon la date et l'heure ----
      # Jutras&Bourgault V2.0, 2024; étape a) Associer par dates et par heures les données mesurées par les sondes de niveau hydrostatique et la pression atmosphérique
      cal.meteoStat.data <- left_join(file.to.calibrate, meteoStat.data.hourly.res.site, by = join_by(date.time.UTC.0)) %>%
        select("scan.id", "date.time.UTC.0","raw.value.kPa_pres.abs", "temperature_dC", contains("calibrated.value.cm"),
               `date.AAAA-MM-JJ`, "time.HH.MM.SS", `date.time.tz.orig`, "date.time.ms", pres.kpa.res.ms, everything()) # enlever les nombreuses colonnes qui n'ont pas rapport dans ces démarches
      
      # Jutras&Bourgault V2.0, 2024; étape b)	Calculer la hauteur d’eau au-dessus de la sonde par la soustraction de la pression atmosphérique, convertie en cm d’eau, à la pression mesurée par la sonde
      # Jutras&Bourgault V2.0, 2024; étape b.i)	La conversion de kPa en cm d’eau est : 1 kPa = 10,1972 cm d’eau
      cal.meteoStat.data$pression.eau.kPa <- cal.meteoStat.data$raw.value.kPa_pres.abs - cal.meteoStat.data$pres.kpa.res.ms
      cal.meteoStat.data$hauteur.eau.cm.pre <- cal.meteoStat.data$pression.eau.kPa * 10.197162129779 # règle de trois
      cal.meteoStat.data$hauteur.eau.cm <- cal.meteoStat.data$hauteur.eau.cm.pre # dépend de la façon dont les mesures de longueurs en cm sont prises
      cal.meteoStat.data <- cal.meteoStat.data %>% select("scan.id", "date.time.UTC.0","raw.value.kPa_pres.abs", pression.eau.kPa, hauteur.eau.cm, everything())
      
      # Jutras&Bourgault V2.0, 2024; étape c)
      # Jutras&Bourgault V2.0, 2024; étape c.i)	La profondeur de la nappe phréatique par rapport à la surface du sol =
      # ((La longueur du fil + La constante CDS) – La longueur du puits d’observation qui dépasse la surface du sol) – La hauteur d’eau au-dessus de la sonde
      # c. Convertir la hauteur d’eau au-dessus de la sonde en profondeur de la nappe phréatique par rapport à la surface du sol, puis en valeur relative au sol (*-1)
      cal.meteoStat.data$calibrated.value.cm.ms <- round(x = (cal.meteoStat.data$long.fil.CDS.cm - cal.meteoStat.data$out.mean.cm - cal.meteoStat.data$hauteur.eau.cm), digits = 2) * -1
      
    }  # Calibration MeteoStat
    
    # si sonde barométrique LA BONNE ANNÉE, calibration / barometric station (bs)
    annee <- as.character(unique(lubridate::year(cal.meteoStat.data$date.time.UTC.0)))
    if (any(grepl(files.uid.df$site.uid[i], barometric.station) & any(grepl(annee, barometric.station)))) {
      # ouverture des données de la station appropriée ----
      barometric.data <- read_rds(paste0("connectivite/data/raw/barometric.station.", files.uid.df$site.uid[i],".RDS")) # enregistré lors de l'exécution de la fonction : raw.to.clean_ll()
      
      # assembler données du HOBO et données de la barometric station selon la date et l'heure ----
      # Jutras&Bourgault V2.0, 2024; étape a) Associer par dates et par heures les données mesurées par les sondes de niveau hydrostatique et la pression atmosphérique
      cal.meteoStat.baro.data <- full_join(cal.meteoStat.data, barometric.data) %>% 
        select("scan.id", "date.time.UTC.0","raw.value.kPa_pres.abs", "temperature_dC", contains("calibrated.value.cm"),
               `date.AAAA-MM-JJ`, "time.HH.MM.SS", `date.time.tz.orig`, "date.time.ms", pres.kpa.res.ms, pressure.kPa.bs, everything()) # enlever les nombreuses colonnes qui n'ont pas rapport dans ces démarches
      
      # Jutras&Bourgault V2.0, 2024; étape b)	Calculer la hauteur d’eau au-dessus de la sonde par la soustraction de la pression atmosphérique, convertie en cm d’eau, à la pression mesurée par la sonde
      # Jutras&Bourgault V2.0, 2024; étape b.i)	La conversion de kPa en cm d’eau est : 1 kPa = 10,1972 cm d’eau
      cal.meteoStat.baro.data$pression.eau.kPa.bs <- cal.meteoStat.baro.data$raw.value.kPa_pres.abs - cal.meteoStat.baro.data$pressure.kPa.bs
      cal.meteoStat.baro.data$hauteur.eau.cm.bs <- cal.meteoStat.baro.data$pression.eau.kPa.bs * 10.197162129779 # règle de trois
      
      # Jutras&Bourgault V2.0, 2024; étape c)
      # Jutras&Bourgault V2.0, 2024; étape c.i)	La profondeur de la nappe phréatique par rapport à la surface du sol =
      # ((La longueur du fil + La constante CDS) – La longueur du puits d’observation qui dépasse la surface du sol) – La hauteur d’eau au-dessus de la sonde
      # c. Convertir la hauteur d’eau au-dessus de la sonde en profondeur de la nappe phréatique par rapport à la surface du sol, puis en valeur relative au sol (*-1)
      cal.meteoStat.baro.data$calibrated.value.cm.bs <- round(x = (cal.meteoStat.baro.data$long.fil.CDS.cm - cal.meteoStat.baro.data$out.mean.cm - cal.meteoStat.baro.data$hauteur.eau.cm.bs), digits = 2) * -1
      
      # format final -> nom final (et ajout de métadonnées plus bas)
      ll.cal <- cal.meteoStat.baro.data %>% # ceci est donc le format final, à intégrer dans la liste ll.clean
        select(scan.id, raw.value = raw.value.kPa_pres.abs, contains("calibrated.value.cm"), date.time.UTC.0, `date.time.tz.orig`, "prcp.ms") # retirer des colonnes intermédiaires et mm format que ll.clean[[i]]$data
      # enlevé aussi : `date.AAAA-MM-JJ`, time.HH.MM.SS, date.time.tz.orig, long.fil.CDS.cm, out.mean.cm, hauteur.eau.cm 
      
    } else { # Calibration sonde barométrique / barometric station (bs)
      
      # Si aucune station barométrique, enregistrement final
      # si pas de sonde barométrique, juste enregistrer ll.cal (nom final, ll.cal <- cal.meteoStat.data) sans autre modification
      # format final -> nom final
      ll.cal <- cal.meteoStat.data %>% # ceci est donc le format final, à intégrer dans la liste ll.clean
        select(scan.id, raw.value = raw.value.kPa_pres.abs, contains("calibrated.value.cm"), date.time.UTC.0, `date.time.tz.orig`, "prcp.ms") # retirer des colonnes intermédiaires et mm format que ll.clean[[i]]$data
      # enlevé aussi : `date.AAAA-MM-JJ`, time.HH.MM.SS, date.time.tz.orig, long.fil.CDS.cm, out.mean.cm, hauteur.eau.cm 
      ll.cal$well.uid <- rep(files.uid.df$well.uid[i], times = nrow(file.to.calibrate))
      ll.cal$file.uid <- rep(files.uid.df$file.uid[i], times = nrow(ll.cal))
    }
    
    { # Vérification avec le bulleur, lorsque disponible
      ll.cal$calibrated.value.cm.blo <- map_dbl( # merci à GoogleIA, j'apprends à utiliser la programmation fonctionnelle (l'univers PURRR)
        ll.cal$date.time.UTC.0, 
        ~ {
          # On cherche l'index de la date correspondante (ll.cal$date.time.UTC.0 = .x) dans tidy.cal.bulleur.data.pre
          idx <- match(.x, tidy.cal.bulleur.data$in.bulleur.date.time.UTC.0)
          # Si trouvé, on divise par 10, sinon on met NA
          if (!is.na(idx)) round(tidy.cal.bulleur.data$bulleur.rel.to.surface.mm[idx]/10, 2) else NA_real_
        }
      )
    } # vérification avec le bulleur, lorsque disponible
    
    # ajout de métadonnées
    files.uid.df$well.uid[i] <- unique(tidy.cal.bulleur.data$well.uid)
    ll.cal$well.uid <- rep(files.uid.df$well.uid[i], times = nrow(ll.cal))
    ll.cal$site <- sub("Titre de tracé : ","",raw.ll.files.i[[2]][1])
    ll.cal$file.uid <- rep(files.uid.df$file.uid[i], times = nrow(ll.cal))
    ll.cal$probe.brand <- files.uid.df$probe.brand[i]
    
    # pour que la boucle fonctionne, 16 avril 2026
    offset.all <- matrix(NA)
    
    ### création de la liste dans la liste [[i]]  ----
    tidy.WTD.data.i <- list("data" = ll.cal, "metadata" = raw.ll.files.i[[2]], 
                            "verif.data" = tidy.cal.bulleur.data, "odyssey.mean" = offset.all) 
    
  } # le fichier du level logger correspondant à la position i; [1] : data (dataframe), [2] : metadata (character string)
  return(tidy.WTD.data.i)
}

# ============================================================================= /
#  Calibration data ----
# ============================================================================= /
# raw.to.clean_cal.data
# données de bulleur, emplacement des puits, nom de fichier, long. fil, etc.
# time.zone <- tz
raw.to.clean_cal.data <- function(cal.data.file, time.zone) { # ne calibre pas encore les données
  cal.data.0 <- cal.data.file %>% # format numérique
    mutate(across(matches("out\\.|.cm$"), as.numeric), # .* "n'importe quel caractère"
           long.fil.cm = as.numeric(long.fil.cm))
  
  # ajout constante de distance de la sonde (CDS) : distance entre mesure de fil (voir protocole mesure de fil) et emplacement exact de la mesure de pression ou de mS/cm par la sonde
  CDS <- data.frame(type = c("HOBO U20", "HOBO U20L", "ODYSSEY", "other"), # Hobo seulement : mesure longueur du fil tel que dans protocole; à la limite de la boîte de sonde. Les constantes de longueur de boîte de sonde à la sonde à l'interface intérieur de la sonde sont ajoutées à cette étape-ci.
                    constante = c("12.93", "13.3", "0", "0")) %>%
    mutate_at('constante', as.numeric) # liste des types de SNH avec lesquelles j'ai pris des données; chaque "marque/modèle" (type) est traitée de façon différente
  brand.i <-  ifelse(length(cal.data.0$probe.brand[which(grepl(files.uid.df[i,1], cal.data.0$file.uid))])==0,"other", cal.data.0$probe.brand[which(grepl(files.uid.df[i,1], cal.data.0$file.uid))])
  cal.data.0$long.fil.CDS.cm <- cal.data.0$long.fil.cm + CDS$constante[CDS$type == brand.i]
  
  # vérification de valeurs OUT
  cal.data.0 <- cal.data.0 %>% 
    mutate(out.R = ifelse(is.na(out.1.a.cm), round((out.1.a.cm + out.1.b.cm + out.1.c.cm)/3, digits = 1), as.numeric(out.mean.cm)))
  if(all(cal.data.0$out.R == round(cal.data.0$out.mean.cm, digits = 2), na.rm =T))  { # si TOUS TRUE (fonction any()) = changer nom de out.R et supprimer la mesure entrée manuellement // si FALSE = avertissement
    print("out moyenne ok")
  } else { stop("Attention, le out entré dans cal.data.1 (syn. level_logger_calibration_all.csv) n'est pas identique à la moyenne hauteurs de la saison.") } 
  
  # ménage de colonnes (conserver juste out.mean.cm)
  cal.data.1 <- cal.data.0 %>% 
    select(!c(long.fil.cm, "out.1.a.cm", "out.1.b.cm", "out.1.c.cm", "out.2.a.cm", "out.2.b.cm", "out.2.c.cm", "out.R"))
  
  # création de colonnes à identifiant unique 
  cal.data.1$period.file.uid <- paste0(cal.data.1$day.begining.aaaa.mm.dd.hh.mm, "--", cal.data.1$day.end.aaaa.mm.dd.hh.mm, ".",cal.data.1$file.uid)
  # format POSIX begining et end
  cal.data.1$day.begining.aaaa.mm.dd.hh.mm <- ymd_hm(cal.data.1$day.begining.aaaa.mm.dd.hh.mm, tz = tz)
  # vérifié, et si fxn tz utilisée avant fxn raw.to.clean_cal.data, le tz change en fonction du raw.ll.files.i (ce qui est attendu)
  cal.data.1$day.end.aaaa.mm.dd.hh.mm <- ymd_hm(cal.data.1$day.end.aaaa.mm.dd.hh.mm, tz = tz)
  
  # séquence de formatage wide-to-long et nettoyage des données bulleurs (parse date, UTC.0 et transformations bulleur en format utilisable = (in.bulleurs - out)
  step <- c("cal", "bulleur") # utilisé pour créer des colonnes dans la "boucle large-to-long maison"
  cal.bulleur.list.appendd <- list()
  # if(nrow(unique(cal.data.1[which(grepl(files.uid.df[i,1], cal.data.1$file.uid)),])) > 0) {
  odyssey.data.pre <- unique(cal.data.1[which(grepl(files.uid.df[i,1], cal.data.1$file.uid)),]) # plusieurs périodes valides pour un fichier: toutes les coller ensemble (si plusieurs ligne dans la recherche : cal.data$period.file.uid[which(grepl(files.uid.df[k,1], cal.data$file.uid))]) toutes colonnes conservées
  
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
    # j <- 1
    odyssey.cols <- colnames(odyssey.data.list[[j]])
    numbers <- regexpr("[0-9]+", odyssey.cols)
    nstep <- as.numeric(regmatches(odyssey.cols, numbers)) # pour chaque objet de la liste, trouver nom de colonnes extraire leur chiffre associé, le cas échéant, puis enlever le chiffre et refaire le df
    odyssey.data <- list()
    for (k in unique(nstep)) {
      remove <- paste(setdiff(unique(nstep), k), collapse = "|")
      odyssey.data.j.k <- odyssey.data.list[[j]] %>% select(!matches(remove)) %>% # sélect si contient j dans les noms de colonne; j'obtiens les colonnes de chiffre (step) k, je crée un df avec juste ces colonnes; j'ajoute une colonne avec le chiffre
        mutate(!!paste0(step[j],".no") := rep(k, nrow(odyssey.data.pre)),
               period.file.uid = odyssey.data.pre$period.file.uid) #,
      # row.uid = odyssey.data.pre$row.uid)
      colnames(odyssey.data.j.k) <- sub('[[:digit:]]+', '', colnames(odyssey.data.j.k)) # nom colonne sans chiffre
      odyssey.data[[k]] <- odyssey.data.j.k
    }
    if(j == 1) {  cal.bulleur.list.appendd[[1]] <- do.call(rbind, odyssey.data) } else { # rbind les lignes des j df
      # cal.bulleur.list.appendd[[2]] <- do.call(rbind, odyssey.data) %>% dplyr::filter(ifelse(in.bulleur.prof.cm == NA, bulleur.no == 1, ) # df %>% filter(!is.na(a)) //, drop_na(., in.bulleur.prof.cm)
      cal.bulleur.list.appendd[[2]] <- do.call(rbind, odyssey.data) %>% 
        # garder ligne 1 (même si in.bulleur == NA; par exemple si la nappe est tombée sous la limite de détection au bulleur)
        # et toutes les lignes qui, dans la colonne in.bulleur, ont une valeur non-NA
        dplyr::filter(row_number() == 1 | (!is.na(in.bulleur.prof.cm))) 
    }
    # traitement identique que la sonde soit HOBO ou ODYSSEY jusqu'à ce point-ci permet d'obtenir les mêmes colonnes !
    # seule différence : cal.no 1-2-3 qui est créé mais qui ne veut rien dire pour les HOBO, donc supprimer la valeur
    if(grepl("hobo", raw.ll.files[i])) {
      cal.bulleur.list.appendd[[1]]$cal.no <- "NA"
      cal.bulleur.list.appendd[[1]] <- distinct(cal.bulleur.list.appendd[[1]]) # pour les hobo, enlever la valeur cal.no = 1-2-3 et distinct pour passer de 3 lignes à une ligne
    }
  }
  rm(odyssey.data.pre); rm(odyssey.data.bulleur); rm(odyssey.data.cal); rm(odyssey.data); rm(odyssey.data.j.k); rm(j); rm(k) # supprimer vieux objets (fait automatiquement dans une fonction)
  
  # préparation de la date-heure des in.bulleur, en prévision de la comparaison de date-heure entre tableaux
  # explication des noms : tidy.bulleur.data.pre.0 == structure de cal.bulleur.list.appendd[[2]]
  tidy.bulleur.data.pre.0 <- cal.bulleur.list.appendd[[2]] %>% mutate(date.JJ.MM.AAAA_time.HH.MM.SS_tz = paste0(in.bulleur.date.aaaammdd, " ", in.bulleur.time.tz.orig, " ", time.zone)) # tz original /       # normal qu'il y ait des NA dans le df bulleur, les enlever (dépend du nombre de données de bulleur prises)
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
    mutate(bulleur.prof.mm = (as.numeric(in.bulleur.prof.cm) - out.mean.cm) * 10) %>% # données de bulleur finales (in.bulleur-out) et en mm pour correspondre aux cal.val
    mutate(bulleur.rel.to.surface.mm = (in.bulleur.rel.to.surface.cm + out.mean.cm) * 10) %>% # données de bulleur finales (in.bulleur-out) et en mm pour correspondre aux cal.val
    select(!c(date.time.UTC.0pre, date.time.UTC.0pre.1, date.time.roundd, in.bulleur.prof.cm, in.bulleur.rel.to.surface.cm))
  # retour à la sous-liste pour utilisation dans la fonction clean.to.calibrated_ll (itération de calibration pour les sondes ODYSSEY, n'impacte pas les autres marques de sondes)
  cal.bulleur.list.appendd[[2]] <- tidy.bulleur.data
  {
    # update 16 mars 2026, je comprends pas ma note...
    # OK / idée : revenir à cal.bulleur.list.appendd[[1]] et [[2]] (ce dernier à la place de tidy.bulleur.data) 
    # toutes données répétées autrement, juste garder bulleur vs cal data séparées pour l'instant
    # parce que la version finale de cal.data est celle qui comportera les valeurs ODYSSEY finales...
    # il faudrait que mes fonctions et le résultat ne soit pas impacté si juste sondes HOBO... pour passer au suivant
    # pour cela, voir format final si contient des colonnes inutiles -> demander à FRancis un argument pour les enlever automatiquement selon type de sondes utilisées
  } # note à supprimer
  return(cal.bulleur.list.appendd)
}

# ============================================================================= /
#  Graphs & visualisation ----
# ============================================================================= /
# dual.axis.calculation
# yaxis.left <- tidy.weather.data$pres.kpa
# yaxis.right <- tidy.weather.data$temp
# abs.ratio <- 0.2
dual.axis.calculation <- function(yaxis.left, yaxis.right, abs.ratio = NULL) {
  parameters.list <- list(ratio = NA, offset = NA, right.max = NA, right.min = NA, left.max = NA, left.min = NA)
  
  # ratio des axes (source : multiples forums)
  parameters.list$left.max <- max(yaxis.left, na.rm = T)
  parameters.list$left.min <- min(yaxis.left, na.rm = T)
  left_range <- left.max - left.min
  
  parameters.list$right.max <- max(yaxis.right, na.rm = T)
  parameters.list$right.min <- min(yaxis.right, na.rm = T)
  right_range <- right.max - right.min
  
  # facteur de mise à l'échelle pour le ratio et offset à partir du haut du range de l'axe gauche
  if(!is.null(abs.ratio)) {
    parameters.list$ratio <- (left_range * abs.ratio) / right_range
    parameters.list$offset <- right.max - (right_range * abs.ratio) 
  }
  return(parameters.list)
}

# theme.Aliz
{
  # https://rfortherestofus.com/2025/04/ggplot2-theme
  # theme.Aliz <- function() {
  #   # Set base theme and font family ============================================= #
  #   theme_minimal(
  #     base_family = "Libre Franklin"
  #   ) +
  #     # Overwrite base theme defaults ============================================ #
  #   theme(
  #     # Text elements ========================================================== #
  #     plot.title = element_text(
  #       size = 18,
  #       face = "bold",
  #       color = "#333333",
  #       margin = margin(b = 10)
  #     ),
  #     plot.subtitle = element_text(
  #       size = 14,
  #       color = "#999999",
  #       margin = margin(b = 10)
  #     ),
  #     plot.caption = element_text(
  #       size = 13,
  #       color = "#777777",
  #       margin = margin(t = 15),
  #       hjust = 0
  #     ),
  #     axis.text = element_text(
  #       size = 11,
  #       color = "#333333"
  #     ),
  #     plot.title.position = "plot",
  #     plot.caption.position = "plot",
  #     # Line elements ========================================================== #
  #     panel.grid.minor = element_blank(),
  #     panel.grid.major = element_line(
  #       linetype = "dashed",
  #       linewidth = 0.15,
  #       color = "#999999"
  #     ),
  #     panel.grid.major.x = element_blank(),
  #     axis.ticks.x = element_line(
  #       linetype = "solid",
  #       linewidth = 0.25,
  #       color = "#999999"
  #       ),
  #     axis.ticks.length.x = unit(4, units = "pt")
  #   )
  # }
}

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
  if (grepl("hobo|barometric.station", raw.ll.files[i])) {
    site.name.pre <- gsub("\\\"", '', raw.ll.files.i[[2]])[1] # extraire nom de site fichier origine
    site.name <- sub("Titre de tracé : ","",site.name.pre)
    coords <- c(zones$latitude[zones$site==site.name][1], zones$longitude[zones$site==site.name][1]) # extraire la bonne lat, long selon le nom du site
    tz <- tz_lookup_coords(coords[1], coords[2], method = "fast", warn = FALSE) # trouver le UTC selon la lat long
    return(tz)
  }
}

# # zone.site
# ABANDON
# # trouver le nom de site à partir de pt GPS (rayon 500m à 5 km) # attention à la distance si j'ajoute GPB à mes sites !
# # zone.shp <- "~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp"
# # site.uid <- trmnt.uid.i.site.uid.pre
# zone.site <- function(site.uid) {
#   zones <- read_sf("~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp") %>% as.data.frame(zones) # ouvrir données du shapefile pour accéder les zones
#   site.name <- zones$site[zones$site.uid == site.uid]
#   return(site.name)
# }


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