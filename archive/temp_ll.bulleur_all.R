#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                           Bulleur, tableau de vérification
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2026-01-07
# Date mise à jour : 
# Pourquoi : Vérifications : quelle erreur moyenne pour les bulleurs ? Seulement pour les Hobo, pour les Odyssey on utilise la valeur pour calibrer
# Idée : avec l'erreur moyenne de bulleur, ajuster ou donner une fourchette d'erreur pour les Odyssey...
# NOTES : 

# LEXIQUE :
{ 
  # SNH : sonde de niveau hydrostatique / synonymes : LL : level logger; sonde, probe
  # NP : Nappe phréatique / synonymes : water table / WTD
  # tz : time zone, syn. fuseau horaire
}
##########################################################################-

# Librairies ----
if (!require("openxlsx")) install.packages("openxlsx") # lire les excel
if (!require("tidyverse")) install.packages("tidyverse") # méta package // gosser avec des suites de caractères, str_replace, [...]

# Fichiers à charger directement ----
# cal.data <- read.xlsx("connectivite/data/clean/cal.data.xlsx", sep = ";")
tidy.cal.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.cal.data.RDS") # issu du code "data_water.table_all_v3.0"
tidy.WTD.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.RDS") # issu du code "data_water.table_all_v3.0"

# .rs.restartR()
# source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd_v2.0.R")
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

# retrait de colonnes inutiles de tidy.cal.cata
tidy.cal.data <- tidy.cal.data %>% 
  select(!c(27:39)) %>% distinct() # enlever les données temporaires associées à la calibration des sondes Odyssey

# fichier de consigne de données
water.table.verif <- data.frame()
# boucle de vérification au bulleur pour chaque mesure de bulleur (tidy.cal.data)
if (nrow(tidy.WTD.data[[m]]$data) != 0) { # si le fichier SNH n'est pas vide
  for (tidy.cal.data.line in 1:nrow(tidy.cal.data)) {
    print(tidy.cal.data.line)
    tidy.cal.data.line <- tidy.cal.data[tidy.cal.data.line,] # filtrer ll.bulleur (level_logger_calibration_all.csv) par le ligne "n" (vérification n au bulleur)
    tidy.WTD.data.m.n <- tidy.WTD.data.m$data[tidy.WTD.data.m$data$date.time.UTC.0 == # fitlrer les données du fichier SNH par la période (unique) de la ligne n = vérification au bulleur
                                                tidy.cal.data.line$in.bulleur.date.time.UTC.0,]
    water.table.verif[tidy.cal.data.line, 1:4] <- tibble("probe.uid" = sonde.m, # créer le dataframe de vérification pour les lignes "n" de la SNH "m"
                                                         "file.extraction.date" = date.m,
                                                         "probe.measure.cm" = tidy.WTD.data.m.n$calibrated.value.cm,
                                                         "bulleur.mesure.cm" = tidy.cal.data.line$in.bulleur.rel.to.surface.mm/10)
  } 
  # 
  # water.table.verif[nrow(water.table.verif) + 1:nrow(water.table.verif.n), 1:4] <- water.table.verif.n # inscrire les données dans le dataframe final, à la dernière ligne
} else if (nrow(tidy.WTD.data[[m]]$data) == 0)  {
    water.table.verif[nrow(water.table.verif) + 1, 1:4] <- data.frame("probe.uid" = sonde.m, # si ll.clean[[j]]$data est vide, mettre NA dans le dataframe
                                                                      "file.extraction.date" = date.m,
                                                                      "probe.measure.cm" = NA,
                                                                      "bulleur.mesure.cm" = NA)
    } 

  # m<-97
  print(m)
  tidy.WTD.data.m <- tidy.WTD.data[[m]]
  # extraire # sonde des différentes marques de SNH
  if (grepl("odyssey", tidy.WTD.data.m[[2]][11])) { # ODYSSEY
    # où trouver no de sonde dans ODYSSEY metadata
    metadata.line <- tidy.WTD.data.m$metadata[12] # probe.uid
    numbers <- gregexpr("[0-9]+", metadata.line)
    sonde.m <- regmatches(metadata.line, numbers)
    # où trouver le file.uid dans ODYSSEY metadata
    metadata.line <- tidy.WTD.data.m$metadata[10] # probe.uid
    file.uid.m <- gsub("file.uid : ", "", metadata.line)
    # où trouver la date d'extraction dans ODYSSEY metadata
    date.line <- tidy.WTD.data.m$metadata[13] # probe.uid
    date.numbers <- gregexpr("[0-9]+", date.line)
    date.m <- unlist(regmatches(date.line, date.numbers))
  } else if (grepl("hobo", tidy.WTD.data.m$metadata[4])) { # HOBO
    # où trouver no de sonde dans HOBO metadata
    metadata.line <- tidy.WTD.data.m$metadata[5] # probe.uid
    numbers <- gregexpr("[0-9]+", metadata.line)
    sonde.m <- unlist(regmatches(metadata.line, numbers))
    # où trouver le file.uid dans HOBO metadata
    metadata.line <- tidy.WTD.data.m$metadata[3] # file.uid
    file.uid.m <- gsub("file.uid : ", "", metadata.line)
    # où trouver la date d'extraction dans HOBO metadata
    date.line <- tidy.WTD.data.m$metadata[6]
    date.numbers <- gregexpr("[0-9]+", date.line)
    date.m <- unlist(regmatches(date.line, date.numbers))
  }
  # fichiers de consigne de données
  water.table.verif <- data.frame()
  
  
  # PROBLÈME DE BOUCLE !!
  
  
  
  # boucle de vérification au bulleur pour chaque mesure de bulleur (tidy.cal.data)
  if (nrow(tidy.WTD.data[[m]]$data) != 0) { # si le fichier SNH n'est pas vide
    for (tidy.cal.data.line in 1:nrow(tidy.cal.data)) {
      print(tidy.cal.data.line)
      tidy.cal.data.line <- tidy.cal.data[tidy.cal.data.line,] # filtrer ll.bulleur (level_logger_calibration_all.csv) par le ligne "n" (vérification n au bulleur)
      tidy.WTD.data.m.n <- tidy.WTD.data.m$data[tidy.WTD.data.m$data$date.time.UTC.0 == # fitlrer les données du fichier SNH par la période (unique) de la ligne n = vérification au bulleur
                                                  tidy.cal.data.line$in.bulleur.date.time.UTC.0,]
      water.table.verif[n, 1:4] <- tibble("probe.uid" = sonde.m, # créer le dataframe de vérification pour les lignes "n" de la SNH "m"
                                            "file.extraction.date" = date.m,
                                            "probe.measure.cm" = tidy.WTD.data.m.n$calibrated.value.cm,
                                            "bulleur.mesure.cm" = tidy.cal.data.line$in.bulleur.rel.to.surface.mm/10)
    } 
    }
    # 
    # water.table.verif[nrow(water.table.verif) + 1:nrow(water.table.verif.n), 1:4] <- water.table.verif.n # inscrire les données dans le dataframe final, à la dernière ligne
  } else if (nrow(tidy.WTD.data[[m]]$data) == 0)  {
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





