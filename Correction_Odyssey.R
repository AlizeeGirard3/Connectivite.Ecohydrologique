#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                          Correction, Odyssey calibration data
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2025-05-11
# Date mise à jour : 
# Pourquoi : Les Odyssey ont initialement été calibrées en inversant les longueurs de fil 20 et 80/140
#            Ce code vise à inverser les valeurs.
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
# voir courriel de Sylvain, 2 mai 2025

# LEXIQUE :
# SNH : sonde de niveau hydrostatique / synonymes : LL : level logger; sonde, probe

##########################################################################-

# .rs.restartR()
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")
source("general.scripts/scripts/fonctions.R")

# Librairies ----
if (!require("conflicted")) install.packages("conflicted") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("readxl")) install.packages("readxl") # lire les excel
if (!require("openxlsx")) install.packages("openxlsx") # lire les excel
if (!require("stringr")) install.packages("stringr") # gosser avec des suites de caractères, str_replace, [...]
if (!require("dplyr")) install.packages("dplyr") # entre autres : left_join()
if (!require("tidyr")) install.packages("tidyr") # entre autres : extract_numeric() / extract_numeric() is deprecated: please use readr::parse_number() instead

# importer et préparer donnees dans R ----
level_logger_calibration_all <- read.csv("connectivite/data/raw/level_logger_calibration_all.csv", sep = ";")
level_logger_calibration_all_others <- level_logger_calibration_all %>% dplyr::filter(measure_type != "calibration" | is.na(measure_type))

level_logger_calibration_all_140_1 <- level_logger_calibration_all %>% dplyr::filter(long.fil.cm == "140") %>% dplyr::filter(cal.order == "1")
level_logger_calibration_all_140_1$cal.length.cm <- sub("2", "14", level_logger_calibration_all_140_1$cal.length.cm)
level_logger_calibration_all_140_2 <- level_logger_calibration_all %>% dplyr::filter(long.fil.cm == "140") %>% dplyr::filter(cal.order == "2")
level_logger_calibration_all_140_2$cal.length.cm <-sub("14", "2", level_logger_calibration_all_140_2$cal.length.cm)

level_logger_calibration_all_80_1 <- level_logger_calibration_all %>% dplyr::filter(long.fil.cm == "80") %>% dplyr::filter(cal.order == "1")
level_logger_calibration_all_80_1$cal.length.cm <- sub("2", "8", level_logger_calibration_all_80_1$cal.length.cm)
level_logger_calibration_all_80_2 <- level_logger_calibration_all %>% dplyr::filter(long.fil.cm == "80") %>% dplyr::filter(cal.order == "2")
level_logger_calibration_all_80_2$cal.length.cm <-sub("8", "2", level_logger_calibration_all_80_2$cal.length.cm)

level_logger_calibration_all <- rbind(level_logger_calibration_all_80_1, level_logger_calibration_all_80_2, level_logger_calibration_all_140_1, 
                                              level_logger_calibration_all_140_2, level_logger_calibration_all_others)
level_logger_calibration_all$cal.length.cm <- as.integer(level_logger_calibration_all$cal.length.cm)
level_logger_calibration_all <- arrange(level_logger_calibration_all, probe.uid)

if("level_logger_calibration_all.csv" %in% list.files("connectivite/data/raw"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { write.csv(level_logger_calibration_all, file = "connectivite/data/raw/level_logger_calibration_all.csv") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }
