#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                       Scrit de recherche de la station météo
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2025-04-16
# Date mise à jour : 
# Pourquoi : Rechercher Station_ID pour inscrire dans le cal_data
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
# LEXIQUE :
##########################################################################-

# .rs.restartR()
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")
source("general.scripts/scripts/fonctions.R")

# Librairies ----
if (!require("conflicted")) install.packages("conflicted") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
# if (!require("readxl")) install.packages("readxl") # lire les excel
# if (!require("openxlsx")) install.packages("openxlsx") # lire les excel
# if (!require("stringr")) install.packages("stringr") # gosser avec des suites de caractères, str_replace, [...]
# if (!require("dplyr")) install.packages("dplyr") # entre autres : left_join()
# if (!require("tidyr")) install.packages("tidyr") # entre autres : extract_numeric() / extract_numeric() is deprecated: please use readr::parse_number() instead
if (!require("sf")) install.packages("sf"); if (!require("lutz")) install.packages("lutz") # GIS in R
# if (!require("lubridate")) install.packages("lubridate")
# options(lubridate.verbose = TRUE) # pour expliciter ce que les fonctions font
# librairies de weathercan
if (!require("weathercan")) install.packages("weathercan") # Integrating data from weathercan (ECCC/CCCS), Gouvernement du Canada
stations_dl()
stations_meta()
# if (!require("naniar")) install.packages("naniar") # Checking data completeness
# if (!require("mapview")) install.packages("mapview") ## Spatial analyses
# if (!require("parsedate")) install.packages("parsedate") # lire les excel
# option d'arrêter le code si message d'erreur (source fonctions.R)
# options(error=pause)
# options(error=NULL) # annuler

# A. Importer toutes données pertinentes  ----
cal.data <- read.csv("connectivite/data/raw/level_logger_calibration_all.csv", sep = ";", dec = ",")
# site.name <- "Burnt Church" 
site.name <- "Président-Ouest" 


# BOUCLE AVEC TOUS MES SITES**




# extraire la bonne lat, long ----
# créer une couche géomatique (QGIS) auquel référer avec la fonction read_sf("")
zones <- read_sf("~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp")
zones <- as.data.frame(zones)
head(zones); str(zones)
coords <- c(zones$latitude[zones$site==site.name][1], zones$longitude[zones$site==site.name][1])
# OU inscrire manuellement les coordonnées dans l'objet "coords"
# coords <- "inscrire manuellement" # format (exemple) :  num [1:2] 46.7 -71.1
stations <- stations_search(coords = c(zones$latitude[zones$site == site.name][1],
                                             zones$longitude[zones$site == site.name][1]), dist = 25)

# aller sur le site : https://climate.weather.gc.ca/historical_data/search_historic_data_e.html ----
# entrer les coordonnées pour trouver la distance de la station
# entrer le station_id (trouver l'ID dans l'objet R "stations") dans la colonne "cal.station_id" de level_logger_calibration_all.csv

# stations_meta()
?stations_search
?stations()
(stations_search_results <- stations_search(name = "MIRAMICHI RCS"))
library(dplyr)
(stations_filter_results <- dplyr::filter(stations(), station_id == 10808))
