#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                       Scrit de recherche de la station météo
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2025-04-16
# Date mise à jour : 2025-09-29
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
if (!require("tidyverse")) install.packages("tidyverse") # entre autres : left_join()
# if (!require("tidyr")) install.packages("tidyr") # entre autres : extract_numeric() / extract_numeric() is deprecated: please use readr::parse_number() instead
if (!require("sf")) install.packages("sf"); if (!require("lutz")) install.packages("lutz") # GIS in R
# if (!require("lubridate")) install.packages("lubridate")
# options(lubridate.verbose = TRUE) # pour expliciter ce que les fonctions font
# if (!require("mapview")) install.packages("mapview") ## Spatial analyses
# if (!require("parsedate")) install.packages("parsedate") # lire les excel
# option d'arrêter le code si message d'erreur (source fonctions.R)
# options(error=pause)
# options(error=NULL) # annuler
## trouver le station_id (canada) ----
if (!require("weathercan")) install.packages("weathercan") # Integrating data from weathercan (ECCC/CCCS), Gouvernement du Canada
stations_dl()
stations_meta()
## MeteoStat (téléchargement) ----
# Package importation données de Meteostat (source)
# if (!require("remotes")) install.packages("remotes") 
# remotes::install_github("wegar-2/okeanos.meteostat")
# library("okeanos.meteostat")
# # fonctions (6) : 
# ?bIsScalarOfClass()
# bIsScalarOfType()
# cGetMeteostatStatusCodeMessage()
# dtGetDailyStationDataOverUpToOneYear() #:	Fetch daily Meteostat data for a station
# dtGetMeteostatWeatherStationsDict()  #:	Download the full list set of stations
# dtStationsDict() #: Dictionary of Meteostat API weather stations
# # NE FONCTIONNE PAS !!

# 31 octobre 2025, télécharger indez MANUELLEMENT, mettre là (voir path ci-dessous)
index <- readLines("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/Repertoire des stations FR.csv")[-c(1:3)] 
if (!require("fs")) install.packages("fs") # pour obtenir la date de naissance du fichier
birth_time <- file_info("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/Repertoire des stations FR.csv") %>% 
  as.data.frame() %>% select(birth_time)
if((as.Date(trunc(as.POSIXct(birth_time[1,1], format = "%d/%m/%Y"), "day")) + 90) < as.Date(trunc(Sys.time(), "day"))) { 
  stop("Attention, index vieux de plus de 90 jours. Retélécharger l'index et vérifier que les stations référées ont les données jusqu'à 2025.") } 
  # date création de l'index + 90 jours est-il plus grand que date création d'aujourd'hui ?
  # (as.Date(2025-10-31) + 90) <  as.Date("2026-01-30") # EXEMPLE délais trop long, retélécharger index
  # TRUE = ARRÊT de tout
  
stations.names <- c("BEAUPORT", "TRACADIE", "MIRAMICHI RCS", "RIVIERE-DU-LOUP")
index.df <- read.csv(text = index, sep = ",") %>% as.data.frame() %>% 
  dplyr::filter(Nom %in% c("BEAUPORT", "TRACADIE", "MIRAMICHI RCS", "RIVIERE-DU-LOUP")) %>% 
  dplyr::filter(Année.de.fin == 2025)

















# A. Importer/modifier toutes données pertinentes  ----
cal.data <- read.csv("connectivite/data/raw/level_logger_calibration_all.csv", sep = ";", dec = ",") 
zones <- read_sf("~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp") %>% # couche géomatique (QGIS) à laquelle référer avec la fonction read_sf("")
  as.data.frame(zones) %>% 
  dplyr::filter(descriptio == "Site confirmé")
head(zones); str(zones)

### tableau avec les station ID de chaque site   ----
# { # i<- 5 #
# print(i)
# site.name.i <- zones$site[i]
# coords <- c(zones$latitude[zones$site==site.name.i][1], zones$longitude[zones$site==site.name.i][1])
# stations <- stations_search(coords = coords, dist = 25, interval = "hour") }
#### MANUELLEMENT : trouver la station ID (canada+(lat, long) et la distance du site de recherche et trouver le station ID sur MeteoStat[-> sur le site de MétéoStat])  ----
# source :
station_id.phd <- data.frame("phd.site.UID" = NA, "phd.site.name"= NA,"station_name" = NA, "station_id_canada" = NA, "station_id_MeteoStat" = NA, "lat.station" = NA, "long.station" = NA, "dist_from_zone" = NA)
station_id.phd[1,1:8] <- c("STH", "St-Henri","BEAUPORT",27803,71578,46.8,-71.2,18.14627)
station_id.phd[2,1:8] <- c("INK", "Inkerman","TRACADIE",6205,71719,48.01,-64.49, 49.50673) # MISCOU ISLAND (AUT)
station_id.phd[3,1:8] <- c("BRNTC", "Burnt Church","MIRAMICHI RCS", 10808,"AOYMS",47.01,-65.47,27.63049)
station_id.phd[4,1:8] <- c("PRO", "Président-Ouest","RIVIERE-DU-LOUP",8539,71578,47.81,-69.55,3.021966)
station_id.phd[5,1:8] <- c("GPB", "Grande Plée Bleue", "BEAUPORT",27803,71578,46.8,-71.2,12.499890)
# write.csv(station_id.phd, file = "~/Desktop/station_id.phd.csv")

glimpse(stations()) %>% 
  as_tibble() %>% 
  dplyr::filter(prov == c("NB", "QC")) %>%
  # dplyr::filter(end == 2025) %>%
  dplyr::filter(!station_name == c("MONTREAL PERSILLIER", "OSKELANEO 2")) %>% # coords = NA
  dplyr::filter(interval == "hour") %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_write("~/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/ECCC_stations_dispo.shp",
           delete_layer = T) # attention écrase fichier
# file.remove(c("~/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/ECCC_stations_dispo.shp","~/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/ECCC_stations_dispo.dbf",
#               "~/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/ECCC_stations_dispo.prj","~/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/ECCC_stations_dispo.shx"))
# Rivière-du-Loup NON, INDISPONIBLE (seulement en 1980...)


# %>% 
#   # dplyr::filter(Identification.Station == "8539") %>%
#   dplyr::filter(grepl("Loup", Nom))
# donc ici bon no de station à jour !!


# B. Aperçu des données disponibles en ligne  ----
# Weather Can
glimpse(stations()) %>% 
  as_tibble() %>% 
  dplyr::filter(prov == c("NB", "QC")) %>%
  # dplyr::filter(end == 2025) %>%
  dplyr::filter(!station_name == c("MONTREAL PERSILLIER", "OSKELANEO 2")) %>% # coords = NA
  dplyr::filter(interval == "hour") %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_write("~/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/ECCC_stations_dispo.shp",
           delete_layer = T) # attention écrase fichier
# file.remove(c("~/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/ECCC_stations_dispo.shp","~/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/ECCC_stations_dispo.dbf",
#               "~/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/ECCC_stations_dispo.prj","~/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/ECCC_stations_dispo.shx"))
# Rivière-du-Loup NON, INDISPONIBLE (seulement en 1980...)
# MIRAMICHI NON, j'en 2023
# TRACADIE PE, start , en = NULL
# BEAUPORT PE, start , en = NULL
# Tests BEAUPORT et TRACADIE

data.availability.test <- stations() %>%
  as_tibble() %>%
  dplyr::filter(prov == "NB") %>%
  # dplyr::filter(end == 2025) %>%
  dplyr::filter(station_name == "TRACADIE") # %>% 
  # dplyr::filter(end == 1988) #-> il ne comprend pas que end = NA N'EST PAS 1988 !!


# C. Téléchargement des données disponibles en ligne  ----
RDL.ECCC_data <- weather_dl(station_ids = station_id.phd$station_id_canada[station_id.phd$station_name == "RIVIERE-DU-LOUP"], interval = "hour")
# quand ça fonctionne, transposer en boucle





### importer données des stations ID de chaque site   ----
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


