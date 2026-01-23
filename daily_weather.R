#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                             Daily weather data dowloads
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2025-05-025
# Date mise à jour : 22 janvier 2026
# Pourquoi : afficher données de MétéoStat
# Structure :
# —— connectivite
#         |—— archive
#         |—— data
#                     |—— raw
#                     |—— extracted_raw    <- raw feuilles numériques terrain (plusieurs onglets pour un site), extrait en un df par onglet, tous site confondu (script "data_sites_all")
#                     |—— clean
#         |—— output
#                     |—— data
#                     |—— figures
#         |—— scripts
# NOTES : 
# 

# LEXIQUE :
# NP : Nappe phréatique / synonymes : water table
# ECCC/CSSS : Environnement and Climate Change Canada / Canadian Centre for Climate Services 
# tz : time zone, syn. fuseau horaire

##########################################################################-


# ============================================================================= /
# Initialisation ----
# ============================================================================= /
# Librairies (autres initialisées dans le script sourcé)
if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("ggpubr")) install.packages("ggpubr") # ggarrange()
if (!require("grDevices")) install.packages("grDevices") # pdf()
if (!require("gridExtra")) install.packages("gridExtra") # multiplot()

# Données, dossier directeur fonctions et à charger directement
# .rs.restartR()
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")
source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd_v3.0.R")
# source("general.scripts/scripts/fonctions_generales.R") # CADUQUE ? appel du fichier de métadonnées de projet

# tidy.WTD.data <- readRDS("connectivite/data/clean/tidy.WTD.data.RDS") # obtenu via le script "/scripts/data_water.table.all(v.X).R"
# tidy.cal.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.cal.data.RDS") # obtenu via le script "/scripts/data_water.table.all(v.X).R"
# ele.profiles <- readRDS("connectivite/data/clean/elevation.profiles.RDS") # obtenu via le script "/scripts/data_silte_all(v.X).R" et traité avec

# ============================================================================= /
#  Examination des données bruttes et nettoyage ----
# ============================================================================= /
# listes de données
weather.files <- list.files(path = "connectivite/data/raw", pattern = "meteoStat.data.", full.names = T) # issus directement de MeteoStat, script "recherche_station_meteo_ID_v2.0.r"
# https://dev.meteostat.net/parameters
# https://dev.meteostat.net/formats.html
zones <- read_sf("~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp") %>% # couche géomatique (QGIS) à laquelle référer avec la fonction read_sf("")
  as.data.frame(zones) %>% 
  dplyr::filter(descriptio == "Site confirmé")

# consigne de données
weather.data.list <- list()

# boucle pour chaque site, agglomérer les onglets pertinents
for(file.no in 1:length(weather.files)) {
  # file.no <- 1
  file.step <- weather.files[file.no]
  site.name <- str_extract(file.step, "(?<=(hourly|daily)\\.).*(?=\\.csv)") # merci à Google IA... C'est compliqué les regex
  coords <- c(zones$latitude[zones$site==site.name], zones$longitude[zones$site==site.name]) # extraire la bonne lat, long selon le nom du site
  tz <- tz_lookup_coords(coords[1], coords[2], method = "fast", warn = FALSE) # trouver le UTC selon la lat long
  
  # traitement selon le type de donnée
  if(grep("hourly", weather.files)) {
    daily.weather.raw  <- read.csv(file.step)
    
    # fonction : filtrer données meteoStat : si "metno_forecast", extraire nom de colonne et retirer les colonnes où nom a une partie correspondant
    daily.weather.raw.filtrd <- filter.raw.file(object.to.filter = daily.weather.raw, type = "MeteoStat")
    
    
    # daily.weather <- daily.weather.pre[,-grep("X", colnames(daily.weather.pre))]
    # [1] "year"        "month"       "day"         "hour"        "temp"        "temp_source" "rhum"        "rhum_source"
    # [9] "prcp"        "prcp_source" "wdir"        "wdir_source" "wspd"        "wspd_source" "wpgt"        "wpgt_source"
    # [17] "pres"        "pres_source" "cldc"        "cldc_source" "coco"        "coco_source"
    daily.weather.0 <- daily.weather.raw %>% mutate(date.time = paste(year, month, day, hour))
    daily.weather.0$date.time <- ymd_h(daily.weather.0$date.time, tz = "UTC-0") + 1 # date-temps des données bruttes = UTC-0 source :https://dev.meteostat.net/formats.html
    daily.weather.1 <- daily.weather.0 %>% select(date.time, everything(), -c("year", month, day, hour, X)) # ajuster la date et l'heure et ajout d'une seconde, sinon, les données 00:00:00 étaient effacées !
    
  } # daily
  if(grep("daily", weather.files)) {
    daily.weather.raw  <- read.csv(file.step)
    # daily.weather <- daily.weather.pre[,-grep("X", colnames(daily.weather.pre))]
    # [1] "year"        "month"       "day"         "hour"        "temp"        "temp_source" "rhum"        "rhum_source"
    # [9] "prcp"        "prcp_source" "wdir"        "wdir_source" "wspd"        "wspd_source" "wpgt"        "wpgt_source"
    # [17] "pres"        "pres_source" "cldc"        "cldc_source" "coco"        "coco_source"
    daily.weather.0 <- daily.weather.raw %>% mutate(date.time = paste(year, month, day, hour))
    daily.weather.0$date.time <- ymd_h(daily.weather.0$date.time, tz = "UTC-0") + 1 # date-temps des données bruttes = UTC-0 source :https://dev.meteostat.net/formats.html
    daily.weather.1 <- daily.weather.0 %>% select(date.time, everything(), -c("year", month, day, hour, X)) # ajuster la date et l'heure et ajout d'une seconde, sinon, les données 00:00:00 étaient effacées !
    
    
    # rendue là
    
    
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
    
    
    
  } # hourly
  # %>% mutate(pressure.kPa = pres * 0.1) # pression donnée en hPa (hectopascal). 1 hPa = 0,1 kPa. Example: convert 15 hPa to kPa: 15 hPa = 15 × 0.1 kPa = 1.5 kPa
  
} # file in weather.files

# étape 1 : aggréger toutes les données météoStat 
# 1.1 : daily -> UTC.0
# 1.2 : nettoyage date-heure
# 1.3 : hourly
# 1.4 : nettoyage date-heure
# étape 2 : join les données météoStat, répétition des données daily à chaque ligne heure

# utlimement : je veux afficher données horaire de barométrie et TROUVER LE MOOYEN D'ENLEVER LA VARIATION DE PRESSION journalière !! 


