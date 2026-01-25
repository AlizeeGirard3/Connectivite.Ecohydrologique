#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                       Daily weather data cleaning and visualisation
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
station_id.phd <- read.csv("connectivite/data/raw/station_id.phd.csv") # issu du script "Recherche_station_meteo_ID_v2.0.r", trouver "station.name"

# consigne de données
weather.data.list <- list()

# boucle pour chaque site, agglomérer les onglets pertinents
for(file.no in 1:length(weather.files)) {
  print(file.no)
  # file.no <- 9
  file.step <- weather.files[file.no]
  site.name <- iconv(str_extract(file.step, "(?<=(hourly|daily)\\.).*(?=\\.csv)"), to = "UTF-8-MAC") # merci à Google IA... C'est compliqué les regex /  # merci google IA pour m'aider à traiter mes noms de site avec un accent francophone...
  coords <- c(zones$latitude[zones$site==site.name], zones$longitude[zones$site==site.name]) # extraire la bonne lat, long selon le nom du site
  tz <- tz_lookup_coords(coords[1], coords[2], method = "fast", warn = FALSE) # trouver le UTC selon la lat long
  
  # traitement selon le type de donnée
  if(grepl("hourly", weather.files[file.no])) {
    weather.raw  <- read.csv(file.step)
    # fonction : filtrer données meteoStat : si "metno_forecast", extraire nom de colonne et retirer les colonnes où nom a une partie correspondant
    weather.raw.filtrd <- filter.raw.file(object.to.filter = weather.raw, type = "MeteoStat")

    # préparation de la colonnes de jointure "date.time.UTC.0" et de la colonne de temps au time zone du site
    weather.0 <- weather.raw.filtrd %>% mutate(date.time.UTC.0pre = paste(year, month, day, hour))
    weather.0$date.time.UTC.0pre <- ymd_h(weather.0$date.time.UTC.0pre, tz = "UTC") + 1 # date-temps des données bruttes = UTC-0 source :https://dev.meteostat.net/formats.html. Spécifier "UTC" dans la fonction = notation de lubridate pour UTC-0.
    weather.1 <- weather.0 %>% select(date.time.UTC.0pre, everything(), -c(year, month, day, hour)) # ajuster la date et l'heure et ajout d'une seconde, sinon, les données 00:00:00 étaient effacées !
    weather <- weather.1 %>% 
      mutate(date.time.SiteTZ = with_tz(as.POSIXct(weather.1[["date.time.UTC.0pre"]], tz = "UTC"), tz = tz), 
             date.time.UTC.0 = gsub("[+]00:00", "Z", format_iso_8601(date.time.UTC.0pre))) %>% 
      select(!date.time.UTC.0pre)
    
    # ajout de colonnes d'identification (station.name, d'où provient les données)
    weather <- weather %>% 
      mutate(station.name = station_id.phd$station_name[station_id.phd$phd.site.name == site.name],
             "tz.col" = tz,
             initial.type = "MeteoStat hourly")
    
    # placer dans la liste de recueil des fichiers, à l'endroit "file.no"
    weather.data.list[[file.no]] <- weather
  } # hourly
  if(grepl("daily", weather.files[file.no])) {
    weather.raw  <- read.csv(file.step)
    # fonction : filtrer données meteoStat : si "metno_forecast", extraire nom de colonne et retirer les colonnes où nom a une partie correspondant
    weather.raw.filtrd <- filter.raw.file(object.to.filter = weather.raw, type = "MeteoStat")
    
    # préparation de la colonnes de jointure "date.time.UTC.0" et de la colonne de temps au time zone du site
    weather.0 <- weather.raw.filtrd %>% mutate(date.time.UTC.0pre = paste(year, month, day))
    weather.0$date.time.UTC.0pre <- ymd(weather.0$date.time.UTC.0pre, tz = "UTC") + 1 # date-temps des données bruttes = UTC-0 source :https://dev.meteostat.net/formats.html. Spécifier "UTC" dans la fonction = notation de lubridate pour UTC-0.
    weather.1 <- weather.0 %>% select(date.time.UTC.0pre, everything(), -c("year", "month", "day")) # ajuster la date et l'heure et ajout d'une seconde, sinon, les données 00:00:00 étaient effacées !
    weather <- weather.1 %>% 
      mutate(date.time.SiteTZ = with_tz(as.POSIXct(weather.1[["date.time.UTC.0pre"]], tz = "UTC"), tz = tz), 
             date.time.UTC.0 = gsub("[+]00:00", "Z", format_iso_8601(date.time.UTC.0pre))) %>% 
      select(!date.time.UTC.0pre)
    
    # ajout de colonnes d'identification (station.name, d'oü provient les données)
    weather <- weather %>% 
      mutate(station.name = station_id.phd$station_name[station_id.phd$phd.site.name == site.name],
             "tz.col" = tz, 
             initial.type = "MeteoStat daily")
    
    # placer dans la liste de recueil des fichiers, à l'endroit "file.no"
    weather.data.list[[file.no]] <- weather
  } # daily
} # file in weather.files

# # joindre les données horaires et journalières
# tidy.weather.data <- weather.data.list %>%
#   map(~ .x %>% mutate(across(everything(), as.character))) %>% # d'abord, tout en caractères, car classe des NA en arrière plan posait problème
#   reduce(full_join, na_matches = "na") %>% # précision de la gestion des NA pour débugger (voir code débuggage ci-dessous), cela ajoutait 13 lignes autrement; merci à GoogleIA pour l'aide au débuggage
#   select(date.time.SiteTZ, tz.col, date.time.UTC.0 , station.name, everything()) %>% 
#   group_by(tz.col) %>% 
#   mutate(date.time.SiteTZ = ymd_hms(date.time.SiteTZ, tz = unique(tz.col))) %>% # considère que le tz est celui spécifié dans la col.tz, donc ne change pas le tz
#   arrange(date.time.SiteTZ, .by_group = T)


# RNEDUE LÀ
# SUITE DU NETTOYAGE ***!
# joindre les données horaires et journalières
tidy.weather.data <- weather.data.list %>%
  map(~ .x %>% mutate(across(everything(), as.character))) %>% # d'abord, tout en caractères, car classe des NA en arrière plan posait problème
  reduce(full_join, na_matches = "na") # %>% # précision de la gestion des NA pour débugger (voir code débuggage ci-dessous), cela ajoutait 13 lignes autrement; merci à GoogleIA pour l'aide au débuggage

# ICI,  SPÉCIFIER CE QUE JE VEUX... ou avant
# √ supprimer temp du df daily
# √ vérifier si temp max et min hour == daily
# √ pluvio vérifier, je croyais que je ne l'avais pas dans hourly mais il y a des données... comprendre
# ce qu'elles représentent
# comprendre ce que veut dire "pres"... 


# TEMP À 20H issue de daily data n'est pas valide... 
# en fait, il faut prendre les données de daily et les coller à chaque ligne de horaire, en spécifiant 
# moyenne du jour ou NA ou garder juste les valeurs que j'ai pas dans horaire (pluvio, ex.)

# ultimement : je veux afficher données horaire de barométrie et TROUVER LE MOYEN D'ENLEVER LA VARIATION DE PRESSION journalière !! 

