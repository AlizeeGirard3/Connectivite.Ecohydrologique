#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                       Daily weather data cleaning and visualisation
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2025-05-25
# Date mise à jour : 22 janvier 2026
# Caduque : 6 avril 2026
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
# on sait que la pression ne change pas à l’échlle régionale, mais capteurs mauvais a une erreur à cause de la température // et voir les articles sur l’ÉT horaire

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
# if (!require("gridExtra")) install.packages("gridExtra") # multiplot()
if (!require("patchwork")) install.packages("patchwork")
if (!require("slider")) install.packages("slider") # sélection d'une fenêtre glissante

# Données, dossier directeur fonctions et à charger directement
# .rs.restartR()
setwd("~/Documents/Doctorat/_R_Stats_PhD")
source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd_v3.1.R")
# source("general.scripts/scripts/fonctions_generales.R") # CADUQUE ? appel du fichier de métadonnées de projet

# ============================================================================= /
# Lecture, agglomération des données ----
# ============================================================================= /
# listes de données
weather.files <- list.files(path = "connectivite/data/raw", pattern = "meteoStat.data.", full.names = T) # issus directement de MeteoStat, script "recherche_station_meteo_ID_v2.0.r"
# https://dev.meteostat.net/parameters
# https://dev.meteostat.net/formats.html
# https://dev.meteostat.net/faq.html
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
    # fonction : filtrer données meteoStat
    weather.raw.filtrd <- filter.raw.file(object.to.filter = weather.raw, type = "MeteoStat")

    # préparation de la colonnes de jointure "date.time.UTC.0" et de la colonne de temps au time zone du site
    weather.0 <- weather.raw.filtrd %>% mutate(date.time.UTC.0pre = paste(year, month, day, hour))
    weather.0$date.time.UTC.0pre <- ymd_h(weather.0$date.time.UTC.0pre, tz = "UTC") + 1 # date-temps des données bruttes = UTC-0 source :https://dev.meteostat.net/formats.html. Spécifier "UTC" dans la fonction = notation de lubridate pour UTC-0.
    weather.1 <- weather.0 %>% select(date.time.UTC.0pre, everything(), -c("day", "hour")) # ajuster la date et l'heure et ajout d'une seconde, sinon, les données 00:00:00 étaient effacées !
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
    weather.1 <- weather.0 %>% select(date.time.UTC.0pre, everything(), -c("day")) # ajuster la date et l'heure et ajout d'une seconde, sinon, les données 00:00:00 étaient effacées !
    weather <- weather.1 %>%
      mutate(date.time.SiteTZ = with_tz(as.POSIXct(weather.1[["date.time.UTC.0pre"]], tz = "UTC"), tz = tz),
             date.time.UTC.0 = gsub("[+]00:00", "Z", format_iso_8601(date.time.UTC.0pre))) %>%
      select(!date.time.UTC.0pre)

    # ajout de colonnes d'identification (station.name, d'oü provient les données)
    weather <- weather %>%
      mutate(station.name = station_id.phd$station_name[station_id.phd$phd.site.name == site.name],
             "tz.col" = tz,
             initial.type = "MeteoStat daily") %>%
      select(!c(temp, temp_source, pres, pres_source))

    # placer dans la liste de recueil des fichiers, à l'endroit "file.no"
    weather.data.list[[file.no]] <- weather
  } # daily
} # file in weather.files
rm(weather); rm(weather.0); rm(weather.1); rm(weather.raw); rm(weather.raw.filtrd)

# joindre les données horaires et journalières
source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/general.scripts/scripts/fonctions.R")
tidy.weather.data <- weather.data.list %>%
  map(~ .x %>% mutate(across(everything(), as.character))) %>% # d'abord, tout en caractères, car classe des NA en arrière plan posait problème
  reduce(full_join, na_matches = "na") %>% # précision de la gestion des NA pour débugger (voir code débuggage ci-dessous), cela ajoutait 13 lignes autrement; merci à GoogleIA pour l'aide au débuggage
  # select(date.time.SiteTZ, tz.col, date.time.UTC.0 , station.name, everything()) %>%
  mutate(pres.kpa = as.numeric(pres)/10,
         temp = as.numeric(temp), 
         month  = month.df$month.en[as.numeric(tidy.weather.data$month)]) %>% 
  group_by(tz.col) %>%
  mutate(date.time.SiteTZ = ymd_hms(date.time.SiteTZ, tz = unique(tz.col))) %>% # considère que le tz est celui spécifié dans la col.tz, donc ne change pas le tz
  arrange(date.time.SiteTZ, .by_group = T) %>% 
  select(date.time.SiteTZ, tz.col, date.time.UTC.0 , station.name, pres.kpa, pres_source, everything(), -"pres")

## stockage des résultats (écrase version précédante) ====
# format R des tidy.weather.data (une liste)
if("tidy.weather.data.RDS" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { saveRDS(tidy.weather.data, file = "connectivite/data/clean/tidy.weather.data.RDS") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }
s
# ============================================================================= /
#  Examination des données bruttes ----
# ============================================================================= /

tidy.weather.data$pres.kpa <- as.numeric(tidy.weather.data$pres)/10
list <- split(tidy.weather.data, c(tidy.weather.data$station.name, tidy.weather.data$year, tidy.weather.data$month)) # équivalent à toute la boucle sous "graph.topo.list <- list()"
chaque.graph <- map(list, ~ ggplot(.x, aes(date.time.SiteTZ, pres.kpa)) + # pression en HPa/10 -> kpa
                      geom_line() +
                      scale_x_datetime(date_breaks = "4 months", date_labels = "%y/%b/%d") + 
                      ggtitle(unique(.$station.name)) +
                      theme_bw() + 
                      theme(plot.title = element_text(hjust = 0.5), 
                            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5)))
# tous graphiques arrangés automatiquement (merci aux nouvelles fonctions apparues depuis mon M.Sc. <3 !!)
mes_graphiques <- wrap_plots(chaque.graph, 
                             nrow = length(list)) # ,
                             # widths = 200, 
                             # heights = 200)
mes_graphiques


# TRACADIE SEULEMENT 
# tidy.weather.data$pres <- as.numeric(tidy.weather.data$pres)/10
# tidy.weather.data.tracadie <- tidy.weather.data %>% dplyr::filter(station.name == "TRACADIE")
# tidy.weather.data.graph <- tidy.weather.data.tracadie %>% 
#   ggplot(aes(date.time.SiteTZ, pres)) + # pression en HPa/10 -> kpa
#   geom_line() +
#   # scale_x_datetime(date_breaks = "2 weeks", date_labels = "%y/%b/%d") + 
#   ggtitle(unique(tidy.weather.data$station.name)) +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5))
# tidy.weather.data.graph


# ============================================================================= /
# Relation pression atmophérique ~ température ambiante ----
# ============================================================================= /
tidy.weather.data <- readRDS(file = "connectivite/data/clean/tidy.weather.data.RDS") 

# analyse de corrélation sur l'ensemble du jeux de données
tidy.weather.data$temp <- as.numeric(tidy.weather.data$temp)
tidy.weather.data$pres <- as.numeric(tidy.weather.data$pres.kpa)
cor.test(tidy.weather.data$temp, tidy.weather.data$pres, method = "spearman")
# Spearman's rank correlation rho
# 
# data:  tidy.weather.data$temp and tidy.weather.data$pres
# S = 5.3203e+13, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#         rho 
# -0.03509721 
# Un rho de -0,035 signifie que la température n'explique quasiment aucune variation de la pression dans ce jeu de données précis

## test du 24h  ----
# Dans votre boucle, après avoir nettoyé vos données :
df_24h <- tidy.weather.data %>%
  arrange(date.time.UTC.0) %>%
  mutate(
    # Calcule la corrélation sur une fenêtre glissante de 24h
    cor_glissante = slide2_dbl(
      .x = temp, 
      .y = pres, 
      .f = ~cor(.x, .y, method = "spearman"),
      .before = 24,
      .complete = TRUE
    )
  )
summary(df_24h$cor_glissante)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   -1.00   -0.71   -0.33   -0.22    0.23    1.00   37748 
#     Médiane à -0.33 : La relation est globalement négative (la pression baisse quand il fait chaud), ce qui valide votre capteur.
#     Min à -1.00 : Certains jours, la corrélation est parfaite (thermique pure).
#     Max à +1.00 : Certains jours, la pression monte avec la température 
#     Moyenne à -0.22 : C'est bien plus significatif que votre -0.03 global, car cela montre l'effet thermique quotidien moyen


# ============================================================================= /
# autres tests
# ============================================================================= /
# mm données que sur ECCC en ligne ?
tidy.weather.data.beauport.oct.2025 <- tidy.weather.data %>% 
  dplyr::filter(station.name == "BEAUPORT",
                date.time.SiteTZ >= "2025-10-01",
                date.time.SiteTZ <= "2025-10-30") # mm valeurs que sur Environnement Canada mm date (26 janvier 2026)
                                                  # différence que sur ECCC pas de données de pression...
# oui..
