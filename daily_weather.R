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
# daily.weather <- read.csv("connectivite/data/raw/", , sep = ";", dec = ",")

# étape 1 : aggréger toutes les données météoStat 
# 1.1 : daily -> UTC.0
# 1.2 : nettoyage date-heure
# 1.3 : hourly
# 1.4 : nettoyage date-heure
# étape 2 : join les données météoStat, répétition des données daily à chaque ligne heure

# utlimement : je veux afficher données horaire de barométrie et TROUVER LE MOOYEN D'ENLEVER LA VARIATION DE PRESSION journalière !! 



