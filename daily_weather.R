#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                             Daily weather data dowloads
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2025-05-025
# Date mise à jour : 
# Pourquoi : télécharger les données météo quotidiennes et les sauvegarder
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
# 

# LEXIQUE :
# NP : Nappe phréatique / synonymes : water table
# ECCC/CSSS : Environnement and Climate Change Canada / Canadian Centre for Climate Services 
# tz : time zone, syn. fuseau horaire

##########################################################################-


# RENDUE LÀ


# fichiers "R data serialized" (RDS) à charger directement
# ll.clean<-readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/ll.clean.RDS") # issu de section A.1 du code ci-présent

# .rs.restartR()
source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/general.scripts/scripts/fonctions.R")
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

# Librairies ----
if (!require("conflicted")) install.packages("conflicted") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("readxl")) install.packages("readxl") # lire les excel
if (!require("openxlsx")) install.packages("openxlsx") # lire les excel
if (!require("stringr")) install.packages("stringr") # gosser avec des suites de caractères, str_replace, [...]
if (!require("dplyr")) install.packages("dplyr") # entre autres : left_join()
if (!require("tidyr")) install.packages("tidyr") # entre autres : extract_numeric() / extract_numeric() is deprecated: please use readr::parse_number() instead
if (!require("sf")) install.packages("sf"); if (!require("lutz")) install.packages("lutz") # GIS in R
if (!require("lubridate")) install.packages("lubridate")
options(lubridate.verbose = TRUE) # pour expliciter ce que les fonctions font
# librairies de weathercan
if (!require("weathercan")) install.packages("weathercan") # Integrating data from weathercan (ECCC/CCCS), Gouvernement du Canada
stations_dl()
stations_meta()
# if (!require("naniar")) install.packages("naniar") # Checking data completeness
# if (!require("mapview")) install.packages("mapview") ## Spatial analyses
if (!require("parsedate")) install.packages("parsedate") # lire les excel
# option d'arrêter le code si message d'erreur (source fonctions.R)
# options(error=pause)
# options(error=NULL) # annuler

# A  Donnée issues des sonde de niveau hydrostatique ----
SNH <- as.vector(c("_odyssey", "_hobo"), mode = "character") # liste des types de SNH avec lesquelles j'ai pris des données; chaque "marque" est traitée de façon différente

# A.1 nettoyage et enregistrement en RDS ----
# fonction : modifications automatisées pour chaque fichier issus d'une période de mesures des level loggers
