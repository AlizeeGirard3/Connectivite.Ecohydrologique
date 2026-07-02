#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                             Functions - Category selection
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : 2026-06-29
# Date mise à jour : 
# Pourquoi : function pour sélectionner rapidement les catégories de groupement des graphiques séries chronologiques d'hydrologie
# NOTES : 
 
###########################################################################-

# Initialisation ----
# Librairies
library(conflicted) # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("dplyr")) install.packages("dplyr") # pour manipulation donnees (pipe, etc)
# if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("withr")) install.packages("withr") # T'o Québec icitte (date-time en français)

# Dossier de travail et fonctions
setwd("~/Documents/Doctorat/_R_Stats_PhD")
source("general.scripts/scripts/fonctions_generales.R") # appel du fichier de métadonnées de projet
source("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/scripts/fonctions_phd_v3.2.R")

# Import des fichiers de données récents
tidy.WTD.data <- readRDS("connectivite/data/clean/tidy.WTD.data.RDS")
tidy.cal.data <- readRDS("~/Documents/Doctorat/_R_Stats_PhD/connectivite/data/clean/tidy.cal.data.RDS")
ele.profiles <- readRDS("connectivite/data/clean/elevation.profiles.RDS")

## Test sélection des catégories ----
# exemple : 
# - site : 
#   - INK
# - parcelle / position hydrologique relative (voir /Users/Aliz/Documents/Doctorat/Plan de doctorat/onglet TERMINOLOGIE)
#   - position de puits = +14m
# - série temporelle :
#   - before-control before-impact / after-control after-impact / before-after control-impact

# Idées : 
# - utiliser la fonction "uid.to.columns" (AG, 2026) dans fonctions_phd_v3.2 pour séparer le UID en colonnes descriptives des catégories
# - créer un dataframe avec les identifiants années = before ou after

str(tidy.cal.data)
str(tidy.WTD.data)



