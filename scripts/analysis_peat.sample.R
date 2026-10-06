#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
#                             Peat sample, cleaning script
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #

# Description -------------------------------------------------------------
########################################################################## -
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2026-10-05
# Date mise à jour : 
# Pourquoi : 
# créer/mettre à jour une liste des UID des échantillons de tourbe (peat.sample_UID)
# analyser les échantillons de tourbe
# ** Données tirées de ** : data_sites_all.R

# Structure :
# —— connectivite
#         |—— archive
#         |—— data
#                     |—— raw (ici feuilles numériques terrain comportant plusieurs onglets pour un site; data_SITE.UID)
#                     |—— extracted_raw <- data_SITE.UID extraits en une base de données par onglet (4), tous site confondus (script "data_sites_all")
#                     |—— clean
#         |—— output
#                     |—— data
#                     |—— figures
#         |—— scripts
# NOTES : 
# 
########################################################################## -

# ============================================================================= /
# Initialisation ----
# ============================================================================= /
# Librairies
if (!require("tidyverse")) install.packages("tidyverse") # gosser avec des suites de caractères, str_replace, [...]
if (!require("readxl")) install.packages("readxl") # lire les excel

# Données, dossier directeur fonctions et à charger directement
# .rs.restartR()
# setwd("/Users/Aliz/Library/CloudStorage/OneDrive-UniversitéLaval/_FIELD.LAB WORK 2025/Laboratoire/LOI")
setwd("~/Documents/Doctorat/_R_Stats_PhD/connectivite")
source("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/scripts/fonctions_phd_v3.2.R")

# Import données (pour tout modification à la source: data_sites_all.R)
read.csv("data/extracted_raw/canopy.peat.fauna.csv", dec = ",", sep = ";")

###  ----

chiffres_extraits <- str_extract_all(canopy.peat.fauna_visualisation$peat.sample_UID_R.randNum, "[0-9]+") |> 
  flatten_chr() # aplatir la liste en un vecteur de texte
liste_chiffres.pre <- as.numeric(chiffres_extraits) 
liste_chiffres <- liste_chiffres.pre[!is.na(liste_chiffres.pre)]




