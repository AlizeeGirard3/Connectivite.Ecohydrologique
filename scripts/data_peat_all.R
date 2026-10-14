#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
#                             Peat sample, cleaning script
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #

# Description -------------------------------------------------------------
########################################################################## -
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2026-10-13
# Date mise à jour :
# Pourquoi : 
# peat_data -> tidy
# ** Données tirées de peat.csv (modifications dans /Users/Aliz/Library/CloudStorage/OneDrive-UniversitéLaval/_FIELD_LAB.WORK/_Feuilles_données_pas_local/aaaammdd_peat.csv)

# Structure :
# —— connectivite
#         |—— archive
#         |—— data
#                     |—— raw (ici feuilles numériques terrain comportant plusieurs onglets pour un site; data_SITE.UID)
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
peat.sample_join <- readRDS("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/peat.sample_join.RDS")


### REPRENDRE ----

chiffres_extraits <- str_extract_all(canopy.peat.fauna_visualisation$peat.sample_UID_R.randNum, "[0-9]+") |> 
  flatten_chr() # aplatir la liste en un vecteur de texte
liste_chiffres.pre <- as.numeric(chiffres_extraits) 
liste_chiffres <- liste_chiffres.pre[!is.na(liste_chiffres.pre)]

# NETTOYAGE À FARE
# OBTENIR : DATAFRAME AVEC colonnes : site, emplacement.UID, DATA (bulk dens, Von Post, pH, cond.corrigée, LOI, canopée)
# anova one-way : bulk density groupe-site ou pas ? (mixt des routes et écotone, et aussi en les séparant ?)





