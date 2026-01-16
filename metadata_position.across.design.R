#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                  Metadata, well position accross experimental design
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2026-01-16
# Date mise à jour : 2026-01-08
# Pourquoi : Pour ajouter les données de positionnement des données (végétation, puits, abiotique, élévation), le long des transects
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
{ 
  # SNH : sonde de niveau hydrostatique / synonymes : LL : level logger; sonde, probe
  # ll : level logger; sonde de niveau hydrostatique / synonymes : sonde, probe, SNH
  # NP : Nappe phréatique / synonymes : water table
  # cal.data, syn. connectivite/data/raw/level_logger_calibration_all.csv
}
##########################################################################-

# fichiers "R data serialized" (RDS) à charger directement
tidy.WTD.data <-readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.RDS")

# .rs.restartR()
source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd_v3.0.R")
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

# Librairies ----
# -> packages dans le code sourcé

# Elevation x Well position along transect ----
# données tidy où ajouter

# j'aurai besoin des données d'élévation

#








