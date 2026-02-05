#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                           Peat physicochemistry analyses
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2025-11-05
# Date mise à jour : 
# Pourquoi : Analyses statistiques de physico-chimie de la tourbe
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
##########################################################################-

# .rs.restartR()
# source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/general.scripts/scripts/fonctions.R")
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")


# Librairies ----
if (!require("tidyverse")) install.packages("tidyverse") # gosser avec des suites de caractères, str_replace, [...]
# if (!require("conflicted")) install.packages("conflicted") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
# if (!require("readxl")) install.packages("readxl") # lire les excel
# if (!require("openxlsx")) install.packages("openxlsx") # lire les excel
# if (!require("stringr")) install.packages("stringr") # gosser avec des suites de caractères, str_replace, [...]



# NETTOYAGE À FARE
# OBTENIR : DATAFRAME AVEC colonnes : site, emplacement.UID, DATA (bulk dens, Von Post, pH, cond.corrigée, LOI, canopée)
# anova one-way : bulk density groupe-site ou pas ? (mixt des routes et écotone, et aussi en les séparant ?)






