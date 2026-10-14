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
#                     |—— extracted_raw (fichiers légèrement modifiés ou joints avec métadonnées ou quadrat_uid)
#                     |—— clean
#         |—— output
#                     |—— data
#                     |—— figures
#         |—— scripts
# NOTES : 
# À FAIRE dans script analysis : anova one-way : bulk density groupe-site ou pas ? (mixt des routes et écotone, et aussi en les séparant ?)
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

# ============================================================================= /
# Mise à jour données récentes, tous data)_SITE.UID ----
# ============================================================================= /
# Non-applicable : faire à partir de script data_sites_all.R pour joindre métadonnées et nouveaux quadrat.uid

# ============================================================================= /
# Nettoyage ----
# ============================================================================= /
## filtre global & uid.to.column ----
peat.sample_join_fltrd <- filter.raw.file(peat.sample_join) # script "fonction_phd_vX" (AG, 2025+)
peat.sample_tidy <- uid.to.columns(file.to.restructure = peat.sample_join_fltrd, type = "cal.data")) # script "fonction_phd_vX" (AG, 2025+)

# ============================================================================= /
# [...] ----
# ============================================================================= /

