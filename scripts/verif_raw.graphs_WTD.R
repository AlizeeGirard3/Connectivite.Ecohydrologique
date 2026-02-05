#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                           Raw visualisation of WTD data
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2026-01-07
# Date mise à jour : 
# Pourquoi : Visualisation des données de nappe non-traitées (raw), suivi des problèmes et décisions
# NOTES : Lorsque résolu, je les place en bas du script

# LEXIQUE :
{ 
  # SNH : sonde de niveau hydrostatique / synonymes : LL : level logger; sonde, probe
  # NP : Nappe phréatique / synonymes : water table / WTD
}
##########################################################################-

# Librairies ----
if (!require("tidyverse")) install.packages("tidyverse") # méta package // gosser avec des suites de caractères, str_replace, [...]
if (!require("ggplot2")) install.packages("ggplot2")

# Fichiers à charger directement ----
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")
# tidy.cal.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.cal.data.RDS") # issu du code "data_water.table_all_v3.0"
tidy.WTD.data.df <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.df.RDS") # issu du code "data_water.table_all_v3.0"
# tidy.WTD.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.RDS") # issu du code "data_water.table_all_v3.0"
extracted.list_data <- lapply(tidy.WTD.data, `[[`, 4)
tidy.WTD.data.df.9janv <- do.call(rbind, extracted.list_data)


## Sondes avec trop de glitch, à lisser ----
# explications potentielles pour les glitch aléatoires : lorsque la nappe tombe en dessous de mon éventail de détection (sous la sonde)


## Deuxième signal à pas temporel constant (?)
# ne correspondant pas à la diminution (ÉT, outflow)-élévation de nappe (pluie) données bruttes avec 1 seul glitch à supprimer rapidement ----
# 41359_20241125
odyssey.data <- tidy.cal.data%>% dplyr::filter(cal.no == "3") %>% 
  dplyr::filter(file.uid == "41359_20241125")
# ce signal semble revenir aux X PAS DE TEMPS

#### 22195241_20251202 & ----
#### 22195241_20250721 ----
# aussi, voir verif_blowing.pipe -> à PRO, la différence des bulleur était de ~13.5 et 21 cm 



# ============================================================================= /
#  ZONE DES PROBLÈMES RÉSOLU ----
# ============================================================================= /

## Un seul glitch à supprimer rapidement ----
# (probablement causés à retrait de sonde lors de la mesure de bulleur)
# voir ancienne versions des graphiques dans les archives
### 22224392_20251210 ----
data.to.sort <- tidy.WTD.data.df %>% 
  dplyr::filter(file.uid =="22224392_20251210") %>% 
  arrange(desc(calibrated.value.cm))
# à 2025-05-29T10:00:01Z mesure raw = 101.073 alors que les données avant et après sont à 111.953 + 111.897 (fiable)
(111.953+111.897)/2 # je prends la moyenne et l'entre directement dans les données bruttes (.csv)
rm(data.to.sort)

### 41359_20251203 ----
# voir ancienne versions des graphiques dans les archives
data.to.sort <- tidy.WTD.data.df %>% 
  dplyr::filter(file.uid =="41359_20251203") %>% 
  arrange(desc(calibrated.value.cm))
# à 2025-10-16T15:00:01Z mesure raw = -94.43868 alors que les données avant et après sont à (voir suivant, fiable)
(-50.34814-50.25113)/2 # je prends la moyenne et l'entre directement dans les données bruttes (.csv)

