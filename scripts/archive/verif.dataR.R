#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                           Bulleur, tableau de vérification
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2026-01-07
# Date mise à jour : 
# Pourquoi : Vérifications : quelle erreur moyenne pour les bulleurs ? Seulement pour les Hobo, pour les Odyssey on utilise la valeur pour calibrer
# Idée : avec l'erreur moyenne de bulleur, ajuster ou donner une fourchette d'erreur pour les Odyssey...
# NOTES : 

# LEXIQUE :
{ 
  # SNH : sonde de niveau hydrostatique / synonymes : LL : level logger; sonde, probe
  # NP : Nappe phréatique / synonymes : water table / WTD
  # tz : time zone, syn. fuseau horaire
}
##########################################################################-

# Librairies ----
if (!require("openxlsx")) install.packages("openxlsx") # lire les excel
if (!require("tidyverse")) install.packages("tidyverse") # méta package // gosser avec des suites de caractères, str_replace, [...]

# Fichiers à charger directement ----
# cal.data <- read.xlsx("connectivite/data/clean/cal.data.xlsx", sep = ";")
tidy.cal.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.cal.data.RDS") # issu du code "data_water.table_all_v3.0"
tidy.WTD.data.df <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.df.RDS") # issu du code "data_water.table_all_v3.0"
# tidy.WTD.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.RDS") # issu du code "data_water.table_all_v3.0"

# .rs.restartR()
# source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd_v2.0.R")
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

# retrait de colonnes inutiles de tidy.cal.cata
tidy.cal.data <- tidy.cal.data %>% 
  select(!c(27:39)) %>% distinct() # enlever les données temporaires associées à la calibration des sondes Odyssey

# fichiers de consigne de données
water.table.verif <- data.frame()

# extraction des métadonnées
# for (m in 1:length(tidy.WTD.data)) {
# for (m in 1:length(tidy.WTD.data)) {
# if (nrow(tidy.WTD.data[[m]]$data) != 0) { # si le fichier SNH n'est pas vide
for (tidy.cal.data.line in 1:nrow(tidy.cal.data)) {
  print(tidy.cal.data.line)
  
  # extraction données de tidy.cal.data pour la ligne "tidy.cal.data.line"
  tidy.cal.data.line.df <- tidy.cal.data[tidy.cal.data.line,] # filtrer ll.bulleur (level_logger_calibration_all.csv) par le ligne "n" (vérification n au bulleur)
  
  # extraction données de tidy.WTD.data pour la ligne "tidy.cal.data.line"
  probe.uid <- tidy.cal.data.line.df$probe.uid
  date.line <- tidy.cal.data.line.df$file.uid
  date.extraction <- sub(".*_", "", date.line)

  # filtrer tidy.WTD.data.df (toutes données) par le file_uid & le moment de la mesure de bulleur
  tidy.WTD.data.match.cal.line <- tidy.WTD.data.df[tidy.WTD.data.df$file.uid == date.line & tidy.WTD.data.df$date.time.UTC.0 == tidy.cal.data.line.df$in.bulleur.date.time.UTC.0, ]
  
  water.table.verif[tidy.cal.data.line, 1:4] <- tibble("probe.uid" = probe.uid, # créer le dataframe de vérification pour les lignes "n" de la SNH "m"
                                                       "file.extraction.date" = date.extraction,
                                                       "probe.measure.cm" = tidy.WTD.data.match.cal.line$calibrated.value.cm,
                                                       "bulleur.mesure.cm" = tidy.cal.data.line.df$in.bulleur.rel.to.surface.mm/10)
}
# autres colonnes : quelconque stat -> erreur-type...


# FINIR EN VÉRIFIANT QUE CHAQUE BULLEUR À DES DONNÉES DE SONDE DAN CAL DATA -> REROULER -> RÉENREGISTRER
# puis travailler sur ci-dessous si jamais aucune données   
# 
# else if (nrow(tidy.WTD.data[[m]]$data) == 0)  {
#   water.table.verif[nrow(water.table.verif) + 1, 1:4] <- data.frame("probe.uid" = sonde.m, # si ll.clean[[j]]$data est vide, mettre NA dans le dataframe
#                                                                     "file.extraction.date" = date.m,
#                                                                     "probe.measure.cm" = NA,
#                                                                     "bulleur.mesure.cm" = NA)
# } 

# À FAIRE 
# vérifier comment on présente typiquement ces données
# -> à quoi servent-elles ? suite avec Laurence





