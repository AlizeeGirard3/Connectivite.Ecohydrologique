#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                                      Sp. identification
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : May 9th 2025
# Fonction : pour visualiser les données d'élévation à Inkerman
# NOTES : données caduques, ces données d'élévation sont issues de données DSM, alors que je cherchais le DEM
# – A DEM (Digital Elevation Model) Represents the bare-Earth surface, removing all natural and built features;
# – A DSM (Digital Surface Model) captures both the natural and built/artificial features of the environment, as shown below;
# – A DTM (Digital Terrain Model)  typically augments a DEM, by including vector features of the natural terrain, such as rivers and ridges. A DTM may be interpolated to generate a DEM, but not vice versa.
###########################################################################-

#### bibliotheques a charger (installer avant si pas fait)
if (!require("conflicted")) install.packages("conflicted") # Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
# if (!require("reshape2")) install.packages("reshape2") # pour importer Google Sheets directement
# if (!require("plyr")) install.packages("plyr") # pour manipulation donnees
# if (!require("dplyr")) install.packages("dplyr") # pour manipulation donnees
if (!require("ggplot2")) install.packages("ggplot2")
# # install.packages("devtools")
# # devtools::install_github("refunders/refund.shiny")
# if (!require("refund.shiny")) install.packages("refund.shiny") # pour enregistrer des graphiques sous forme de RData (besoin dans ma boucle)
# if (!require("tidyverse")) install.packages("tidyverse") # pour manipulation donnees

# importer et préparer donnees dans R ----
setwd("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD")
source("general.scripts/fonctions.R")
