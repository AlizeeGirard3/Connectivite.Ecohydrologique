#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                             Elevation profile graphs
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : 2024-12-09
# Pourquoi : pour visualiser les données de nappe phréatique 
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
###########################################################################-


#### bibliotheques a charger (installer avant si pas fait)
library(conflicted) # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("dplyr")) install.packages("dplyr") # pour manipulation donnees (pipe, etc)
if (!require("ggplot2")) install.packages("ggplot2")

##### importer et préparer donnees dans R
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

load("connectivite/data/clean/ll.clean.RData")
# obtenu via le script "/scripts/data_water.table.all.R"

for (i in 1:length(ll.clean)) {
  paste(i)
  
  
  # CHANTIER ICI
  
  # IL NE COMPREND PAS À QUI JE RÉFÈRE
  
  
  
  # extraire no de sonde
  texte <- ll.clean[[i]]$metadata[4]
  numbers <- gregexpr("[0-9]+", texte)
  result <- regmatches(texte, numbers)
  (probe.serial.no.i <- as.numeric(unlist(result)))

  ll.cal <- as.data.frame(ll.clean[[i]][1]) # $data // ll.cal ce sont les données calibrées finales, reprise du nom dans le script d'origine "data_water.table.all.R"
  class(ll.cal); head(ll.cal); str(ll.cal); colnames(ll.cal)
  
  # graphiques de nappe phréatique
  graph.wt <- ll.cal %>% ggplot(mapping = aes(y = data.calibrated.value.mm, x = data.date.time.UTC.0)) +
    geom_line(group = 1) +
    # ARRANGER
    # ggtitle(paste0("Transect ", transect.i), ) +
    # labs(y = "Élévation (m; Canada, 2024)", x = "Transect N-S") +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  graph.wt
  
  }

# # Check if the file does not exist
# file_to_check <- paste0('output/figures/Elevation_Inkerman_graph',transect[i],'.png')
# if(!file.exists(file_to_check)){ # si c'est PAS VRAI (le file n'existe pas, on poursuit) = VRAI, si c'est VRAI = FAUX (else if -> message d'erreur, empêche d'écraser)
#   ggplot2::ggsave(paste0('output/figures/Elevation_Inkerman_graph',transect[i],'.png'), graph, width = 4.7, height = 2.4)
#   
#   # otherwise print a message
# }else if(file.exists(file_to_check)){
#   
#   stop("The file already exists in the current directory!")
# }
# Choix CONSCIENT d'écraser
# ggplot2::ggsave(paste0('output/figures/Elevation_Inkerman_graph',transect[i],'.png'), graph, width = 4.7, height = 2.4)
