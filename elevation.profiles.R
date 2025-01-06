#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                             Elevation profile graphs
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : Oct. 23rd 2024
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
# if (!require("ggplot2")) install.packages("ggplot2") 
# # install.packages("devtools")
# # devtools::install_github("refunders/refund.shiny")
# if (!require("refund.shiny")) install.packages("refund.shiny") # pour enregistrer des graphiques sous forme de RData (besoin dans ma boucle)
# if (!require("tidyverse")) install.packages("tidyverse") # pour manipulation donnees

# importer et préparer donnees dans R ----
setwd("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD")
source("general.scripts/fonctions.R")

# idée : faire reproductible pour tous les sites : 
# JE VEUC MONTER UN GRAPHIQUE D'ÉLÉVATION X = DIST SUR LE TRANSECT, Y = ZIP LEVEL ÉLÉVATION, ET
# AFFICHER CE GRAPHIQUE AVEC UN POINT QUI INDIQUE LA POSITION DU PUITS LE LONG DU TRANSECT, À CÔTÉ DU GRAOHIQUE DE NAPPE PHRÉATIQUE
# ICI NETTOYER (ET CRÉER LE GRAPHIQUE)
# ET SOIT 1. APPELER LES IMAGES À PARTIR DU SCIRPT DE VISUALISAITON (OPTION PRÉFÉRÉE, TROUVER QUEL OBJET PEUT ÊTRE APPELÉ...)
# OU 2. METTRE TOUT CE SCIPT de graphique AVEC celui de water table, et garder ici le script de nettoyage

# idée : pour chaque site, comment m'organiser ? ici boucle pour chaque traitement pour un site, mais insérer ceci dans une
# autre boucle qui lierait chaque fichier contenant de la microtopo

microtopo <- readxl::read_xlsx("connectivite/data/raw/data_STH.xlsx",
                  sheet = "microtopo") #%>% group_by("ID.unique")
metadata.all <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/raw/metadata.all.RDS")

for (i in 1:length(unique(na.omit(microtopo$trmnt.uid)))) {
  print(i)
  trmnt.uid.i <- unique(na.omit(microtopo$trmnt.uid))[i]
  # trouver le site
  trmnt.uid.i.site.uid.pre <- sub("[.].*", "", trmnt.uid.i) # replacer par rien tout ce qui se trouve après le premier point [.]
  trmnt.uid.i.site.uid.pre
  site.name.i <- unique(metadata.all$site[metadata.all$site.uid %in% trmnt.uid.i.site.uid.pre])
# graphiques de nappe phréatique
graph.wt <- microtopo %>% ggplot(mapping = aes(y = elevation.cm , x = distance.m)) + # doit être en as.POSICct, mais avec la date et l'heure. Repartir de zéro dans le script water.tanle_all??
  geom_line(group = 1) +
  # scale_x_datetime(
  #   date_minor_breaks = "day", date_breaks = "2 weeks", date_labels = "%D:%H") +
  ggtitle(paste0(site.name, ", transect ", trmnt.uid.i)) +
  labs(y = "Élévation du terrain (cm)", x = "Distance de la zone perturbée (m)") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
print(graph.wt) # imprimer dans R

# ATTENTION !! surpasser consciemment dans la boucle
# ggsave(paste0("connectivite/output/figures/",site.name, "_", probe.serial.no.i, "_", transect.id.i,".pdf"), graph.wt, width = 12, height = 8)


}



##### ARCHIVE
# importer et préparer donnees dans R ----
# setwd("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite")
# 
# Elevation <- readxl::read_xlsx("data/raw/data_INK.xlsx",
#                   sheet = "cad.Elevation") #%>% group_by("ID.unique")
# ungroup(Elevation)
# transect <- unique(Elevation$ID.unique)
# 
# ##### créer des graphiques d'élévation Nord->Sud
# for (i in 1:(length(transect))) {
#   print(i)
#   transect.i =  transect[i]
#   # liste_graphs[[i]] <- paste0("graph_", i)
#   graph <- Elevation %>% dplyr::filter(ID.unique == transect.i) %>% ggplot(mapping = aes(y = Value, x = `Distance.N-S`)) +
#    # facet_grid(~ ID.unique) +
#   geom_point(shape = ".") +
#   geom_line(group = 1) +
#   ggtitle(paste0("Transect ", transect.i), ) +
#   labs(y = "Élévation (m; Canada, 2024)", x = "Transect N-S") +
#   theme_bw() + theme(plot.title = element_text(hjust = 0.5))
# 
#   # if(paste0('Elevation_Inkerman_graph', transect[i],'.png') %in% list.files("connectivite/output/figures"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
#   #   stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
#   # } else { ggplot2::ggsave(paste0('output/figures/Elevation_Inkerman_graph',transect[i],'.png'), graph, width = 4.7, height = 2.4)  }
# 
# }
# # ggarrange(graph_1,graph_2,graph_3,graph_4,graph_5,graph_6,
# #                   ncol = 3, nrow = 2, common.legend = T)



