#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                Raw (calibrated) water table data visualisation graphs
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : 2024-12-09
# Pourquoi : pour visualiser les données de nappe phréatique 
# NOTES : 
###########################################################################-

setwd("~/Documents/Doctorat/_R.&.Stats_PhD")
source("general.scripts/fonctions.R") # appel du fichier de métadonnées de projet

# Import de données ----
ll.clean <- readRDS("connectivite/data/clean/ll.clean.RDS")
# obtenu via le script "/scripts/data_water.table.all.R"
# importer le graphique que topographie


# Librairies ----
library(conflicted) # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("dplyr")) install.packages("dplyr") # pour manipulation donnees (pipe, etc)
if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("ggpubr")) install.packages("ggpubr") # ggarrange()
if (!require("stringr")) install.packages("stringr") # str_to_title
if (!require("grDevices")) install.packages("grDevices") # pdf()

# graph.wt <- list()
for (i in 1:length(ll.clean)) {
  paste(i)
  
  # extraire no de sonde
  texte <- ll.clean[[i]]$metadata[4]
  numbers <- gregexpr("[0-9]+", texte)
  result <- regmatches(texte, numbers)
  (probe.serial.no.i <- as.numeric(unlist(result)))
  
  # extraire nom de transect/puits
  cal.data <- read.csv("connectivite/data/raw/level.logger.calibration.all.csv", sep = ";")
  colnames(cal.data)
  transect.id.i.pre <- cal.data$well.id[cal.data$probe.serial.no == probe.serial.no.i]
  transect.id.i <- unique(transect.id.i.pre[!is.na(transect.id.i.pre)])
  
  # extraire nom de site
  site.name.pre <- sub("SiteName","",ll.clean[[i]]$metadata[1])
  site.name.pre.1 <- gsub(",", "", site.name.pre) # ici ce serait ST-HENRI, ça me gosse
  site.name <- str_to_title(site.name.pre.1)
  
  # créer objet contenant les données
  ll.cal <- ll.clean[[i]]$data # ll.cal ce sont les données calibrées finales, reprise du nom dans le script d'origine "data_water.table.all.R"
  class(ll.cal); head(ll.cal); str(ll.cal); colnames(ll.cal)
  ll.cal$date.time.tz.orig <- as.POSIXct(ll.cal$date.time.tz.orig, tryFormats = )
  # ici joint avec les info de distance (?)

  # graphiques de nappe phréatique
  graph.wt <- ll.cal %>% ggplot(mapping = aes(y = calibrated.value.mm/10, x = date.time.tz.orig)) + # doit être en as.POSICct, mais avec la date et l'heure. Repartir de zéro dans le script water.tanle_all??
    geom_line(group = 1) +
    scale_x_datetime(
      date_minor_breaks = "day", date_breaks = "2 weeks", date_labels = "%D:%H") +
    ggtitle(paste0(site.name, ", sonde no ", probe.serial.no.i, " à l'emplacement ", transect.id.i)) +
    # geom_point() + # ajouter le point indiquant le puits est à quelle distance et posititon sur le transect d'écotone
    labs(y = "Hauteur de nappe phréatique (cm)", x = "Date") +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
  print(graph.wt) # imprimer dans R
  
  # ajouter le profile topographique avec l'indication de la position relative du puits
  source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/elevation.profiles.R")
  # attention, à date je n'ai que un site, mais si je veux que ça fitte le bon site, je vais devoir
  # spécifier quel site pour quel graph... réfléchir plus tard
  
  # graph.wt + annotation_custom(ggplotGrob(graph.topo), xmin = ll.cal$date.time.tz.orig[1500] , xmax = ll.cal$date.time.tz.orig[3500], 
  #                   ymin = round(min(ll.cal$calibrated.value.mm)/10), ymax = max(ll.cal$calibrated.value.mm)/30)
  
  
  # ATTENTION !! surpasser consciemment dans la boucle
  # ggsave(paste0("connectivite/output/figures/",site.name, "_", probe.serial.no.i, "_", transect.id.i,".pdf"), graph.wt, width = 12, height = 8)
  
  # # si inexistant, imprimer dans le dossier (ou outrepasser de façon consciente)
  # if(paste0(site.name, "_", probe.serial.no.i, "_", transect.id.i,".pdf") %in% list.files("connectivite/output/figures/"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  #   stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
  # } else { ggsave(paste0("connectivite/output/figures",site.name, "_", probe.serial.no.i, "_", transect.id.i,".pdf"), graph.wt, width = 12, height = 8)  }
  
  
  
  

  # CHANTIER
  
  # ÇA GOSSE ÇA ARRÊTE TOUTE LA BOUCLE !!!! j'ai essayé de mettre ça dans une liste, mais après ça bugait... je ne sais pas comment sortir ça de là donc.
  # plusieurs n'affichent rien, pourquoi ?
  # print dans RMarkdown (?), cela serait généré dans tout un seul pdf
  # ou à partir du terminal (je peux normalement passer de R studio au temrinal, mais puis-je le faire à paarti du même script ? sinon source()??)
  # https://apple.stackexchange.com/questions/230437/how-can-i-combine-multiple-pdfs-using-the-command-line
  
}
# Afficher tous les graphiques

# pdf("filename.pdf", width = 8, height = 12) # Open a new pdf file
# n <- length(graph.wt)
# nCol <- floor(sqrt(n))
# do.call("grid.arrange", c(graph.wt, ncol=nCol))
# dev.off() # Close the file


# if(paste0('Elevation_Inkerman_graph', transect[i],'.png') %in% list.files("connectivite/output/figures"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)


#   stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
# } else { ggplot2::ggsave(paste0('output/figures/Elevation_Inkerman_graph',transect[i],'.png'), graph, width = 4.7, height = 2.4)  }



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

