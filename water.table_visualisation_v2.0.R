#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                Raw (calibrated) water table data visualisation graphs
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : 2024-12-09
# Date mise à jour : 2026-01-12
# Pourquoi : pour visualiser les données de nappe phréatique 
# NOTES : 
# Ressources
# https://r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
# https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
###########################################################################-

setwd("~/Documents/Doctorat/_R.&.Stats_PhD")
source("general.scripts/scripts/fonctions_generales.R") # appel du fichier de métadonnées de projet

# Import de données ----
tidy.WTD.data <- readRDS("connectivite/data/clean/tidy.WTD.data.RDS")
# obtenu via le script "/scripts/data_water.table.all(v.X).R"
# importer le graphique que topographie

# Librairies ----
library(conflicted) # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("dplyr")) install.packages("dplyr") # pour manipulation donnees (pipe, etc)
if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("ggpubr")) install.packages("ggpubr") # ggarrange()
if (!require("stringr")) install.packages("stringr") # str_to_title
if (!require("grDevices")) install.packages("grDevices") # pdf()
if (!require("gridExtra")) install.packages("gridExtra") # multiplot()
# if (!require("withr")) install.packages("withr") # T'o Québec icitte (date-time en français)

# Dossier de travail
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

# Import du fichier de données récent
tidy.WTD.data <- readRDS("connectivite/data/clean/tidy.WTD.data.RDS")

### Aperçu des offets - Sondes Odyssey
for (j in 1:length(tidy.WTD.data)) {
  print(j)
  # j<-14
  tidy.WTD.data.j <- tidy.WTD.data[[j]]
  
  # ODYSSEY
  if (grepl("odyssey", tidy.WTD.data.j$metadata[11])) {
    # où trouver no de sonde dans ODYSSEY
    metadata.line <- tidy.WTD.data.j$metadata[12] # probe.uid
    numbers <- gregexpr("[0-9]+", metadata.line)
    sonde <- regmatches(metadata.line, numbers)
  } 
  else if (grepl("hobo", tidy.WTD.data.j$metadata[4])) {
    # où trouver no de sonde dans HOBO
    metadata.line <- tidy.WTD.data.j$metadata[5] # probe.uid
    numbers <- gregexpr("[0-9]+", metadata.line)
    sonde <- regmatches(metadata.line, numbers)
  }
  # données à visualiser
  data <- tidy.WTD.data[[j]]$data
  if (length(data) > 0) {
    hist(data$calibrated.value.cm, warn.unused = F, 
         main = paste("Histograme des données de sonde no ", paste(sonde,"\n"))) # en cm
  } 
}

vérif.1 <- tidy.cal.data%>% dplyr::filter(cal.no == "3")
vérif.1 = vérif.1[-which(is.na(vérif.1$offset_cm)),] # règle l'avertissement d'avoir retiré 22 lignes contenant des
vérif.1$probe.uid <- as.character(vérif.1$probe.uid)

ggplot(vérif.1, aes(x = probe.uid, y = offset_cm)) +
  scale_y_continuous(breaks = seq(-160, 160, by = 20)) +
  geom_segment(aes(x=probe.uid, xend=probe.uid, y=0, yend=offset_cm)) +
  geom_point(size=1, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Offsets des sondes Odyssey,\npar identifiant unique de sonde\n(années confondues)")

# coord_cartesian() pour zoomer sur une fenetre dans le graph, sans enlever les données **
# theme(xmin, xmax, ymin, ymax)
 
## Visualisation de la variation de la nappe phréatique et du positionnement du puits le long du transect
for (i in 1:length(tidy.WTD.data)) {
  if (!is.null(tidy.WTD.data[[i]])) {
    # i<-5
    paste(i)
    tidy.WTD.data[[i]]
    # bouble pour les ODYSSEY
    if (grepl("odyssey", tidy.WTD.data[[i]]$metadata[11])) { #}
      # extraire no de sonde
      file.uid.i <- gsub(".*: ", "", tidy.WTD.data[[i]]$metadata[10])
      # extraire nom de transect/puits
      cal.data <- read.csv("connectivite/data/raw/level_logger_calibration_all.csv", sep = ";")
      colnames(cal.data)
      well.uid <- cal.data %>% dplyr::filter(file.uid==file.uid.i) %>% distinct(well.uid)
      
      texte <- tidy.WTD.data[[i]]$metadata[4]
      numbers <- gregexpr("[0-9]+", texte)
      result <- regmatches(texte, numbers)
      (probe.serial.no.i <- as.numeric(unlist(result)[1]))
      
      # extraire nom de site
      site.name.pre <- sub("SiteName","",tidy.WTD.data[[i]]$metadata[1])
      site.name.pre.1 <- gsub(",", "", site.name.pre) # ici ce serait ST-HENRI, ça me gosse
      site.name <- str_to_title(site.name.pre.1)
      
      # créer objet contenant les données
      ll.cal <- tidy.WTD.data[[i]]$data # ll.cal ce sont les données calibrées finales, reprise du nom dans le script d'origine "data_water.table.all.R"
      # class(ll.cal); head(ll.cal); str(ll.cal); colnames(ll.cal)
      ll.cal$date.time.tz.orig <- as.POSIXct(ll.cal$date.time.tz.orig, tryFormats = )
      
      # graphiques de nappe phréatique
      graph.wt <- ll.cal %>% ggplot(mapping = aes(y = calibrated.value.cm, x = date.time.tz.orig)) + # ici HOBO je dois faire *-1 pour avoir la hauteur relative (nég si en dessous de surface)
        geom_line(group = 1) +
        scale_x_datetime(date_breaks = "2 weeks", date_labels = "%y/%b/%d") + 
        ggtitle(paste0(site.name, ", sonde no ", probe.serial.no.i, " à l'emplacement\n", well.uid, "\n",
                       "nombre de ligne du fichier : ", nrow(ll.cal))) +
        labs(y = "Hauteur de nappe phréatique (cm)\nrelative à la surface", x = "Date") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
      print(graph.wt) # imprimer dans R
      
    }  
    if (grepl("hobo", tidy.WTD.data[[i]]$metadata[4])) { #}
      # extraire no de sonde
      file.uid.i <- gsub(".*: ", "", tidy.WTD.data[[i]]$metadata[3])
      # extraire nom de transect/puits
      cal.data <- read.csv("connectivite/data/raw/level_logger_calibration_all.csv", sep = ";")
      colnames(cal.data)
      well.uid <- cal.data %>% dplyr::filter(file.uid==file.uid.i) %>% distinct(well.uid)
      
      texte <- tidy.WTD.data[[i]]$metadata[5]
      numbers <- gregexpr("[0-9]+", texte)
      result <- regmatches(texte, numbers)
      (probe.serial.no.i <- as.numeric(unlist(result)[1]))
      
      # extraire nom de site
      site.name <- gsub(".*: ", "", tidy.WTD.data[[i]]$metadata[1])
      
      # créer objet contenant les données
      ll.cal <- tidy.WTD.data[[i]]$data # ll.cal ce sont les données calibrées finales, reprise du nom dans le script d'origine "data_water.table.all.R"
      # class(ll.cal); head(ll.cal); str(ll.cal); colnames(ll.cal)
      ll.cal$date.time.tz.orig <- as.POSIXct(ll.cal$date.time.tz.orig, tryFormats = )
      
      graph.wt <- ll.cal %>% ggplot(mapping = aes(y = calibrated.value.cm*-1, x = date.time.tz.orig)) + # ici HOBO je dois faire *-1 pour avoir la hauteur relative (nég si en dessous de surface)
        geom_line(group = 1) +
        scale_x_datetime(date_breaks = "2 weeks", date_labels = "%y/%b/%d") + 
        ggtitle(paste0(site.name, ", sonde no ", probe.serial.no.i, " à l'emplacement\n", well.uid, "\n",
                       "nombre de ligne du fichier : ", nrow(ll.cal))) +
        labs(y = "Hauteur de nappe phréatique (cm)\nrelative à la surface", x = "Date") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
      print(graph.wt) # imprimer dans R
      
      # ATTENTION !! surpasser consciemment dans la boucle
      # ggsave(paste0("connectivite/output/figures/",site.name, "_", probe.serial.no.i, "_", transect.id.i,".pdf"), graph.wt, width = 12, height = 8)
    }
  } 
}



# ============================================================================= /
#  CHANTIER ----
# ============================================================================= /


# ÇA GOSSE ÇA ARRÊTE TOUTE LA BOUCLE !!!! j'ai essayé de mettre ça dans une liste, mais après ça bugait... je ne sais pas comment sortir ça de là donc.
# plusieurs n'affichent rien, pourquoi ?
# print dans RMarkdown (?), cela serait généré dans tout un seul pdf
# ou à partir du terminal (je peux normalement passer de R studio au temrinal, mais puis-je le faire à paarti du même script ? sinon source()??)
# https://apple.stackexchange.com/questions/230437/how-can-i-combine-multiple-pdfs-using-the-command-line

# }
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

