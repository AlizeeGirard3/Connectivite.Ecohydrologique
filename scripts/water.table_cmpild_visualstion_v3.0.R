#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#             Compiled (replicates) water table data visualisation graphs
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : 2025-12-05-01
# Date mise à jour :
# Pourquoi : pour visualiser les données de nappe phréatique compilées par réplicats en préparation de l'affiche
# des conférences de mai et juin 2026 (Halifax, Nouvelle-Écosse et IPS - Irlande)
# NOTES : 
# Ressources
# https://r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
# https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
# https://finchstudio.io/blog/ggplot-dual-y-axes/
# Lexique :
# A: abrupte (exp.unit, treatment)
# D: douce (exp.unit, treatment)
# C: contrôle (exp.unit, treatment)

###########################################################################-

# ============================================================================= /
# Initialisation ----
# ============================================================================= /
# Librairies
library(conflicted) # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("dplyr")) install.packages("dplyr") # pour manipulation donnees (pipe, etc)
if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("ggpubr")) install.packages("ggpubr") # ggarrange()
if (!require("stringr")) install.packages("stringr") # str_to_title
if (!require("grDevices")) install.packages("grDevices") # pdf()
if (!require("gridExtra")) install.packages("gridExtra") # multiplot()
# if (!require("withr")) install.packages("withr") # T'o Québec icitte (date-time en français)

# Dossier de travail et fonctions
# .rs.restartR()
setwd("~/Documents/Doctorat/_R_Stats_PhD")
source("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/scripts/fonctions_phd_v3.2.R")
source("general.scripts/scripts/fonctions_generales.R")


# ============================================================================= /
# Import fichiers ----
# ============================================================================= /
tidy.WTD.data.df <- readRDS("connectivite/data/clean/tidy.WTD.data.df.RDS")
tidy.cal.data.pre <- readRDS("~/Documents/Doctorat/_R_Stats_PhD/connectivite/data/clean/tidy.cal.data.RDS")
# ele.profiles <- readRDS("connectivite/data/clean/elevation.profiles.RDS")
# obtenu via le script "/scripts/data_water.table.all(v.X).R"


# ============================================================================= /
# Filtrage et préparation ----
# ============================================================================= /
str(tidy.WTD.data.df)
str(tidy.cal.data.pre)

## tidy.WTD.data.df ----
# sélection du site d'intérêt (Inkerman, expérience "Écotone")
tidy.WTD.INK.pre <- tidy.WTD.data.df %>% 
  dplyr::filter(site == "Inkerman", 
                !stringr::str_detect(well.uid, "^INK\\.ch2\\.E"), # enlever les puits hors écotone
                !stringr::str_detect(well.uid, "^INK\\.ch3")) # enlever chapitre 3 (routes)
  # mutate(probe.uid = as.integer(gsub("_.*", "", file.uid)))  # caduque utiliser le file.uid # créer colonne probe.uid
head(tidy.WTD.INK.pre, n = 3)
# table(tidy.WTD.INK.pre$probe.uid)
table(tidy.WTD.INK.pre$well.uid)

## tidy.cal.data ----
# concat.colnames(tidy.cal.data.pre)
tidy.cal.data <- tidy.cal.data.pre %>% 
  select('file.uid', 'lat', 'long', 'measure_status', 'site.uid', 'type', 'relative.distance', 
         'year', 'well.uid', 'trmnt.uid', 'lab.probe.id', 'probe.uid', 'probe.brand') %>% 
  # conserver uniquement les colonnes utiles (autrement chaque métadonnée est répliquée, lignes réplquée pour chaque mesure de bulleur)
  dplyr::filter(!well.uid == "INK.ch2.MareD1_A1.m9,8m.pre") %>% # puits hors design (extra)
  dplyr::distinct()

## grouper ou créer groupes pour les compilation par réplicats (fonctions_phd_v3.2.R) ----
# uid.to.columns(tidy.WTD.INK) # fonction mésadaptée après traitement, ajuster... ()
# -> idée : inclure ces colonnes dans le df tidy...
tidy.WTD.INK <- left_join(tidy.WTD.INK.pre, tidy.cal.data, by = c("well.uid", "file.uid", "probe.brand"))
colnames(tidy.WTD.INK)
table(tidy.WTD.INK$well.uid)
table(tidy.WTD.INK$measure_status)
table(tidy.WTD.INK$type)
# je vais groupper par : exp.unit (ex. MareA) & distance
groupes <- tidy.WTD.INK %>%
  group_by(exp.unit, relative.distance) %>% 
  group_keys()

tidy.WTD.INK %>%
  group_by(type, relative.distance) %>% 
  n_groups()
# 38 groupes

## calcul des stats par groupe ----
tidy.WTD.INK.compld <- tidy.WTD.INK <- tidy.WTD.INK %>%
  group_by(date.time.UTC.0, type, relative.distance, source_calib) %>% 
  mutate(
    mean.WTD = mean(calibrated.value.cm, na.rm = TRUE),
    sd.WTD = sd(calibrated.value.cm, na.rm = TRUE)) %>%
  distinct(date.time.UTC.0, type, relative.distance, source_calib, 
           .keep_all = TRUE) %>% 
  ungroup() %>% 
  separate(type, into = c("exp.unit", "replicate"), sep = -1) # ajouter à la source : fonction uid.to.columns **
colnames(tidy.WTD.INK.compld)


# ============================================================================= /
# Graphique ----
# ============================================================================= /
tidy.WTD.INK.compld %>% dplyr::filter(well.uid == )
graph.example <- tidy.WTD.INK.compld %>% 
  group_by(date.time.UTC.0, type, relative.distance, source_calib) %>% 
  ggplot(aes(y = mean.WTD, x = date.time.tz.orig)) +
  geom_line() +
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%y/%b/%d") + 
  ggtitle(paste0(site.name, ", sonde no ", probe.serial.no.i, " à l'emplacement\n", well.uid, "\n",
                 "nombre de ligne du fichier : ", nrow(ll.cal))) +
  labs(y = "Hauteur de nappe phréatique (cm)\nrelative à la surface", x = "Date") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
print(graph.wt)



# for 1 
## Aperçu des offets - Sondes Odyssey ----
for (j in 1:length(tidy.WTD.data)) {
  print(j)
  # j<-14
  tidy.WTD.data.j <- tidy.WTD.data[[j]]
  
  if (!is.null(tidy.WTD.data.j)) {
    if (grepl("odyssey", tidy.WTD.data.j$metadata[11])) {
      # où trouver no de sonde dans ODYSSEY
      metadata.line <- tidy.WTD.data.j$metadata[12] # probe.uid
      numbers <- gregexpr("[0-9]+", metadata.line)
      sonde <- regmatches(metadata.line, numbers)
    } else if (grepl("hobo", tidy.WTD.data.j$metadata[4])) {
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
 
## Visualisation de la variation de la nappe phréatique et du positionnement du puits le long du transect ----
for (i in 1:length(tidy.WTD.data)) {
  if (!is.null(tidy.WTD.data[[i]])) {
    # i<-5
    paste(i)
    tidy.WTD.data[[i]]
    # bouble pour les ODYSSEY
    if (grepl("odyssey", tidy.WTD.data[[i]]$metadata[11])) { #}
      # extraire no de sonde
      file.uid.i <- gsub(".*: ", "", tidy.WTD.data[[i]]$metadata[10])
      # extraire nom de puits
      cal.data <- read.csv("connectivite/data/raw/level_logger_calibration_all.csv", sep = ";")
      colnames(cal.data)
      well.uid <- cal.data %>% dplyr::filter(file.uid==file.uid.i) %>% distinct(well.uid)
      # extraire nom de transect d'élévation
      trmnt.uid.aaaa <- cal.data %>% 
        dplyr::filter(file.uid==file.uid.i) %>% 
        mutate(trmnt.uid.aaaa = paste0(trmnt.uid, ".", str_extract(file.uid.i, "(?<=_).{4}"))) %>% 
        select(trmnt.uid.aaaa)
      # trmnt.uid <- cal.data %>% 
      #   dplyr::filter(file.uid==file.uid.i) %>% 
      #   select(trmnt.uid)
      
      # extraire numéro de sonde
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
      
      # graphiques de profil d'élévation
      # graph.WTD.ele <- wrap_plots(GRAPH)
      # d'abord créer le graph, puis l'arranger de la mm largeur et mettre une flèche à l'endroit du puits
      # sélectionner les données à afficher
      ele.profiles.sbset <- ele.profiles %>% 
        dplyr::filter("trmnt.uid.aaaa" == trmnt.uid.aaaa)
      GRAPH <- ggplot(ele.profiles, aes(distance.m, elevation.m)) +
        geom_line() +
        ggtitle(paste0("Transect et année :", trmnt.uid.aaaa)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      GRAPH

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

