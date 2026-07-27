#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                             Elevation profile graphs
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : Oct. 23rd 2024
# Date mise à jour : 
# - 6 mai 2026 (code : profile pas mare INK (gentle et controle) pour afficher)
# - 27 juillet 2026 : ajout sélection des Lat, Long UNIQUES pour insertion dans QGIS
# Fonction : pour visualiser les données d'élévation à Inkerman
# NOTES : données caduques, ces données d'élévation sont issues de données DSM, alors que je cherchais le DEM
# – A DEM (Digital Elevation Model) Represents the bare-Earth surface, removing all natural and built features;
# – A DSM (Digital Surface Model) captures both the natural and built/artificial features of the environment, as shown below;
# – A DTM (Digital Terrain Model)  typically augments a DEM, by including vector features of the natural terrain, such as rivers and ridges. A DTM may be interpolated to generate a DEM, but not vice versa.

# Notes : 
# idée : faire reproductible pour tous les sites : 
# JE VEUX MONTER UN GRAPHIQUE D'ÉLÉVATION X = DIST SUR LE TRANSECT, Y = ZIP LEVEL ÉLÉVATION, ET
# AFFICHER CE GRAPHIQUE AVEC UN POINT QUI INDIQUE LA POSITION DU PUITS LE LONG DU TRANSECT, À CÔTÉ DU GRAPHIQUE DE NAPPE PHRÉATIQUE
# ICI NETTOYER (ET CRÉER LE GRAPHIQUE)
# ET SOIT 1. APPELER LES IMAGES À PARTIR DU SCIRPT DE VISUALISAITON (OPTION PRÉFÉRÉE, TROUVER QUEL OBJET PEUT ÊTRE APPELÉ...)
# OU 2. METTRE TOUT CE SCIPT de graphique AVEC celui de water table, et garder ici le script de nettoyage
# idée : pour chaque site, comment m'organiser ? ici boucle pour chaque traitement pour un site, mais insérer ceci dans une
# autre boucle qui lierait chaque fichier contenant de la microtopo
  
###########################################################################-

# ============================================================================= /
# Initialisation ----
# ============================================================================= /
#  Libraries
if (!require("conflicted")) install.packages("conflicted") # Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("patchwork")) install.packages("patchwork")
if (!require("lubridate")) install.packages("lubridate")
# autres packages dans le script sourcé

# Données, dossier directeur fonctions et à charger directement
setwd("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD")
source("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/scripts/fonctions_phd_v3.2.R") 
# ici est créé le : connectivite/data/clean/elevation.profiles.RDS" **

# ============================================================================= /
# Lecture, nettoyage, visualisation des fichier bruts ----
# ============================================================================= /
# REEXTRAIRE DONNÉES ** -> CHANGÉ NOMS DES COLONNES
ele.profiles.raw <- readxl::read_xlsx("connectivite/data/extracted_raw/microtopo.xlsx")

# Nettoyage des données
ele.profiles <- filter.raw.file(ele.profiles.raw) # script "fonctions_phd_v3.0.R"
ele.profiles$distance.m <- round(as.numeric(ele.profiles$distance.m), digits = 2)
ele.profiles$elevation.cm <- round(as.numeric(ele.profiles$elevation.cm), digits = 2)
ele.profiles <- ele.profiles %>% 
  mutate(elevation.m = elevation.cm/100)
  
# dates
ele.profiles$`date.aaaa.mm.dd` <- ymd(ele.profiles$`date.aaaa.mm.dd`)

# ============================================================================= /
# Extraction unique des Lat, Long pour insertion dans QGIS ----
# ============================================================================= /
ele.profiles$lat.long <- paste0(ele.profiles$perm.plot.uid.NOmicrotopo.aaaa, ";", ele.profiles$lat.garmin.dd, ";", ele.profiles$long.garmin.dd, ";", ele.profiles$lat.long.location)

lat.long.column <- data.frame(unique(ele.profiles$lat.long))
colnames(lat.long.column)[1] <- "UID;lat;long;location"
write_csv(lat.long.column, file = "/Users/Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/_microtopo_lat.long.csv")

# ============================================================================= /
# Visualisation des fichier bruts ----
# ============================================================================= /
## Graphique avec purrrr (tidyverse) ----
list <- split(ele.profiles, ele.profiles$trmnt.uid.aaaa) # équivalent à toute la boucle sous "graph.topo.list <- list()"
chaque.graph <- map(list, ~ ggplot(.x, aes(distance.m, elevation.m)) + 
                        geom_line() +
                        ggtitle(unique(.$trmnt.uid.aaaa)) +
                        theme_bw() + 
                        theme(plot.title = element_text(hjust = 0.5)))

# tous graphiques arrangés automatiquement (merci aux nouvelles fonctions apparues depuis mon M.Sc. <3 !!)
mes_graphiques <- wrap_plots(chaque.graph, 
                             nrow = length(list),
                             widths = 2, 
                             heights = 10)
# merci à l'IA qui m'a aidée... débuts avec purrr / prompt : "faire un graphique par objet dans une liste avec map() et R"
# ggsave(mes_graphiques, 
#        filename = "connectivite/output/figures/mes_graphiques_elevation_20260116.jpeg",
#        device = "jpeg", height = 120, width = 30, units = "cm")
# arranger affichage de l'élévation, etc !
# arriver à naviguer pour extraire le bon graph avec le mon level logger et afficher au dessus ou en background (?)


# idée : fonction où les arguments sont les identifiants de traitements où je veux confondre les points de repère spatiaux

# ============================================================================= /
#  Examination des données brutes et nettoyage ----
# ============================================================================= /
# transect (trmnt.uid.aaaa) en cours de résolution (résolus plus loin ci-dessous)

## STH ----
#### STH.D2.2025.2 vs STH.D2.2025.1 ----
# Pente retravaillée. Est-ce que j'élimine les données avant ou si elles risquent d'avoir affecté la dynaimque de nappe en pente ?
# A priori, le retravail (peaufinage suite au reprofilage) de pente visait une meilleure répartition de l'eau pour la végétation 
# plutôt que la dynamique d'écoulement. Vérifier si très différentes, dans quel cas ne pas les joindre. Si assez similaire, en faire une moyenne.
trmnt.to.compare <- c("STH.ch2.D2.2025.2", "STH.ch2.D2.2025.1")
ele.profiles.subset <- ele.profiles[ele.profiles$trmnt.uid.aaaa %in% trmnt.to.compare,]
# rejoindre les points de repère (*le zéro du transect n'est pas à la même place*)
# puits (m1m) à 0,67 m vs à 1,5 m -> ajouter 0.83 partout au transect STH.D2.2.2025
ele.profiles.subset <- ele.profiles.subset %>% 
  mutate(distance.m = ifelse(trmnt.uid.aaaa == "STH.ch2.D2.2025.2", distance.m + 0.83, distance.m))
GRAPH <- ggplot(ele.profiles.subset) +
  geom_line(aes(distance.m, elevation.m,
                  group = trmnt.uid.aaaa, 
                  color = trmnt.uid.aaaa)) +
  # geom_smooth(aes(distance.m, elevation.m, # tentatives de smoothing...
  #               group = trmnt.uid.aaaa, 
  #               color = trmnt.uid.aaaa), se = F) +
  # stat_smooth(aes(y = elevation.m, x = distance.m), method = lm, formula = y ~ poly(x, 19), se = F, fullrange = F)+
  # ggtitle(paste0("Transect et année :", trmnt.to.compare)) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2))
GRAPH

## transect (trmnt.uid.aaaa) résolus ---- 


## INK ----
### INK.ch3.sprd.2025.2, INK.ch3.sprd.2025.1 et INK.ch3.rmvd ----
trmnt.to.compare.2 <- c("INK.ch3.sprd.2025.2", "INK.ch3.sprd.2025.1", "INK.ch3.rmvd.2025")
ele.profiles.subset.2 <- ele.profiles[ele.profiles$trmnt.uid.aaaa %in% trmnt.to.compare.2,]
unique(na.omit(ele.profiles.subset.2$trmnt.uid.aaaa))
GRAPH <- ggplot(ele.profiles.subset.2) +
  geom_line(aes(distance.m, elevation.m,
                group = trmnt.uid.aaaa, 
                color = trmnt.uid.aaaa)) +
  ggtitle(paste0("Transect et année :", trmnt.to.compare.2[1],"\n",
                 trmnt.to.compare.2[2], ", ", trmnt.to.compare.2[3])) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2))
GRAPH

## transect (trmnt.uid.aaaa) résolus ---- 

# ============================================================================= /
# uid.to.columns (scripts/fonctions_phd_v3.2.R) ----
# ============================================================================= /
uid.to.columns(ele.profiles, type = "other")

# ============================================================================= /
#  Enregistrement des données propres ----
# ============================================================================= /
# Format RDS des ele.profiles nettoyé ----
if("elevation.profiles.RDS" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { saveRDS(ele.profiles, file = "connectivite/data/clean/elevation.profiles.RDS") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }

