#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                             Elevation profile graphs
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : 6 mai 2026
# Date mise à jour : 
# Fonction : pour visualiser les données d'élévation issues du ZIP level, à Inkerman
# Notes : 
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
ele.profiles <- readRDS("connectivite/data/clean/elevation.profiles.RDS") # ** scripts/data_elevation.profiles.R = créé le connectivite/data/clean/elevation.profiles.RDS" **

# ============================================================================= /
# Préparation des données ----
# ============================================================================= /
unique(ele.profiles$trmnt.uid)
# uid.to.columns(ele.profiles, type = "other") # arranger ***

# créer les identifiants de groupement
ele.profiles.INK.GvsC <- ele.profiles %>%
  dplyr::filter(site.uid == "INK", 
                !stringr::str_detect(trmnt.uid, "^INK\\.ch2\\.E"), # enlever les transects hors écotone
                !stringr::str_detect(trmnt.uid, "^INK\\.ch2\\..*MareA"), # enlever les transects hors écotone
                !stringr::str_detect(trmnt.uid.aaaa, ".pre"), # enlever les transects "pre"
                !stringr::str_detect(trmnt.uid, "^INK\\.ch3"), # enlever chapitre 3 (routes)
                trmnt.uid != "INK.ch2.MareC1") %>% 
  # sélectionner + faire une moyenne + sd de la pente des deux réplicats
    separate(trmnt.uid, into = c("exp.unit_trmnt", "replicate"), sep = -1, remove = FALSE) %>% # ajouter à la source : fonction uid.to.columns **
    separate(exp.unit_trmnt, into = c("trmnt", "slope"), sep = -1, remove = FALSE) # %>%  # ajouter à la source : fonction uid.to.columns **

# avant de pouvoir calculer les profils de pente par réplicat, je dois
# 1. transloquer par rapport au canal (=0)
# 2. interpoler et lisser la courbe
# 3. calculer la moyenne et sd des profils... avec ce code
# en vue des congrès mai et juin 2026, je prends un seul des deux profils ou les deux superposés 
# # je vais groupper par : traitement (pasMare D1 et D2 ensemble) et distance (ex. MareA -> moyenne des réplicats) & distance
# ele.profiles.INK.GvsC %>%
#   group_by(exp.unit_trmnt) %>% 
#   group_keys()
# 
# ele.profiles.INK.GvsC.compild <- ele.profiles.INK.GvsC %>% 
#     group_by(exp.unit_trmnt, distance.m) %>%
#     # moyenne + sd à chaque heure
#     mutate(
#       mean.elev = mean(elevation.m, na.rm = TRUE),
#       sd.elev = sd(elevation.m, na.rm = TRUE),
#       ymin_elev = mean.elev - sd.elev,
#       ymax_elev = mean.elev + sd.elev) %>%
#     ungroup() 

# ============================================================================= /
# INK gentle vs Control pour affiches 2026 ----
# ============================================================================= /
## séquence de couleurs ----
pal_sequence <- c("#1b019b", "#FF6B6B", "#FFB04FFF", "#679C35FF", "rgba(0,0,0,0)", "#6A359CFF","#CD1076FF")
# "#6497B1FF", "#6A359CFF", "#FFB04FFF", "#679C35FF", "#CD1076FF" # autres idées

### graphique en plotly ----
pasMareDvsC.p14.30m.elevation.plotly <- plot_ly(
  height = 400, 
  width = 700) %>%
  # add_ribbons(
  #   data = ele.profiles.INK.GvsC,
  #   x = ~ distance.m,
  #   ymin = ~ ymin_elev,
  #   ymax = ~ ymax_elev,
  #   # color = ~ exp.unit_trmnt,
  #   # colors = pal_sequence[c(3:4, 6:7)],
  #   opacity = 0.2,
  #   # inherit = FALSE,
  #   legendgroup = ~ exp.unit_trmnt,
  #   showlegend = FALSE) %>%
  add_lines( # axe Y principal
    data = ele.profiles.INK.GvsC,
    x = ~ distance.m,
    y = ~ elevation.m,
    color = ~ trmnt.uid,
    line = list(width = 1.5), 
    inherit = FALSE,
    legendgroup = ~ trmnt.uid) %>%
  plotly::layout(
    title = list(
      text = "Elevation profiles along the gentle slope and control transects and position of wells.",
      font = list(size = 12)),
    margin = list(r = 60, l = 60, b = 0, t = 37), # marges; hauteur globale
    plot_bgcolor = pal_sequence[5],
    paper_bgcolor = pal_sequence[5],
    xaxis = list(
      title = "Distance (m)",
      showgrid = TRUE,
      autosize = TRUE,
      gridcolor = "#f0f0f0",
      linecolor = "black",
      mirror = TRUE,
      showline = TRUE),
    yaxis = list(
      title = "Elevation (m)",
      showgrid = TRUE,
      gridcolor = "#f0f0f0",
      showline = TRUE,
      linecolor = "black",
      linewidth = 1,
      mirror = TRUE,
      rangemode = "tozero"),
    legend = list(
      orientation = "h",
      x = 0.5, 
      xanchor = "center",  
      y = -0.25, 
      yanchor = "top"))
pasMareDvsC.p14.30m.elevation.plotly

  