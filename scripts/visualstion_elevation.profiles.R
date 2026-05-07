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
if (!require("plotly")) install.packages("plotly")
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
  separate(exp.unit_trmnt, into = c("trmnt", "slope"), sep = -1, remove = FALSE) %>%  # ajouter à la source : fonction uid.to.columns **
  mutate(trmnt.uid = fct_recode(factor(trmnt.uid),
                                "Gentle slope (n = 1)" = "INK.ch2.pasMareD1",
                                "Control treatment (n = 1)" = "INK.ch2.pasMareC2"))
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
## séquence de couleurs et lignes ----
pal_sequence <- c("#1b019b", "#FF6B6B", "#98FB98", "#548B54", "rgba(0,0,0,0)", "#DB7093","#8B475D", "#EE799F",       "#7CCD7C")
                 # pluie     # temp.  # contrl 14  # contrl 30 # transparent  # gentl 14 # gentl 30  # elev. Gentle # elev. contrl
# https://r-charts.com/colors/, palegreen et palevioletred

### graphique en plotly ----
pasMareDvsC.p14.30m.elevation.plotly <- plot_ly(
  height = 250, 
  width = 700) %>%
  add_lines(
    data = ele.profiles.INK.GvsC,
    x = ~ distance.m,
    y = ~ elevation.m,
    color = ~ trmnt.uid,
    colors = pal_sequence[c(9, 8)],
    line = list(width = 2),
    # linetype = ~ trmnt.uid,
    # linetypes = mes_styles,
    legendgroup = ~ trmnt.uid) %>% 
  plotly::layout(
    title = list(
      text = "Elevation profiles along the gentle slope and control transects and position of wells.",
      font = list(size = 13),
      y = 0.98),
    # Remplacer b = 60 par b = 10 ou 20 pour couper le vide sous la légende
    margin = list(r = 10, l = 55, b = 80, t = 20, pad = 0), 
    plot_bgcolor = pal_sequence[5],
    paper_bgcolor = pal_sequence[5],
    xaxis = list(
      title = list(
        text = "Distance (m)", 
        standoff = 5),
      autosize = FALSE,
      gridcolor = "#f0f0f0",
      linecolor = "black",
      mirror = TRUE,
      showline = TRUE,
      automargin = TRUE),
    yaxis = list(
      title = "Elevation (m)",
      font = list(size = 14), # <-- Taille du titre Y
      tickfont = list(size = 12),    # <-- Taille des chiffres Y
      showgrid = TRUE,
      gridcolor = "#f0f0f0",
      showline = TRUE,
      linecolor = "black",
      linewidth = 1,
      mirror = TRUE,
      rangemode = "tozero"),
    legend = list(
      font = list(size = 11),
      orientation = "h",
      x = 0.5, 
      xanchor = "center",  
      y = -0.25, 
      yanchor = "top")) %>% 
  config(
    toImageButtonOptions = list(
      format = 'png',
      filename = 'elevation',
      height = 250,
      width = 700,
      scale = 4 # Augmente la résolution par 4
    ))
pasMareDvsC.p14.30m.elevation.plotly


  