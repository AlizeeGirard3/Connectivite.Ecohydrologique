#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#             Compiled (replicates) water table data visualisation graphs
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : 2026-05-01
# Date mise à jour : 2026-05-05 (pour ANOVA Mare/pasMare par distance)
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
# slope : slope-type of treatement (A, D, C)

###########################################################################-

# ============================================================================= /
# Initialisation ----
# ============================================================================= /
# Librairies
library(conflicted) # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("dplyr")) install.packages("dplyr") # pour manipulation donnees (pipe, etc)
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("plotly")) install.packages("plotly")
# if (!require("ggpubr")) install.packages("ggpubr") # ggarrange()
if (!require("stringr")) install.packages("stringr") # str_to_title
# if (!require("grDevices")) install.packages("grDevices") # pdf()
# if (!require("gridExtra")) install.packages("gridExtra") # multiplot()
# if (!require("withr")) install.packages("withr") # T'o Québec icitte (date-time en français)
if (!require("DHARMa")) install.packages("DHARMa") # linear mixed models diagnostiques
if (!require("slider")) install.packages("slider") # moyenne mobiles : slider_dbl()

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
                !stringr::str_detect(well.uid, "^INK\\.ch3"),  # enlever chapitre 3 (routes)
                date.time.tz.orig %within% interval("2025-07-01 00:00:01", # conserver juillet et août uniquement
                                                    "2025-08-30 23:00:01"),
                source_calib %in% "ms") %>% # 4 mai : choix de "ms" ("blo" == métavalidation, exclue pour graph de poster) / graphique en ggplot : on voit que source_calib "bs" donne une courbe bizarre, je filtre out
  mutate(temp.mean = slide_dbl(temp.ms, mean, .before = 24, .complete = FALSE))
  # mutate(probe.uid = as.integtemp.ms# mutate(probe.uid = as.integer(gsub("_.*", "", file.uid)))  # caduque utiliser le file.uid # créer colonne probe.uid
head(tidy.WTD.INK.pre, n = 3)
# table(tidy.WTD.INK.pre$probe.uid)
table(tidy.WTD.INK.pre$well.uid)

## tidy.cal.data ----
# concat.colnames(tidy.cal.data.pre)
table(tidy.cal.data.pre$type)
tidy.cal.data <- tidy.cal.data.pre %>%
  dplyr::filter(site.uid == "INK", 
                !stringr::str_detect(well.uid, "^INK\\.ch2\\.E"), # enlever les puits hors écotone
                !stringr::str_detect(well.uid, "^INK\\.ch3")) %>% # enlever chapitre 3 (routes)
  select('file.uid', 'lat', 'long', 'measure_status', 'site.uid', "type", 'relative.distance', 
         'year', 'well.uid', 'trmnt.uid', 'lab.probe.id', 'probe.uid', 'probe.brand') %>% 
  # conserver uniquement les colonnes utiles (autrement chaque métadonnée est répliquée, lignes réplquée pour chaque mesure de bulleur)
  dplyr::filter(!well.uid %in% c("INK.ch2.MareD1_A1.m9,8m.pre", "INK.ch2.MareC1.p7m.2025")) %>% # INK.ch2.MareD1_A1.m9,8m.pre -> puits hors design (opportuniste) et INK.ch2.MareC1.p7m.2025 -> je suis le C de pasMare à la place (2025 pas assez de sonde pour les deux)
  dplyr::distinct() %>% # enlever les lignes répétées (dûes aux bulleurs)
  separate(type, into = c("exp.unit_trmnt", "replicate"), sep = -1, remove = FALSE) %>% # ajouter à la source : fonction uid.to.columns **
  separate(exp.unit_trmnt, into = c("trmnt", "slope"), sep = -1, remove = FALSE) # ajouter à la source : fonction uid.to.columns **
table(tidy.cal.data$type) # MareA1    MareA2    MareC1    MareD1    MareD2 pasMareA1 pasMareA2 pasMareC2 pasMareD1 pasMareD2 
table(tidy.cal.data$exp.unit_trmnt) # MareA    MareC    MareD pasMareA pasMareC pasMareD 
table(tidy.cal.data$trmnt) # Mare pasMare
table(tidy.cal.data$slope) # Mare pasMare
colnames(tidy.cal.data)
str(tidy.cal.data)
# pour les fins de comparer les groupes entre par et pas mare par distance, utiliser exp.unit_trmnt
# si non pertinents, ces groupes peuvent être rassemblés par slope_relative distance

## grouper ou créer groupes pour les compilation par réplicats (fonctions_phd_v3.2.R) ----
tidy.WTD.INK <- left_join(tidy.WTD.INK.pre, tidy.cal.data, by = c("well.uid", "file.uid", "probe.brand"))
colnames(tidy.WTD.INK)
table(tidy.WTD.INK$well.uid)
table(tidy.WTD.INK$measure_status)
table(tidy.WTD.INK$exp.unit_trmnt)
# je vais groupper par : exp.unit (ex. MareA -> moyenne des réplicats) & distance
groupes <- tidy.WTD.INK %>%
  group_by(exp.unit_trmnt, relative.distance) %>% 
  group_keys()

tidy.WTD.INK %>%
  group_by(exp.unit_trmnt, relative.distance) %>% 
  n_groups()
# 20 groupes

## sous-groupe (Mare/pasMare) utiles ? ----
# vérifications de structure & suppositions
boxplot(calibrated.value.cm ~ tidy.WTD.INK$relative.distance, tidy.WTD.INK) # variation relativement homogène
hist(tidy.WTD.INK$calibrated.value.cm) # on doit centrer réduire
hist(scale(tidy.WTD.INK$calibrated.value.cm)) # mieux mais pas encore normal, left-skewed
shapiro.test(sample(scale(tidy.WTD.INK$calibrated.value.cm), 5000))

### ANOVA -> colonne "trmnt" (pasMare et Mare) différents ? ----
set.seed(3)
mod.anova <- lm(calibrated.value.cm ~ trmnt, data = tidy.WTD.INK)
anova(mod.anova) # significatif mais j'ai tlmnt de données...
residus_sim <- simulateResiduals(fittedModel = mod.anova, n = 250)
# plot(residus_sim) # lent

#### violon plot (chavauchements) puisque trop d'observations, p-value perd sa pertinence; ----
# transformer le résultat en dataframe
df_res <- data.frame(
  residus = resid(mod.anova),
  groupe = factor(mod.anova$model$trmnt))

ggplot(df_res, aes(x = "Tous les Groupes", y = residus, fill = groupe)) +
  geom_violin(position  = "identity", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  theme_minimal() + 
  labs(title = "Superposition directe des résidus",
       x = "",
       y = "Valeur des résidus")
# se chavauchent, selon Google IA :
# violons se superposent et forment une masse commune autour de la ligne 0, cela confirme que
# résidus respectent les deux hypothèses fondamentales de l'ANOVA (homoscédasticité et indépendance des erreurs par groupe)
# ANOVA est significative, mais on ne sait pas l'ampleur de l'effet donc...
### ...quelle moyenne pour chaque groupe ? ----
moyennes_base <- aggregate(calibrated.value.cm ~ trmnt, 
                           data = tidy.WTD.INK, 
                           FUN = mean, 
                           na.rm = TRUE)
print(moyennes_base) # 4 mai 2026
# trmnt calibrated.value.cm
# 1    Mare           -74.01516
# 2 pasMare           -81.52380
# interprétation : pasMare a une moyenne de nappe plus BASSE de 
abs(moyennes_base$calibrated.value.cm[2] - moyennes_base$calibrated.value.cm[1])
# [1] 7.508643
# ça correspond à la présomption que les mares sont des "réserves" d'eau ou que ce côté de l'expérience reçoit plus d'eau de l'amont
## ...CONCLUSION du 4 mai 2026: ----
# on conserve les groupes Mare / pasMare

## calcul des stats par groupe ----
# vérification des moyennes
tidy.WTD.INK.compld.summry <- tidy.WTD.INK %>%
  mutate(exp.unit_trmnt_dist = paste0(exp.unit_trmnt, ".", relative.distance)) %>% 
  group_by(date.time.UTC.0, exp.unit_trmnt_dist, source_calib) %>% 
  # tableur "groupes" = exp.unit_trmnt, relative.distance -> combiné dans exp.unit_trmnt_dist
  # source_calib =  choisir éventuellement, (caduque 4 mai : choix de "ms" //), mais pour l'instant les deux valeurs sont considérées)
  # moyenne + sd à chaque heure
  summarize(
    mean.WTD = mean(calibrated.value.cm, na.rm = TRUE),
    sd.WTD = sd(calibrated.value.cm, na.rm = TRUE))

tidy.WTD.INK.compld <- tidy.WTD.INK %>%
  mutate(exp.unit_trmnt_dist = paste0(exp.unit_trmnt, ".", relative.distance)) %>% 
  group_by(date.time.UTC.0, exp.unit_trmnt_dist, source_calib) %>% 
  # tableur "groupes" = exp.unit_trmnt, relative.distance -> combiné dans exp.unit_trmnt_dist
  # source_calib =  choisir éventuellement, (caduque 4 mai : choix de "ms" //), mais pour l'instant les deux valeurs sont considérées)
  # moyenne + sd à chaque heure
  mutate(
    mean.WTD = mean(calibrated.value.cm, na.rm = TRUE),
    sd.WTD = sd(calibrated.value.cm, na.rm = TRUE),
    ymin_WTD = mean.WTD - sd.WTD,
    ymax_WTD = mean.WTD + sd.WTD) %>%
  ungroup() %>% 
  distinct(date.time.UTC.0, exp.unit_trmnt_dist, source_calib, .keep_all = TRUE)
colnames(tidy.WTD.INK.compld)
table(tidy.WTD.INK.compld$type)
table(tidy.WTD.INK.compld$exp.unit_trmnt) # combiner exp.unit_trmnt + relative.distance pour afficher les courbes de WTD ~ temps
table(tidy.WTD.INK.compld$exp.unit_trmnt_dist)

# ============================================================================= /
# Graphique ----
# ============================================================================= /
## data subset : ajustements ----
MareD.p30m.data <- tidy.WTD.INK.compld %>% 
  dplyr::filter(exp.unit_trmnt_dist == "MareD.p30m") %>% # ajuster un graphique exemple 
  subset(source_calib %in% "ms") ## graphique en ggplot : on voit que source_calib "bs" donne une courbe bizarre, je filtre out

### (caduque) graphique en ggplot ----
# MareD.p30m.graph <- ggplot(MareD.p30m.data) +
#   geom_line(
#     data = subset(MareD.p30m.data, source_calib %in% "ms"),
#     aes(x = date.time.tz.orig, y = mean.WTD)) +
#   geom_bar(aes(x = date.time.tz.orig, y = -1*prcp.ms), stat = "identity", fill = "lightblue", alpha = 0.5) +
#   scale_x_datetime(date_breaks = "2 weeks", date_labels = "%y/%b/%d") +
#   ggtitle(paste0("Inkerman, à l'emplacement MareD.p30m")) +
#   labs(y = "Hauteur de nappe phréatique (cm)\nrelative à la surface", 
#        x = "Date") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
# print(MareD.p30m.graph)
# # on voit ici que source_calib "bs" donne une courbe bizarre, je filtre out

## séquence de couleurs ----
pal_sequence <- c("#1b019b", "#FF6B6B", "#FFB04FFF", "#679C35FF")
# "#6497B1FF", "#6A359CFF", "#FFB04FFF", "#679C35FF", "#CD1076FF" # autres idées

## m1m ----
# comment transposer en boucle ??

## p7m ----

## p14m ----

## p30m ----
### data subset (pasMareDvsC.p30m.data) : ajustements ----
pasMareDvsC.p30m.data <- tidy.WTD.INK.compld %>% 
  dplyr::filter(exp.unit_trmnt_dist %in% c("pasMareD.p30m", "pasMareC.p30m")) %>% # ajuster un graphique exemple 
  mutate(exp.unit_trmnt_dist = fct_recode(factor(exp.unit_trmnt_dist),
                                          "Gentle slope (+30 m; n = 2)" = "pasMareD.p30m",
                                          "Control treatment (+30 m, n = 1)" = "pasMareC.p30m"))
table(pasMareDvsC.p30m.data$exp.unit_trmnt_dist)

### graphique en plotly ----
pasMareDvsC.p30m.data.plotly <- plot_ly() %>%
  add_ribbons(
    data = pasMareDvsC.p30m.data,
    x = ~ date.time.tz.orig,
    ymin = ~ ymin_WTD,
    ymax = ~ ymax_WTD,
    color = ~ exp.unit_trmnt_dist,  # On lie à la colonne de traitement
    colors = pal_sequence[3:4], 
    opacity = 0.2, # Ruban très transparent
    inherit = FALSE,
    legendgroup = ~ exp.unit_trmnt_dist,
    showlegend = FALSE) %>% # On ne l'affiche pas dans la légende pour ne pas faire doublon
  add_lines( # axe Y principal
    data = pasMareDvsC.p30m.data,
    x = ~ date.time.tz.orig,
    y = ~ mean.WTD,
    color = ~ exp.unit_trmnt_dist,
    line = list(width = 1.5), 
    inherit = FALSE,
    legendgroup = ~ exp.unit_trmnt_dist) %>%
  add_bars( # axe Y secondaire inversé
    data = pasMareDvsC.p30m.data,
    x = ~ date.time.tz.orig,
    y = ~ prcp.ms,
    yaxis = "y2",
    name = "Precipitations (mm)", # name = "Précipitations",
    marker = list(color = "#1b019b", opacity = 0.5),
    inherit = FALSE) %>%
  add_lines( # axe Y tertiaire (nouveau)
    data = pasMareDvsC.p30m.data,
    x = ~ date.time.tz.orig,
    y = ~ temp.mean, # Adaptez le nom de votre colonne de température ici
    yaxis = "y3",
    name = "Temperature (°C)",
    line = list(color = "#FF6B6B", width = 1.2),
    inherit = FALSE) %>%
  plotly::layout(
    title = "", # titre programmé manuellement
    margin = list(r = 60, l = 60, b = 80, t = 60), # marges; hauteur globale
    # width = 700,  # largeur fixe pour forcer l'espace
    # margin = list(r = 200, l = 60, b = 80, t = 60), # marges; largeur globale
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    xaxis = list(
      title = "Date",
      type = "date",
      tickformat = "%y/%b/%d",
      tickangle = -45,
      showgrid = TRUE,
      # marges (r = droite, l = gauche, b = bas, t = haut)
      autosize = FALSE,
      gridcolor = "#f0f0f0",
      linecolor = "black",
      mirror = FALSE,
      showline = FALSE,
      domain = c(0, 0.98)),
    yaxis = list(
      title = "Water table depth (cm)",
      domain = c(0, 0.62),
      showgrid = TRUE,
      gridcolor = "#f0f0f0",
      linecolor = "black",
      mirror = FALSE,
      showline = FALSE),
    yaxis2 = list(
      title = "Precipitations (mm)",
      domain = c(0.67, 1.0),
      side = "left",
      autorange = "reversed", # inversé pour que la pluie tombe du haut
      showgrid = FALSE,
      linecolor = "black",
      showgrid = FALSE,
      showline = FALSE),
    yaxis3 = list(
      title = "Moving average temperature\n(°C, 24h window)",
      domain = c(0.67, 1.0),
      overlaying = "y2",       # superposé à l'axe de pluie en haut
      side = "right",          # placé à droite pour ne pas gêner la pluie
      showgrid = FALSE,
      linecolor = "black",
      showgrid = FALSE,
      showline = FALSE), 
    legend = list(orientation = "h", y = -0.20, x = 0.5, xanchor = "center"),
    annotations = list( # simule le titre via une annotation positionnée dans le vide supérieur
      list( # titre principal
        text = "<b>Ecotone experimental units at Inkerman (N.-B., Canada)",
        xref = "paper", yref = "paper",
        x = 0.5, 
        y = 1.08,                  # Tout en haut du graphique
        showarrow = FALSE,
        font = list(size = 14, color = "black"),
        xanchor = "center", yanchor = "bottom"),
      list( # titre graph temp & préc
        text = "Precipitations and temperature", # text = "Précipitations et Température"
        xref = "paper", yref = "paper",
        x = 0.5, 
        y = 1.02,
        showarrow = FALSE,
        font = list(size = 12, color = "black"),
        xanchor = "center", yanchor = "bottom"),
      list( # titre water table
        text = "Water table dynamic in reprofiled [gentle slope] vs control treatments\n(relative distance to main : +30 m)",
        xref = "paper", yref = "paper",
        x = 0.5,
        y = 0.58,
        showarrow = FALSE,
        font = list(size = 12, color = "black", face = "bold"),
        xanchor = "center",
        yanchor = "bottom")))
pasMareDvsC.p30m.data.plotly
# pour enregistrer pas le choix d'utiliser le bouton dans la fenêtre Plotly parce que sinon Python et tout 



