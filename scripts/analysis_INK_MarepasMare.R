#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#             Analysis of pertinence of subgrouping Mare, pasMare
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : 2026-05-05
# Date mise à jour : 
# Pourquoi : analyse en boostrap pour déterminer la la vraisemblance que Mare, pasMare soient deux groupes différents 
# NOTES : vérification des suppositions de normalité non-concluentes, procéder en non-paramétrique (bootstrap)
# Ressources
# cours FOR-7046 (H26)
# Lexique :
# ldscape.dist: landscape = Mare ou pasMare 
###########################################################################-

# ============================================================================= /
# Initialisation ----
# ============================================================================= /
# Librairies
library(conflicted) # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("dplyr")) install.packages("dplyr") # pour manipulation donnees (pipe, etc)
if (!require("DHARMa")) install.packages("DHARMa") # linear mixed models diagnostiques

# Dossier de travail et fonctions
# .rs.restartR()
setwd("~/Documents/Doctorat/_R_Stats_PhD")
source("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/scripts/fonctions_phd_v3.2.R") # charger packages généraux
# source("general.scripts/scripts/fonctions_generales.R")


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
                !stringr::str_detect(well.uid, "^INK\\.ch3"))  # enlever chapitre 3 (routes)
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
str(tidy.WTD.INK)
# pour continuer, il faut définir si Mare et pasMare sont des groupes "vrais" ou non

# ============================================================================= /
# Analyse de pertinence Mare, pasMare ----
# ============================================================================= /
## globalement : sous-groupe (Mare/pasMare) utiles ? ----
### Mann-Whitney U Test -> colonne "trmnt" (pasMare et Mare) différents ? ----
# version non-paramétrique d'ANOVA
set.seed(3)
wilk.test <- wilcox.test(calibrated.value.cm ~ trmnt, data = tidy.WTD.INK)
# Wilcoxon rank sum test with continuity correction
#
# data:  calibrated.value.cm by trmnt
# W = 333018416, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

### ANOVA -> colonne "trmnt" (pasMare et Mare) différents ? ----
# version paramétrique, mais supposition de normalité non-respecté ***
mod.anova <- lm(calibrated.value.cm ~ trmnt, data = tidy.WTD.INK)
anova(mod.anova) # significatif mais j'ai tlmnt de données...
# residus_sim <- simulateResiduals(fittedModel = mod.anova, n = 250) # lent
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
### ...CONCLUSION du 4 mai 2026: ----
# on conserve les groupes Mare / pasMare (même résultat que wilcox.test())

## par distance : sous-groupe (Mare/pasMare) utiles ? ----
str(tidy.WTD.INK)

# créer une colonne avec l'info (exp.unit_trmnt [ex. MareA], relative.distance)
tidy.WTD.INK.dist.df <- tidy.WTD.INK %>% 
  dplyr::filter(slope != "C") %>% # enlever contrôles pas assez d'info
  mutate(ldscape.dist = paste0(trmnt, ".", relative.distance))
distances <- unique(tidy.WTD.INK.dist.df$relative.distance)

# une analyse par distance (pasMare vs Mare)
results <- list()
for(dist in 1:length(distances)) {
  tidy.WTD.INK.dist <- tidy.WTD.INK.dist.df %>% 
    dplyr::filter(relative.distance == distances[dist])
  res <- wilcox.test(calibrated.value.cm ~ ldscape.dist, tidy.WTD.INK.dist)
  results[[dist]] <- list(
    p.value = res$p.value,
    statistic = res$statistic)
}

## ABANDON 5 MAI 2026 - par distance & traitement : sous-groupe (Mare/pasMare) utiles ? ----
# str(tidy.WTD.INK)
# 
# # créer une colonne avec l'info (exp.unit_trmnt [ex. MareA], relative.distance)
# tidy.WTD.INK.dist.groupes <- tidy.WTD.INK %>% 
#   mutate(ldscape.dist = paste0(exp.unit_trmnt, ".", relative.distance)) %>% 
#   group_by(ldscape.dist) %>% 
#   group_keys()
# 
# tidy.WTD.INK.dist.df.2 <- tidy.WTD.INK %>% 
#   dplyr::filter(slope != "C") %>% # enlever contrôles pas assez d'info
#   mutate(ldscape.dist.2 = paste0(exp.unit_trmnt, ".", relative.distance),
#          tmrnt.dist = paste0(trmnt, ".", relative.distance))
# 
# # trouver le moyen de séparer en groupes **
# dists <- unique(tidy.WTD.INK.dist.df.2$relative.distance)
# 
# # une analyse par traitement (pasMare vs Mare) et par distance
# results.2 <- list()
# for(dist in 1:length(dists)) {
#   tidy.WTD.INK.dist.2 <- tidy.WTD.INK.dist.df.2 %>%
#     dplyr::filter(grepl(dists[dist], ldscape.dist.2))
#   res <- wilcox.test(calibrated.value.cm ~ ldscape.dist.2, tidy.WTD.INK.dist.df.2)
#   results[[dist]] <- list(
#     p.value = res$p.value,
#     statistic = res$statistic)
# }

### ...CONCLUSION du 5 mai 2026: ----
