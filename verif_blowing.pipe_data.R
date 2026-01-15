#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                           Bulleur, tableau de vérification
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2026-01-07
# Date mise à jour : 
# Pourquoi : # Suivis de l'examination des données
# Idée : avec l'erreur moyenne de bulleur, ajuster ou donner une fourchette d'erreur pour les Odyssey...
# NOTES : 

# LEXIQUE :
{ 
  # SNH : sonde de niveau hydrostatique / synonymes : LL : level logger; sonde, probe
  # NP : Nappe phréatique / synonymes : water table / WTD
  # tz : time zone, syn. fuseau horaire
}
##########################################################################-

# Librairies ----
if (!require("openxlsx")) install.packages("openxlsx") # lire les excel
if (!require("tidyverse")) install.packages("tidyverse") # méta package // gosser avec des suites de caractères, str_replace, [...]
if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("hrbrthemes")) install.packages("hrbrthemes") # theme_ipsum par disponible au 12 janvier 2026
# install.packages('https://cran.r-project.org/src/contrib/Archive/hrbrthemes/hrbrthemes_0.1.0.tar.gz', type='source', repos=NULL)
if (!require("viridis")) install.packages("viridis")

# Dossier directeur et sourçage ----
# .rs.restartR()
# source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd_v2.0.R")
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

# Fichiers à charger directement ----
# cal.data <- read.xlsx("connectivite/data/clean/cal.data.xlsx", sep = ";")
tidy.cal.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.cal.data.RDS") # issu du code "data_water.table_all_v3.0"
# tidy.WTD.data.df <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.df.RDS") # issu du code "data_water.table_all_v3.0"
tidy.WTD.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.RDS") # issu du code "data_water.table_all_v3.0"
extracted.list_data <- lapply(tidy.WTD.data, `[[`, 4) # tidy.WTD.cata[[1]] -> data
tidy.WTD.data.df.9janv <- do.call(rbind, extracted.list_data) # bind_rows identique à rbind, mais ne donne pas de message d'erreur

# retrait de colonnes inutiles de tidy.cal.cata
tidy.cal.data <- tidy.cal.data %>%
  group_by(probe.brand) %>% 
  distinct()
  # select(!c(29:38)) %>% distinct() # enlever les données temporaires associées à la calibration des sondes Odyssey

# Tableau compilation ----
# fichiers de consigne de données
water.table.verif <- data.frame()
# extraction des métadonnées
for (tidy.cal.data.line in 1:nrow(tidy.cal.data)) {
  # tidy.cal.data.line <- 2
  print(tidy.cal.data.line)
  
  # extraction données de tidy.cal.data pour la ligne "tidy.cal.data.line"
  tidy.cal.data.line.df <- tidy.cal.data[tidy.cal.data.line,] # filtrer ll.bulleur (level_logger_calibration_all.csv) par le ligne "n" (vérification n au bulleur)
  
  # extraction données de tidy.WTD.data pour la ligne "tidy.cal.data.line"
  probe.uid <- tidy.cal.data.line.df$probe.uid
  date.line <- tidy.cal.data.line.df$file.uid
  date.extraction <- sub(".*_", "", date.line)
  
  # filtrer tidy.WTD.data.df (toutes données) par le file_uid & le moment de la mesure de bulleur
  tidy.WTD.data.match.cal.line <- tidy.WTD.data.df.9janv %>%
  dplyr::filter(tidy.WTD.data.df.9janv$file.uid == date.line & tidy.WTD.data.df.9janv$date.time.UTC.0 == tidy.cal.data.line.df$in.bulleur.date.time.UTC.0)
  # tidy.WTD.data.match.cal.line <- tidy.WTD.data.df.13janv %>% 
  #   dplyr::filter(tidy.WTD.data.df.13janv$file.uid == date.line & tidy.WTD.data.df.13janv$date.time.UTC.0 == tidy.cal.data.line.df$in.bulleur.date.time.UTC.0)
  
  # données extraites à l'heure du bulleur
  long.fil.cm <- tidy.cal.data.line.df$long.fil.CDS.cm
  hauteur.eau.cm <- tidy.WTD.data.match.cal.line$hauteur.eau.cm
  
  # dataframe compilation données
  water.table.verif[tidy.cal.data.line, 1:11] <- tibble("probe.uid" = probe.uid, # créer le dataframe de vérification pour les lignes "n" de la SNH "m"
                                                        "file.extraction.date" = date.extraction,
                                                        "probe.measure.cm" = -1*(ifelse(length(tidy.WTD.data.match.cal.line$calibrated.value.cm) == 0, NA, tidy.WTD.data.match.cal.line$calibrated.value.cm)),
                                                        "bulleur.rel.to.surf.cm" = tidy.cal.data.line.df$bulleur.rel.to.surface.mm/10, 
                                                        "abs.diff.well.uid.cm" = abs(ifelse(length(tidy.WTD.data.match.cal.line$calibrated.value.cm) == 0, NA, (-1*tidy.WTD.data.match.cal.line$calibrated.value.cm)) - tidy.cal.data.line.df$bulleur.rel.to.surface.mm/10), # absolute value
                                                        "9janv.fil.moins.Heau.idBULLEUR" = (ifelse(length(long.fil.cm) == 0, NA, long.fil.cm) - tidy.cal.data.line.df$out.mean.cm - ifelse(length(hauteur.eau.cm) == 0, NA, hauteur.eau.cm)),
                                                        # SI BULLEUR - OUT, ALORS FIL MOINS OUT AUSSI !!
                                                        "well.uid" = tidy.cal.data.line.df$well.uid,
                                                        "bulleur.no" = tidy.cal.data.line.df$bulleur.no, 
                                                        "donnée.aberrente" = NA,
                                                        "offset" = "NA",#ifelse(),
                                                        "measure.status" = NA)
  
}

# ODYSSEY ----
# tous les offsets
tidy.cal.data <- readRDS("connectivite/data/clean/tidy.cal.data.RDS")
vérif.1 <- tidy.cal.data %>% 
  dplyr::filter(cal.no == "3",
                probe.brand == "ODYSSEY")
vérif.1 <- vérif.1[-which(is.na(vérif.1$offset_cm)),] # règle l'avertissement d'avoir retiré 22 lignes contenant des
vérif.1$probe.uid <- as.character(vérif.1$probe.uid)

## Graphique de tous les offsets ----
# Violin boxplot); code original tiré de The R graph gallery, 2025, https://r-graph-gallery.com/violin_and_boxplot_ggplot2.html
sample_size = vérif.1 %>% group_by(probe.uid) %>% summarize(num = n())

vérif.1 %>%
  group_by(probe.uid) %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(as.character(probe.uid), "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = offset_cm, fill = as.character(probe.uid))) +
  geom_violin(width = 1.4, show.legend = FALSE, drop = FALSE) +
  geom_boxplot(width = 0.1, color = "grey", alpha = 0.2, show.legend = FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  # theme(legend.position = "none",
        # plot.title = element_text(size=11)) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  # theme.Aliz() + # crée plein de warnings incompréhensibles
  labs(title = "Offsets des sondes Odyssey,\npar identifiant unique de sonde\n(années confondues)") + 
  xlab("")
vérif.1$offset_cm

# #### 42564_20241125 ----
# water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 42564 & water.table.verif$file.extraction.date == "20241125"] <- "aberrent"
# #### 42564_20241125 ----
# water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 42564 & water.table.verif$file.extraction.date == "20251203"] <- "aberrent"
### 42565_20251203 ----



# HOBO ----
# fichiers/sondes en cours de résolution (résulus ci-dessous)
#### 22063159_20251210 ----
unique(water.table.verif[water.table.verif$probe.uid == 22063159,])
# 35.4678290; bulleur #1 
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22063159 & water.table.verif$file.extraction.date == "20251210" & water.table.verif$bulleur.no == 1] <- "aberrent"
# water.table.verif$measure.status[water.table.verif$probe.uid == 22063159 & water.table.verif$bulleur.no == 1] <- "rejected"
# water.table.verif$measure.status[water.table.verif$probe.uid == 22063159 & water.table.verif$bulleur.no == 1] <- "accept.if.resolved"
# # ne sais pas pourquoi/quoi faire encore // la mesure de bulleur 1 est à rejeter ?? les autres sont ok (erreur de lecture probable)

#### 22220787_20251128 ----
unique(water.table.verif[water.table.verif$probe.uid == 22220787 & water.table.verif$file.extraction.date == "20251128",])
# well : STH.D2.m1m.2025.bis (sensé être tjrs proche de la surface)
# 96.36888, 89.72716, 87.59968, 93.91374
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22220787 & water.table.verif$file.extraction.date == "20251128"] <- "aberrent"
# si possible vérifier WTD avec sondes -1m ou canal en mm temps (mais pour l'instant - 14 janvier 2026- aucune n'a fonctionné...)
# je suggère d'ajouter le offset moyen (à faire quand tout est décidé sur le traitement de ces données)
water.table.verif$offset[water.table.verif$well.uid == "STH.D2.m1m.2025.bis"] <- mean(water.table.verif$abs.diff.well.uid.cm[water.table.verif$well.uid == "STH.D2.m1m.2025.bis"])

#### 22224413_20251202 ----
unique(water.table.verif[water.table.verif$probe.uid == 22224413,])
# 19.86240 (INK.Mare.A1.m1m.2025) et 19.81335 (PRO.ch1.C00.keptChannels.2025) de diff absolue... La sonde donne +20cm de prof à PRO et -20cm de prof à INK !
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22224413] <- "aberrent"
# test 1 -> heure où on n'a pas atteint la nappe (nappe supposée être à + de...
# [longueur du bulleur]
# sonde pas dans l'eau si la nappe atteint : fil+CDS-OUT =...
133-68.1
# [1] 64.952 cm sous la surface)...
tidy.WTD.data.df.9janv %>%
  dplyr::filter(file.uid == "22224413_20251202" & date.time.UTC.0 == "2025-09-17T17:00:01Z") %>% 
  select(calibrated.value.cm)
#        calibrated.value.cm
# 1            83.09174 cm sous la surface, 
# alors que la sonde est à ~65 cm sous le sol (fil-OUT)... 
# et que mon bulleur fait 121.92 pieds (CONFIRMER) - OUT = 53.82 cm
# mais impossible que la sonde ait repéré de la nappe SOUS elle-même
# ne sais pas pourquoi/quoi faire encore

#### 22224407_20250106 ----
unique(water.table.verif[water.table.verif$probe.uid == 22224407,])
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22224407] <- "aberrent"
# pas de glitch
# ne sais pas pourquoi/quoi faire encore

### / mesures prises par JLG avant mon arrivée ----
#### 10279769_20250106 ----
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 10279769 & water.table.verif$well.uid == "INK.ED.70m.2024"] <- "aberrent"
tidy.WTD.data.df.9janv %>%
  dplyr::filter(file.uid == "10279769_20250106", 
                date.time.tz.orig > "2024-07-06	10:00:01",
                date.time.tz.orig < "2024-07-06	14:00:01")
# scan.id raw.value calibrated.value.cm date.AAAA-MM-JJ time.HH.MM.SS   date.time.tz.orig      date.time.UTC.0 long.fil.CDS.cm out.long.tuyau.sol.cm hauteur.eau.cm          file.uid
# 1      60   109.355            33.75172      2024-07-06      12:00:01 2024-07-06 12:00:01 2024-07-06T15:00:01Z          183.63                 68.25       81.62828 10279769_20250106
# 2      61   109.302            34.80202      2024-07-06      13:00:01 2024-07-06 13:00:01 2024-07-06T16:00:01Z          183.63                 68.25       80.57798 10279769_20250106
# 3      62   109.298            34.84281      2024-07-06      14:00:01 2024-07-06 14:00:01 2024-07-06T17:00:01Z          183.63                 68.25       80.53719 10279769_20250106
#  √ ok pas un glitch 
# ~ 10 cm de diff -> expliqué par non-standardisation de la mesure de fil...

#### 22195241_20251202 ----
# différence absolue pour 2 bulleurs à PRO : 13.48299623, 20.97851203
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22195241 & water.table.verif$file.extraction.date == "20251202"] <- "aberrent"
tidy.WTD.data.df.9janv %>%
  dplyr::filter(file.uid == "22195241_20251202", 
                date.time.tz.orig > "2025-10-07	09:00:01",
                date.time.tz.orig < "2025-10-07	12:00:01")
# aussi, étrange date.AAAA-MM-JJ a une heure erronnée tjrs associée... : "2025-10-07 01:00:00"
tidy.WTD.data.df.9janv %>%
  dplyr::filter(file.uid == "22195241_20251202", 
                date.time.tz.orig > "2025-11-12 05:00:01",
                date.time.tz.orig <= "2025-11-12 09:00:01")
# mais la courbe est relativement stable, avec un signal journalier (augm. températures journalières impact la pression atm ?)
# remesurer le fil et le out (à faire)



### / autres VÉRIFICATIONS À FAIRE (12 janv) ----
#### 22220781 ----
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22220781] <- "aberrent"

#### 22224386 ----
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22224386] <- "aberrent"

#### 22224396 ----
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22224396] <- "aberrent"

#### 22195248 ----
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22195248] <- "aberrent"

#### 22224400 ----
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22224400] <- "aberrent"

#### 22063138 ----
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22063138] <- "aberrent"

#### 22224412 ----
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22224412] <- "aberrent"


# enregistrer cette table de données (métadonnées vérifications 2025)



# ============================================================================= /
#  ZONE DES PROBLÈMES RÉSOLU ----
# ============================================================================= /
#### 22220787_20251128 ----
# water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22220787] <- "aberrent"
# water.table.verif$offset.hobo[water.table.verif$well.uid == "STH.D2.m1m.2025.bis"] <- mean(water.table.verif$abs.diff.well.uid.cm[water.table.verif$well.uid == "STH.D2.m1m.2025.bis"])
# grosse différence, mais constante; j'ajuste avec un offset correspondant à la moyenne de la différence


#### 20853328_20250106 ----
# # water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 20853328 & water.table.verif$file.extraction.date == "20250106" & water.table.verif$abs.diff.well.uid.cm == "35.6952261"] <- "aberrent"
# tidy.WTD.data.df.13janv %>%
#   dplyr::filter(file.uid == "20853328_20250106", 
#                 date.time.tz.orig >= "2024-07-06 11:00:01",
#                 date.time.tz.orig < "2024-07-06 15:00:01")
# # raw.value calibrated.value.cm date.AAAA-MM-JJ time.HH.MM.SS   date.time.tz.orig      date.time.UTC.0 long.fil.CDS.cm out.long.tuyau.sol.cm hauteur.eau.cm          file.uid
# # 1      60   103.403            69.69523      2024-07-06      12:00:01 2024-07-06 12:00:01 2024-07-06T15:00:01Z          183.63                    93       20.93477 20853328_20250106
# # 2      61   103.361            70.63337      2024-07-06      13:00:01 2024-07-06 13:00:01 2024-07-06T16:00:01Z          183.63                    93       19.99663 20853328_20250106
# # 3      62   103.358            70.66396      2024-07-06      14:00:01 2024-07-06 14:00:01 2024-07-06T17:00:01Z          183.63                    93       19.96604 20853328_20250106
# # donnée bulleur n'est pas un glitch, c'est stable avant et après
# # revérifié donnée originale papier (JLG; IrisD) ou éliminer la donnée
# # voir verso feuille originale manuscrite (notes de réflexion)
# # demander à Iris comment ils prennaient la "mesure de fil"
# # Heau au moment du bulleur (logger) + WTD (logger) <- ces deux dernières sont fiables + 93 (OUT) + 12,14 (ils auraient pris la longueur au trou du fil, à 0,79 cm du haut de la sonde, donc ajouter 12,14)
# 20.93477+69.69523+93+12.14
# # [1] 195.77
# # et si finalement je n'ajoute pas le "CDS-modifié" (12,24), j'obtiens une valeur très proche de la longueur de fil originale...
# 20.93477+69.69523+93
# # [1] 183.63
# # ils auraient donc mesuré la sonde également, jusqu'au 2e trou (trou du bas, proche du senseur, voir protocole -> legel loggers -> U20 sensor location)
# # décision, je change la longueur de fil originale dans cal.data à la donnée inscrite sur la feuille (186,5)- CDS (car il s'ajoute automatiquemeent dans les scripts).
# 183.63-12.93
# # ci-haut invalide
# # nouvelle tentative :
# # si -34 bulleur = vrai, et que out - vrai, alors quel fil et Heau donnent la nappe à -34 ?
# 127+20.93477 
# # [1] 147.9348
# # non incorrigible !! éliminée

# ============================================================================= /
#  CHANTIER ----
# ============================================================================= /

# autres colonnes : quelconque stat -> erreur-type...

water.table.verif.summrzd.well <- water.table.verif %>%
  select(!c(contains("NA."), donnée.aberrente)) %>%
  dplyr::filter(!startsWith(as.character(probe.uid), "4")) %>%
  drop_na() %>% 
  group_by(well.uid) %>%
  summarise(mean.diff.well.uid = mean(abs.diff.well.uid.cm), # absolute value
            sd.diff.well.uid = sd(abs.diff.well.uid.cm)) # absolute value


water.table.verif.plot.probe <- water.table.verif %>%
  select(!c(NA.1, donnée.aberrente)) %>%
  dplyr::filter(!startsWith(as.character(probe.uid), "4")) %>%
  drop_na()

# VÉRIF (initialement) ----
# # fichiers de consigne de données
# water.table.verif <- data.frame()
# 
# # extraction des métadonnées
# for (tidy.cal.data.line in 1:nrow(tidy.cal.data)) {
#   # tidy.cal.data.line <- 161
#   print(tidy.cal.data.line)
#   
#   # extraction données de tidy.cal.data pour la ligne "tidy.cal.data.line"
#   tidy.cal.data.line.df <- tidy.cal.data[tidy.cal.data.line,] # filtrer ll.bulleur (level_logger_calibration_all.csv) par le ligne "n" (vérification n au bulleur)
#   
#   # extraction données de tidy.WTD.data pour la ligne "tidy.cal.data.line"
#   probe.uid <- tidy.cal.data.line.df$probe.uid
#   date.line <- tidy.cal.data.line.df$file.uid
#   date.extraction <- sub(".*_", "", date.line)
#   
#   # filtrer tidy.WTD.data.df (toutes données) par le file_uid & le moment de la mesure de bulleur
#   tidy.WTD.data.match.cal.line <- tidy.WTD.data.df[tidy.WTD.data.df$file.uid == date.line & tidy.WTD.data.df$date.time.UTC.0 == tidy.cal.data.line.df$in.bulleur.date.time.UTC.0, ]
#   
#   water.table.verif[tidy.cal.data.line, 1:10] <- tibble("probe.uid" = probe.uid, # créer le dataframe de vérification pour les lignes "n" de la SNH "m"
#                                                        "file.extraction.date" = date.extraction,
#                                                        "probe.measure.cm" = ifelse(length(tidy.WTD.data.match.cal.line$calibrated.value.cm) == 0, NA, tidy.WTD.data.match.cal.line$calibrated.value.cm),
#                                                        "bulleur.rel.to.surf.cm" = tidy.WTD.data.df$in.bulleur.rel.to.surface.mm/10, 
#                                                        "abs.diff.well.uid.cm" = abs(probe.measure.cm - bulleur.rel.to.surf.cm), # absolute value
#                                                        "well.uid" = tidy.WTD.data.df$well.uid,
#                                                        "bulleur.no" = tidy.WTD.data.df$bulleur.no, 
#                                                        "NA.9" = NA,
#                                                        "NA.8" = NA, 
#                                                        "NA.10" = NA)
# }
# autres colonnes : quelconque stat -> erreur-type...

#### # summaire par WELL.UID ----

# 
# # VISUALISATION
# # graph (violin boxplot); code original tiré de The R graph gallery, 2025, https://r-graph-gallery.com/violin_and_boxplot_ggplot2.html
# water.table.verif.plot.well <- water.table.verif %>% 
#   dplyr::filter(startsWith(as.character(probe.uid), "4")) 
# sample_size = water.table.verif.plot.well %>% group_by(well.uid) %>% summarize(num = n())
# 
# water.table.verif.plot.well %>% 
#   group_by(well.uid) %>%
#   left_join(sample_size) %>%
#   mutate(myaxis = paste0(well.uid, "\n", "n=", num)) %>%
#   ggplot(aes(x = myaxis, y = abs.diff.well.uid.cm, fill = well.uid)) +
#   geom_violin(width=1.4) +
#   geom_boxplot(width=0.1, color="grey", alpha=0.2) +
#   scale_fill_viridis(discrete = TRUE) +
#   theme(
#     legend.position="none",
#     plot.title = element_text(size=11)
#   ) +
#   ggtitle("Water.table.verif.plot.well") +
#   xlab("")



#### summaire par PROBE.UID ----
# water.table.verif.summrzd.probe <- water.table.verif %>% 
#   group_by(probe.uid) %>% 
#   summarise(mean.diff.probe.uid = mean(abs.diff.well.uid.cm), # absolute value
#             sd.diff.probe.uid = sd(abs.diff.well.uid.cm)) # absolute value


#### VISUALISATION ----
# graph (violin boxplot); code original tiré de The R graph gallery, 2025, https://r-graph-gallery.com/violin_and_boxplot_ggplot2.html


#   
# sample_size = water.table.verif.plot.probe %>% group_by(probe.uid) %>% summarize(num = n())
# water.table.verif.plot.probe %>%
#   left_join(sample_size) %>%
#   ungroup() %>%
#   ggplot(aes(x = as.character(bulleur.no), y = abs.diff.well.uid.cm, fill = as.character(bulleur.no))) +
#   # # mutate(myaxis = paste0(probe.uid, "\n", "n=", num)) %>%
#   # ggplot(aes(x = as.character(probe.uid), y = abs.diff.well.uid.cm, fill = as.character(probe.uid))) +
#   geom_violin(width=1.4) +
#   geom_boxplot(width=0.1, color="grey", alpha=0.2) +
#   # scale_fill_viridis(discrete = TRUE) +
#   theme(legend.position="none",
#         plot.title = element_text(size=11), 
#         axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#   ggtitle("water.table.verif.plot.probe") +
#   xlab("probe.uid")



# À FAIRE 
# vérifier comment on présente typiquement ces données
# -> à quoi servent-elles ? suite avec Laurence





# ggplot(vérif.1, aes(x = probe.uid, y = offset_cm)) +
#   scale_y_continuous(breaks = seq(-160, 160, by = 20)) +
#   geom_segment(aes(x=probe.uid, xend=probe.uid, y=0, yend=offset_cm)) +
#   geom_point(size=1, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +
#   theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#   labs(title = "Offsets des sondes Odyssey,\npar identifiant unique de sonde\n(années confondues)")

