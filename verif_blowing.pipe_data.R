#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                           Bulleur, tableau de vérification
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2026-01-07
# Date mise à jour : 
# Pourquoi : Vérifications : quelle erreur moyenne pour les bulleurs ? Seulement pour les Hobo, pour les Odyssey on utilise la valeur pour calibrer
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

# Fichiers à charger directement ----
# cal.data <- read.xlsx("connectivite/data/clean/cal.data.xlsx", sep = ";")
tidy.cal.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.cal.data.RDS") # issu du code "data_water.table_all_v3.0"
# tidy.WTD.data.df <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.df.RDS") # issu du code "data_water.table_all_v3.0"
tidy.WTD.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.RDS") # issu du code "data_water.table_all_v3.0"
extracted.list_data <- lapply(tidy.WTD.data, `[[`, 4) # tidy.WTD.cata[[1]] -> data
tidy.WTD.data.df.9janv <- do.call(rbind, extracted.list_data) # bind_rows identique à rbind, mais ne donne pas de message d'erreur

# .rs.restartR()
# source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd_v2.0.R")
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

# retrait de colonnes inutiles de tidy.cal.cata
tidy.cal.data <- tidy.cal.data %>% 
  select(!c(29:38)) %>% distinct() # enlever les données temporaires associées à la calibration des sondes Odyssey

# # fichiers de consigne de données
water.table.verif <- data.frame()
# extraction des métadonnées
for (tidy.cal.data.line in 1:nrow(tidy.cal.data)) {
  # tidy.cal.data.line <- 161
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
  
  # données extraites à l'heure du bulleur
  long.fil.cm <- tidy.cal.data.line.df$long.fil.CDS.cm
  hauteur.eau.cm <- tidy.WTD.data.match.cal.line$hauteur.eau.cm
  
  # dataframe compilation données
  water.table.verif[tidy.cal.data.line, 1:11] <- tibble("probe.uid" = probe.uid, # créer le dataframe de vérification pour les lignes "n" de la SNH "m"
                                                        "file.extraction.date" = date.extraction,
                                                        "probe.measure.cm" = -1*(ifelse(length(tidy.WTD.data.match.cal.line$calibrated.value.cm) == 0, NA, tidy.WTD.data.match.cal.line$calibrated.value.cm)),
                                                        "bulleur.rel.to.surf.cm" = tidy.cal.data.line.df$bulleur.rel.to.surface.mm/10, 
                                                        "abs.diff.well.uid.cm" = abs(ifelse(length(tidy.WTD.data.match.cal.line$calibrated.value.cm) == 0, NA, (-1*tidy.WTD.data.match.cal.line$calibrated.value.cm)) - tidy.cal.data.line.df$bulleur.rel.to.surface.mm/10), # absolute value
                                                        "9janv.fil.moins.Heau.idBULLEUR" = (ifelse(length(long.fil.cm) == 0, NA, long.fil.cm) - tidy.cal.data.line.df$out.long.tuyau.sol.cm - ifelse(length(hauteur.eau.cm) == 0, NA, hauteur.eau.cm)),
                                                        # SI BULLEUR - OUT, ALORS FIL MOINS OUT AUSSI !!
                                                        "well.uid" = tidy.cal.data.line.df$well.uid,
                                                        "bulleur.no" = tidy.cal.data.line.df$bulleur.no, 
                                                        "donnée.aberrente" = NA,
                                                        "offset.hobo" = NA,
                                                        "measure.status" = NA)
}

water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22063159 & water.table.verif$bulleur.no == 1] <- "aberrent"
water.table.verif$measure.status[water.table.verif$probe.uid == 22063159 & water.table.verif$bulleur.no == 1] <- "rejected"
# la mesure de bulleur 1 est rejetée, les autres sont ok (erreur de lecture probable)

water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22220787] <- "aberrent"
water.table.verif$offset.hobo[water.table.verif$well.uid == "STH.D2.m1m.2025.bis"] <- mean(water.table.verif$abs.diff.well.uid.cm[water.table.verif$well.uid == "STH.D2.m1m.2025.bis"])
# grosse différence, mais constante; j'ajuste avec un offset correspondant à la moyenne de la différence

water.table.verif$measure.status[water.table.verif$probe.uid == 22063159 & water.table.verif$bulleur.no == 1] <- "accept.if.resolved"
# ne sais pas pourquoi/quoi faire encore

water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22224413] <- "aberrent"
# test 1 -> heure où on n'a pas atteint la nappe...
tidy.WTD.data.df.9janv %>%
  dplyr::filter(file.uid == "22224413_20251202" & date.time.UTC.0 == "2025-09-17T17:00:01Z") %>% 
  select(calibrated.value.cm)
# bulleur sur le terrain à heure filtrée = + de 119 cm de profondeur (incluant le out)...
# MAIS... résultat probe (calibrated.value.cm (négatif)) = 83.09174
# ça ne fonctionne pas : le out est de 68 cm et le fil (total de range détectable de WTD) = 119 cm; la sonde aurait détecté la nappe SOUS ELLE MÊME
# test 2 -> heure où on n'a pas atteint la nappe...
tbrnk <- tidy.WTD.data.df.9janv %>%
  dplyr::filter(file.uid == "22224413_20251202" & date.time.UTC.0 == "2025-10-07T16:00:01Z") %>% 
  select(calibrated.value.cm)
# bulleur sur le terrain à heure filtrée = + de 119 cm de profondeur (incluant le out)...
# MAIS... résultat probe (calibrated.value.cm (négatif)) = 84.32559
water.table.verif$measure.status[water.table.verif$probe.uid == 22224413] <- "accept.if.resolved"
# ne sais pas pourquoi/quoi faire encore


# AUTRES VÉRIFICATIONS À FAIRE (rerouler le script avant):
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22224407] <- "aberrent"
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 20853328] <- "aberrent"
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22195241] <- "aberrent"
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22220781] <- "aberrent"
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22224386] <- "aberrent"
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22224396] <- "aberrent"
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22195248] <- "aberrent"
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22224400] <- "aberrent"
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 10279769] <- "aberrent"
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22063138] <- "aberrent"
water.table.verif$donnée.aberrente[water.table.verif$probe.uid == 22224412] <- "aberrent"

# enregistrer cette table de données (métadonnées vérifications 2025)





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

# # summaire par WELL.UID ----

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



# summaire par PROBE.UID ----
# water.table.verif.summrzd.probe <- water.table.verif %>% 
#   group_by(probe.uid) %>% 
#   summarise(mean.diff.probe.uid = mean(abs.diff.well.uid.cm), # absolute value
#             sd.diff.probe.uid = sd(abs.diff.well.uid.cm)) # absolute value


# VISUALISATION ----
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


