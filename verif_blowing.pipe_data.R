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
if (!require("hrbrthemes")) install.packages("hrbrthemes")
if (!require("viridis")) install.packages("viridis")

# Fichiers à charger directement ----
# cal.data <- read.xlsx("connectivite/data/clean/cal.data.xlsx", sep = ";")
tidy.cal.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.cal.data.RDS") # issu du code "data_water.table_all_v3.0"
tidy.WTD.data.df <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.df.RDS") # issu du code "data_water.table_all_v3.0"
tidy.WTD.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.RDS") # issu du code "data_water.table_all_v3.0"

# .rs.restartR()
# source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd_v2.0.R")
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

# retrait de colonnes inutiles de tidy.cal.cata
tidy.cal.data <- tidy.cal.data %>% 
  select(!c(29:38)) %>% distinct() # enlever les données temporaires associées à la calibration des sondes Odyssey

# VÉRIF DE DONNÉE : (utilise élément # 4 de la liste (hobo.verif.9janv), présentant des colonnes de métadonnées supplémentaires) ---- 
# si WTD (hobo) = (fil + CDS) - out - Heau, alors
# (in.bulleur - out) == (fil + CDS) - Heau et
# WTD == (bulleur - out) - Heau
extracted.list_data <- lapply(tidy.WTD.data, `[[`, 4) # tidy.WTD.cata[[1]] -> data
tidy.WTD.data.df.9janv <- do.call(rbind, extracted.list_data) # bind_rows identique à rbind, mais ne donne pas de message d'erreur

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
  long.fil.cm <- tidy.WTD.data.match.cal.line$long.fil.CDS.cm
  hauteur.eau.cm <- tidy.WTD.data.match.cal.line$hauteur.eau.cm
  
  # dataframe compilation données
  water.table.verif[tidy.cal.data.line, 1:10] <- tibble("probe.uid" = probe.uid, # créer le dataframe de vérification pour les lignes "n" de la SNH "m"
                                                        "file.extraction.date" = date.extraction,
                                                        "probe.measure.cm" = ifelse(length(tidy.WTD.data.match.cal.line$calibrated.value.cm) == 0, NA, tidy.WTD.data.match.cal.line$calibrated.value.cm),
                                                        "bulleur.rel.to.surf.cm" = tidy.cal.data.line.df$bulleur.rel.to.surface.mm/10, 
                                                        # "9janv.fil.moins.Heau.idBULLEUR" = -1*(ifelse(length(long.fil.cm) == 0, NA, long.fil.cm) - ifelse(length(hauteur.eau.cm) == 0, NA, hauteur.eau.cm)),
                                                        "9janv.fil.moins.Heau.idBULLEUR" = (ifelse(length(long.fil.cm) == 0, NA, long.fil.cm) + - ifelse(length(hauteur.eau.cm) == 0, NA, hauteur.eau.cm)),
                                                        "9janv.bulleur.moins.out.moins.Heau.idCAL.VAL" = ifelse(length(tidy.WTD.data.match.cal.line$hauteur.eau.cm) == 0, NA, tidy.cal.data.line.df$bulleur.rel.to.surface.mm/10 - tidy.WTD.data.match.cal.line$hauteur.eau.cm),
                                                        "abs.diff.well.uid.cm" = abs(probe.measure.cm - bulleur.rel.to.surf.cm), # absolute value
                                                        "well.uid" = tidy.WTD.data.df.9janv$well.uid,
                                                        "bulleur.no" = tidy.WTD.data.df.9janv$bulleur.no, 
                                                        "NA.1" = NA,
                                                        "NA.2" = NA)
}

# RENDUE LÀ
# tout revérifier les calculs !!!!
# les négatifs et tout !! les schémas


# autres colonnes : quelconque stat -> erreur-type...


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
# summary
water.table.verif.summrzd <- water.table.verif %>% 
  group_by(well.uid) %>% 
  summarise(mean.diff.well.uid = mean(abs(probe.measure.cm - bulleur.rel.to.surf.cm)), # absolute value
            sd.diff.well.uid = sd(probe.measure.cm - bulleur.rel.to.surf.cm)) # absolute value

# VISUALISATION ----
# graph (violin boxplot); code original tiré de The R graph gallery, 2025, https://r-graph-gallery.com/violin_and_boxplot_ggplot2.html
sample_size = water.table.verif %>% group_by(well.uid) %>% summarize(num = n())
water.table.verif %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(well.uid, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = abs.diff.well.uid.cm, fill = well.uid)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("")
  


# À FAIRE 
# vérifier comment on présente typiquement ces données
# -> à quoi servent-elles ? suite avec Laurence