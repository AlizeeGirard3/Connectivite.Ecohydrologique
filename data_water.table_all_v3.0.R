#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                 Water table, data extraction from raw probe files
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2024-12-09
# Date mise à jour : 2026-01-07
# Pourquoi : pour l'ensemble du traitement des données de nappe phréatique 
# Structure :
# —— connectivite
#         |—— archive
#         |—— data
#                     |—— raw
#                     |—— clean
#         |—— output
#                     |—— data
#                     |—— figures
#         |—— scripts
# NOTES : 
# version avec la calibration corrigée
# V3.0 tranféré en fonctions dans "fonction_phd_v2.0.R" et complété la calibration automatisée des sondes Odyssey

# LEXIQUE :
{ 
# SNH : sonde de niveau hydrostatique / synonymes : LL : level logger; sonde, probe
# NP : Nappe phréatique / synonymes : water table
# tz : time zone, syn. fuseau horaire
# cal.data et ll.bulleur, syn. connectivite/data/raw/level_logger_calibration_all.csv
# pattern universel d'appellation des fichiers de SNH : probe.uid_site.uid_datedextraction_probe.brand.csv
  }
##########################################################################-

# fichiers "R data serialized" (RDS) à charger directement
# tidy.WTD.data <-readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.RDS") # issu du code ci-présent / non à jour **

# .rs.restartR()
source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd_v2.0.R")
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

# Librairies ----
# -> packages dans le code sourcé

# toutes manips, des données brutes aux données calibrées et propres (tidy)
raw.ll.files <- list.files(path = "connectivite/data/raw", pattern = "_odyssey|_hobo", full.names = T) # equivalent à ll.pre (ancien) # mettre dans "pattern" tous les ID de SNH listés dans l'objet SNH
tidy.WTD.data <- list()
s = Sys.time() # compte le temps d'exécution
##### boucle pour transformer les fichiers bruts
for (i in 1:length(raw.ll.files)) {
  # i<-1
  print(i)
  raw.ll.files[i]
{  # rendue là : éliminer les raw.ll.files pour les i où cal.data <- ligne "rejected"
  # example : sonde 30 : "connectivite/data/raw/22220783_INK_20251202_hobo.csv"
  # idée : créer un df pour croiser les informations
  # attention ne pas remplacer le i -> plutôt inscrire directement NA dans   
  # tidy.WTD.data <- list()
  # tidy.WTD.data[[i]] <- list("data" = ll.cal, "metadata" = raw.ll.files.i[[2]], "verif.data" = NA) } # le fichier du level logger correspondant à la position i; [1] : data (dataframe), [2] : metadata (character string)
}  
  #### lecture et séparation des données et métadonnées
  raw.ll.files.i.init <- data.metadata(raw.ll.files[i]) # objet temporaire pour ajouter des lignes
  raw.ll.files.i <- metadata(raw.ll.files.i.init)
  files.uid.df <- files.uid(raw.ll.files.i.init); rm(raw.ll.files.i.init)
  
  #### ménage de la date et heure
  tz <- zone.tz("~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp")
  ll.clean <- raw.to.clean_ll(raw.ll.files.i[[1]])  # NOTES : début = installation du puits + 48h de rabattement de la NP / ou non, si puits intallé d'avance, dans quel cas inscrire début officiel - 24h) # fin = heure de retrait // note : données de date en format xlsx ça lit TOUT CROCHE, transformé en csv fonctionne bien
  
  ##### cal.data
  cal.data <- raw.to.clean_cal.data("connectivite/data/raw/level_logger_calibration_all.csv") # import et nettoyage, bon format de date
  brand.i <- ifelse(length(cal.data$probe.brand[which(grepl(files.uid.df[i,1], cal.data$file.uid))])==0,"other", cal.data$probe.brand[which(grepl(files.uid.df[i,1], cal.data$file.uid))])
  
  #### calibration des sondes
  ll.cal.pre.i <- concatenate.ll(ll.clean)
  tidy.WTD.data[[i]] <- clean.to.calibrated_ll(ll.cal.pre.i)
  # rm(tz)
}
Sys.time()-s # temps d'exécution de la boucle
warnings()
# vérifier que les erreurs sont tjrs la meme affaire inutile -> incomplete final line, tenté de régler le problème, mais sans succès; 
# et different length (ça le dit quand le "cal" est vide, et ça met des NA, ce qui est parfait)

# format R des tidy.WTD.data (une liste)
if("tidy.WTD.data" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { saveRDS(tidy.WTD.data, file = "connectivite/data/clean/tidy.WTD.data.RDS") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }

# format tableur des tidy.WTD.data
extracted.list_data <- lapply(tidy.WTD.data, `[[`, 1) # tidy.WTD.cata[[1]] -> data
tidy.WTD.data.df <- do.call(dplyr::bind_rows, extracted.list_data) # bind_rows identique à rbind, mais ne donne pas de message d'erreur
# format RDS des tidy.cal.data (formaté en wide-to-long)
if("tidy.WTD.data.df" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { saveRDS(tidy.WTD.data.df, file = "connectivite/data/clean/tidy.WTD.data.df.RDS") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }
# à faire : coder vérif bulleur pour les hobo

# format xlsx des cal.data aux dates formatées (colonnes originales, formaté "wide")
if("cal.data.csv" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { openxlsx::write.xlsx(cal.data, file = "connectivite/data/clean/cal.data.xlsx", sep = ";", dec = ",", keepNA = TRUE, na.string = "NA") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }

# format tidy des cal.data (formaté en wide-to-long)
extracted.list_verif.data <- lapply(tidy.WTD.data, `[[`, 3) # tidy.WTD.cata[[3]] -> verif.data
tidy.cal.data <- do.call(dplyr::bind_rows, extracted.list_verif.data)
tidy.cal.data <- tidy.cal.data %>% dplyr::filter(is.na(tidy.cal.data) %>% rowSums() != length(tidy.cal.data)) # enlever les lignes complètement composées de NA (tous les hobo en date du 7 janvier)
# format RDS des tidy.cal.data (formaté en wide-to-long)
if("tidy.cal.data.RDS" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { saveRDS(tidy.cal.data, file = "connectivite/data/clean/tidy.cal.data.RDS") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }
