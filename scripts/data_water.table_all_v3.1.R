#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                 Water table, data extraction from raw probe files
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2024-12-09
# Date mise à jour : 2026-03-11
# Pourquoi : pour l'ensemble du traitement des données de nappe phréatique
# Structure :
# —— connectivite
#         |—— archive
#         |—— data
#                     |—— raw
#                     |—— extracted_raw    <- raw feuilles numériques terrain (plusieurs onglets pour un site), extrait en un df par onglet, tous site confondu (script "data_sites_all")
#                     |—— clean
#         |—— output
#                     |—— data
#                     |—— figures
#         |—— scripts
# NOTES : 
# version avec la calibration corrigée
# V3.0 tranféré en fonctions dans "fonction_phd_v2.0.R" et complété la calibration automatisée des sondes Odyssey et Hobo
# V3.1 ajout de la calibration avec SONDES BAROMÉTRIQUES dans FONCTIONS V3.1

# LEXIQUE :
{ 
  # SNH : sonde de niveau hydrostatique / synonymes : LL : level logger; sonde, probe
  # ll : level logger; sonde de niveau hydrostatique / synonymes : sonde, probe, SNH
  # NP : Nappe phréatique / synonymes : water table
  # tz : time zone, syn. fuseau horaire
  # cal.data, syn. connectivite/data/raw/level_logger_calibration_all.csv
  # patron universel d'appellation des fichiers de SNH : probe.uid_site.uid_datedextraction_probe.brand.csv
  # ms : MeteoStat
  # bs : barometric station
}
##########################################################################-

# Initialisation ----
# Librairies
# -> packages dans le code sourcé "fonctions_phd_v3.0.R"

# Données, dossier directeur fonctions et à charger directement
# .rs.restartR()
source("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/scripts/fonctions_phd_v3.1.R")
setwd("~/Documents/Doctorat/_R_Stats_PhD")

# fichiers "R data serialized" (RDS) à charger directement
# tidy.WTD.data <-readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.RDS") # issu du code ci-présent
# tidy.WTD.data.df et tidy.cal.data systématiquement produits avec dernière version de tidy.WTD.data, par concaténation des sous-listes

# Données brutes aux données calibrées et propres (tidy) ====
raw.ll.files.pre <- order.list(path = "connectivite/data/raw", pattern = "_odyssey|_hobo|barometric.station") # ordonner avec "barometric.station" en premier, pour ensuite aider à calibrer
raw.ll.files <- filter.raw.file(object.to.filter = raw.ll.files.pre[[1]], path.filtering.object = "connectivite/data/raw/level_logger_calibration_all.csv") # script "fonctions_phd_v3.0.R"
barometric.station <- raw.ll.files.pre[[2]]
cal.data.file.pre <- filter.raw.file(path.filtering.object = 
                                       "/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/raw/level_logger_calibration_all.csv") # © AlizéeGirard, script "fonctions_phd_vX.r"
cal.data.file <- uid.to.columns(file.to.restructure = cal.data.file.pre, type = "cal.data") # © AlizéeGirard, script "fonctions_phd_vX.r"
rm(list=c(ls(pattern='.pre')))

## boucle et préalables pour transformer les fichiers bruts ====
tidy.WTD.data <- list()
s = Sys.time() # compte le temps d'exécution
for (i in 1:length(raw.ll.files)) {
  # i <-74 # début des ODYSSEY
  # i <-7
  print(i)
  raw.ll.files[i]
  
  #### lecture et séparation des données et métadonnées
  raw.ll.files.i.init <- data.metadata(raw.ll.files[i]) # objet temporaire pour ajouter des lignes
  raw.ll.files.i <- metadata(raw.ll.files.i.init)
  
  files.uid.df <- files.uid(raw.ll.files.i.init); rm(raw.ll.files.i.init)
  
  #### ménage de la date et heure
  tz <- zone.tz("~Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectitite_PhD_Mergin_26nov24/Ecotone.restauration.zone.pt.shp")
  files.uid.df$tz_orig[i] <- tz
  
  ##### cal.bulleur.list.appendd (liste des cal.data, séparées en bulleur et en données de calibration Odyssey. Si autre marque, la l'élément [[2]] donne juste des NA)
  if (!grepl("barometric.station", raw.ll.files[i])) {
    cal.bulleur.list.appendd <- raw.to.clean_cal.data(cal.data.file, time.zone = tz) # import et nettoyage, bon format de date
  }
    
  #### créer le level.logger propre 
  # -> sondes de niveau hydrostatique : reste à concatener + calibrer
  # -> sondes de niveau hydrostatique : FIN (enregistrement en .RDS dans cette fonction, utilisation pour calibration des sondes du même site.UID)
  ll.clean <- raw.to.clean_ll(raw.ll.files.i[[1]])  # NOTES : début = installation du puits + 48h de rabattement de la NP / ou non, si puits intallé d'avance, dans quel cas inscrire début officiel - 24h) # fin = heure de retrait // note : données de date en format xlsx ça lit TOUT CROCHE, transformé en csv fonctionne bien
  
  if (!grepl("barometric.station", raw.ll.files[i])) {
    #### concaténation des périodes valides
    ll.cal.pre.i <- concatenate.ll(ll.clean)
    #### calibration des sondes
    tidy.WTD.data[[i]] <- clean.to.calibrated_ll(ll.cal.pre.i)
  }
  # rm(tz)
}
Sys.time()-s # temps d'exécution de la boucle
# warnings() # vérifier que les erreurs sont tjrs la meme affaire inutile -> incomplete final line, tenté de régler le problème, mais sans succès; 

## stockage des résultats (écrase version précédante) ====
# format R des tidy.WTD.data (une liste)
if("tidy.WTD.data.RDS" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { saveRDS(tidy.WTD.data, file = "connectivite/data/clean/tidy.WTD.data.RDS") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }

# format tableur LONG des tidy.WTD.data
extracted.list_data <- lapply(tidy.WTD.data, `[[`, 1) # tidy.WTD.data[[1]] -> data
# colnames_verif.data.tidy.WTD <- map(extracted.list_data, names) # tidy.WTD.data[[3]] -> verif.data
tidy.WTD.data.df.large <- do.call(rbind, extracted.list_data) # bind_rows identique à rbind, mais ne donne pas de message d'erreur
tidy.WTD.data.df <- tidy.WTD.data.df.large %>%
  pivot_longer(cols = contains("calibrated.value.cm"),
               names_to = "source_calib",
               values_to = "calibrated.value.cm") %>%
  mutate(source_calib = as.factor(gsub("calibrated.value.cm.", "", source_calib))) %>%
  dplyr::filter(!is.na(calibrated.value.cm))
{ ## enregistrement en RDS
  if("etiquette.source_calib.fr.txt" %in% list.files("connectivite/data/clean"))  {
    stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
  } else {
    etiquette.source_calib.fr <- 
      'c("blo" = "Mesure manuelle avec le bulleur",
"bs" = "Pression atmosphérique (sonde barométrique in situ)",
"lin" = "Interpolation linéaire (Dataflow Systems PTY Ltd.,2026),
"ms" = "Pression atmosphérique (MeteoStat.org, 2025")'
    writeLines(text = etiquette.source_calib.fr, con = "connectivite/data/clean/etiquette.source_calib.fr.txt")
  }
} ## enregistrement en RDS de la légende pour les graphiques et tableaux ----
# format RDS des tidy.WTD.data.df (formaté en wide-to-long)
if("tidy.WTD.data.df.RDS" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { saveRDS(tidy.WTD.data.df, file = "connectivite/data/clean/tidy.WTD.data.df.RDS") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }

# format tidy des cal.data (formaté en wide-to-long)
extracted.list_verif.data <- lapply(tidy.WTD.data, `[[`, 3) # tidy.WTD.data[[3]] -> verif.data
# colnames_verif.data <- map(extracted.list_verif.data, names) # tidy.WTD.data[[3]] -> verif.data
tidy.cal.data <- do.call(rbind, extracted.list_verif.data)    #   / archive (ligne suivante) : # tidy.cal.data <- tidy.cal.data %>% dplyr::filter(is.na(tidy.cal.data) %>% rowSums() != length(tidy.cal.data)) # enlever les lignes complètement composées de NA (tous les hobo en date du 7 janvier)
# format RDS des tidy.cal.data (formaté en wide-to-long)
if("tidy.cal.data.RDS" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { saveRDS(tidy.cal.data, file = "connectivite/data/clean/tidy.cal.data.RDS") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }

# 
# # format tidy des weather data
# extracted.list_verif.data.weather <- lapply(tidy.WTD.data, `[[`, 4) # tidy.WTD.data[[4]] -> verif.data
# tidy.weather.data <- do.call(rbind, extracted.list_verif.data.weather)    #   / archive (ligne suivante) : # tidy.cal.data <- tidy.cal.data %>% dplyr::filter(is.na(tidy.cal.data) %>% rowSums() != length(tidy.cal.data)) # enlever les lignes complètement composées de NA (tous les hobo en date du 7 janvier)
# tidy.weather.data.df <- tidy.weather.data %>%
#   pivot_longer(cols = contains("pressure"),
#                names_to = "source_calib.pressure",
#                values_to = "pressure.kPa") %>%
#   mutate(source_calib.pressure = as.factor(gsub(".*kPa.","", source_calib.pressure))) %>%
#   dplyr::filter(!is.na(pressure.kPa)) %>% 
#   pivot_longer(cols = contains("temp"),
#                names_to = "source_calib.temp",
#                values_to = "temperature.dC") %>%
#   mutate(source_calib.temp = as.factor(str_sub(source_calib.temp, -2))) %>%
#   str_sub("Inkerman25", -2) 
#   dplyr::filter(!is.na(temperature.dC))
# # format RDS des tidy.cal.data (formaté en wide-to-long)
# if("tidy.cal.data.RDS" %in% list.files("connectivite/data/clean"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
#   stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
# } else { saveRDS(tidy.cal.data, file = "connectivite/data/clean/tidy.cal.data.RDS") } # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }
