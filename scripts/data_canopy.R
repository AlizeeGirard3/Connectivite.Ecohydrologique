#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
#              Canopy data, cleaning script (following data_sites_all.R)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #

# Description -------------------------------------------------------------
########################################################################## -
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2026-10-15
# Date mise à jour : 
# Pourquoi : nettoyage vers format final tidy de l'onglet "canopy.peat.fauna" (dont peat est extrait et éliminé, voir script data_sites_all.R)
#
# Structure :
# —— connectivite
#         |—— archive
#         |—— data
#                     |—— raw (ici feuilles numériques terrain comportant plusieurs onglets pour un site; data_SITE.UID)
#                     |—— extracted_raw ** d'ici initialement
#                     |—— clean ** vers ici propre final
#         |—— output
#                     |—— data
#                     |—— figures
#         |—— scripts
# NOTES : 
# ** extracted_raw / data_SITE.UID extraits en une base de données par onglet (4), tous site confondus (script "data_sites_all"), nettoyé sommairement
# Section Enregistrement final : mise dans data/clean
# toujours modifier à la source et rerouler séquentiellement les scripts (voir /Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/_README.md)
########################################################################## -

# ============================================================================= /
# Initialisation ----
# ============================================================================= /
# Librairies
# au besoin, outre celles chargées dans le script fonctions_phd_vX.R

# Données, dossier directeur fonctions et à charger directement
# .rs.restartR()
# setwd("/Users/Aliz/Library/CloudStorage/OneDrive-UniversitéLaval/_FIELD.LAB WORK 2025/Laboratoire/LOI")
setwd("~/Documents/Doctorat/_R_Stats_PhD/connectivite")
source("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/scripts/fonctions_phd_v3.2.R")

# Import données (pour toute modification à la source: data_sites_all.R)
# canopy.peat.fauna <- read.csv("data/extracted_raw/canopy.peat.fauna.csv", 
#                                             sep = ";", quote = "\"'",
#                                             dec = ",", fileEncoding = "UTF-8-BOM") # bon formatage, puis, enregistrement en RDS
canopy.peat.fauna <- readRDS("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/canopy.peat.fauna.RDS") |> 
  select(!c(contains("peat.sample_"), tree.core_))
colnames(canopy.peat.fauna)
str(canopy.peat.fauna)

# ============================================================================= /
# Mise à jour données récentes, tous data_SITE.UID ----
# ============================================================================= /
# Non-applicable : faire à partir de script data_sites_all.R pour joindre métadonnées et nouveaux quadrat.uid

# ============================================================================= /
# Nettoyage ----
# ============================================================================= /
# idées (à faire)
# supprimer colonnes "peat"
# ouverture de canopée pts/96 -> % ?
#

# ============================================================================= /
# Enregistrement final ----
# ============================================================================= /



# ____________ CHANTIER - CHANTIER - CHANTIER - CHANTIER____________






# if(paste0(names(env.data.sitewise[[1]])[j],".csv") %in% list.files("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw"))  {
#   stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.") }

# for (n in names(env.data.sitewise[[1]])) {
#   # saveRDS(, file = paste0("data/clean/", n, ".Rds"))
#   
# }


# si fichier n'existe pas déjà :
# filter la base de données, recréer des fichier distincts par site.uid, stocker dans data/clean
# sinon, arrêt et avertissement
URLs.list <- vector()
site.uids <- unique(tidy.weather.data.res$site.uid.ms)
for (site in 1:length(site.uids)) {
  URLs.list[site] <- paste0("meteoStat.data.hourly.res.", unique(tidy.weather.data.res$site.uid.ms)[site], ".csv")
}
if(any(URLs.list %in% list.files("connectivite/data/clean")))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { 
  for (j in 1:length(site.uids)) {
    # j<-1
    tidy.weather.data.j <- tidy.weather.data.originales %>% 
      dplyr::filter(site.uid.ms == site.uids[j]) %>% 
      select(-site.uid.ms)
    write.csv(tidy.weather.data.j,  paste0("connectivite/data/clean/meteoStat.data.hourly.", site.uids[j], ".res.csv"), row.names = FALSE)
  }
}

## version originale ----
URLs.list <- vector()
site.uids <- unique(tidy.weather.data.originales$site.uid.ms)
for (site in 1:length(site.uids)) {
  URLs.list[site] <- paste0("meteoStat.data.hourly.", unique(tidy.weather.data.originales$site.uid.ms)[site], ".csv")
}
if(any(URLs.list %in% list.files("connectivite/data/clean")))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else {
  for (j in 1:length(site.uids)) {
    # j<-1
    tidy.weather.data.j <- tidy.weather.data.originales %>%
      dplyr::filter(site.uid.ms == site.uids[j]) %>%
      select(-site.uid.ms)
    write.csv(tidy.weather.data.j,  paste0("connectivite/data/clean/meteoStat.data.hourly.", site.uids[j], ".csv"), row.names = FALSE)
  }
}
