#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
#              Vegetation data, cleaning script (following data_sites_all.R)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #

# Description -------------------------------------------------------------
########################################################################## -
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2026-10-15
# Date mise à jour : 
# Pourquoi : nettoyage vers format final tidy des onglets "vegetation_trees.shr" "vegetation_lower.str"
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
vegetation_trees.shr.pre <- readRDS("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/vegetation_trees.shr.RDS")
colnames(vegetation_trees.shr)
str(vegetation_trees.shr)

vegetation_lower.str.pre <- readRDS("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/vegetation_lower.str.RDS")
colnames(vegetation_lower.str)
str(vegetation_lower.str)

# ============================================================================= /
# Mise à jour données récentes, tous data_SITE.UID ----
# ============================================================================= /
# Non-applicable : faire à partir de script data_sites_all.R pour joindre métadonnées et nouveaux quadrat.uid

# ============================================================================= /
# Nettoyage ----
# ============================================================================= /
## idées (à faire) ----
# - ggIA me recommande de transformer en wide-to-long immédiatement
# - joindre les lower et higher stata d'abord...
# - joindre et utiliser NOM LATIN plutôt que l'abbr
# - faire correspondre circ.cm.NO <-> diam.cm.NO avec circonférence = π * diamètre 
#   - m'assurer de vérifier les incongruïtés possibles... tape en pouces, etc.
# - ADDITIONNER LES INDIVIDUS COMME PROTOCOLE COUPE ARBRES : semis dans nb ind, dhp dans diam.. -> comptés séparément
# - trouver comment gérer les données emboîtées (voir cours de stats); hypothèse de mémoire :
#   - conserver la structure emboîtée et faire un join + une transformation wide-to-long
# 

## aggrégation des df strates ----

intersect(colnames(vegetation_trees.shr), colnames(vegetation_lower.str))
# déjà le dom.sp.NO et le measure.status sont respectifs à la base de donnée, donc PAS UNE CLÉ DE JOINTURE

## cols "nb.ind.1", "diam.cm.1", "circ.cm.1" ----
# actions séquentielles (?), coder en fonction éventuellement
### comptage du nombre d'arbres ----
# TEMP à jumeler/transférer dans fonctions_phd_vX.R
# ADDITIONNER LES INDIVIDUS COMME PROTOCOLE COUPE ARBRES : semis dans nb ind, dhp dans diam.. -> comptés séparément donc additionner
# stratégie: 
#  -> utiliser une syntaxe purrr pour parcourir toutes les colonnes contenant "diam.cm" "circ.cm."
#  -> compter le nombre d'élément entre les ; jusqu'au NA ou ERREUR
#  -> écrire résultat (nombre d'ind) dans nb.ind.NO
colnames(vegetation_trees.shr)
# vegetation_trees.shr.tree.count <- vegetation_trees.shr.pre |> 
  
### circ.cm.NO <-> diam.cm.NO ----
# circonférence (cm) = π * diamètre (cm)


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
