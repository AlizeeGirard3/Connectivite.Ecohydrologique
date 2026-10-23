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
#
# LEXIQUE
# sp.NO : numéro d'espèce dominante (pas en ordre d'importance)
#
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


# ============================================================================= /
# Expolorations initiales ----
# ============================================================================= /
# pour toute modification à la source: data_sites_all.R
## vegetation_trees.shr ----
### pivot longer ----
vegetation_trees.shr.pre <- readRDS("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/vegetation_trees.shr.RDS") |> 
  mutate(source.df.tab = "trees.shr") |>  # onglet/feuille d'origine de la base de données "data/raw/data_SITE.UID.xlsx"
  # les renames sont caduques si je pivote vers format long d'office avant l'aggrégation
  rename(trees.shr_measure.status = measure.status) |>
  rename_with(~ paste0("trees.shr_", .x, recycle0 = TRUE),
              starts_with("dom.")) |>
  pivot_longer(cols = matches("(.*)\\.[1-5]$"),
               names_to = c(".value", "sp.NO"), # code de GGIA pour cette partie précise, aide pour le concept général
               names_pattern = "(.*)\\.([1-5]$)") # ou : (.*)\\.(\\d+)
# explication du names_pattern : '.*' opérateur "glouton"/"greedy" qui capture le plus d'affaires possible
# puis, \\. signifie "escape point, donc point comme élément typographique plustôt que comme symbole codé du regex
# les 3 groupes de parenthèses indiquent les 2 parties à séparer
# le \\d+ veut dire "tout ce qui se trouve après, fonctionne aussi en restreignant aux chiffres ([1-9]$) où le $ signifie FIN de la chaîne de charactères
colnames(vegetation_trees.shr.pre)
str(vegetation_trees.shr.pre)

### nettoyage ----
# uniformisation des NA vrais, "NA", 0 dans 3 colonnes et retrait si les 3 colonnes n'avaient aucune donnée (NA)
vegetation_trees.shr.pre_cleand <- vegetation_trees.shr.pre |> 
  mutate(across(c(nb.ind, diam.cm, circ.cm), ~ ifelse(.x == 0 | .x == "NA", NA, .x))) |> 
  dplyr::filter(!if_all(c(nb.ind, diam.cm, circ.cm), is.na))  # enlève beaucoup de lignes inutiles OK

### count trees ----
for(n in 1:nrow(vegetation_trees.shr.pre_cleand)) {
  print(paste("ligne =", n))
  # vérification 1 : fin = ;NA
  if(grepl(".*;NA", vegetation_trees.shr.pre_cleand$diam.cm[n], ignore.case = T) == FALSE) {
    stop(paste0(c("Le nombre d'arbres ne fini pas par 'NA', le compte serait erroné. Corriger les donnéres originales, quadrat.UID : ", vegetation_trees.shr.pre_cleand$quadrat.uid[n])))
  }
  tree.count.n <- length(unlist(gregexec(";", vegetation_trees.shr.pre_cleand$diam.cm[n]))) # logique de compter les ";" : j'ai mis NA à la fin pour que chaque ARBRE ait un ";" à sa droite (1:1)
 
   # vérification 2 : soit données dans colonne DHP.cm SOIT dans circ.cm
  verif.2 <- vegetation_trees.shr.pre_cleand |> 
    dplyr::filter(is.na(nb.ind))
  if() {
    stop(paste0(c("Le nombre d'arbres ne fini pas par 'NA', le compte serait erroné. Corriger les donnéres originales, quadrat.UID : ", vegetation_trees.shr.pre_cleand$quadrat.uid[n])))
  }
  
  print(paste("nombre =", tree.count.n))
  if(!is.na(vegetation_trees.shr.pre_cleand$nb.ind[n]) && !is.na(vegetation_trees.shr.pre_cleand$diam.cm[n])) {
    stop("La valeur de la colonne nb.ind.# n'est pas vide, elle sera écrasée. Vérifier méthodologie, corriger les données orinigales.")
  }
  vegetation_trees.shr.pre_cleand$nb.ind <- tree.count.n
}


# vegetation_trees.shr.pre_count <- vegetation_trees.shr.pre_cleand |> 
#   mutate(compte.arbres = case_when(
#     grep(";",   diam.cm),
#   )

## vegetation_lower.str ----
vegetation_lower.str.pre <- readRDS("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/vegetation_lower.str.RDS") # |> 
  # mutate(source.df.tab = "lower.str") |> # onglet/feuille d'origine de la base de données "data/raw/data_SITE.UID.xlsx"
  # les renames sont caduques si je pivote vers format long d'office avant l'aggrégation
  # rename(trees.shr_measure.status = measure.status) |> 
  # rename_with(
  #   ~ paste0("lower.str_", .x, recycle0 = TRUE),
  #   starts_with("dom.")) |> 
  # pivot_longer()
colnames(vegetation_lower.str.pre)
str(vegetation_lower.str.pre)


# noms communs
intersect(colnames(vegetation_trees.shr.pre), colnames(vegetation_lower.str.pre))
# déjà le dom.sp.NO et le measure.status sont respectifs à la base de donnée, donc PAS UNE CLÉ DE JOINTURE


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
# - joindre et utiliser NOM LATIN plutôt que l'abbr -> NON l'abbr = utile dans les ordinations !
# - faire correspondre circ.cm.NO <-> diam.cm.NO avec circonférence = π * diamètre 
#   - m'assurer de vérifier les incongruïtés possibles... tape en pouces, etc.
# - ADDITIONNER LES INDIVIDUS COMME PROTOCOLE COUPE ARBRES : semis dans nb ind, dhp dans diam.. -> comptés séparément
# - trouver comment gérer les données emboîtées (voir cours de stats); hypothèse de mémoire :
#   - conserver la structure emboîtée et faire un join + une transformation wide-to-long
# 
## analyses et comment m,y rendre
# • ggIA 16 oct 2026 :
# L'Analyse en Coordonnées Principales (PCoA / MDS) avec l'indice de Gower : L'indice de distance de Gower est l'un des rares indices 
# capables de gérer des variables de natures différentes (quantitatives, qualitatives) et d'ignorer mathématiquement les NA lors du calcul 
# des distances entre sites, sans supprimer la ligne. Vous pouvez utiliser la fonction vegdist(..., method = "gower", na.rm = TRUE) du package vegan.
# -> je préfère qqchose de plus intuitif, je vais analyser les strates SÉPARÉMENT...

## aggrégation des df strates ----
# ATTENTION, LES DONNÉES SONT DE NATURE DIFFÉRENTE : -> lower.str = %recouvrement vs -> trees.shr = nb et diamètre
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
