#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
#                           Site data, cleaning script
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #

# Description -------------------------------------------------------------
########################################################################## -
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2025-11-05
# Date mise à jour : 2026-10-15
# Pourquoi : Aggrégation des data_SITE.UID en quatre bases de données : names(env.data.sitewise[[1]]) = 
# "microtopo"            "canopy.peat.fauna"    "vegetation_trees.shr" "vegetation_lower.str";
# nettoyage des données de site;
# jonction des nouveaux quadrat.uid annuels avec les fichiers de consigne de laboratoire
#
# Structure :
# —— connectivite
#         |—— archive
#         |—— data
#                     |—— raw (ici feuilles numériques terrain comportant plusieurs onglets pour un site; data_SITE.UID)
#                     |—— extracted_raw **
#                     |—— clean
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
# if (!require("conflicted")) install.packages("conflicted") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("writexl")) install.packages("writexl") # écrire les excel
# if (!require("stringr")) install.packages("stringr") # gosser avec des suites de caractères, str_replace, [...]

# Données, dossier directeur fonctions et à charger directement
# .rs.restartR()
# setwd("/Users/Aliz/Library/CloudStorage/OneDrive-UniversitéLaval/_FIELD.LAB WORK 2025/Laboratoire/LOI")
setwd("~/Documents/Doctorat/_R_Stats_PhD/connectivite")
source("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/scripts/fonctions_phd_v3.2.R")

# COMMENT/UNCOMMENT HERE (next line)
# ============================================================================= /
# Mise à jour données récentes, tous data)_SITE.UID ----
# ============================================================================= /
# Retrait de la date du nom des feuilles de données (aaaammjj_data_SITE.UID.xlsx -> data_SITE.UID.xlsx)
path.rename <- "~/Documents/Doctorat/_R_Stats_PhD/connectivite/data"
if (length(list.files(path = path.rename, pattern = "_data")) != 0) { # s'il y a un fichier correspondant au motif (pattern)
  data_SITE.UID <- list.files(path = path.rename, pattern = "_data") |>
    basename() |> # basename(conserve le "path" dans le nom, en arrière-plan)
    str_remove("\\d+?(?=_)_")
  ## explications sur le regex :
  ## \\d+? : cherche des chiffres (\\d), une ou + (+) fois les charactères qui vont suivre (positive lookahead pour le underscore; (?=_))
  ## mais de façon paresseuse (le "?") soit LE MOINS DE FOIS POSSIBLE
  ## (?=_) : "positive lookahead", cherche avant la 1ere barre en bas, arrête la recherche (côté paresseux);
  ## enlève la barre en bas qui a causé l'arrêt de la recherche aussi
  file.rename(from = list.files(path = path.rename, pattern = "_data", full.names = TRUE), to = data_SITE.UID)
}

# ============================================================================= /
# Filtre global et extraction des data_SITE.UID (\filter.raw.file()), nettoyage et enregistrement en RDS ----
# ============================================================================= /
# listes de données et filtration
raw.env.data.pre <- list.files(path = "data/raw", pattern = "data_", full.names = T) # mettre dans "pattern" tous les ID de SNH listés dans l'objet SNH
raw.env.data <- raw.env.data.pre[grep("[$]", raw.env.data.pre, invert = T)] # fichiers cachés (p.ex. : "~$data_BRNTC.xlsx") à retirer du vecteur

env.data.sitewise <- list()
## boucle pour chaque site, agglomérer les onglets pertinents ----
for (i in 1:(length(raw.env.data))) {
  print(i)
  # i<-1
  
  raw.env.data.i <- read_excel_sheets_NO.BUGS(raw.env.data[i]) # script "fonction_phd_vX" (AG, 2025+)
  # LONG à cause de la conversion de TOUTE les cellules en texte
  ## caduque tests : trouver problème 29 juill QGIS (microtopo)
  # unique(raw.env.data.i[[1]]$perm.plot.uid.NOmicrotopo.aaaa) # ok tout y est... (certains UID disparaîssent dans QGIS)
  # table(raw.env.data.i[[1]]$lat.garmin.dd) # ici juste des décimales en virgule ?
  # table(raw.env.data.i[[1]]$long.garmin.dd) # PROBLÈME ici : ya des virgules ET des points décimaux... dû à l'import...
  
  for (z in names(raw.env.data.i)) {
    raw.env.data.i[[z]] <- raw.env.data.i[[z]] %>%
      mutate(across(everything(), as.character))
    # colnames(raw.env.data.i$microtopo) <- as.character(colnames(raw.env.data.i$microtopo))
  }
  env.data.sitewise[[i]] <- env.data.sitewise[[i]] <- raw.env.data.i # liste (de site) contenant une liste (d'onglets pertinents)
}
print("Avertissement OK : rencontre des 'NA' et 'NO' et les transforme en vrai NA")

# ============================================================================= /
# Décision d'archivage de version existante ----
# ============================================================================= /
# explication : si les fichiers N'existent PAS déjà, écrire les fichiers
# s'ils existent, aller vérifier ce qu'en faire, et écraser/réécrire en conservant la version précédante au besoin (uncomment suivant)
for (n in names(env.data.sitewise[[1]])) { # n c'est chaque feuille dans env.data.sitewise // [[1]] pas grave lequel des site, car ils comportent les mm données
  j <- which(n == names(env.data.sitewise[[1]]))
} # index pour le path et nom de fichier .xslx OU .csv
# if(paste0(names(env.data.sitewise[[1]])[j], ".xlsx") %in% list.files("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw"))  {
#   stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.") }
if(paste0(names(env.data.sitewise[[1]])[j],".csv") %in% list.files("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw"))  {
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.") }

# ============================================================================= /
# Aggrégation des onglets du même nom en base de données tous sites ----
# ============================================================================= /
## extraire tous les noms de colonnes ----
## utilité : pour examiner les problèmes (warnings), pour définir les classes
## Merci à Google IA pour l'aide dans PURRR (24/07/2026).
# lapply(env.data.sitewise, class) # j'ai quel type d'objet ?
# liste_aplatie <- flatten(env.data.sitewise) # applatir (éliminer un niveau liste de liste -> liste longue)
# mapped_df <- map(liste_aplatie, colnames) |> # extraire tous les colnames
#   enframe(name = "table_name", value = "column_name") |> # inscrire en colonne 1 le nom de la liste
#   unchop(column_name) |> # la colonne "column_name' précédante contient plein d'info illisible (un paquet de column names trouvés), on doit la déployer (unchop)
#   distinct()
## enregistrer (utilitaire)
# saveRDS(mapped_df, file = "/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/mapped_df.RDS")
# # 2027-10-07 : enregistrer (ajouter les classes désirées, objet utilisé dans la fonction "class.selection" du script ""fonctions_phd_vX.R" (AG, 2025+)")
# if (file.exists("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/mapped_df_class.xlsx")) {
#   stop("NE PAS ÉCRASER") # donner accès à R pour modification
# }
# readr::write_excel_csv2(x = mapped_df, file = "/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/mapped_df_class.csv", na = "")

## aggrégation ----
env.data.merged <- list()
env.data.merged.sf <- list() # pour objets-sf seulement (incompatibles avec .csv ou .xlsx, enregistrer en .RDS ou .gpkg)
for (n in names(env.data.sitewise[[1]])) { # n c'est chaque feuille dans env.data.sitewise // [[1]] pas grave lequel des site, car ils comportent les mm données
  # 1 à 6 ce sont mes 6 sites
  # n <- names(env.data.sitewise[[1]])[2]
  print(n)
  if(n == "microtopo") {
    print("Attention, très LONG car microtopo est un tableur avec beaucoup de lignes") }
  
  env.data.n <- bind_rows(env.data.sitewise[[1]][[n]],
                          env.data.sitewise[[2]][[n]],
                          env.data.sitewise[[3]][[n]],
                          env.data.sitewise[[4]][[n]],
                          env.data.sitewise[[5]][[n]],
                          env.data.sitewise[[6]][[n]])
  # autant de ligne que DE SITE sinon les sites ultérieurs vont manquer dans les données
  # caduque # env.data.n <- bind_rows(cat(paste0("env.data.sitewise[[", n, "]][[n]]"), ")")) # AJUSTER CETTE FORMULE au besoin
  env.data.n.pre <- env.data.sitewise %>%
    purrr::map(n) %>%
    dplyr::bind_rows()
  
  # créer l'objet
  env.data.n.filterd <- filter.raw.file(env.data.n.pre) # issu du script "fonctions_phd_vX.R" (AG, 2025+)
  
  ### nettoyage ----
  #### classes ----
  env.data.n.class <- class.selection(env.data.n.filterd) |> 
    select(any_of(c("site.uid", "perm.plot.uid.NO.quadrat.aaaa", "trmnt.uid.rel.dist.quadrat.aaaa")), everything())
  
  # #### uid.to.columns ----
  env.data.n.class.uid.to.columns <- uid.to.columns(file.to.restructure = env.data.n.class, type = "other") |> # LONG; © AlizéeGirard, script "fonctions_phd_vX.r" (aide GGIA)
    select(any_of(c("site.uid", "chapter", "type", "perm.plot.replicate", "perm.plot.type", "relative.distance", "quadrat.letter",
                    "year", "perm.plot.uid.NO.quadrat.aaaa", "trmnt.uid.rel.dist.quadrat.aaaa")), everything())
  
  # #### col.order ----
  env.data.n.class.uid.to.columns.ordrd <- order.cols(env.data.n.class.uid.to.columns) |> 
    select(any_of(c("site.uid", "perm.plot.uid.NO.quadrat.aaaa", "trmnt.uid.rel.dist.quadrat.aaaa", "trmnt.uid.rel.dist.aaaa")), everything())
  
  ### liste finale ----
  env.data.n <- env.data.n.class.uid.to.columns.ordrd # nom final (stratégie : changer juste ici et pas en-dessous à plusieurs autres endroit)
  env.data.merged[[n]] <- env.data.n # liste (de feuillets) contenant les données de chaque site concatennés ensemble
  
  ## enregistrements ----
  ### .RDS ----
  # stratégie : les .csv et le env.data.merged (créé sous la boucle) peuvent-être visualisés MAIS PAS MODIFIÉS,
  #             TOUTES modifications à la source seulement, puis rouler
  #             et effectuer les analyses sur les .rds
  saveRDS(env.data.n, file = paste0("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/", n,".RDS"))
  
  ### .GPKG (sf-objects) SI NÉCESSAIRE (long & LOURD en Mo) ----
  ### COMMENT/UNCOMMENT HERE (next line)
  # #### création
  # if (T %in% grepl(".*long.*", colnames(env.data.n))) {
  #   col.index <- c(grep(".*long.garmin*", colnames(env.data.n)), grep(".*lat.garmin*", colnames(env.data.n)))
  #   coord.names <- colnames(env.data.n)[col.index[1:2]]
  #
  #   env.data.n.sf <- env.data.n %>%
  #     st_as_sf(coords = coord.names, na.fail = F) %>%
  #     st_set_crs("EPSG:4326")
  #   # √ EPSG:4326 avec le GPS Garmin / possibles erreurs, arranger si deviennent pertinentes pour les analyses -> certaines ont été extraites de QGIS avec la projection précise associée à la province/MTM
  # }
  # #### mise en liste
  # env.data.merged.sf[[n]] <- env.data.n.sf
  # #### enregistrement .GPKG dans ~/desktop/QGIS/...
  # if (T %in% grepl(".*long.*", colnames(env.data.n))) {
  #   st_write(env.data.merged.sf[[n]],
  #            paste0("/Users/Aliz/Desktop/QGIS/_Connectivite_PhD/Mergin/_Connectivite_PhD_Mergin_20260824/_Connectitite_PhD_Mergin_20260824/sf_", n, ".gpkg"))
  # }
  #### COMMENT/UNCOMMENT TO HERE (previous line)
  
  ### .CSV "extracted_raw" SI NÉCESSAIRE (visualisation seulement)  ----
  ### COMMENT/UNCOMMENT HERE (next line)
  path <- paste0("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/", n,".csv")
  if (file.exists(path)) Sys.chmod(path, mode = "0755") # donner accès à R pour modification
  readr::write_excel_csv2(x = env.data.merged[[n]], file = path, na = "")
  #### mise en lecture seule
  Sys.chmod(path, mode = "0444", use_umask = FALSE) # "system change mode"
  # Options : 0444 : lecture seule; 0666 : lecture & écriture
  #### COMMENT/UNCOMMENT TO HERE (previous line)
  
  # x.ARCHIVES ----
  # ARCHIVE : exporter en .xlsx / cause problèmes de type de données et impossible d'ajouter directement dans QGIS
  #   writexl::write_xlsx(
  #     env.data.merged[[n]],
  #     path = paste0(
  #       "/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/",
  #       n,
  #       ".xlsx"))
  # exporter en .csv
  # ARCHIVE : exporter en .csv / cause problèmes de type de données et impossible d'ajouter directement dans QGIS
  # readr::write_delim(
  #     env.data.merged[[n]],
  #     file = paste0(
  #       "/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/",
  #       n,
  #       ".csv"),
  #     encoding = "UTF-8-BOM",
  #     delim = ";") # encodage FRANÇAIS pour les commentaires
  # ARCHIVE : ne fonctionne pas car incapable d'appliquer le format UTF-8-BOM
  # write.table(
  #   x = env.data.merged[[n]],
  #   file = paste0(
  #     "/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/",
  #     n,
  #     ".csv"
  #   ),
  #   sep = ";",          # Force le point-virgule entre les colonnes (comme le csv2)
  #   dec = ",",          # Conserve la virgule pour les décimales
  #   row.names = FALSE,  # Évite d'ajouter des numéros de lignes parasites
  #   na = "",            # Remplace les NA par du vide propre
  #   fileEncoding = "UTF-8" # Assure la compatibilité des accents sur votre Mac
  # )
  # x.fin ----
}
print("Avertissement (microtopo) OK : il y a des lignes vides. Juste m'assurer d'avoir copié la coordonnée dans chaque ligne d'élévation-distance.")
# enregistrement pour ouverture et visualisation rapide. Pour modifier, passer pas la boucle et faire chaque env.data.n individuellement
saveRDS(env.data.merged, file = "/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/env.data.merged.RDS")
### COMMENT/UNCOMMENT TO HERE (previous line)

# ============================================================================= /
# Nettoyage (type colonnes, wide-to-long, etc.) ----
# ============================================================================= /
# ouverture rapide de l'état actuel des fiochiers data_site.uid
# env.data.merged <- readRDS("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/env.data.merged.RDS")

## microtopo ----
# microtopo <- readRDS("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/microtopo.RDS")


## canopy.peat.fauna ----
### visualisation pour définir comment l'ouvrir ----
# ne pas ouvrir en .csv ça chie
# canopy.peat.fauna <- read.csv("data/extracted_raw/canopy.peat.fauna.csv", 
#                                             sep = ";", quote = "\"'",
#                                             dec = ",", fileEncoding = "UTF-8-BOM") # bon formatage, puis, enregistrement en RDS
canopy.peat.fauna <- readRDS("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/canopy.peat.fauna.RDS")
colnames(canopy.peat.fauna)
str(canopy.peat.fauna)

### modifications ----
# 2026-10-12 retiré les colonnes peat.sample (remplies uniquement en laboratoire). Fichier de consigne de données de laboratoire : peat.sample ou
# tree.core (le plus récent), dans : /Users/Aliz/Library/CloudStorage/OneDrive-UniversitéLaval/_FIELD_LAB.WORK/_Feuilles_données_pas_local/

#### tree.core.metadata ----
# à chaque année, ajouter les nouvelles lignes d'UID sans écraser les données déjà remplies
# obtenir les nouvelles données à partir de canopy.peat.fauna
tree.core.metadata <- canopy.peat.fauna |>
  select(contains(c("perm.plot.uid", "site.uid", "chapter", "type", "perm.plot.replicate", "perm.plot.type", "relative.distance",
                    "quadrat.letter", "year", "tree.core_sp", "tree.core_uid")))

##### mise à jour données récentes, peat.xlsx ----
# retrait de la date du nom des feuilles de données (aaaammjj_peat.xlsx -> peat.xlsx)
if (length(list.files(path = "data/", pattern = "_ring.width_all")) != 0) { # s'il y a un fichier correspondant au motif (pattern)
  peat_file <- list.files(path = "data/", pattern = "_peat") |>
    basename() |> # basename(conserve le "path" dans le nom, en arrière-plan)
    str_remove("\\d+?(?=_)_") # enlève la date; explications sur le regex : voir section Mise à jour [...] ci-haut
  file.rename(from = list.files(path = "~/Documents/Doctorat/_R_Stats_PhD/connectivite/data", pattern = "_peat", full.names = TRUE), to = peat_file)
}
# lecture fichier brut
ring.width.all <- read.xlsx("data/raw/ring.width_all.xlsx")
##### join avec le tree.core ----
tree.core_join <- full_join(tree.core.metadata, ring.width.all)
# visualisation / lecture fichier
tree.core_join
colnames(tree.core_join)
str(tree.core_join)
##### enregistrement dans extracted_raw ----
# pour toutes autres modification, passer au script "data_tree.core_all.R"
# saveRDS(tree.core_join, file = "data/extracted_raw/tree.core_join.RDS")
# m'assurer que les BRNTC.[...].NA.2025 ou .pre suivent vu les changements dans quadrat.uid en oct 2026

#### peat.sample.metadata ----
# à chaque année, ajouter les nouvelles lignes d'UID sans écraser les données déjà remplies
# obtenir les nouvelles données à partir de canopy.peat.fauna
peat.sample.metadata <- canopy.peat.fauna |>
  select(c("perm.plot.uid", "perm.plot.uid", "site.uid", "chapter", "type", "perm.plot.replicate", "perm.plot.type", 
           "relative.distance", "quadrat.letter", "year", , contains("peat.sample_")))
##### mise à jour données récentes, peat.xlsx ----
# retrait de la date du nom des feuilles de données (aaaammjj_peat.xlsx -> peat.xlsx)
if (length(list.files(path = "data/", pattern = "_peat")) != 0) { # s'il y a un fichier correspondant au motif (pattern)
  peat_file <- list.files(path = "data/", pattern = "_peat") |>
    basename() |> # basename(conserve le "path" dans le nom, en arrière-plan)
    str_remove("\\d+?(?=_)_") # enlève la date; explications sur le regex : voir section Mise à jour [...] ci-haut
  file.rename(from = list.files(path = "~/Documents/Doctorat/_R_Stats_PhD/connectivite/data", pattern = "_peat", full.names = TRUE), to = peat_file)
}
# lecture fichier brut (mettre à jour)
peat <- read.xlsx("data/raw/peat.xlsx")
##### join avec le peat.sample ----
peat.sample_join <- full_join(peat.sample.metadata, peat)
# visualisation / lecture fichier
peat.sample_join
colnames(peat.sample_join)
str(peat.sample_join)
##### enregistrement dans extracted_raw ----
# pour toutes autres modification, passer au script "data_peat_all.R"
# saveRDS(peat.sample_join, file = "data/extracted_raw/peat.sample_join.RDS")


## vegetation_trees.shr, visualisation ----
vegetation_trees.shr <- readRDS("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/vegetation_trees.shr.RDS")
colnames(vegetation_trees.shr)
str(vegetation_trees.shr)


## vegetation_lower.str, visualisation ----
vegetation_lower.str <- readRDS("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/vegetation_lower.str.RDS")
colnames(vegetation_lower.str)
str(vegetation_lower.str)


# ============================================================================= /
# Enregistrement final ----
# ============================================================================= /
# voir scripts data_canopy.R, data_peat_all.R, data_tree.core_all.R, data_vegetation_v1.0.R
