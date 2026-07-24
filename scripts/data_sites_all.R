#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
#                           Site data, cleaning script
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #

# Description -------------------------------------------------------------
########################################################################## -
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2025-11-05
# Date mise à jour : 2026-07-02
# Pourquoi : Aggrégation des data_SITE.UID en quatre bases de données : names(env.data.sitewise[[1]]) = 
# "microtopo"            "canopy.peat.fauna"    "vegetation_trees.shr" "vegetation_lower.str"
# et nettoyage des données de site

# Structure :
# —— connectivite
#         |—— archive
#         |—— data
#                     |—— raw (ici feuilles numériques terrain comportant plusieurs onglets pour un site; data_SITE.UID)
#                     |—— extracted_raw <- data_SITE.UID extraits en une base de données par onglet (4), tous site confondus (script "data_sites_all")
#                     |—— clean
#         |—— output
#                     |—— data
#                     |—— figures
#         |—— scripts
# NOTES : 
# 
########################################################################## -

# ============================================================================= /
# Initialisation ----
# ============================================================================= /
# Librairies
if (!require("tidyverse")) install.packages("tidyverse") # gosser avec des suites de caractères, str_replace, [...]
# if (!require("conflicted")) install.packages("conflicted") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("readxl")) install.packages("readxl") # lire les excel
# if (!require("openxlsx")) install.packages("openxlsx") # PROBLÈMES DANS QGIS (importer points en lat, long)/ lire/écrire les excel
if (!require("writexl")) install.packages("writexl") # écrire les excel
# if (!require("stringr")) install.packages("stringr") # gosser avec des suites de caractères, str_replace, [...]

# Données, dossier directeur fonctions et à charger directement
# .rs.restartR()
# setwd("/Users/Aliz/Library/CloudStorage/OneDrive-UniversitéLaval/_FIELD.LAB WORK 2025/Laboratoire/LOI")
setwd("~/Documents/Doctorat/_R_Stats_PhD/connectivite")
source("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/scripts/fonctions_phd_v3.2.R")

# # Retrait de la date du nom des feuilles de données (aaaammjj_data_SITE.UID.xlsx -> data_SITE.UID.xlsx)
data_SITE.UID <- list.files(path = "~/Documents/Doctorat/_R_Stats_PhD/connectivite/data", pattern = "_data") |>
  basename() |> # basename(conserve le "path" dans le nom, en arrière-plan)
  str_remove("\\d+?(?=_)_")
# explications sur le regex :
# \\d+? : cherche des chiffres (\\d), une ou + (+) fois les charactères qui vont suivre (positive lookahead pour le underscore; (?=_))
# mais de façon paresseuse (le "?") soit LE MOINS DE FOIS POSSIBLE
# (?=_) : "positive lookahead", cherche avant la 1ere barre en bas, arrête la recherche (côté paresseux); 
# enlève la barre en bas qui a causé l'arrêt de la recherche aussi
# file.rename(from = list.files(path = "~/Documents/Doctorat/_R_Stats_PhD/connectivite/data", pattern = "_data", full.names = TRUE), to = data_SITE.UID)
# ============================================================================= /
# Filtre global (\filter.raw.file()) et enregistrement en RDS ----
# ============================================================================= /
# listes de données et filtration
raw.env.data.pre <- list.files(path = "data/raw", pattern = "data_", full.names = T) # mettre dans "pattern" tous les ID de SNH listés dans l'objet SNH
raw.env.data <- raw.env.data.pre[grep("[$]", raw.env.data.pre, invert = T)] # fichiers cachés (p.ex. : "~$data_BRNTC.xlsx") à retirer du vecteur

# consigne de données
env.data.sitewise <- list()

## boucle pour chaque site, agglomérer les onglets pertinents ----
for (i in 1:(length(raw.env.data))) {
  print(i)
  # i<-2
  # nom des onglets du classeur .csv et sélection des pertinents
  sheets.pre <- readxl::excel_sheets(raw.env.data[i])
  sheets <- subset(sheets.pre,!grepl(pattern = "À FAIRE|sp_code|validation|READ ME|cad.", sheets.pre)) # keeps any other sheet
  
  raw.env.data.i <- read_excel_sheets(raw.env.data[i]) # script "fonction_phd_vX"
  for (z in names(raw.env.data.i)) {
    raw.env.data.i[[z]] <- raw.env.data.i[[z]] %>% 
      mutate(across(everything(), as.character))    
    
    # colnames(raw.env.data.i$microtopo) <- as.character(colnames(raw.env.data.i$microtopo))
  }
  env.data.sitewise[[i]] <- raw.env.data.i # liste (de site) contenant une liste (d'onglets pertinents)
}
# Extraire tous les noms de colonnes, pour examiner les problèmes (warnings). Merci à Google IA pour l'aide dans PURRR (24/07/2026).
# lapply(env.data.sitewise, class) # j'ai quel type d'objet ?
# liste_aplatie <- flatten(env.data.sitewise) # applatir (éliminer un niveau liste de liste -> liste longue)
# mapped_df <- map(liste_aplatie, colnames) |> # extraire tous les colnames
#   enframe(name = "table_name", value = "column_name") |> # inscrire en colonne 1 le nom de la liste
#   unchop(column_name) # la colonne "column_name' précédante contient plein d'info illisible (un paquet de column names trouvés), on doit la déployer (unchop)
# mapped_df # tous les noms de colonnes, pour examiner les problèmes (warnings)

## décision d'archivage de version existante ----
# explication : si les fichiers n'existent pas déjà, écrire les fichiers
# s'ils existent, aller vérifier ce qu'en faire, et écraser/réécrire en conservant la version précédante au besoin (uncomment suivant)
for (n in names(env.data.sitewise[[1]])) { # n c'est chaque feuille dans env.data.sitewise // [[1]] pas grave lequel des site, car ils comportent les mm données
  j <- which(n == names(env.data.sitewise[[1]])) 
  } # index pour le path et nom de fichier .xslx
if(paste0(names(env.data.sitewise[[1]])[j], ".xlsx") %in% list.files("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw"))  {
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.") }

## aggrégation des onglets du même nom en base de données tous sites ----
### COMMENT/UNCOMMENT HERE (next line)
# env.data.merged <- list()
# for (n in names(env.data.sitewise[[1]])) { # n c'est chaque feuille dans env.data.sitewise // [[1]] pas grave lequel des site, car ils comportent les mm données
#                                            # 1 à 6 ce sont mes 6 sites
#   n
#   env.data.n <- bind_rows(env.data.sitewise[[1]][[n]],
#                           env.data.sitewise[[2]][[n]],
#                           env.data.sitewise[[3]][[n]],
#                           env.data.sitewise[[4]][[n]],
#                           env.data.sitewise[[5]][[n]],
#                           env.data.sitewise[[6]][[n]])
#                           # autant de ligne que DE SITE sinon les sites ultérieurs vont manquer dans les données
#   # env.data.n <- bind_rows(cat(paste0("env.data.sitewise[[", n, "]][[n]]"), ")")) # AJUSTER CETTE FORMULE
#   env.data.n <- env.data.sitewise %>%
#     purrr::map(n) %>%
#     dplyr::bind_rows()
# 
#   env.data.n <- filter.raw.file(env.data.n) # issu du script "fonctions_phd_v3.2.R" (AG, 2025+)
#   env.data.merged[[n]] <- env.data.n # liste (de feuillets) contenant les données de chaque site concatennés ensemble
#   writexl::write_xlsx(
#     env.data.merged[[n]],
#     path = paste0(
#       "/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/",
#       names(env.data.sitewise[[1]])[j],
#       ".xlsx"))
# }
### COMMENT/UNCOMMENT TO HERE (previous line)

# ============================================================================= /
# Nettoyage (type colonnes, wide-to-long, etc.) ----
# ============================================================================= /
## microtopo ----
# EPSG  4269, NAD83
microtopo_visualisation <- readxl::read_xlsx("data/extracted_raw/microtopo.xlsx")
# ok warnings INUTILES
colnames(readxl::read_xlsx("data/extracted_raw/microtopo.xlsx"))
str(microtopo_visualisation)





# canopy.peat.fauna_visualisation <- readxl::read_xlsx("data/extracted_raw/canopy.peat.fauna.xlsx")
# colnames(readxl::read_xlsx("data/extracted_raw/canopy.peat.fauna.xlsx"))
# str(canopy.peat.fauna_visualisation)
# vegetation_trees.shr_visualisation <- readxl::read_xlsx("data/extracted_raw/vegetation_trees.shr.xlsx")
# colnames(readxl::read_xlsx("data/extracted_raw/vegetation_trees.shr.xlsx"))
# str(vegetation_trees.shr_visualisation)
# vegetation_lower.str_visualisation <- readxl::read_xlsx("data/extracted_raw/vegetation_lower.str.xlsx")
# colnames(readxl::read_xlsx("data/extracted_raw/vegetation_lower.str.xlsx"))
# str(vegetation_lower.str_visualisation)
