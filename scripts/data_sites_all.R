#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                           Site data, cleaning script
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2025-11-05
# Date mise à jour : 
# Pourquoi : Nettoyage des données de site
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
# 
##########################################################################-

# ============================================================================= /
# Initialisation ----
# ============================================================================= /
# Librairies
if (!require("tidyverse")) install.packages("tidyverse") # gosser avec des suites de caractères, str_replace, [...]
# if (!require("conflicted")) install.packages("conflicted") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("readxl")) install.packages("readxl") # lire les excel
if (!require("openxlsx")) install.packages("openxlsx") # lire/écrire les excel
# if (!require("stringr")) install.packages("stringr") # gosser avec des suites de caractères, str_replace, [...]

# Données, dossier directeur fonctions et à charger directement
# .rs.restartR()
# setwd("/Users/Aliz/Library/CloudStorage/OneDrive-UniversitéLaval/_FIELD.LAB WORK 2025/Laboratoire/LOI")
setwd("~/Documents/Doctorat/_R_Stats_PhD/connectivite")
source("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/scripts/fonctions_phd_v3.2.R")

# # retirer date feuille de données
# data_SITE.UID <- list.files(path = "~/Documents/Doctorat/_R_Stats_PhD/connectivite/data", pattern = "_data") |>
#   basename() |> 
#   str_remove(".+?(?=_)_")
# file.rename(from = list.files(path = "~/Documents/Doctorat/_R_Stats_PhD/connectivite/data", pattern = "_data", full.names = TRUE), to = data_SITE.UID)
# ============================================================================= /
# Nettoyage et enregistrement en RDS ----
# ============================================================================= /
# listes de données et filtration
raw.env.data.pre <- list.files(path = "data/raw", pattern = "data_", full.names = T) # mettre dans "pattern" tous les ID de SNH listés dans l'objet SNH
raw.env.data <- raw.env.data.pre[grep("[$]", raw.env.data.pre, invert = T)] # fichiers cachés (p.ex. : "~$data_BRNTC.xlsx") à retirer du vecteur

# consigne de données
env.data.sitewise <- list()

# boucle pour chaque site, agglomérer les onglets pertinents
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

env.data.merged <- list()
for (n in names(env.data.sitewise[[1]])) { # n c'est chaque feuille dans env.data.sitewise // [[1]] pas grave lequel des site, car ils comportent les mm données
                                           # 1 à 6 ce sont mes 6 sites 
  n
  env.data.n <- bind_rows(env.data.sitewise[[1]][[n]], 
                          env.data.sitewise[[2]][[n]],
                          env.data.sitewise[[3]][[n]],
                          env.data.sitewise[[4]][[n]],
                          env.data.sitewise[[5]][[n]],
                          env.data.sitewise[[6]][[n]])
                          # autant de ligne que DE SITE sinon les sites ultérieurs vont manquer dans les données
  # env.data.n <- bind_rows(cat(paste0("env.data.sitewise[[", n, "]][[n]]"), ")")) # AJUSTER CETTE FORMULE
  env.data.n <- env.data.sitewise %>% 
    purrr::map(n) %>% 
    dplyr::bind_rows()
  
  env.data.n <- filter.raw.file(env.data.n) # 
  env.data.merged[[n]] <- env.data.n # liste (de feuillets) contenant les données de chaque site concatennés ensemble
  
  j <- which(n == names(env.data.sitewise[[1]])) # index pour le path et nom de fichier .xslx
  # if(paste0(names(env.data.sitewise[[1]])[j], ".xlsx") %in% list.files("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw"))  {
  #   stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
  # } else { # comment here above (3 lines) for the files to be updated ** (warning it will replace)
    write.xlsx(env.data.merged[[n]], 
               file = paste0("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/", 
                             names(env.data.sitewise[[1]])[j], 
                             ".xlsx"),
               guess_max = 12)
  # } # comment here
}
microtopo_visualisation <- readxl::read_xlsx("data/extracted_raw/microtopo.xlsx")
# ok warnings INUTILES
colnames(readxl::read_xlsx("data/extracted_raw/microtopo.xlsx"))
str(microtopo_visualisation)
# canopy.peat.fauna_visualisation <- readxl::read_xlsx("data/extracted_raw/canopy.peat.fauna.xlsx")
# colnames(readxl::read_xlsx("data/extracted_raw/canopy.peat.fauna.xlsx"))
# str(canopy.peat.fauna_visualisation)
# canopy.peat.fauna_visualisation <- readxl::read_xlsx("data/extracted_raw/canopy.peat.fauna.xlsx")
# colnames(readxl::read_xlsx("data/extracted_raw/canopy.peat.fauna.xlsx"))
# str(canopy.peat.fauna_visualisation)
