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

# .rs.restartR()
source("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd_v3.R") # read_excel_all_sheets et autres
# setwd("/Users/Aliz/Library/CloudStorage/OneDrive-UniversitéLaval/_FIELD.LAB WORK 2025/Laboratoire/LOI")
setwd("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite")

# Librairies ----
if (!require("tidyverse")) install.packages("tidyverse") # gosser avec des suites de caractères, str_replace, [...]
# if (!require("conflicted")) install.packages("conflicted") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("readxl")) install.packages("readxl") # lire les excel
if (!require("openxlsx")) install.packages("openxlsx") # lire/écrire les excel
# if (!require("stringr")) install.packages("stringr") # gosser avec des suites de caractères, str_replace, [...]

# Nettoyage et enregistrement en RDS ----
raw.env.data <- list.files(path = "data/raw", pattern = "data_", full.names = T) # mettre dans "pattern" tous les ID de SNH listés dans l'objet SNH

env.data.sitewise <- list()
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
                                           # 1 à 4 ce sont mes 4 sites 
  env.data.n <- bind_rows(env.data.sitewise[[1]][[n]], 
                          env.data.sitewise[[2]][[n]],
                          env.data.sitewise[[3]][[n]],
                          env.data.sitewise[[4]][[n]])
  env.data.merged[[n]] <- env.data.n # liste (de feuillets) contenant les données de chaque site concatennés ensemble
  
  j <- which(n == names(env.data.sitewise[[1]])) # index pour le path et nom de fichier .xslx
  if(paste0(names(env.data.sitewise[[1]])[j], ".xlsx") %in% list.files("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/extracted_raw"))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
    stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
  }
  # write.xlsx(env.data.merged[[n]], file = paste0("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/extracted_raw/", names(env.data.sitewise[[1]])[j], ".xlsx")) # RDS fonctionne mieux avec ma liste que RData// save(ll.clean, file = "connectivite/data/clean/ll.clean.RData") }
}




  
  