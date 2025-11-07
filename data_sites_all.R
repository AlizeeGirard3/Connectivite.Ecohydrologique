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
#                     |—— clean
#         |—— output
#                     |—— data
#                     |—— figures
#         |—— scripts
# NOTES : 
# 
##########################################################################-

# .rs.restartR()
source("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd.R") # read_excel_all_sheets et autres
# setwd("~/Documents/Doctorat/_R.&.Stats_PhD")
setwd("/Users/Aliz/Library/CloudStorage/OneDrive-UniversitéLaval/_FIELD.LAB WORK 2025/Laboratoire/LOI")

# Librairies ----
if (!require("tidyverse")) install.packages("tidyverse") # gosser avec des suites de caractères, str_replace, [...]
# if (!require("conflicted")) install.packages("conflicted") # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("readxl")) install.packages("readxl") # lire les excel
# if (!require("openxlsx")) install.packages("openxlsx") # lire les excel
# if (!require("stringr")) install.packages("stringr") # gosser avec des suites de caractères, str_replace, [...]

# Nettoyage et enregistrement en RDS ----
raw.env.data <- list.files(pattern = "data_") # mettre dans "pattern" tous les ID de SNH listés dans l'objet SNH
env.data <- list()
for (i in 1:(length(raw.env.data))) {
  print(i)
  # i<-2
  # nom des onglets du classeur .csv et sélection des pertinents
  sheets.pre <- readxl::excel_sheets(raw.env.data[i])
  sheets <- subset(sheets.pre,!grepl(pattern = "À FAIRE|sp_code|validation|READ ME|cad.", sheets.pre)) # keeps any other sheet
  
  raw.env.data.i <- read_excel_sheets(raw.env.data[i])
  env.data[[i]] <- raw.env.data.i # liste (de site) contenant une liste (d'onglets pertinents)
}
# 
# combined_output <- reduce(env.data, cat_lists)
# 
# for (i in 1:length(combined_output)) {
# }
# 
# # env.data  liste des sites qui contient liste des feuilles données au nom identique
# 
# COMMENT LUI DIRE QUE JE VEUX 


# # COMMENT CONCATENNER ?
# list1 <- list(integers=c(1:7), letters=letters[1:5],
#               words=c("two", "strings"))
# list2 <- list(letters=letters[1:10], booleans=c(TRUE, TRUE, FALSE, TRUE),
#               words=c("another", "two"), floats=c(1.2, 2.4, 3.8, 5.6))
# 
# input_list <- list(list1, list2, list1, list2)
# 
# combined_output <- reduce(input_list, cat_lists)



# setNames(env.data, sheets)

# list2env(raw.env.data.i, envir=.GlobalEnv)
# sheets %in% names(as.list(.GlobalEnv))


# raw.env.data.i.l <- list()
# for (l in 1:length(raw.env.data.i)) { # si mm fichier.uid.i, coller les périodes ensemble (ainsi, retirer et remettre ne demande pas plus de manipulations et surtout ps des manipulations individuelles)
# list2env(raw.env.data.i, envir=.GlobalEnv)
# # OU
# sheets.i <- list() # liste des feuilles du fichier de consigne de données d'un site "data_SITE.UID"
# for (j in 1:length(raw.env.data.i)) {
#   sheets.i[[j]] <- assign(paste0("raw.env.data.i", j), as.data.frame(raw.env.data.i[[j]]))
# }
# 
# 

# RENDUE LÀ
# nom des feuilles :

# pour chaque feuille, crée un dataframe 
# stocker les objets correspondants dans une liste qui peut être référée à la prochaine itération
# MAP ?


# purrr::map_dfr(raw.env.data.i, 
#                function(x){
#                  read_hobo(x) %>% 
#                    mutate(file = x) %>% 
#                    mutate(hour = hour(date))
#                }) -> data
# 

# qui comporte une concaténation des données de chaque site
  

  
  
  
  