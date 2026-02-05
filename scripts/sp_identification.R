#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                                    Sp. identification
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : May 9th 2025
# Fonction : pour aider à identifier les espèces à Inkerman
# NOTES : 
# - Inventaire botanique en 2024 réutilisé pour filtrer et trouver sp. graminées notamment, plus rapidement
#  - "Species code", script ayant généré la Bdd "plants.xlsx" a été généré dans le script "fonctions.R" du dossier "general scripts"
###########################################################################-

#### bibliotheques a charger (installer avant si pas fait)
if (!require("conflicted")) install.packages("conflicted") # Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
# if (!require("reshape2")) install.packages("reshape2") # pour importer Google Sheets directement
if (!require("plyr")) install.packages("plyr") # pour manipulation donnees
if (!require("dplyr")) install.packages("dplyr") # pour manipulation donnees
# if (!require("ggplot2")) install.packages("ggplot2")
# # install.packages("devtools")
# # devtools::install_github("refunders/refund.shiny")
# if (!require("refund.shiny")) install.packages("refund.shiny") # pour enregistrer des graphiques sous forme de RData (besoin dans ma boucle)
if (!require("tidyverse")) install.packages("tidyverse") # pour manipulation donnees
if (!require("readxl")) install.packages("readxl") # lire les excel
if (!require("openxlsx")) install.packages("openxlsx") # lire les excel

# importer et préparer donnees dans R ----
setwd("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD")

inventories2024 <- read.xlsx("connectivite/data/raw/data_INK.xlsx", sheet = "vegetation_lower.str") %>% 
  dplyr::filter(stringr::str_detect(tr.uid.rel.dist.quadrat.aaaa, ".2024$")) 
inventories2024 <- inventories2024 %>% dplyr::select(grep("dom.sp", colnames(inventories2024)))
inventories2024_1 <- inventories2024$dom.sp.1
inventories2024_2 <- inventories2024$dom.sp.2
inventories2024_3 <- inventories2024$dom.sp.3
inventories2024 <- as.vector(c(inventories2024_1, inventories2024_2, inventories2024_3)) %>% na.omit() %>% unique() %>% as.data.frame()
colnames(inventories2024)[1] <- "Sp_code"

plants <- read.xlsx("Bases de donnees/plants.xlsx")

inventories2024_long <- left_join(inventories2024, plants, by = "Sp_code")
write.xlsx(inventories2024_long, "/Users/Aliz/Desktop/inventories2024.xlsx")
