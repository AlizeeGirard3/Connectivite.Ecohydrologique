setwd("~/Documents/Doctorat/_R_Stats_PhD/connectivite")
source("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/scripts/fonctions_phd_v3.2.R")

generer_chiffre_heure <- function() {
  # 1. Charger la liste des UID existants une seule fois en vecteur simple
  peat.UID <- filter.raw.file(read.xlsx("data/raw/peat.xlsx")) |> 
    select(contains("R.randNum")) |> 
    dplyr::filter(if_all(everything(), ~ !is.na(.x))) |> 
    unlist(use.names = FALSE)
  
  # 2. Boucler jusqu'à trouver un chiffre unique
  repeat {
    graine <- as.numeric(Sys.time()) * 1000000
    graine_valide <- graine %% .Machine$integer.max
    set.seed(graine_valide)
    
    chiffre <- sample(1:1000000, 1)
    
    # Vérifier directement si le chiffre n'est pas dans le vecteur existant
    if (!(chiffre %in% peat.UID)) {
      return(chiffre)
    }
  }
}
generer_chiffre_heure()

