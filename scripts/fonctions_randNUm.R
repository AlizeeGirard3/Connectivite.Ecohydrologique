generer_chiffre_heure <- function() {
    # 1. Convertir l'heure actuelle en nanosecondes/microsecondes (grand entier)
    graine <- as.numeric(Sys.time()) * 1000000
    
    # 2. Forcer la graine à rester dans les limites autorisées par R
    graine_valide <- graine %% .Machine$integer.max
    
    # 3. Appliquer la graine
    set.seed(graine_valide)
    
    # 4. Générer le chiffre (ici entre 1 et 1 000 000 pour éviter le hasard des doublons)
    chiffre <- sample(1:1000000, 1)
    
    return(chiffre)
}
generer_chiffre_heure()
