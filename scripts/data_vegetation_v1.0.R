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
# Nettoyages et vérifications ----
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
# uniformisation des NA vrais, "NA", "na", 0 dans 3 colonnes & retrait si les 3 colonnes n'ont aucune donnée (NA)
vegetation_trees.shr.pre_cleand <- vegetation_trees.shr.pre |> 
  mutate(across(c(nb.ind, diam.cm, circ.cm), ~ ifelse(.x == 0 | toupper(.x) == "NA", NA, .x))) |> # tel quelle ça remet les 0 en NA, veut dire qu'on peut éliminer 
  # la ligne, ya pas cette espèce ni des DHP/circonf (vestige de la méthode de prise de données numériques)
  dplyr::filter(!if_all(c(nb.ind, diam.cm, circ.cm), is.na))  # enlève beaucoup de lignes inutiles OK

### vérifications ----
for(n in 1:nrow(vegetation_trees.shr.pre_cleand)) {
  # n<-436 # circ != NA
  # n<-1 # diam != NA
  # n<-366 # diam && circ == NA
  print(paste("ligne =", n))
  
  # vérification 1 : fin = ;NA
  if(!is.na(vegetation_trees.shr.pre_cleand$diam.cm[n])) {
    if(!grepl("NA$", vegetation_trees.shr.pre_cleand$diam.cm[n], ignore.case = T)) { # si ne fini pas par NA (ou pas cellule == NA), ERREUR
      stop(paste0(c("Le nombre d'arbres ne fini pas par 'NA', le compte serait erroné. Corriger les donnéres originales, quadrat.UID : ", vegetation_trees.shr.pre_cleand$quadrat.uid[n],".")))
    }
  }
  
  # vérification 2 : soit données dans colonne DHP.cm SOIT dans circ.cm
  # stratégie : si les DEUX ne sont ni tous "NA" NI ne continennent de données, ça veut dire que tout VA BIEN, donc continuer
  if(!all(is.na(vegetation_trees.shr.pre_cleand$diam.cm[n]), is.na(vegetation_trees.shr.pre_cleand$circ.cm[n]))) { # si tout est NA (== T): ne rien faire; si FAUX, vérifier...
    # si les DEUX ne sont PAS NA : ERREUR
    if(all(is.na(vegetation_trees.shr.pre_cleand$diam.cm[n]), is.na(vegetation_trees.shr.pre_cleand$circ.cm[n]))) {
      stop(paste0("Les colonnes diam.cm et circ.cm contiennent toutes deux des données alors que ce devrait être l'UNE ou l'AUTRE des méthodes. 
                  Aller corriger données originales; quadrat.uid :", vegetation_trees.shr.pre_cleand$quadrat.uid[n], "."))
    }
  } 
  
  # vérification 3 : soit données dans colonne DHP.cm/circ.cm (oui) SOIT dans nb.ind
  if(!is.na(vegetation_trees.shr.pre_cleand$nb.ind[n]) && !all(is.na(vegetation_trees.shr.pre_cleand$diam.cm[n]), 
                                                               is.na(vegetation_trees.shr.pre_cleand$circ.cm[n]))) {
    stop(paste0("La valeur de la colonne nb.ind.# n'est pas vide, elle sera écrasée. Vérifier méthodologie, 
                Aller corriger données originales; quadrat.uid :", vegetation_trees.shr.pre_cleand$quadrat.uid[n], "."))
  }
}

# STRATÉGIE RENDANT CI-DESSOUS CADUQUE : 
# vérifications boucle en elle-même (mettre dans fonctions ?)
# j'ai deux colonnes, si des données sont dans la colonne 2 ou 3 je veux m'assurer que la colonne 1 soit NA, sinon erreur.
# cette vérif permet d'écraser le NA respectif avec la conversion C = D x π
# CHOISIR SUR QUELLE COLONNE TRAVAILLER, soit CELLE QUI N'EST PAS NA

# vecteur des chiffres
# diam.cm.n <- str_split(vegetation_trees.shr.pre_cleand$diam.cm[n], pattern = ";")
# circ.cm.n <- str_split(vegetation_trees.shr.pre_cleand$circ.cm[n], pattern = ";")
# vegetation_trees.shr.pre_cleand$diam.cm <- if(is.na(diam.cm.n)) 
# circ.cm.n)

# if(is.na(str_count(vegetation_trees.shr.pre_cleand$diam.cm[n], pattern = ";"))) {
#   if(!is.na(str_count(vegetation_trees.shr.pre_cleand$circ.cm[n], pattern = ";"))) {
#     tree.count.n <-  str_count(vegetation_trees.shr.pre_cleand$circ.cm[n], pattern = ";") # logique de compter les ";" : j'ai mis NA à la fin pour que chaque ARBRE ait un ";" à sa droite (1:1)
#   }
# } else if(!is.na(str_count(vegetation_trees.shr.pre_cleand$diam.cm[n], pattern = ";"))) {
#   if(is.na(str_count(vegetation_trees.shr.pre_cleand$circ.cm[n], pattern = ";"))) {
#     tree.count.n <-  str_count(vegetation_trees.shr.pre_cleand$diam.cm[n], pattern = ";") # logique de compter les ";" : j'ai mis NA à la fin pour que chaque ARBRE ait un ";" à sa droite (1:1)
#   }
# } else { # si tout est NA
#   tree.count.n <- NA
# }

# print(paste("nombre =", tree.count.n))
# vegetation_trees.shr.pre_cleand$nb.ind[n] <- tree.count.n
# }


# vegetation_trees.shr.pre_count <- vegetation_trees.shr.pre_cleand |> 
#   mutate(compte.arbres = case_when(
#     grep(";",   diam.cm),
#   )

# ============================================================================= /
# Calcul des équivalences ----
# ============================================================================= /
# circonférence (vestige prise de donnée 2024) <-> diamètre avec la conversion C = D x π
# nb d'individus (parcelle de 20 m2) avec nombre de diamètres/circonférence mesurés
# NOTE MÉTHODOLOGIQUE : parcelle 20 m2 ou 100 m2 pour les DHP/circ ET (additif dans un même parcelle permanente) 
# quadrats A/B/C/... pour les individus de moins de 1,3 m ou sans DHP (compte d'individus)
# sans masquer par des additions ou des moyennes (pas dans le format tidy général), ces données représentent un emboîtement et/ou une moyenne
# l'important reste donc LA LIGNE

# équivalence DHP <-> circonférence ----
# vegetation_trees.shr.pre_cleand_dhp
pattern <- paste(pattern = c("diam.cm",'circ.cm'), collapse = "|")
cols <- colnames(vegetation_trees.shr.pre_cleand[,grep(pattern, colnames(vegetation_trees.shr.pre_cleand))])
for(col in 1:length(cols)) {
  print(cols[col])
  col.name <- cols[col]

  # stratégie : par une boucle, appliquer les modifications sur le vecteur diam/circ, puis remplacer la valeur nb.ind (vérification 3. faite √ on n'écrase rien)
  vegetation_trees.shr.pre_cleand.cmpltd <- vegetation_trees.shr.pre_cleand |> 
    select(!!col.name) 
  # si non NA, 
  # vecteur finissant par NA -> supprimer le NA
  # str_split avec ";"
  # vecteur de charactères où "," doit changer pour "." pour convertir en chiffre
  # conserver colonne-liste (vecteur de chiffres)
  # -> cette colonne liste devra être transformée en format long un moment donné....
  # me renseigner sur l'usage de ces données (rendue là, 30 octobre 2026)
  
  # |> 
  #   mutate()
}

# ou version base R (+ compréhensible et directement testable)
test <- vegetation_trees.shr.pre_cleand |>
  mutate(
    diam_liste = map(`diam.cm`, function(x) {
      x |> 
        str_remove(";NA$") %>%        # Supprime le ";NA" à la fin de la chaîne
        str_split(";") %>%            # Sépare les valeurs dans une liste de caractères
        pluck(1) %>%                  # Extrait le premier vecteur de la liste de str_split
        str_replace(",", ".") %>%     # Remplace la virgule décimale par un point pour R
        as.numeric()                  # Convertit en vecteur numérique
      
    }),
    nb_valeurs_non_na = map_int(diam_liste, ~ sum(!is.na(.x)))
    )
    
  # vecteur des chiffres
  # diam.cm.n <- str_split(vegetation_trees.shr.pre_cleand$diam.cm[n], pattern = ";")
  # circ.cm.n <- str_split(vegetation_trees.shr.pre_cleand$circ.cm[n], pattern = ";")
  # vegetation_trees.shr.pre_cleand$diam.cm <- if(is.na(diam.cm.n)) 
  # circ.cm.n)
  

# for(n in 1:nrow(vegetation_trees.shr.pre_cleand)) {
#   # n<-436 # circ != NA
#   # n<-1 # diam != NA
#   # n<-366 # diam && circ == NA
#   print(paste("ligne =", n))
#   
#   # vérification 1 : fin = ;NA
#   if(!is.na(vegetation_trees.shr.pre_cleand$diam.cm[n])) {


# ============================================================================= /
# Expolorations initiales ----
# ============================================================================= /

  
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
