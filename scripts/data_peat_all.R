#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
#                             Peat sample, cleaning script
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #

# Description -------------------------------------------------------------
########################################################################## -
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2026-10-13
# Date mise à jour :
# Pourquoi : 
# peat_data -> tidy
# ** Données tirées de peat.csv (modifications dans /Users/Aliz/Library/CloudStorage/OneDrive-UniversitéLaval/_FIELD_LAB.WORK/_Feuilles_données_pas_local/aaaammdd_peat.csv)

# Structure :
# —— connectivite
#         |—— archive
#         |—— data
#                     |—— raw (ici feuilles numériques terrain comportant plusieurs onglets pour un site; data_SITE.UID)
#                     |—— extracted_raw (fichiers légèrement modifiés ou joints avec métadonnées ou quadrat_uid)
#                     |—— clean
#         |—— output
#                     |—— data
#                     |—— figures
#         |—— scripts
# NOTES : 
# À FAIRE dans script analysis : anova one-way : bulk density groupe-site ou pas ? (mixt des routes et écotone, et aussi en les séparant ?)
#
# LEXIQUE :
# T : température
# EC: conductivité électrique
########################################################################## -

# ============================================================================= /
# Initialisation ----
# ============================================================================= /
# Librairies
if (!require("tidyverse")) install.packages("tidyverse") # gosser avec des suites de caractères, str_replace, [...]
if (!require("readxl")) install.packages("readxl") # lire les excel

# Données, dossier directeur fonctions et à charger directement
# .rs.restartR()
# setwd("/Users/Aliz/Library/CloudStorage/OneDrive-UniversitéLaval/_FIELD.LAB WORK 2025/Laboratoire/LOI")
setwd("~/Documents/Doctorat/_R_Stats_PhD/connectivite")
source("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/scripts/fonctions_phd_v3.2.R")

# Mise à jour données récentes, tous peat.xlsx
# retrait de la date du nom des feuilles de données (aaaammjj_peat.xlsx -> peat.xlsx)
if (length(list.files(path = "data/", pattern = "_peat")) != 0) { # s'il y a un fichier correspondant au motif (pattern)
  peat_file <- list.files(path = "data/", pattern = "_peat") |>
    basename() |> # basename(conserve le "path" dans le nom, en arrière-plan)
    str_remove("\\d+?(?=_)_") # enlève la date; explications sur le regex : voir section Mise à jour [...] ci-haut
  file.rename(from = list.files(path = "~/Documents/Doctorat/_R_Stats_PhD/connectivite/data", pattern = "_peat", full.names = TRUE), to = peat_file)
}

# Import données récentes
peat.pre <- read.xlsx("data/raw/peat.xlsx")
peat.sample.metadata <- readRDS("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/peat.sample.metadata.RDS")  # fonction uid.to.column() appliquée par le script qui a généré peat.sample.metadata.RDS (data_sites_all.R)
# peat.sample_join <- readRDS("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/data/extracted_raw/peat.sample_join.RDS")


# ============================================================================= /
# Expolorations initiales ----
# ============================================================================= /
## peat.pre ----
str(peat.pre)
colnames(peat.pre)
nrow(peat.pre)

## peat.sample.metadata ----
str(peat.sample.metadata)
colnames(peat.sample.metadata)
nrow(peat.sample.metadata)


# ============================================================================= /
# Nettoyages et agglomération ----
# ============================================================================= /
## join avec le peat.sample ----
peat.sample_join.pre <- full_join(peat.sample.metadata, peat.pre)

## filtre des lignes vides ----
col.vector <- colnames(peat.pre)[!colnames(peat.pre) %in%
                                   c("peat.sample_measure.status", "perm.plot.uid")] # spécification des colonnes à examiner (exclure colonnes qui ne sont jamais vides)
peat.sample_join <- peat.sample_join.pre |> 
  dplyr::filter(if_any(all_of(col.vector), ~ !is.na(.)))

## éliminer objets intermédiaires ----
rm(list = ls(.GlobalEnv, pattern = "\\.pre"))

## visualisation / lecture fichier ----
peat.sample_join
colnames(peat.sample_join)
str(peat.sample_join)
nrow(peat.sample_join)

## filtre global ----
peat.sample_join_fltrd <- filter.raw.file(peat.sample_join) # script "fonction_phd_vX" (AG, 2025+)
colnames(peat.sample_join_fltrd) # √ ok enlevé métadonnées de traitement en laboratoire (voir fichier brut peat.pre au besoin)

## correction EC ~ (pH, T) (Sjörs, 1950) ----
# Le pH-conductimètre Fisher Accumet AB200 Benchtop pH-Conductivity Meter compense automatiquement les données de conductivité 
# électrique (EC) en fonction de la température. Il faut toutefois appliquer une correction à l’EC mesurée puisque le pH influence
# la mesure. Pour se faire, on utilise la formule de Sjörs (1950) adaptée aux tourbières :
# Conductivité électrique corrigée (ECcorr) = 
# ECmesurée – (10 - pH x constante)
# Où la constante = 325 000 à 20 °C et 349 820 à 25 °C.
# La correction de l’EC permet de compenser l’effet des ions H+ et se calcul à partir de la formule tirée de Sjörs (1950) lorsque l’EC est prise ou compensée pour une température de 20 °C, ce qui est le cas de la plupart des appareils.


# ============================================================================= /
# [...] ----
# ============================================================================= /


# ============================================================================= /
# [...] ----
# ============================================================================= /

# ============================================================================= /
# [...] ----
# ============================================================================= /

