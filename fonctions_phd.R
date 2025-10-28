# Description -------------------------------------------------------------
###########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création : 2025-10-28
# Pourquoi : 
# Pour charger les fonctions utiles à traiter données de SHN
# NOTES : 
#   SNH : sondes de niveau hydrostatique
# La fonction s'applique à un fichier brut, traite, corrige et nettoye, et produit un fichier de données aux composantes identiques, peu importe le type de sonde.
# Produit un dataframe rectangulaire. Les métadonnées sont à part (ou créer liste avec données + métadonnées).
#   Merci à Francis Lessard pour ses idées.
# référer aux fonctions en copiant-collant seulement la ligne suivante
source("/Users/Aliz/Documents/Doctorat/_R.&.Stats_PhD/connectivite/scripts/fonctions_phd.R")

# ============================================================================= /
#  Libraries ----
# ============================================================================= /
if (!require("tidyverse")) install.packages("tidyverse") # “meta”-package



# ============================================================================= /
#  EN CHANTIER ----
# ============================================================================= /
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")

read_hobo <- function(path){
  read.csv(path, sep = "\t") %>% 
    slice(-(1:5)) %>% # enlever métadonnées, traitées à part
    separate(1, into = c("scan_no", "date", "hour", "raw", "calibrated"), sep = ",") %>% # ligne 1 = nom des colonnes
    # nettoyer données dates et heure
    mutate(date = gsub("\\s+", "", date)) %>% # "\\s+" = enlever les espaces
    mutate(hour = gsub("\\s+", "", hour)) %>% 
    mutate(hour = gsub(":", "/", hour)) %>% 
    mutate(date = paste0(date, "/", hour)) %>% 
    mutate(date = as.POSIXct(date, format = "%d/%m/%Y/%H/%M/%OS")) %>% # combiner date et heure
    dplyr::select(-hour) -> tidy.data # enelver vielle colonne heure inutile
  
  return(tidy.data)
}


# tests
# read_hobo <- function(path){
read.csv("connectivite/data/raw//10279777_INK_20250106_hobo.csv", sep = "\t") %>%  # 
# read.csv("connectivite/data/raw//20573974_INK_20250106_hobo.csv", sep = "\t") %>%  # 
# read.csv("connectivite/data/raw//10279769_INK_20250106_hobo.csv", sep = "\t") %>%  # 
  #,"Date Heure, GMT-04:00","Pres. abs., kPa (LGR S/N: 10279777, SEN S/N: 10279777)","Temp., °C (LGR S/N: 10279777, SEN S/N: 10279777)","Coupleur détaché (LGR S/N: 10279777)","Coupleur attaché (LGR S/N: 10279777)","Hôte connecté (LGR S/N: 10279777)","Arrêté (LGR S/N: 10279777)","Fin de fichier (LGR S/N: 10279777)"  # slice(-(1)) %>% # enlever métadonnées, traitées à part (slice) 
  slice(-1) %>% # ligne 1 = nom des colonnes
  separate(1, into = c("scan_no", "date.hour", , "raw","notes.1","notes.2","notes.3","notes.4","notes.5", "notes.6"), sep = ",") %>% 
  tail() #head()
  # nettoyer données dates et heure
  mutate(date = gsub("\\s+", "", date)) %>% # "\\s+" = enlever les espaces
  mutate(hour = gsub("\\s+", "", hour)) %>% 
  mutate(hour = gsub(":", "/", hour)) %>% 
  mutate(date = paste0(date, "/", hour)) %>% 
  mutate(date = as.POSIXct(date, format = "%d/%m/%Y/%H/%M/%OS")) %>% # combiner date et heure
  dplyr::select(-hour) -> data # enelver vielle colonne heure inutile
  return(data)
# }
head(data)

# UTILISATION DANS DATA_WATER.TABLE_ALL
SNH <- as.vector(c("_odyssey", "_hobo"), mode = "character") # liste des types de SNH avec lesquelles j'ai pris des données; chaque "marque" est traitée de façon différente
raw.ll.files <- list.files(path = "connectivite/data/raw/", pattern = "_odyssey|_hobo", full.names = T) # equivalent à ll.clean (ancien)
for (i in 1:length(raw.files)) {
  i <- 2
  # ajouter condition : si hobo vs odyssey <- dans une fonction ?
  if (grepl(SNH[1], raw.ll.files[i])) {
    NULL
    # data.i -> tidy.WTD.data[[i]]
  }
  else if (grepl(SNH[2], raw.ll.files[i]))
  data.i <- read_hobo(raw.ll.files[i])
  data.i <- tidy.WTD.data[[i]] # ne fonctionne pas
}
head(data.i)





# lapply(raw.files, read_hobo) -> tidy.WTD.data # ne fonctionne pas

x <- raw.files[[1]]
purrr::map(raw.files, # gérer des données en liste, renvoyer une liste (équivalent à lapply dans base R)
           function(x){
             read_hobo(x) %>% 
               mutate(file = x) %>% # ajouter une colonne avec le nom de fichier
           }) -> data

purrr::map_dfr(files,  # gérer des données en liste, renvoyer un dataframe aux lignes concatennées (lapply ne fait pas ça)
               function(x){
                 read_hobo(x) %>% 
                   mutate(file = x) %>% 
                   mutate(hour = hour(date))
               }) -> data
data %>% 
  filter(hour == 20) # filtre à traver l'ENSEMBLE DES DONNÉES !




# ============================================================================= /
#  HOBO ----
# ============================================================================= /
## read_hobo ----

NONÀJOURread_hobo <- function(path){
  read.csv(path, sep = "\t") %>% 
    slice(-(1:5)) %>% # enlever métadonnées, traitées à part
    separate(1, into = c("scan_no", "date", "hour", "raw", "calibrated"), sep = ",") %>% # ligne 1 = nom des colonnes
    # nettoyer données dates et heure
    mutate(date = gsub("\\s+", "", date)) %>% # "\\s+" = enlever les espaces
    mutate(hour = gsub("\\s+", "", hour)) %>% 
    mutate(hour = gsub(":", "/", hour)) %>% 
    mutate(date = paste0(date, "/", hour)) %>% 
    mutate(date = as.POSIXct(date, format = "%d/%m/%Y/%H/%M/%OS")) %>% # combiner date et heure
    dplyr::select(-hour) -> data # enelver vielle colonne heure inutile
  
  return(data)
}

## read_hobo_header ----
NONÀJOURread_hobo_header <- function(path){
  read.csv(path, sep = "\t") %>% 
    slice((1:5)) -> data
  return(data)
  
}


# ============================================================================= /
#  ODYSSEY ----
# ============================================================================= /

## read_odyssey ----
# comprend des sous-fonctions qui se trouvent ensuite, en ordre
read_odyssey <- function(path){
  
  data.metadata.odyssey(path) # sous fonction, ci-dessous  √ OK 20102025
  file.name.verif(raw.ll.files.i) # objet raw.ll.files.i créé dans fonction précédante √ OK 20102025
  # [...]
  return(tidy.WTD.data.i) # retourne la liste des objets nettoyés, composée de data + metadata
}

## data.metadata.odyssey ----
# séparer données et métadonnées
data.metadata.odyssey <- function(path) {
  raw.ll.files.0 <- readLines(path) # lire en format texte
  # Warning message:
  #   In readLines(paste0("connectivite/data/raw/", ll.pre[i])) :
  #   incomplete final line found on 'connectivite/data/raw/[...].csv'
  # c'est chill, je n'ai pas réussi à arranger ça, mais vérifié √ pas de problème
  # enlever espaces inutiles
  raw.ll.files.1 <- gsub(" ,", ",", raw.ll.files.0)
  raw.ll.files.2 <- gsub(" ", "", raw.ll.files.1) # enlever tous les espaces dans le subset de données
  
  ### création des subsets data & metadata ----
  # notes : les noms réfèrent à l'étape et non à une matrice en particulier, les objets seront remplacés au fil de la boucle. 
  # l'info importante est consignée dans la liste ll.clean[i], à la fin
  raw.ll.files.2.metadata <-  raw.ll.files.2[c(1:9)] # inclus les anciens noms de colonnes, qui sont dans un format et un ordre bizzare
  raw.ll.files.2.data <- raw.ll.files.2[-c(1:9)]
  raw.ll.files.i <- list(raw.ll.files.2.data, raw.ll.files.2.metadata)
  return(raw.ll.files.i)
}

## metadata.verif ----
# vérification : probe.uid dans les métadonnées == nom du fichier
metadata.verif <- function(x) {
  # trouver le probe.uid.i (== probe.uid, logger serial no) dans les metadata
  texte <- x[[2]][4] # logger serial no, en base R
  numbers <- gregexpr("[0-9]+", texte)
  result <- regmatches(texte, numbers)
  probe.uid.i <- as.numeric(unlist(result))
  # no du level logger dans le nom du fichier brut (.csv), correspond à l'item "i" de la présente boucle
  texte <- raw.ll.files[i]
  numbers <- gregexpr("[0-9]+", texte)
  result <- regmatches(texte, numbers)
  fichier <- as.numeric(unlist(result))
  # test logger.serial.no == nom du fichier
  if(!(probe.uid.i %in% fichier)) { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc "else" statement)
    stop(paste0("Attention, le nom du fichier ne correspond pas au numéro de série du level logger. Fichier problématique : i = ", paste(i), "; ", raw.ll.files[i]))
  }
}



## data.tidying.odyssey ----
#



# ____Rendue là_____

  
# tests










## read_odyssey ----
# (fonction qui met en action les fonctions précédantes)

## read_odyssey_header ----




# ============================================================================= /
#  À appliquer sur tidy.WTD.data ----
# ============================================================================= /
x <- raw.files[[1]]
purrr::map(tidy.WTD.data, # gérer des données en liste, renvoyer une liste (équivalent à lapply dans base R)
           function(x){
             mutate(file.name = x) %>% # ajouter une colonne avec le nom de fichier
           }) -> data

purrr::map_dfr(tidy.WTD.data,  # gérer des données en liste, renvoyer un dataframe aux lignes concatennées (lapply ne fait pas ça)
               function(x){
                 read_hobo(x) %>% # ajuster; seulement si je 
                   mutate(file.name = x) %>% # ajouter une colonne avec le nom de fichier
               }) -> data
data %>% 
  filter(hour == 20) # filtre à travers l'ENSEMBLE DES DONNÉES !
# il faudra surement enlever les métadonnées...

