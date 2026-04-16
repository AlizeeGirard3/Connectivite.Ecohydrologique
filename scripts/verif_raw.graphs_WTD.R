#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                           Raw visualisation of WTD data
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2026-01-07
# Date mise à jour : 
# Pourquoi : Visualisation des données de nappe non-traitées (raw), suivi des problèmes et décisions
# NOTES : Lorsque résolu, je les place en bas du script

# LEXIQUE :
{ 
  # SNH : sonde de niveau hydrostatique / synonymes : LL : level logger; sonde, probe
  # NP : Nappe phréatique / synonymes : water table / WTD
}
##########################################################################-

# ============================================================================= /
# Initialisation ----
# ============================================================================= /
# Librairies ----
if (!require("tidyverse")) install.packages("tidyverse") # méta package // gosser avec des suites de caractères, str_replace, [...]
if (!require("ggplot2")) install.packages("ggplot2")

# Fichiers à charger directement ----
setwd("~/Documents/Doctorat/_R_Stats_PhD")
tidy.WTD.data.df.import <- readRDS("connectivite/data/clean/tidy.WTD.data.df.RDS") # issu du code "data_water.table_all_v3.1"
offsets.all <- readRDS("connectivite/data/clean/20260416_offsets.all.RDS") # issu du code "data_water.table_all_v3.1"

# archives
# tidy.cal.data <- readRDS("connectivite/data/clean/archive_v3.0/tidy.cal.data.RDS") # issu du code "data_water.table_all_v3.0"
# tidy.WTD.data.df <- readRDS("connectivite/data/clean/archive_v3.0/tidy.WTD.data.df.RDS") # issu du code "data_water.table_all_v3.0"
# tidy.WTD.data <- readRDS("connectivite/data/clean/archive_v3.0/tidy.WTD.data.RDS") # issu du code "data_water.table_all_v3.0"
# extracted.list_data <- lapply(tidy.WTD.data, `[[`, 4)
# tidy.WTD.data.df.9janv <- do.call(rbind, extracted.list_data)

# ============================================================================= /
# JOIN : offset + tidy.WTD.data.df.import ----
# ============================================================================= /
tidy.WTD.data.offset <- full_join(tidy.WTD.data.df.import, offsets.all, 
                                  by = c("date.time.UTC.0" = "time", "file.uid"),
                                  relationship = 'many-to-many')

# ============================================================================= /
# PROBLÈMES ----
# ============================================================================= /
## Odyssey ----

## 42564_20241125
# timestamp <- tidy.WTD.data.offset %>%
#   filter(file.uid == "42564_20241125",
#          source_calib == "blo",
#          !is.na(offsets)) %>% 
#   pull(date.time.tz.orig)
# df_42564_20241125 <- tidy.WTD.data.offset %>%  
#   filter(file.uid == "42564_20241125",
#          source_calib == "lin",
#          timestamp >= date.time.tz.orig - hours(48),
#          timestamp <= date.time.tz.orig + hours(48))
#   # je veux visualiser les événements BULLEUR +et- 48h
# 
# ggplot(df_42564_20241125, aes(x = date.time.tz.orig, y = calibrated.value.cm)) +
#   geom_line(color = "steelblue") +
#   annotate("text", 
#            x = timestamp, y = max(df_42564_20241125$calibrated.value.cm),
#            label = "Événement Bulleur", vjust = -1, color = "red") +
#   theme_minimal()
## 16 avril, ce graph fonctionne, je veux en faire une boucle pour chaque Odyssey et période +/- 48h bulleur
# pour évaluer si des bulleurs doivent être associés à d'autres périodes temporelles
# si j'élimine des données, transformer cal.data : dupliquer ligne, celle avec valeur erronnée = measure.status == rejected, puis
# ligne dupliquée enlever la valeur aberrante (manière la plus simple)   




## Hobo ----


## Sondes avec trop de glitch, à lisser ----
# explications potentielles pour les glitch aléatoires : lorsque la nappe tombe en dessous de mon éventail de détection (sous la sonde)

# je ne comprends pas mes commentaires ci-dessous (16 avril 2026)
# ## Deuxième signal à pas temporel constant (?)
# # ne correspondant pas à la diminution (ÉT, outflow)-élévation de nappe (pluie) données bruttes avec 1 seul glitch à supprimer rapidement ----
# # 41359_20241125
# odyssey.data <- tidy.cal.data%>% dplyr::filter(cal.no == "3") %>% 
#   dplyr::filter(file.uid == "41359_20241125")
# # ce signal semble revenir aux X PAS DE TEMPS

#### 22195241_20251202 & ----
#### 22195241_20250721 ----
# aussi, voir verif_blowing.pipe -> à PRO, la différence des bulleur était de ~13.5 et 21 cm 


# ============================================================================= /
#  ZONE DES PROBLÈMES RÉSOLU ----
# ============================================================================= /

## Un seul glitch à supprimer rapidement ----
# (probablement causés à retrait de sonde lors de la mesure de bulleur)
# voir ancienne versions des graphiques dans les archives
### 22224392_20251210 ----
data.to.sort <- tidy.WTD.data.df %>% 
  dplyr::filter(file.uid =="22224392_20251210") %>% 
  arrange(desc(calibrated.value.cm))
# à 2025-05-29T10:00:01Z mesure raw = 101.073 alors que les données avant et après sont à 111.953 + 111.897 (fiable)
(111.953+111.897)/2 # je prends la moyenne et l'entre directement dans les données bruttes (.csv)
rm(data.to.sort)

### 41359_20251203 ----
# voir ancienne versions des graphiques dans les archives
data.to.sort <- tidy.WTD.data.df %>% 
  dplyr::filter(file.uid =="41359_20251203") %>% 
  arrange(desc(calibrated.value.cm))
# à 2025-10-16T15:00:01Z mesure raw = -94.43868 alors que les données avant et après sont à (voir suivant, fiable)
(-50.34814-50.25113)/2 # je prends la moyenne et l'entre directement dans les données bruttes (.csv)

