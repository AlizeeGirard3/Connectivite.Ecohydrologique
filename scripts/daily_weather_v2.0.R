#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                       Daily weather data cleaning and visualisation
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Description -------------------------------------------------------------
##########################################################################-
# Fait par :      Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2025-05-25
# Date mise à jour : 14 avril janvier 2026 -> MAJ : enlevé données journalières et nettoyages pour fitter le data_water.table_all_v3.1.R
# Pourquoi : afficher données de MétéoStat et faire des analyses sur le jeu de données (corriger pour l'scillation quotidienne)
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
# on sait que la pression ne change pas à l’échelle régionale, mais capteurs mauvais a une erreur à cause de la température // 
# et voir les articles sur l’ÉT horaire

# LEXIQUE :
# NP : Nappe phréatique / synonymes : water table
# ECCC/CSSS : Environnement and Climate Change Canada / Canadian Centre for Climate Services 
# tz : time zone, syn. fuseau horaire

##########################################################################-

# ============================================================================= /
# Initialisation ----
# ============================================================================= /
# Librairies (autres initialisées dans le script sourcé)
if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("grDevices")) install.packages("grDevices") # pdf()
if (!require("lubridate")) install.packages("lubridate") # hour()
if (!require("nlme")) install.packages("nlme") # lme()
if (!require("slider")) install.packages("slider") # sélection d'une fenêtre glissante
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("moments")) install.packages("moments") # coefficient d'asymétrie (pour évaluer normalité avec > 5000 observations)
if (!require("sf")) install.packages("sf") # GIS in R

# Données, dossier directeur fonctions et à charger directement
# .rs.restartR()
setwd("~/Documents/Doctorat/_R_Stats_PhD")
source("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/connectivite/scripts/fonctions_phd_v3.1.R")
source("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/general.scripts/scripts/fonctions.R") # month.df

# ============================================================================= /
# Lecture, agglomération des données ----
# ============================================================================= /
# listes de données
weather.files <- list.files(path = "connectivite/data/raw", pattern = "meteoStat.data.", full.names = T) # issus directement de MeteoStat, script "recherche_station_meteo_ID_v2.0.r"
# https://dev.meteostat.net/parameters
# https://dev.meteostat.net/formats.html
# https://dev.meteostat.net/faq.html
station_id.phd <- read.csv("connectivite/data/raw/station_id.phd.csv") # issu du script "Recherche_station_meteo_ID_v2.0.r", trouver "station.name"

# consigne de données
weather.data.list <- list()

# boucle pour chaque site, agglomérer les onglets pertinents
for(file.no in 1:length(weather.files)) {
  file.step <- weather.files[file.no]
  site.uid <- iconv(str_extract(file.step, "(?<=(hourly|daily)\\.).*(?=\\.csv)"), to = "UTF-8-MAC") # merci à Google IA... C'est compliqué les regex /  # merci google IA pour m'aider à traiter mes noms de site avec un accent francophone...
  
  weather.pre <- read.csv(file.step)
  weather <- weather.pre %>% 
    mutate(station.name = station_id.phd$station_name[station_id.phd$phd.site.UID == site.uid],
           "site.uid" = site.uid)
  
  weather.data.list[[file.no]] <- weather # placer dans la liste de recueil des fichiers, à l'endroit "file.no"
}

# joindre les données 
tidy.weather.data.raw <- weather.data.list %>%
  map(~ .x %>% mutate(across(everything(), as.character))) %>% # d'abord, tout en caractères, car classe des NA en arrière plan posait problème
  reduce(full_join, na_matches = "na") %>% # précision de la gestion des NA pour débugger (voir code débuggage ci-dessous), cela ajoutait 13 lignes autrement; merci à GoogleIA pour l'aide au débuggage
  mutate(pres.kpa = as.numeric(pres)/10) %>%  # pression donnée en hPa (hectopascal). 1 hPa = 0,1 kPa. Example: convert 15 hPa to kPa: 15 hPa = 15 × 0.1 kPa = 1.5 kPa
  select(site.uid, station.name, pres.kpa, pres_source, everything(), -"pres", -"X")

# ============================================================================= /
# Relation pression atmophérique ~ température ambiante ----
# ============================================================================= /
## transformations ----
tidy.weather.data.raw.1 <- tidy.weather.data.raw %>% 
  mutate(across(c(temp, pres.kpa),
                ~ scale(as.numeric(.x)),
                .names = "{.col}.std")) %>% 
  # # vérfication, standardisation manuelle, sans scale(), donne mm chose 14 avril 2026 √
  # mutate(pres.kpa.mean = mean(pres.kpa, na.rm = T)) %>% 
  # mutate(pres.kpa.sd = sd(pres.kpa, na.rm = T)) %>% 
  mutate(across(station.name, as.factor)) # %>% 
  # mutate(hour = lubridate::hour(date.time.SiteTZ))

## analyse de corrélation sur l'ensemble du jeux de données ----
cor.test(tidy.weather.data.raw.1$temp.std, tidy.weather.data.raw.1$pres.kpa.std, method = "spearman")
# Spearman's rank correlation rho
# 
# data:  tidy.weather.data$temp and tidy.weather.data$pres
# S = 5.3203e+13, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#         rho 
# -0.03509721 
# Un rho de -0,035 signifie que la température n'explique quasiment aucune variation de la pression dans ce jeu de données précis

# ## analyse de corrélation sur 24h  ----
# df_24h <- tidy.weather.data.raw.1 %>%
#   arrange(date.time.UTC.0) %>% # refaire, caduque
#   mutate(
#     # calcul de la corrélation sur une fenêtre glissante de 24h
#     cor_glissante = slide2_dbl(
#       .x = temp.std, 
#       .y = pres.kpa.std, 
#       .f = ~cor(.x, .y, method = "spearman"),
#       .before = 24,
#       .complete = TRUE
#     )
#   )
# summary(df_24h$cor_glissante)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# #   -1.00   -0.71   -0.33   -0.22    0.23    1.00   37748 
# #     Médiane à -0.33 : La relation est globalement négative (la pression baisse quand il fait chaud), ce qui valide votre capteur.
# #     Min à -1.00 : Certains jours, la corrélation est parfaite (thermique pure).
# #     Max à +1.00 : Certains jours, la pression monte avec la température 
# #     Moyenne à -0.22 : C'est bien plus significatif que le -0.03 global, car cela montre l'effet thermique quotidien moyen

## correction de l'effet température, calcul des résidus de modèle pres~temp ----
### i. vérification de la distribution de la variable réponse ----
hist(tidy.weather.data.raw.1$pres.kpa)
ks.test(x = tidy.weather.data.raw.1$pres.kpa, y = 'pnorm', alternative = 'two.sided')
qqnorm(tidy.weather.data.raw.1$pres.kpa)
qqline(tidy.weather.data.raw.1$pres.kpa)
# shapiro.test(tidy.weather.data$pres.kpa) # + de 5000 données, non-normal presque automatiquement
# est-ce donc "assez" normal ?, coefficient d'asymétrie (pckg moments) :
skewness(tidy.weather.data.raw.1$pres.kpa, na.rm = TRUE)
# [1] -0.519292
# La décision statistique (selon google IA) :
# si Test T ou une Régression Linéaire : ignorer ce résultat; théorème central limite compense largement une asymétrie de -0.52

### ii. mod.climate -----
plot(tidy.weather.data.raw.1$pres.kpa.std, tidy.weather.data.raw.1$temp.std)
mod.climate.0 <- lm(pres.kpa.std ~ temp.std,
                    data = tidy.weather.data.raw.1, 
                    na.action = na.exclude) 

mod.climate.1 <- lme(pres.kpa.std ~ temp.std, 
                     random = ~ 1 | station.name, 
                     data = tidy.weather.data.raw.1, 
                     na.action = na.exclude)
summary(mod.climate.1)
#### summary mod.climate.1 ----
# Linear mixed-effects model fit by REML
# Data: tidy.weather.data.tr 
# AIC      BIC    logLik
# 191262 191298.5 -95627.02
# 
# Random effects:
#   Formula: ~1 | station.name
# (Intercept)  Residual
# StdDev:  0.09301493 0.9962314
# 
# Fixed effects:  pres.kpa.std ~ temp.std 
# Value  Std.Error    DF   t-value p-value
# (Intercept)  0.00050454 0.04666514 67557  0.010812  0.9914
# temp.std    -0.03410255 0.00382762 67557 -8.909600  0.0000
# Correlation: 
#   (Intr)
# temp.std 0     
# 
# Standardized Within-Group Residuals:
#   Min          Q1         Med          Q3         Max 
# -5.37124448 -0.55194908  0.06653494  0.65633898  3.43461339 
# 
# Number of Observations: 67562
# Number of Groups: 4 
#### fin summary mod.climate.1 ----

# mod.climate.2 <- lme4::lmer(pres.kpa.std ~ temp.std + hour + 1 | station.name, 
#                             data = tidy.weather.data.tr, 
#                             REML = T)
# isSingular
mod.climate.3 <- lm(pres.kpa.std ~ temp.std + as.numeric(hour),
                    data = tidy.weather.data.raw.1, 
                    na.action = na.exclude)
#### summary mod.climate.3 ----
# Call:
#   lm(formula = pres.kpa.std ~ temp.std + hour, data = tidy.weather.data.tr)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.4397 -0.5480  0.0677  0.6634  3.5227 
# 
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.0255653  0.0074891   3.414 0.000641 ***
#   temp.std    -0.0304565  0.0038622  -7.886 3.17e-15 ***
#   hour        -0.0022241  0.0005589  -3.979 6.92e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9994 on 67559 degrees of freedom
# (3622 observations deleted due to missingness)
# Multiple R-squared:  0.001271,	Adjusted R-squared:  0.001242 
# F-statistic:    43 on 2 and 67559 DF,  p-value: < 2.2e-16
#### fin summary mod.climate.3 ----

### iii. sélection de modèle ----
AIC(mod.climate.0, mod.climate.1, mod.climate.3)
# df      AIC
# mod.climate.0  3 191667.5
# mod.climate.1  5 191262.0 # meilleur
# mod.climate.3  4 191653.7

### iv. vérification des suppositions ----
plot(mod.climate.1) # pas tant hétéroscédastique
hist(residuals(mod.climate.1)) # ça a l'air normal, même si légère asymétrie à gauche
skewness(na.omit(residuals(mod.climate.1))) # asymétrie modérée

### v. résidus de pression enregistré pour calibrer sondes ----
# explications : pression résiduelle + moyenne (pour revenir aux données bruttes et non centrées-réduites),
# signifie pression non-expliquée par le modèle mod.climate.1, comportant l'effet de la station ou de la température) 
# calibration des sondes, voir les fonctions (fonctions_phs_v3.1.R) et le traitement complet des sondes 
# (data_water.table_all_v3.1.R) pour la calibration utilisant ces données corrigée (implantée 13 avril 2026)
tidy.weather.data <- tidy.weather.data.raw.1 %>% 
  mutate(pres.kpa.res = 
           (residuals(mod.climate.1) * 
              sd(tidy.weather.data.raw.1$pres.kpa, na.rm = T)) +
           mean(tidy.weather.data.raw.1$pres.kpa, na.rm = T)) %>%
  # vérif : données originales donnent la même valeur (pres.kpa.res et pres.kpa.res.2)
  # mutate(pres.kpa.res.2 =
  #          (residuals(mod.climate.1) * pres.kpa.sd) + pres.kpa.mean)
  select(site.uid, station.name, pres.kpa, pres.kpa.res, everything(), -c(temp.std, pres.kpa.std)) # enlever colonnes inutiles (temporaires, utilisées pour la régression linéaire seulement)

### vi. enregistrement ----
# si fichier n'existe pas déjà :
# filter la base de données, recréer des fichier distincts par site.uid, stocker dans data/clean
# sinon, arrêt et avertissement
URLs.list <- vector()
for (site in 1:length(unique(tidy.weather.data$site.uid))) {
  URLs.list[site] <- paste0("meteoStat.data.hourly.res.", unique(tidy.weather.data$site.uid)[site], ".csv")
}
if(any(URLs.list %in% list.files("connectivite/data/clean")))  { # si TRUE = STOP et warning // si FALSE = continuer la boucle (donc rien, donc IF statement)
  stop("Attention, un fichier du même nom se trouve dans le dossier. En outrepassant cet avertissement, le fichier ancier sera effacé et remplacé.")
} else { 
  for (i in 1:length(unique(tidy.weather.data$site.uid))) {
    # i<-1
    tidy.weather.data.i <- tidy.weather.data %>% 
      dplyr::filter(site.uid == unique(tidy.weather.data$site.uid)[i])
    write.csv(tidy.weather.data.i,  paste0("connectivite/data/clean/meteoStat.data.hourly.res.", unique(tidy.weather.data$site.uid)[i], ".csv"), row.names = FALSE)
  }
}

# ============================================================================= /
# Autres tests ----
# ============================================================================= /
# mm données que sur ECCC en ligne ?
tidy.weather.data.beauport.oct.2025 <- tidy.weather.data %>% 
  dplyr::filter(station.name == "BEAUPORT",
                date.time.SiteTZ >= "2025-10-01",
                date.time.SiteTZ <= "2025-10-30") # mm valeurs que sur Environnement Canada mm date (26 janvier 2026)
# différence que sur ECCC pas de données de pression...
# oui..
