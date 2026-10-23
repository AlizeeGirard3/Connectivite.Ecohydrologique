#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                              Analyses, réflexions préliminaires
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Description -------------------------------------------------------------
##########################################################################-
# Fait par :  Alizée Girard
# Affiliation :   ULaval
# Date création initiale : 2026-10-22
# Date mise à jour :
# Pourquoi : Réflexions préliminaires
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
# SOURCES : 
# 
# NOTES : 
# Corrélation entre les prédicteurs (X) : fonction cor() sur  matrice de données (vérif si variables explicatives liées entre elles)
# Corrélation entre les coefficients (β) : vcov(mod) -> puis  transformer en matrice de corrélation
# VIF (Variance Inflation Factor) : quantifie l'impact de ces corrélations sur la précision de votre modèle
#
# LEXIQUE :
#
##########################################################################-

# ============================================================================= /
# Initialisation ----
# ============================================================================= /
# .rs.restartR()

# environnement de travail
# setwd("/Users/Aliz/Documents/Doctorat/_R_Stats_PhD/FOR7046")

# script contenant des fonctions utiles
# source("scripts/_Alizee/fonctions.R")

# autres librairies utiles, le cas échéant
# # note : Autres packages chargés dans le script sourcé "fonctions.r". Notamment Tidyverse: méta package incluant dplyr, tidyr, purrr, stringr, lubridate, ect.
# if (!require("AER")) install.packages("AER") # dispertion test stat (distr Poisson)
# if (!require("car")) install.packages("car") # autocorrélation problématique ? vif()
# # detach(package:lme4)
# # if (!require("nlme")) install.packages("nlme") # linear mixed models
# # detach(package:nlme)
# if (!require("lme4")) install.packages("lme4") # linear mixed models
# if (!require("DHARMa")) install.packages("DHARMa") # linear mixed models diagnostiques
# if (!require("AICcmodavg")) install.packages("AICcmodavg") # dignostique multimodèle aictab() / nécessite tous modèles mm type
# # if (!require("emmeans")) install.packages("emmeans") # caduque  // SE pour modèles glmer()
# if (!require("performance")) install.packages("performance") # pour un quasi R2

# ============================================================================= /
# Import des données ----
# ============================================================================= /
# tidy.female.db <- readRDS("data/clean/tidy.female.db.rds")

