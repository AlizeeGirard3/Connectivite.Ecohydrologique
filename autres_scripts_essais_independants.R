
# Données triées par emplacement (tests, 12 janvier 2026) ----
# tidy.WTD.data.df <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.df.RDS") # issu du code "data_water.table_all_v3.0"
# tidy.cal.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.cal.data.RDS") %>% # issu du code "data_water.table_all_v3.0"
#   dplyr::filter(startsWith(well.uid, "STH.D2.m1m.2025"))
# 
# tidy.WTD.data.df.well <- tidy.WTD.data.df %>% 
#   dplyr::filter()
#   left_join(tidy.WTD.data.df, tidy.cal.data)




# Données lissées (tests, 12 janvier 2026) ----
tidy.WTD.data.df <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.df.RDS") # issu du code "data_water.table_all_v3.0"
tidy.cal.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.cal.data.RDS") # issu du code "data_water.table_all_v3.0"
tidy.WTD.data <- readRDS("~/Documents/Doctorat/_R.&.Stats_PhD/connectivite/data/clean/tidy.WTD.data.RDS") # issu du code "data_water.table_all_v3.0"

# test 1 : smoother avec la fonction smooth

# CHERCHER QUELS PARAMÈTRES APPLIQUER, COMPARER AVANT APRÈS
# tidy.WTD.data.df.smooth <- smooth(tidy.WTD.data.df)
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/smooth

# test 2 : smoother avec la fonction Savitzky-Golay
# tidy.WTD.data.df.savgol <- 
# https://www.youtube.com/watch?v=caJAeMTZbgM


# test 3 : smoother avec les deux fonctions (smooth, Savitzky-Golay)
# tidy.WTD.data.df.smooth.savgol <- 



# Thème standardisé de graphiques de nappe ----


# y référer après dans water.table_visualisation


# tests visualisation données bruttes issues de MeteoStats
setwd("~/Documents/Doctorat/_R.&.Stats_PhD")
library(tidyverse)
meteoStat.example <- read.csv("connectivite/data/raw/meteoStat.data.Inkerman.csv")
colnames(meteoStat.example)
meteoStat.example <- meteoStat.example %>%
  mutate(YYYYMMDDHHMM.unprsd = paste0(year, "-", month, "-", day, "-", hour))
  meteoStat.example <- meteoStat.example %>%
  mutate(YYYYMMDDHHMMSS = parse_date_time(YYYYMMDDHHMM.unprsd, ))
meteoStat.example %>% 
  ggplot() + 
  geom_line(aes(, pres))





# tentatives knit en HTML (voir cours 1 FOR-7046) ----
if (!require("rmarkdown")) install.packages("rmarkdown")
if (!require("tools")) install.packages("tools")

render(
  input = "~/Documents/Doctorat/_FOR-7046_H26_Bayésien/20260114_cours 1/reproducible/Exemple-latex.tex",
  output_format = "html_document"
)
# # on ne peut pas setwd ?
# 
render(
  input = "~/Documents/Doctorat/_FOR-7046_H26_Bayésien/20260114_cours /1reproducible/Exemple-latex.tex",
  output_format = "pdf_document"
)
setwd("~/Documents/Doctorat/_FOR-7046_H26_Bayésien/20260114_cours 1")
texi2pdf("Reproducible/Exemple-latex.tex")
#Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
# Running 'texi2dvi' on '/reproducible/Exemple-latex.tex' failed. 
# Messages:
#   /opt/R/arm64/bin/texi2dvi: cannot read /reproducible/Exemple-latex.tex, skipping.


# autres codes ----
# option d'arrêter le code si message d'erreur (source fonctions.R)
# options(error=pause)
# options(error=NULL) # annuler

