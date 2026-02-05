
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


# tentative WeatherCan ----
if (!require("weathercan")) install.packages("weathercan") # Integrating data from weathercan (ECCC/CCCS), Gouvernement du Canada
#### extraction des données de ECCC/CCCS et ménage ----
# transformer eccc.data avec le mm format de colonne que ll.cal.pre.i 0$date.time.tz.orig
station_ids <- "7010566"
stations[grep("BEAUPORT",stations$station_name), ]
## A tibble: 6 × 17
# prov  station_name station_id climate_id WMO_id TC_id   lat   lon  elev tz        interval start   end normals normals_1991_2020 normals_1981_2010
# <chr> <chr>             <dbl> <chr>       <dbl> <chr> <dbl> <dbl> <dbl> <chr>     <chr>    <dbl> <dbl> <lgl>   <lgl>             <lgl>            
# 1 QC    BEAUPORT           5207 7010566        NA NA     46.9 -71.2  84.1 Etc/GMT+5 day       1982  1985 FALSE   FALSE             FALSE            
# 2 QC    BEAUPORT           5207 7010566        NA NA     46.9 -71.2  84.1 Etc/GMT+5 hour        NA    NA FALSE   FALSE             FALSE            
# 3 QC    BEAUPORT           5207 7010566        NA NA     46.9 -71.2  84.1 Etc/GMT+5 month     1982  1985 FALSE   FALSE             FALSE            
# 4 QC    BEAUPORT          27803 7010565     71578 XBO    46.8 -71.2  10   Etc/GMT+5 day       1999  2023 FALSE   FALSE             FALSE            
# 5 QC    BEAUPORT          27803 7010565     71578 XBO    46.8 -71.2  10   Etc/GMT+5 hour      1999  2023 FALSE   FALSE             FALSE            
# 6 QC    BEAUPORT          27803 7010565     71578 XBO    46.8 -71.2  10   Etc/GMT+5 month       NA    NA FALSE   FALSE             FALSE            
# # ℹ 1 more variable: normals_1971_2000 <lgl>
eccc.data.pre.0 <- weather_dl(station_ids, start = "2025-10-01", end = "2025-10-30", time_disp = "none") 


