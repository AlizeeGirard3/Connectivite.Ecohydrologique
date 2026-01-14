
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





















