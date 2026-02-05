# New level_logger_calibration_all.csv

# NE FONCTIONNE PAS TROP COMPLIQUÉ POUR LES CODES DE COMPRENDRE COMMENT AMALGAMER LES DONNÉES

# if (!require("dplyr")) install.packages("dplyr") # entre autres : left_join()
# 
# setwd("~/Documents/Doctorat/_R.&.Stats_PhD")
# 
# pretestCalib <- read.csv("connectivite/data/raw/level.logger.calibration.all.csv", sep = ";", dec = ",")
# str(pretestCalib)
# as.POSIXct(pretestCalib$'bulleur.1.date.aaaa-mm-dd')
# 
# #  $ out.long.tuyau.sol.cm        : chr  "17,4" "17,4" "28,6" "28,6" ...
# testCalibOdyssey <- readxl::read_xlsx("connectivite/data/raw/TestCalibOdyssey.xlsx",
#                                sheet = "ALIZ_vérif_25mars") #%>% group_by("ID.unique")
# str(testCalibOdyssey)
# testCalibOdyssey$`bulleur.1.date.aaaa-mm-dd` <- as.POSIXct(testCalibOdyssey$`bulleur.1.date.aaaa-mm-dd`)
# testCalibOdyssey$bulleur.1.time.tz.orig <- as.character(testCalibOdyssey$bulleur.1.time.tz.orig)
# 
# level_logger_calibration_all <- full_join(testCalibOdyssey, pretestCalib)
# 
# write.csv(level_logger_calibration_all, "connectivite/data/raw/level_logger_calibration_all.csv")
