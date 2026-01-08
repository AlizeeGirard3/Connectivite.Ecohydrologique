# Script créé le 26 mars pour extraire la donnée raw de ll au moment de la mesure de bulleur
# dans le but de calibrer
# À faire : tout modifier le script data_water.table_all en fonction de ceci



# ARCHIVE, INSÉRÉ DANS DATA.WATER.TABLE.ALL (JANVIER 2026)





# FONCTIONNE, 24 DÉC. 2025 (trouver et nettoyer moment mesure de bulleur)
# traitement automatique et nettoyage des données de bulleur
# i <-80 # example avec plusieurs l
# l<-1
bulleur.data <- list()
for (l in 1:length(unique(cal.data$period.file.uid[which(grepl(files.uid.df[i,1], cal.data$file.uid))]))) { # si mm fichier.uid.i, coller les périodes ensemble (ainsi, retirer et remettre ne demande pas plus de manipulations et surtout ps des manipulations incividuelles)
  
  bulleur.data.j.pre <- unique(cal.data[which(grepl(files.uid.df[i,1], cal.data$file.uid)), c(which(grepl("period.file.uid", colnames(cal.data))), which(grepl("bulleur", colnames(cal.data))))])
  
  # extraire les chiffres des colonnes bulleur
  bulleur.cols <- colnames(bulleur.data.j.pre) # logger serial no, en base R
  numbers <- regexpr("[0-9]+", bulleur.cols)
  nbulleur <- as.numeric(regmatches(bulleur.cols, numbers))
  for (j in 1:length(unique(nbulleur))) {
    bulleur.data.j <- bulleur.data.j.pre %>% dplyr::select(grep(j, bulleur.cols)) %>%  # sélect si contient j dans les noms de colonne
      # j'obtiens les colonnes avec j, je crée un df aec juste ces colonnes
      # j'ajoute une colonne avec le chiffre }
      mutate(bulleur.no = rep(j, nrow(bulleur.data.j.pre)),
             period.file.uid = bulleur.data.j.pre$period.file.uid)
    colnames(bulleur.data.j) <- sub('[[:digit:]]+', '', colnames(bulleur.data.j)) # nom colonne sans chiffre
    bulleur.data[[j]] <- bulleur.data.j
  }
  # rbind les lignes des j df
  bulleur.data.appendd.l <- do.call(rbind, bulleur.data) %>% drop_na(., in.bulleur.prof.cm)
}
# bulleur.data.i <- do.call(rbind, bulleur.data.appendd.l) # row bind -> on colle deux df de structure identique (les ll.cal.pre.i) de différents i.l, associées à différents temps de la période de mesure de la sonde i

# bulleur.data.appendd.l$in.bulleur.time.tz.orig <- gsub(":00", ":01", bulleur.data.appendd.l$in.bulleur.time.tz.orig) # sinon, les données 00:00:00 étaient effacées !

tidy.bulleur.data.pre <- bulleur.data.appendd.l %>% mutate(date.JJ.MM.AAAA_time.HH.MM.SS_tz = paste0(in.bulleur.date.aaaammdd, " ", in.bulleur.time.tz.orig, " ", tz)) # tz original
tidy.bulleur.data.pre$date.time.tz.orig <- readr::parse_datetime(tidy.bulleur.data.pre$date.JJ.MM.AAAA_time.HH.MM.SS_tz, format = '%Y-%m-%d %H:%M:%S %Z', locale = readr::locale(tz = tz)) # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation

# RENDU LÀ
# POURQUOI CA CRÉE UNE LISTE INFINE ?

# tidy.bulleur.data.pre$date.time.roundd <- round(tidy.bulleur.data.pre$date.time.tz.orig, units="hours")
# tidy.bulleur.data.pre$date.time.roundd <- gsub("00:00", "00:01", tidy.bulleur.data.pre$date.time.roundd) # sinon, les données 00:00:00 étaient effacées !




# PROCHAINE ÉTAPE: 
# moment du bulleur -> tidy date -> comparer à données tiyd au mm moment -> extraire cal.value à ce moment là -> mettre dans cal. data (remplacer valeur NA de la case)



ll.offset.measurement.df <- data.frame(file.uid = NA, offset.measurement.bulleur.time = NA) # pour stocker les fihcier.uid (aussi première colonne de cal.data)
for (i in 1:length(tidy.WTD.data)) {
  # x<-95
  print(i)
  tidy.WTD.data[[i]] # début de la loop pour les ODYSSEY (if() prochaine ligne)
  if (any(grepl(SNH[1], tidy.WTD.data[[i]]$metadata))) {  # début de la loop pour les ODYSSEY
    # if(i %in% c(6,11)) { # erreur aux sondes 6 et 11 de l'ancien ordre ?
    #   ll.offset.measurement.df[i,1] <- gsub("file.uid : ", "", tidy.WTD.data[[i]]$metadata[10])
    #   ll.offset.measurement.df[i,2] <- "NA"
    # } else {
    ll.offset.measurement.df[i,1] <- gsub("file.uid : ", "", tidy.WTD.data[[i]]$metadata[10])
    # ll.offset.measurement.df[i,2] <- tidy.WTD.data[[i]]$data$raw.value.mm[tidy.WTD.data[[i]]$data$date.time.tz.orig == '2024-11-14 10:00:01'] }
    # ll.offset.measurement.df[i,2] <- tidy.WTD.data[[i]]$data$raw.value.mm[tidy.WTD.data[[i]]$data$date.time.tz.orig == 'moment du bulleur trouvé automatiquement'] }
    
    # ICI, SENSÉ ÊTRE LE MOMENT OÙ J'AI UNE MESURE DE BULLEUR    
    # SI J'EN AI PLSUIERUS, FAIRE UNE MOYENNE DU OFFSET **
     # }
}}






# NON ANCIEN INVALIDE
# 
# # for (l in 1:length(unique(cal.data$period.file.uid[which(grepl(file.uid.df[i,1], cal.data$file.uid))]))) { # si mm fichier.uid.i, coller les périodes ensemble (ainsi, retirer et remettre ne demande pas plus de manipulations et surtout ps des manipulations incividuelles)
# #   cal.data$in.bulleur1.prof.cm <- as.numeric(cal.data$in.bulleur1.prof.cm)
# i<-95
# bulleur.data.pre.0 <- cal.data[which(grepl(files.uid.df[i,1], cal.data$file.uid)), c(which(grepl("period.file.uid", colnames(cal.data))), which(grepl("bulleur", colnames(cal.data))))]
# bulleur.no <- str_extract(colnames(bulleur.data.pre.0), "\\d+")
# bulleur.data.pre.1 <- rbind(bulleur.data.pre.0, bulleur.no) 
# colnames(bulleur.data.pre.1) <- sub('[[:digit:]]+', '', colnames(bulleur.data.pre.1))
# 
# for (j in 2:length(unique(colnames(bulleur.data.pre.1)))) {
#   # bulleur.data.pre.2 <- bulleur.data.pre.1 %>% select(c(which(colnames(bulleur.data.pre.1) == "period.file.uid"), grep(pattern = paste(colnames(bulleur.data.pre.1)[j]), x = colnames(bulleur.data.pre.1))))
#   
# }
# 
# # ne fonctionn pas aprce que colonnes mm nom
# # ///
# colnames.bulleur.data.pre <- colnames(cal.data[which(grepl(files.uid.df[i,1], cal.data$file.uid)), c(which(grepl("period.file.uid", colnames(cal.data))), which(grepl("bulleur", colnames(cal.data))))])
# colnames.bulleur.data <- sub('[[:digit:]]+', '', colnames.bulleur.data.pre)
# 
# for (j in 2:length(unique(colnames.bulleur.data))) { }
# bulleur.data.pre <- cal.data %>% dplyr::select(c(which(colnames(bulleur.data.pre.1) == "period.file.uid"), grep(pattern = paste(colnames(bulleur.data.pre.1)[j]), x = colnames(bulleur.data.pre.1)))) %>% 
#   dplyr::filter(grepl(files.uid.df[i,1], cal.data$file.uid))
# # ne fonctionne pas non plus !!! je tourne en rond !!
# 
# 
# # bulleur.data.long <- bulleur.data %>% pivot_longer(, names_to = "bulleur", values_to = "value")
# # rbind(bulleur.data$`1`)
# 
# # bulleur.data.pre.1 <- bulleur.data.pre.0 %>% select(!c(grep(".obs", colnames(bulleur.data.pre.0)), grep("date.time.UTC.0", colnames(bulleur.data.pre.0))))
# 
# # parse_number(colnames(bulleur.data.pre))
# ## [1] 10  6  4 15
# # orig.colnames <- as.data.frame(t(colnames(bulleur.data.pre)))
# # bulleur.data.pre[1,] <- colnames(bulleur.data.pre)
# 
# # gregexpr("[0-9]+", colnames(bulleur.data))
# 
# 
# # exemple ci-dessous tiré de mon code
#   # if (length(unique(cal.data$period.file.uid[which(grepl(files.uid.df[i,1], cal.data$file.uid))])) != 0) { print(l)
#   #   cal.data.i.l <- unique(cal.data[which(grepl(files.uid.df[i,1], cal.data$file.uid)),
#   #                                   c("file.uid", "site.uid", "well.uid", "trmnt.uid", 'lab.probe.id', 'probe.uid', 'probe.brand',
#   #                                     "day.begining.aaaa.mm.dd.hh.mm", 'day.end.aaaa.mm.dd.hh.mm', "period.file.uid")])[l,] # cal.data.i.l = les infos dont j'ai besoin pour recouper selon la période l du fichier i
#   #   period.file.uid.l <- cal.data.i.l$period.file.uid
#   #   # recoupage de ll.pre.data selon cal.data selon début et fin des mesures et retrait de colonnes
#   #   ll.clean.l.pre <- ll.clean %>%
#   #     dplyr::filter(date.time.tz.orig >= cal.data.i.l$day.begining.aaaa.mm.dd.hh.mm) %>% # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data.i.l
#   #     dplyr::filter(date.time.tz.orig <= cal.data.i.l$day.end.aaaa.mm.dd.hh.mm) %>% # <= date de mesure de NP plus petite ou égale à la date end dans cal.data.i.l 
#   #     select("scan.id", "raw.value.kPa_pres.abs", "calibrated.value.cm",  "temperature_dC", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0") # %>%  # date et time sans "UTC.0" sont dans le fuseau horaire d'origine (tz trouvé en croisant les coordonnées "coords")
#   #   # répliquer les données cal.data.k.l à chaque ligne de ll.pre.0.data.4.l.pre
#   #   cal.data.i.l.all <- cal.data[cal.data$period.file.uid == period.file.uid.l,]
#   #   rownames(cal.data.i.l.all) <- NULL
#   #   cal.data.i.l.rep <- cbind(cal.data.i.l.all, rep(row.names(cal.data.i.l.all), each = nrow(ll.clean.l.pre)))
#   #   colnames(cal.data.i.l.rep)
#   #   # assembler les colonnes
#   #   ll.clean.l <- bind_cols(ll.clean.l.pre, cal.data.i.l.rep)
#   #   ll.clean.l <- ll.clean.l %>% select(!"rep(row.names(cal.data.i.l.all), each = nrow(ll.clean.l.pre))")
#   #   # chaque cal.data.k = une section de mesures de la sonde k, durant l'été, associée ou non à une mesure au bulleur et à une longueur de fil
#   #   # vérifications
#   #   head(ll.clean.l); colnames(ll.clean.l); nrow(ll.clean.l)
#   #   # changer pour un nom explicite, fichier encore à calibrer (d'où "pre")
#   #   ll.cal.pre.i.l[[l]] <- ll.clean.l
#   # }
#   # ll.cal.pre.i <- do.call(rbind, ll.cal.pre.i.l) # row bind -> on colle deux df de structure identique (les ll.cal.pre.i) de différents i.l, associées à différents temps de la période de mesure de la sonde i
#   