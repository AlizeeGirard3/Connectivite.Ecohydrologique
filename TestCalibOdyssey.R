# Script créé le 26 mars pour extraire la donnée raw de ll au moment de la mesure de bulleur
# dans le but de calibrer les sondes Odyssey
# À faire : tout modifier le script data_water.table_all en fonction de ceci


# CADUQUE -> MIS DANS data_water.table_all_V2.0 le 5 janvier







# FONCTIONNE, 24 DÉC. 2025 (trouver et nettoyer moment mesure de bulleur)
# traitement automatique et nettoyage des données de bulleur
if(grepl("odyssey", raw.ll.files[i])) {
  for (i in grep("_odyssey", raw.ll.files)) { # pour les fichiers odyssey référés apr leur ordre dans le dossier brut = effectuer les prochaines lignes
    # remplacer pas un IF dans la fonction (encore, comme les autres)
    # i<-70 # exemple avec plusieurs mesures de bulleur 
    # k<-i # example fichier à plusieurs séquences tempporelles dans l'été

    # recherche de lignes et nettoyage
    # explications : pour chaque séquences valides de fichier-emplacement-année, aller chercher les lignes dans cal.data 
    # et créer un tableur spécifique à la sonde (bulleur.data), et nettoyer les données
    step <- c("cal", "bulleur")
    cal.bulleur.list.appendd <- list()
    # boucle si c'est un odyssey : sélectionner les lignes du fichier i
    # fonctionne aussi si plusieurs périodes valides pour un fichier: toutes les coller ensemble (si plusieurs ligne dans la recherche : cal.data.pre$period.file.uid[which(grepl(files.uid.df[k,1], cal.data.pre$file.uid))])
    odyssey.data.pre <- unique(cal.data.pre[which(grepl(files.uid.df[i,1], cal.data.pre$file.uid)),]) # actuellement, toutes colonnes conservées // c(which(grepl("period.file.uid", colnames(cal.data.pre))), grep("row.uid", colnames(cal.data.pre)), which(grepl("bulleur", colnames(cal.data.pre))))
    
    # # créer deux tableurs distincts, dans une liste : l'un oiyr les données de calibration, l'un pour les données de bulleur
    odyssey.data.cal <- odyssey.data.pre %>% select(!contains("in.bulleur"))
    odyssey.data.bulleur <- odyssey.data.pre %>% select(!contains("cal."))
    odyssey.data.list <- list(odyssey.data.cal, odyssey.data.bulleur)
    
    # extraire les chiffres des colonnes bulleur/cal
    # pour les colonnes contenant des chiffres sauf "pt.bas", 
    # groupper par "in.bulleur" ou "cal."
    # pour chque groupe
    # faire ces étapes de :
    # transformer les colonnes en lignes aux informations répétées (large ton long)
    
    for(j in 1:length(odyssey.data.list)) {
      # j<-12
      # pour chaque objet de la liste, trouver nom de colonnes extraire leur chiffre associé, le cas échéant, puis enlever le chiffre et refaire le df
      odyssey.cols <- colnames(odyssey.data.list[[j]])
      numbers <- regexpr("[0-9]+", odyssey.cols)
      nstep <- as.numeric(regmatches(odyssey.cols, numbers))
      odyssey.data <- list()
      
      for (k in unique(nstep)) {
        # k<-1
        remove <- paste(setdiff(unique(nstep), k), collapse = "|")
        odyssey.data.j.k <- odyssey.data.list[[j]] %>% select(!matches(remove)) %>% 
          # dplyr::select(!contains(remove, colnames(odyssey.data.list[[j]]))) %>%  # sélect si contient j dans les noms de colonne
          # j'obtiens les colonnes de chiffre (step) k, je crée un df avec juste ces colonnes
          # j'ajoute une colonne avec le chiffre }
          # mutate_at(grep(j, odyssey.cols), rep(k, nrow(odyssey.data.pre))) %>% 
          mutate(!!paste0(step[j],".no") := rep(k, nrow(odyssey.data.pre)),
                 period.file.uid = odyssey.data.pre$period.file.uid,
                 row.uid = odyssey.data.pre$row.uid)
        # odyssey.data.j.k <- odyssey.data.j.k.pre %>% select_if(~ !any(is.na(.)))
        colnames(odyssey.data.j.k) <- sub('[[:digit:]]+', '', colnames(odyssey.data.j.k)) # nom colonne sans chiffre
        odyssey.data[[k]] <- odyssey.data.j.k
      }
    # rbind les lignes des j df
    if(j == 1) {  cal.bulleur.list.appendd[[1]] <- do.call(rbind, odyssey.data) } else { 
      cal.bulleur.list.appendd[[2]] <- do.call(rbind, odyssey.data) %>% drop_na(., in.bulleur.prof.cm)
    }}
    rm(odyssey.data.pre); rm(odyssey.data.bulleur); rm(odyssey.data.cal); rm(odyssey.data); rm(odyssey.data.j.k); rm(j); rm(k) # supprimer vieux objets (fait automatiquement dans une fonction)

    
    # normal qu'il y ait des NA dans le df bulleur, les enlever (dépend du nombre de données de bulleur prises)
    # préparation de la date-heure en prévision de la comparaison de date-heure entre tableaux
    tidy.bulleur.data.pre.0 <- cal.bulleur.list.appendd[[2]] %>% mutate(date.JJ.MM.AAAA_time.HH.MM.SS_tz = paste0(in.bulleur.date.aaaammdd, " ", in.bulleur.time.tz.orig, " ", tz)) # tz original
    tidy.bulleur.data.pre.0$date.time.tz.orig <- readr::parse_datetime(tidy.bulleur.data.pre.0$date.JJ.MM.AAAA_time.HH.MM.SS_tz, format = '%Y-%m-%d %H:%M:%S %Z', locale = readr::locale(tz = tz)) # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
    tidy.bulleur.data.pre <- tidy.bulleur.data.pre.0 %>% mutate(date.time.roundd.pre = round_date(tidy.bulleur.data.pre.0$date.time.tz.orig, "hours"))
    tidy.bulleur.data.pre$date.time.roundd <- gsub("00:00", "00:01", tidy.bulleur.data.pre$date.time.roundd.pre)
    tidy.bulleur.data <- tidy.bulleur.data.pre %>%
      mutate(date.time.roundd = readr::parse_datetime(date.time.roundd, locale = readr::locale(tz = tz))) %>% # remise de date.time.roundd en classe POSIX
      select(!c(date.JJ.MM.AAAA_time.HH.MM.SS_tz, date.time.tz.orig, date.time.roundd.pre))
    # rm(tidy.bulleur.data.pre, tidy.bulleur.data.pre.0)
    # colonne date.time.UTC.0
    tidy.bulleur.data$date.time.UTC.0pre <- with_tz(tidy.bulleur.data$date.time.roundd, tz = "UTC") # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
    tidy.bulleur.data$date.time.UTC.0pre.1 <- format_iso_8601(tidy.bulleur.data$date.time.UTC.0pre)
    tidy.bulleur.data$date.time.UTC.0 <- gsub("[+]00:00", "Z",  tidy.bulleur.data$date.time.UTC.0pre.1)
    tidy.bulleur.data <- tidy.bulleur.data %>%
      mutate(in.bulleur.prof.mm = in.bulleur.prof.cm * 10) %>% # données de bulleur en mm pour correspondre aux cal.val
      mutate(in.bulleur.rel.to.surface.mm = in.bulleur.rel.to.surface.cm * 10) %>% # données de bulleur en mm pour correspondre aux cal.val
      select(!c(date.time.UTC.0pre, date.time.UTC.0pre.1, date.time.roundd, in.bulleur.prof.cm, in.bulleur.rel.to.surface.cm))
    # joindre par la colonne en commun "date.time.UTC.0"
    tidy.cal.bulleur.data.pre <- left_join(tidy.bulleur.data, ll.cal.pre.i)  # comparaions aux données (raw.val, en (UNITÉS?) de sonde (i) au même moment que chaque mesure (ligne) de tidy.bulleur.data // selon Wikipedia, il y aurait des mSiemens/mm qqpart
    tidy.cal.bulleur.data <- full_join(tidy.cal.bulleur.data.pre, cal.bulleur.list.appendd[[1]], relationship = "many-to-many")
    
    # coller la valeur enregistrée (raw.value.mm) au moment du bulleur dans cal.value où cal.no == 3
    tidy.cal.bulleur.data <- tidy.cal.bulleur.data %>% #dplyr::filter() %>% 
      mutate(cal.value = ifelse(cal.no == "3", paste(raw.value.mm), cal.value)) %>% mutate(cal.value = as.numeric(cal.value), 
                                                                                           cal.neg.length_mm= as.numeric(cal.neg.length_mm))
   
    # rm(tidy.cal.bulleur.data.pre); rm(tidy.bulleur.data.pre.0); rm(tidy.bulleur.data.pre) # supprimer vieux objets (fait automatiquement dans une fonction)
    # 
    # 
    # ### calibration ----
    # # PRÉALABLE : utiliser la valeur NÉGATIVE de longueur de fil à la calibration
    # #### étape 1 : si y=ax+b, calcul des termes a et b  ----
    # # FORMULES
    # # a.slope = ( y2 - y1 ) / ( x2 - x1 ), soit la proportion de changement de y pour chaque changement de x
    # # où
    # # y = raw.value aux longueurs 1 et 2 du test de calibration (p. ex. 200 mm et 800 mm ou 1400 mm, pour STH)
    # # x2 = longueur fil test où "cal.order"=2, x1 = longueur fil test où "cal.order"=1
    # # et finalement
    # # b.verticalIntercept = y1 - a.slope * x1
    # {
    #   CDS <- data.frame(type = c("HOBO U20", "HOBO U20L", "ODYSSEY"), # Hobo seulement : mesure longueur du fil tel que dans protocole; à la limite de la boîte de sonde. Les constantes de longueur de boîte de sonde à la sonde à l'interface intérieur de la sonde sont ajoutées à cette étape-ci.
    #                     constante = c("12.93", "13.3", "0")) %>%
    #     mutate_at('constante', as.numeric) # liste des types de SNH avec lesquelles j'ai pris des données; chaque "marque/modèle" (type) est traitée de façon différente
    #   y2 = unique(tidy.cal.bulleur.data$cal.neg.length_mm[tidy.cal.bulleur.data$cal.no=="2"]) # en cm et au négatif
    #   y1 = unique(tidy.cal.bulleur.data$cal.neg.length_mm[tidy.cal.bulleur.data$cal.no=="1"]) # en cm et au négatif
    #   x2 = unique(tidy.cal.bulleur.data$cal.value[tidy.cal.bulleur.data$cal.no =="2"]) + CDS$constante[CDS$type == brand.i] # pour les ODYSSEY, valeur CDS = 0
    #   x1 = unique(tidy.cal.bulleur.data$cal.value[tidy.cal.bulleur.data$cal.no =="1"])
    #   a.slope = ( y2 - y1 ) / ( x2 - x1 ) # sans unité
    #   b.verticalIntercept = y1 - (a.slope * x1) # mm - SU*?
    # }
    # tidy.cal.bulleur.data <- tidy.cal.bulleur.data %>% mutate(cal.neg.length_mm = ifelse(cal.no == "3", (tidy.cal.bulleur.data$cal.value[tidy.cal.bulleur.data$cal.no=="3"]*a.slope)+b.verticalIntercept, cal.neg.length_mm)) %>%
    #   mutate(prof_nappe_odyssey_cm_plus.out = cal.neg.length_mm/10 + tidy.cal.bulleur.data$out.long.tuyau.sol.cm,
    #          prof_nappe_bulleur_cm_plus.out = `in.bulleur.rel.to.surface.mm`/10 + out.long.tuyau.sol.cm,
    #          offset_cm = prof_nappe_odyssey_cm_plus.out - prof_nappe_bulleur_cm_plus.out)


    # TOUT VÉRIFIER !!! et finaliser en comptabilisant cette donnée:
    #     ll.cal.pre.i$calibrated.value.cm = (((ll.cal.pre.i$raw.value.mm*a.slope) + b.verticalIntercept)/10) + cal.probe.i$out.long.tuyau.sol.cm[cal.probe.i$measure_type=="offset_measurement"] - offset_cm
    # aussi : réfléchir à comment utiliser plusieurs données de bulleur pour calibrer
    
    
    
    
    
    
    
    
    
    # pre_prof_nappe_odyssey_mm_to_cm <- cal.length_mm.vector/10 #  sensé donner NA (mais actuellement remplis, à écraser avec calcul automatisé), on va remplir cette donnée avec les nouvelles valeurs -> longueur fictive em mm transformée en cm
    # prof_nappe_odyssey_cm_plus.out <- pre_prof_nappe_odyssey_mm_to_cm + cal.probe.i$out.long.tuyau.sol.cm[cal.probe.i$measure_type=="offset_measurement"]
    # prof_nappe_bulleur_cm_plus.out <- cal.probe.i$`in.bulleur1.rel.to.surface.cm`[cal.probe.i$measure_type=="offset_measurement"] + cal.probe.i$out.long.tuyau.sol.cm[cal.probe.i$measure_type=="offset_measurement"]
    #     # offset_cm <- prof_nappe_odyssey_cm_plus.out - prof_nappe_bulleur_cm_plus.out
    #     ll.cal.pre.i$calibrated.value.cm = (((ll.cal.pre.i$raw.value.mm*a.slope) + b.verticalIntercept)/10) + cal.probe.i$out.long.tuyau.sol.cm[cal.probe.i$measure_type=="offset_measurement"] - offset_cm
    
    
    # tidy.cal.bulleur.data <- full_join(odyssey.data.pre, tidy.cal.bulleur.data.pre)
    # tidy.cal.bulleur.data <- left_join(tidy.cal.bulleur.data, ll.cal.pre.i, by = )  # comparaions aux données (raw.val, en (UNITÉS?) de sonde (i) au même moment que chaque mesure (ligne) de tidy.bulleur.data // selon Wikipedia, il y aurait des mSiemens/mm qqpart
    
    
    
    
    
    
    
    
    
    
    
    
# 
# 
# 
#  tidy.bulleur.cal.data <- left_join(tidy.bulleur.data, ll.cal.pre.i)  # comparaions aux données (raw.val, en (UNITÉS?) de sonde (i) au même moment que chaque mesure (ligne) de tidy.bulleur.data // selon Wikipedia, il y aurait des mSiemens/mm qqpart
# 
#     # RENDUE À : faire le calcul de la droite dans ce script-ci seulement pour trouver la valeur qui va dans cal.data où les lignes cal.ordrer == 0
# 
# 
#     # pour chaque ligne de tidy.bulleur.cal.data, remplacer ll.cal.pre.i pour les "k in grep("_odyssey", raw.ll.files))"
# 
#     # cal.data <- cal.data.pre.1 %>% mutate(calibrated.value.cm <-
#   }
#  }

#   # PROCHAINE ÉTAPE: 
#   
#   # ->   -> extraire cal.value à ce moment là -> mettre dans cal. data (remplacer valeur NA de la case)
#   
#   # À faire aussi : tout modifier le script data_water.table_all en fonction de ceci
#   
#   







# 
# ll.offset.measurement.df <- data.frame(file.uid = NA, offset.measurement.bulleur.time = NA, bulleur.val.mm = NA, raw.value.mm = NA, cal.val.mm = NA) # pour stocker les données (aussi première colonne de cal.data)
# for (k in grep("_odyssey", raw.ll.files)) { # pour les fichiers odyssey référés apr leur ordre dans le dossier brut = effectuer les prochaines lignes
#   # remplacer pas un IF dans la fonction (encore, comme les autres)
#   # i<-80 
#   # k<-i # example fichier à plusieurs séquences tempporelles dans l'été
#   print(k)
#   
#   # recherche de lignes et nettoyage
#   # explications : pour chaque séquences valides de fichier-emplacement-année, aller chercher les lignes dans cal.data 
#   # et créer un tableur spécifique à la sonde (bulleur.data), et nettoyer les données
#   bulleur.data <- list()
#   for (l in 1:length(unique(cal.data.pre.1$period.file.uid[which(grepl(files.uid.df[k,1], cal.data.pre.1$file.uid))]))) { # si mm fichier.uid.i, coller les périodes ensemble (ainsi, retirer et remettre ne demande pas plus de manipulations et surtout ps des manipulations incividuelles)
#     print(i)
#     bulleur.data.j.pre <- unique(cal.data.pre.1[which(grepl(files.uid.df[i,1], cal.data.pre.1$file.uid)), c(which(grepl("period.file.uid", colnames(cal.data))), which(grepl("bulleur", colnames(cal.data))))])
#     
#     # extraire les chiffres des colonnes bulleur
#     bulleur.cols <- colnames(bulleur.data.j.pre) # logger serial no, en base R
#     numbers <- regexpr("[0-9]+", bulleur.cols)
#     nbulleur <- as.numeric(regmatches(bulleur.cols, numbers))
#     for (j in 1:length(unique(nbulleur))) {
#       bulleur.data.j <- bulleur.data.j.pre %>% dplyr::select(grep(j, bulleur.cols)) %>%  # sélect si contient j dans les noms de colonne
#         # j'obtiens les colonnes avec j, je crée un df aec juste ces colonnes
#         # j'ajoute une colonne avec le chiffre }
#         mutate(bulleur.no = rep(j, nrow(bulleur.data.j.pre)),
#                period.file.uid = bulleur.data.j.pre$period.file.uid)
#       colnames(bulleur.data.j) <- sub('[[:digit:]]+', '', colnames(bulleur.data.j)) # nom colonne sans chiffre
#       bulleur.data[[j]] <- bulleur.data.j
#     }
#     # rbind les lignes des j df
#     bulleur.data.appendd.l <- do.call(rbind, bulleur.data) %>% drop_na(., in.bulleur.prof.cm)
#   }
#   # préparation de la date-heure en prévision de la comparaison de date-heure entre tableaux
#   tidy.bulleur.data.pre.0 <- bulleur.data.appendd.l %>% mutate(date.JJ.MM.AAAA_time.HH.MM.SS_tz = paste0(in.bulleur.date.aaaammdd, " ", in.bulleur.time.tz.orig, " ", tz)) # tz original
#   tidy.bulleur.data.pre.0$date.time.tz.orig <- readr::parse_datetime(tidy.bulleur.data.pre.0$date.JJ.MM.AAAA_time.HH.MM.SS_tz, format = '%Y-%m-%d %H:%M:%S %Z', locale = readr::locale(tz = tz)) # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
#   tidy.bulleur.data.pre <- tidy.bulleur.data.pre.0 %>% mutate(date.time.roundd.pre = round_date(tidy.bulleur.data.pre.0$date.time.tz.orig, "hours"))
#   tidy.bulleur.data.pre$date.time.roundd <- gsub("00:00", "00:01", tidy.bulleur.data.pre$date.time.roundd.pre)
#   tidy.bulleur.data <- tidy.bulleur.data.pre %>% 
#     mutate(date.time.roundd = readr::parse_datetime(date.time.roundd, locale = readr::locale(tz = tz))) %>% # remise de date.time.roundd en classe POSIX
#     select(!c(date.JJ.MM.AAAA_time.HH.MM.SS_tz, date.time.tz.orig, date.time.roundd.pre))
#   rm(tidy.bulleur.data.pre, tidy.bulleur.data.pre.0)
#   # colonne date.time.UTC.0
#   tidy.bulleur.data$date.time.UTC.0pre <- with_tz(tidy.bulleur.data$date.time.roundd, tz = "UTC") # pour convertir AM/PM en décimal (0-24h), élément %p voir documentation
#   tidy.bulleur.data$date.time.UTC.0pre.1 <- format_iso_8601(tidy.bulleur.data$date.time.UTC.0pre)
#   tidy.bulleur.data$date.time.UTC.0 <- gsub("[+]00:00", "Z",  tidy.bulleur.data$date.time.UTC.0pre.1)
#   tidy.bulleur.data <- tidy.bulleur.data %>%
#     mutate(in.bulleur.prof.mm = in.bulleur.prof.cm * 10) %>% # données de bulleur en mm pour correspondre aux cal.val
#     mutate(in.bulleur.rel.to.surface.mm = in.bulleur.rel.to.surface.cm * 10) %>% # données de bulleur en mm pour correspondre aux cal.val
#     select(!c(date.time.UTC.0pre, date.time.UTC.0pre.1, date.time.roundd, in.bulleur.prof.cm, in.bulleur.rel.to.surface.cm))
#   # joindre par la colonne en commun "date.time.UTC.0"
#   tidy.bulleur.ll.data <- left_join(tidy.bulleur.data, ll.cal.pre.i)  # comparaions aux données (raw.val, en (UNITÉS?) de sonde (i) au même moment que chaque mesure (ligne) de tidy.bulleur.data // selon Wikipedia, il y aurait des mSiemens/mm qqpart
#   
# 
  

}
  tidy.cal.bulleur.data.list[[i]] <- tidy.cal.bulleur.data
}




# NON ANCIEN INVALIDE
#   
#   
#   
# for (k in 1:length(tidy.WTD.data)) {
#   # x<-95
#   print(k)
#   # tidy.WTD.data[[k]] # début de la loop pour les ODYSSEY (if() prochaine ligne)
#   if (any(grepl("odyssey", tidy.WTD.data[[k]]$metadata))) {}
#   
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