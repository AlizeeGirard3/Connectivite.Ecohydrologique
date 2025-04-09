# tests : où est la ligne 34 ?
# 7 avril 2025
# update : ok arrangé, recherche des périodes avec 00:00:01-> 24:00:01 a arrangé la patente, sans poser plus de problèmes





# ll.clean[[1]] # OK 24 NE DISPARAÎT PAS
 # ll.clean[[2]] # OK 24 NE DISPARAÎT PAS
 # ll.clean[[3]] # OK 24 NE DISPARAÎT PAS
 # ll.clean[[4]] # OK 24 NE DISPARAÎT PAS
 # ll.clean[[6]] # vide
# ll.clean[[11]] # vide

 # ll.clean[[5]] # 24H DISPARAÎT de ll.clean, tester ll.pre.2.data
 # ll.clean[[7]] # 24H DISPARAÎT de ll.clean, tester ll.pre.2.data
 # ll.clean[[8]] # 24H DISPARAÎT de ll.clean, tester ll.pre.2.data
 # ll.clean[[9]] # 24H DISPARAÎT de ll.clean, tester ll.pre.2.data
 # ll.clean[[10]] # 24H DISPARAÎT de ll.clean, tester ll.pre.2.data
 # ll.clean[[12]] # 24H DISPARAÎT de ll.clean, tester ll.pre.2.data
# D'OÙ ELLE DISPARAÎT ??

ll.pre # *ll.clean[[i]] et ll.pre[[i]], même i*
# [1] "10279769_INK_20250106_hobo.csv" "10279777_INK_20250106_hobo.csv" "20573974_INK_20250106_hobo.csv" "20853328_INK_20250106_hobo.csv"
# [5] "41359_STH_20241125_odyssey.CSV" "41361_STH_20241125_odyssey.CSV" "41362_STH_20241125_odyssey.CSV" "41366_STH_20241125_odyssey.CSV"
# [9] "41370_STH_20241125_odyssey.CSV" "41372_STH_20241125_odyssey.CSV" "41376_STH_20241125_odyssey.CSV" "41379_STH_20241125_odyssey.CSV"
# [13] "41387_STH_20241125_odyssey.CSV" "42564_STH_20241125_odyssey.CSV" "42565_STH_20241125_odyssey.CSV" "42566_STH_20241125_odyssey.csv"
# c'est donc pour les odyssey seulement

# options(max.print=1000)
ll.clean[[i]]$data # pour les ODYSSEY
# #      scan.id raw.value.mm calibrated.value.mm date.AAAA-MM-JJ time.HH.MM.SS   date.time.tz.orig      date.time.UTC.0
# 1        25         1765           -20.88352      2024-06-12      15:00:01 2024-06-12 15:00:01 2024-06-12T19:00:01Z
# 2        26         1775           -21.43297      2024-06-12      16:00:01 2024-06-12 16:00:01 2024-06-12T20:00:01Z
# 3        27         1784           -21.92747      2024-06-12      17:00:01 2024-06-12 17:00:01 2024-06-12T21:00:01Z
# 4        28         1794           -22.47692      2024-06-12      18:00:01 2024-06-12 18:00:01 2024-06-12T22:00:01Z
# 5        29         1801           -22.86154      2024-06-12      19:00:01 2024-06-12 19:00:01 2024-06-12T23:00:01Z
# 6        30         1813           -23.52088      2024-06-12      20:00:01 2024-06-12 20:00:01 2024-06-13T00:00:01Z
# ci-dessus 23h+1 = 0h (=24h) en UTC-0
# 7        31         1817           -23.74066      2024-06-12      21:00:01 2024-06-12 21:00:01 2024-06-13T01:00:01Z
# 8        32         1823           -24.07033      2024-06-12      22:00:01 2024-06-12 22:00:01 2024-06-13T02:00:01Z
# 9        33         1829           -24.40000      2024-06-12      23:00:01 2024-06-12 23:00:01 2024-06-13T03:00:01Z
# 10       35         1841           -25.05934      2024-06-13      01:00:01 2024-06-13 01:00:01 2024-06-13T05:00:01Z
# mais ici, 23h+1 = 1h ! le 0h n'existe pas (date tz orig) / on voit no de ligne qu'il manque l'heure 24
# donc aller voir les données originales : (exemple pour 7)
# 31     31,12/06/2024,21:00:00,1817,1817
# 32     32,12/06/2024,22:00:00,1823,1823
# 33     33,12/06/2024,23:00:00,1829,1829
# 34     34,12/06/2024,24:00:00,1836,1836
# 35     35,13/06/2024,01:00:00,1841,1841

# exemple pour i = 7
i<-7 # 8 9 10 11 12 13 14 15 16# 41362(27 mars 2025)
print(i)
ll.pre[i] # début de la loop pour les ODYSSEY (if() prochaine ligne)
# import et ménage
ll.pre.0 <- readLines(paste0("connectivite/data/raw/",ll.pre[i])); str(ll.pre.0) # lire en format texte
# Warning message:
#   In readLines(paste0("connectivite/data/raw/", ll.pre[i])) :
#   incomplete final line found on 'connectivite/data/raw/[...].csv'
# c'est chill, je n'ai pas réussi à arranger ça, mais vérifié √ pas de problème
ll.pre.1 <- gsub(" ,", ",", ll.pre.0); str(ll.pre.1) # replace " ," by "," 
ll.pre.2 <- gsub(" ", "", ll.pre.1); str(ll.pre.2) # enlever tous les espaces dans le subset de données

### création des subsets data & metadata ----
# notes : les noms réfèrent à l'étape et non à une matrice en particulier, les objets seront remplacés au fil de la boucle. 
# l'info importante est consignée dans la liste ll.clean[i], à la fin
ll.pre.2.metadata <-  ll.pre.2[c(1:9)] # inclus les anciens noms de colonnes, qui sont dans un format et un ordre bizzare
ll.pre.2.data <- ll.pre.2[-c(1:9)]

ll.pre.2.data <- as.data.frame(ll.pre.2.data)


# Lignes spécifiques où ça se produit : 
# ll.pre.2.data.4.l <- ll.pre.2.data.4 %>%
#   dplyr::filter(date.time.tz.orig >= cal.data.i.l$day.begining.aaaa.mm.dd.hh.mm) %>% # >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data.i.l
#   dplyr::filter(date.time.tz.orig <= cal.data.i.l$day.end.aaaa.mm.dd.hh.mm) %>% # <= date de mesure de NP plus petite ou égale à la date end dans cal.data.i.l 
#   dplyr::select("scan.id", "raw.value.mm", "calibrated.value.mm", "date.AAAA-MM-JJ", "time.HH.MM.SS", "date.time.tz.orig", "date.time.UTC.0") # %>%  # date et time sans "UTC.0" sont dans le fuseau horaire d'origine (tz trouvé en croisant les coordonnées "coords")
# # vérifications
# head(ll.pre.2.data.4.l); colnames(ll.pre.2.data.4.l); nrow(ll.pre.2.data.4.l)
ll.pre.2.data.3
ll.pre.2.data.3$date.time.tz.orig <- gsub("00:00:01", "24:00:01", ll.pre.2.data.3$date.time.tz.orig)


test <- dplyr::filter(ll.pre.2.data.3, ll.pre.2.data.3$date.time.tz.orig >= cal.data.i.l$day.begining.aaaa.mm.dd.hh.mm) #  >= date de mesure de NP plus grand ou égale à la date beginning dans cal.data.i.l
  
x<-as.data.frame(ll.pre.2.data.3$date.time.tz.orig)
x$cal.data <- as.data.frame(rep(cal.data.i.l$day.begining.aaaa.mm.dd.hh.mm, times = nrow(x)))








# ________ 9 avril


# ll.clean[[1]] 
# ll.clean[[2]] 
# ll.clean[[3]] 
# ll.clean[[4]] 
# ll.clean[[6]] 
# ll.clean[[11]] 
# ll.clean[[5]] # 
# ll.clean[[7]] # 
# ll.clean[[8]] # 
# ll.clean[[9]] # 
# ll.clean[[10]] #
# ll.clean[[12]] #

ll.pre # *ll.clean[[i]] et ll.pre[[i]], même i*
# [1] "10279769_INK_20250106_hobo.csv" "10279777_INK_20250106_hobo.csv" "20573974_INK_20250106_hobo.csv" "20853328_INK_20250106_hobo.csv"
# [5] "41359_STH_20241125_odyssey.CSV" "41361_STH_20241125_odyssey.CSV" "41362_STH_20241125_odyssey.CSV" "41366_STH_20241125_odyssey.CSV"
# [9] "41370_STH_20241125_odyssey.CSV" "41372_STH_20241125_odyssey.CSV" "41376_STH_20241125_odyssey.CSV" "41379_STH_20241125_odyssey.CSV"
# [13] "41387_STH_20241125_odyssey.CSV" "42564_STH_20241125_odyssey.CSV" "42565_STH_20241125_odyssey.CSV" "42566_STH_20241125_odyssey.csv"
# c'est donc pour les odyssey seulement




ll.1 <- ll.clean[[1]]$data
ll.5 <- ll.clean[[5]]$data
ll.8 <- ll.clean[[8]]$data

ll.cal.pre.i 


ll.clean[[5]]$metadata





