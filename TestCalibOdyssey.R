# Script créé le 26 mars pour extraire la donnée raw de ll au moment de la mesure de bulleur
# dans le but de calibrer
# À faire : tout modifier le script data_water.table_all en fonction de ceci


ll.offset.measurement.df <- data.frame(fichier.uid = NA, offset.measurement.bulleur.time = NA) # pour stocker les fihcier.uid (aussi première colonne de cal.data)
for (x in 1:length(ll.clean)) {
  # x<-12
  print(x)
  ll.clean[x] # début de la loop pour les ODYSSEY (if() prochaine ligne)
  if (grepl(SNH[1], ll.clean[x])) {  # début de la loop pour les ODYSSEY
    if(x %in% c(6,11)) {
      ll.offset.measurement.df[x,1] <- ll.clean[[x]]$metadata[10]
      ll.offset.measurement.df[x,2] <- "NA"
    } else {
      ll.offset.measurement.df[x,1] <- ll.clean[[x]]$metadata[10]
      ll.offset.measurement.df[x,2] <- ll.clean[[x]]$data$raw.value.mm[ll.clean[[x]]$data$date.time.tz.orig == '2024-11-14 10:00:01'] }
     }
}
