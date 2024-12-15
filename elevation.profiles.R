#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                             Elevation profile graphs
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Initialized Oct. 23rd 2024
# By Alizée Girard

#### bibliotheques a charger (installer avant si pas fait)
library(conflicted) # ℹ Use the conflicted package to force all conflicts to become errors    ---->>>>  devtools::install_github("r-lib/conflicted")
if (!require("reshape2")) install.packages("reshape2") # pour importer Google Sheets directement
if (!require("plyr")) install.packages("plyr") # pour manipulation donnees
if (!require("dplyr")) install.packages("dplyr") # pour manipulation donnees
if (!require("vegan")) install.packages("vegan") # pour analyses multivariees
if (!require("ggplot2")) install.packages("ggplot2") 
if (!require("ggpubr")) install.packages("ggpubr") # pourquoi déjà ?
if (!require("rmarkdown")) install.packages("rmarkdown") # pour écrire un pdf
# install.packages("devtools")
# devtools::install_github("refunders/refund.shiny")
if (!require("refund.shiny")) install.packages("refund.shiny") # pour enregistrer des graphiques sous forme de RData (besoin dans ma boucle)
if (!require("tidyverse")) install.packages("tidyverse") # pour manipulation donnees
# ── Attaching core tidyverse packages ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 2.0.0 ──
# ── Conflicts ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
# ✖ dplyr::arrange()   masks plyr::arrange()     # ✖ purrr::compact()   masks plyr::compact()       # ✖ dplyr::count()     masks plyr::count()
# ✖ dplyr::desc()      masks plyr::desc()        # ✖ dplyr::failwith()  masks plyr::failwith()      # ✖ dplyr::filter()    masks stats::filter()
# ✖ dplyr::id()        masks plyr::id()          # ✖ dplyr::lag()       masks stats::lag()          # ✖ dplyr::mutate()    masks plyr::mutate()
# ✖ dplyr::rename()    masks plyr::rename()     # ✖ dplyr::summarise() masks plyr::summarise()      # ✖ dplyr::summarize() masks plyr::summarize()

##### importer et préparer donnees dans R
setwd("/Users/Aliz/Documents/Doctorat/R_PhD/Connectivite")

Elevation <- readxl::read_xlsx("/Users/Aliz/Documents/Doctorat/_Connectivité/1&2_Écotones/Inkerman/Data/Data_Inkerman.xlsx",
                  sheet = "Elevation") #%>% group_by("ID.unique")
ungroup(Elevation)
transect <- unique(Elevation$ID.unique)

##### créer des graphiques d'élévation Nord->Sud
for (i in 1:(length(transect))) {
  print(i)
  transect.i =  transect[i]
  # liste_graphs[[i]] <- paste0("graph_", i)
  graph <- Elevation %>% dplyr::filter(ID.unique == transect.i) %>% ggplot(mapping = aes(y = Value, x = `Distance.N-S`)) +
   # facet_grid(~ ID.unique) +
  geom_point(shape = ".") +
  geom_line(group = 1) +
  ggtitle(paste0("Transect ", transect.i), ) +
  labs(y = "Élévation (m; Canada, 2024)", x = "Transect N-S") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

  # Check if the file does not exist
  file_to_check <- paste0('output/figures/Elevation_Inkerman_graph',transect[i],'.png')
  if(!file.exists(file_to_check)){ # si c'est PAS VRAI (le file n'existe pas, on poursuit) = VRAI, si c'est VRAI = FAUX (else if -> message d'erreur, empêche d'écraser)
    ggplot2::ggsave(paste0('output/figures/Elevation_Inkerman_graph',transect[i],'.png'), graph, width = 4.7, height = 2.4)

    # otherwise print a message
  }else if(file.exists(file_to_check)){

    stop("The file already exists in the current directory!")
  }
  
  # Choix CONSCIENT d'écraser
  # ggplot2::ggsave(paste0('output/figures/Elevation_Inkerman_graph',transect[i],'.png'), graph, width = 4.7, height = 2.4)
  
}
# ggarrange(graph_1,graph_2,graph_3,graph_4,graph_5,graph_6,
#                   ncol = 3, nrow = 2, common.legend = T)



