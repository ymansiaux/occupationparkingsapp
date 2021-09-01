# distinction des parc relais : ta_titul = 0
# 
# HYPERCENTRE
# Bourse Jaures CUBPK02
# Tourny CUBPK03
# Grands Hommes CUBPK07
# Gambetta CUBPK24
# Meriadeck CUBPK30
# Front du medoc CUBPK28
# 8 mai 45 CUBPK34
# République CUBPK26
# Victoire CUBPK29
# Victor Hugo CUBPK16
# Salinières CUBPK05
# Camille Julian CUBPK06
# Cité mondiale CUBPK27
# Allees De Chartres CUBPK18
#
# CENTRE
# Porte de Bordeaux CUBPK35
# Meunier CUBPK04
# Paludate CUBPK78
# Saint Jean CUBPK33

# PERIPHERIE
# Bègles Sècheries CUBPK72
# Pessac centre CUBPK40
# Mérignac centre CUBPK39
# Bouscat Libération CUBPK38
# Laharpe CUBPK74

# -hypercentre: Bourse Jaurès, Tourny, Grands hommes, Gambetta, CC Mériadeck, Front du Médoc, 8 mai 45,  République, Victoire, Victor Hugo, Salinières, Camille Julian, Cité mondiale, Allees De Chartres
# -centre: Porte de Bordeaux, Meunier, Saint Jean, Paludate
# -périphérie: Bègles Sècheries, Pessac centre, Mérignac centre, Bouscat Libération, Laharpe

library(dplyr)
library(xtradata)

parkings_relais <- xtradata_requete_features(
  key = "DATAZBOUBB",
  typename = "ST_PARK_P",
  crs = "epsg:4326",
  filter = list("ta_titul" = 0),
  attributes = list("ident", "nom")
)

parkings <- bind_rows(
  data.frame(
    "localisation_parking" = "hypercentre",
    "ident" = c("CUBPK02", "CUBPK03", "CUBPK07", "CUBPK24", "CUBPK30", "CUBPK28", "CUBPK34", "CUBPK26", "CUBPK29", "CUBPK16", "CUBPK05", "CUBPK06", "CUBPK27", "CUBPK18"),
    "parc_relais" = FALSE
  ),
  data.frame(
    "localisation_parking" = "centre",
    "ident" = c("CUBPK27", "CUBPK35", "CUBPK04", "CUBPK78", "CUBPK33", "CUBPK102"),
    "parc_relais" = FALSE
  ),
  data.frame(
    "localisation_parking" = "peripherie",
    "ident" = c("CUBPK72", "CUBPK40", "CUBPK39", "CUBPK38", "CUBPK74"),
    "parc_relais" = FALSE
  ),
  data.frame(
    "ident" = parkings_relais$ident,
    "parc_relais" = TRUE
  )
)

noms_des_parkings <- xtradata_requete_features(
  key = "DATAZBOUBB",
  typename = "ST_PARK_P",
  crs = "epsg:4326",
  attributes = list("ident", "nom")
)

parkings <- inner_join(parkings, noms_des_parkings, by = "ident") %>%
  select(-type)

usethis::use_data(parkings, overwrite = TRUE)
