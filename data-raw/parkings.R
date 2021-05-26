# distinction des parc relais : ta_titul = 0
# 
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
# 
# Cité mondiale CUBPK27
# Porte de Bordeaux CUBPK35
# Meunier CUBPK04
# Salinières CUBPK05
# Paludate CUBPK78
# Saint Jean : https://opendata.bordeaux-metropole.fr/explore/dataset/st_park_p/table/?q=Saint+Jean&sort=-ta_titul
# 
# Bègles Sècheries CUBPK72
# Pessac centre CUBPK40
# Mérignac centre CUBPK39
# Bouscat Libération CUBPK38

# -hypercentre: Bourse Jaurès, Tourny, Grands hommes, Gambetta, CC Mériadeck, Front du Médoc, 8 mai 45,  République, Victoire, Victor Hugo
# -centre: Cité mondiale, Porte de Bordeaux, Meunier, Salinières, Saint Jean, Paludate
# -périphérie: Bègles Sècheries, Pessac centre, Mérignac centre, Bouscat Libération

library(data.table)

parkings_relais <- xtradata_requete_features(
  key = "DATAZBOUBB",
  typename = "ST_PARK_P",
  crs = "epsg:4326",
  filter = list("ta_titul" = 0),
  attributes = list("ident")
)

parkings <- rbind(
  data.table("localisation" = "hypercentre", 
             "ident" = c("CUBPK02", "CUBPK03", "CUBPK07", "CUBPK24", "CUBPK30", "CUBPK28", "CUBPK34", "CUBPK26", "CUBPK29", "CUBPK16"),
             "relais" = FALSE),
  data.table("localisation" = "centre",
             "ident" = c("CUBPK27", "CUBPK35", "CUBPK04", "CUBPK05", "CUBPK78"),
             "relais" = FALSE),
  data.table("localisation" = "peripherie",
             "ident" = c("CUBPK72", "CUBPK40", "CUBPK39", "CUBPK38"),
             "relais" = FALSE),
  data.table("ident" = parkings_relais$ident,
             relais = TRUE),
  fill =  TRUE
)

usethis::use_data(parkings, overwrite = TRUE)
