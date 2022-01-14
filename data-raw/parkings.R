library(dplyr)
library(xtradata)

# parkings_relais <- xtradata_requete_features(
#   key = Sys.getenv("XTRADATA_KEY"),
#   typename = "ST_PARK_P",
#   crs = "epsg:4326",
#   filter = list("ta_titul" = 0),
#   attributes = list("ident", "nom")
# )

parkings <- bind_rows(
  data.frame(
    "localisation_parking" = "hypercentre",
    "ident" = c("CUBPK06", "CUBPK02", "CUBPK18", "CUBPK03", "CUBPK07", "CUBPK24", "CUBPK30", "CUBPK28", "CUBPK34", "CUBPK25", "CUBPK26", "CUBPK29", "CUBPK16", "CUBPK05", "CUBPK15"),
    "parc_relais" = FALSE
  ),
  data.frame(
    "localisation_parking" = "centre",
    "ident" = c("CUBPK105", "CUBPK102", "CUBPK33", "CUBPK78", "CUBPK04", "CUBPK19", "CUBPK100", "CUBPK80", "CUBPK95", "CUBPK27", "CUBPK35", "CUBPK36", "CUBPK14"),
    "parc_relais" = FALSE
  ),
  data.frame(
    "localisation_parking" = "peripherie",
    "ident" = c("CUBPK72", "CUBPK40", "CUBPK39", "CUBPK73", "CUBPK38", "CUBPK74", "CUBPK82"),
    "parc_relais" = FALSE
  ),
  data.frame(
    "ident" = c("CUBPK44", "CUBPK84", "CUBPK49", "CUBPK60", "CUBPK76", "CUBPK54", "CUBPK77", "CUBPK47", "CUBPK57", "CUBPK50", "CUBPK51", "CUBPK45", "CUBPK41", "CUBPK53", "CUBPK43", "CUBPK56", "CUBPK59", "CUBPK42", "CUBPK88", "CUBPK58", "CUBPK91", "CUBPK55", "CUBPK48", "CUBPK52", "CUBPK97", "CUBPK98", "CUBPK40", "CUBPK39"),
    "parc_relais" = TRUE
  )
)

usethis::use_data(parkings, overwrite = TRUE)

# Liste des parkings v 14/01/2022

tibble::tribble(
  ~localisation, ~identifiant,                                  ~nom,
  "hypercentre",    "CUBPK06",                     "Camille Jullian",
  "hypercentre",    "CUBPK02",                     "Bourse - Jaures",
  "hypercentre",    "CUBPK18",                  "Allees De Chartres",
  "hypercentre",    "CUBPK03",                              "Tourny",
  "hypercentre",    "CUBPK07",                       "Grands Hommes",
  "hypercentre",    "CUBPK24",                            "Gambetta",
  "hypercentre",    "CUBPK30",         "Centre Commercial Meriadeck",
  "hypercentre",    "CUBPK28",                      "Front Du Medoc",
  "hypercentre",    "CUBPK34",                            "8 Mai 45",
  "hypercentre",    "CUBPK25",       "Pey Berland - Saint Christoly",
  "hypercentre",    "CUBPK26",                          "Republique",
  "hypercentre",    "CUBPK29",                            "Victoire",
  "hypercentre",    "CUBPK16",                         "Victor Hugo",
  "hypercentre",    "CUBPK05",                          "Salinieres",
  "hypercentre",    "CUBPK15",                          "Clemenceau",
       "centre",   "CUBPK105",   "Gare Saint Jean Arret Minute Nord",
       "centre",   "CUBPK102",   "Gare Saint Jean Arret Minute  Sud",
       "centre",    "CUBPK33",                     "Gare Saint Jean",
       "centre",    "CUBPK78",               "Paludate - Saint Jean",
       "centre",    "CUBPK04",                "Meunier - Saint Jean",
       "centre",    "CUBPK19",                            "Capucins",
       "centre",   "CUBPK100",                     "Bassins À Flots",
       "centre",    "CUBPK80",                         "Cite Du Vin",
       "centre",    "CUBPK95",                          "Grand Parc",
       "centre",    "CUBPK27",                       "Cite Mondiale",
       "centre",    "CUBPK35",                   "Porte De Bordeaux",
       "centre",    "CUBPK36",                            "Bergonie",
       "centre",    "CUBPK14",                    "Bord'eau village",
   "peripherie",    "CUBPK72",                "Begles Les Secheries",
   "peripherie",    "CUBPK40",                       "Pessac Centre",
   "peripherie",    "CUBPK39",                     "Merignac Centre",
   "peripherie",    "CUBPK73",                             "Beaujon",
   "peripherie",    "CUBPK38",                          "Liberation",
   "peripherie",    "CUBPK74",                             "Laharpe",
   "peripherie",    "CUBPK82",                       "Floirac Arena",
  "parc relais",    "CUBPK44",             "Parc-Relais Brandenburg",
  "parc relais",    "CUBPK84",       "Parc-Relais Quarante Journaux",
  "parc relais",    "CUBPK49",             "Parc-Relais La Gardette",
  "parc relais",    "CUBPK60",          "Parc-Relais Gare De Begles",
  "parc relais",    "CUBPK76",     "Parc-Relais Gare De Blanquefort",
  "parc relais",    "CUBPK54",     "Parc-Relais Ravezies Le Bouscat",
  "parc relais",    "CUBPK77",          "Parc-Relais Gare De Bruges",
  "parc relais",    "CUBPK47",       "Parc-Relais Floirac Dravemont",
  "parc relais",    "CUBPK57",      "Parc-Relais Le Haillan Rostand",
  "parc relais",    "CUBPK50",             "Parc-Relais La Gardette",
  "parc relais",    "CUBPK51",                "Parc-Relais Lauriers",
  "parc relais",    "CUBPK45",              "Parc-Relais Buttiniere",
  "parc relais",    "CUBPK41",                   "Parc-Relais Arlac",
  "parc relais",    "CUBPK53",          "Parc-Relais Quatre Chemins",
  "parc relais",    "CUBPK43",                "Parc-Relais Bougnard",
  "parc relais",    "CUBPK56",                  "Parc-Relais Unitec",
  "parc relais",    "CUBPK59", "Parc-Relais Gare De Pessac Alouette",
  "parc relais",    "CUBPK42",         "Parc-Relais Arts Et Metiers",
  "parc relais",    "CUBPK88",             "Parc-Relais Cap Metiers",
  "parc relais",    "CUBPK58",                "Parc-Relais Les Pins",
  "parc relais",    "CUBPK91",      "Parc-Relais Villenave Pyrenees",
  "parc relais",    "CUBPK55",              "Parc-Relais Stalingrad",
  "parc relais",    "CUBPK48",                   "Parc-Relais Galin",
  "parc relais",    "CUBPK52",             "Parc-Relais Les Aubiers",
  "parc relais",    "CUBPK97",              "Parc-Relais Cantinolle",
  "parc relais",    "CUBPK98",         "Parc-Relais De L'Hippodrome",
  "parc relais",    "CUBPK40",                       "Pessac Centre",
  "parc relais",    "CUBPK39",                     "Merignac Centre"
  )

