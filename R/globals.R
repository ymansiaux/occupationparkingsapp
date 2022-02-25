mytimezone <- "Europe/Paris"
debut_donnees <- as.Date("2020-04-01")

parametres_output_DT <- list(
  buttons = list(list(extend = "excel", title = NULL)),
  dom = "Bfrtip"
)

parkings <- rbind(
  data.frame(
    "localisation_parking" = "hypercentre",
    "ident" = c("CUBPK06", "CUBPK02", "CUBPK18", "CUBPK03", "CUBPK07", "CUBPK24", "CUBPK30", "CUBPK28", "CUBPK34", "CUBPK25", "CUBPK26", "CUBPK29", "CUBPK16", "CUBPK05", "CUBPK27", "CUBPK15"),
    "parc_relais" = FALSE
  ),
  data.frame(
    "localisation_parking" = "centre",
    "ident" = c("CUBPK105", "CUBPK102", "CUBPK33", "CUBPK78", "CUBPK04", "CUBPK19", "CUBPK100", "CUBPK80", "CUBPK95", "CUBPK35", "CUBPK36", "CUBPK14"),
    "parc_relais" = FALSE
  ),
  data.frame(
    "localisation_parking" = "peripherie",
    "ident" = c("CUBPK72", "CUBPK40", "CUBPK39", "CUBPK73", "CUBPK38", "CUBPK74", "CUBPK82"),
    "parc_relais" = FALSE
  ),
  data.frame(
    "localisation_parking" = NA,
    "ident" = c("CUBPK44", "CUBPK84", "CUBPK49", "CUBPK60", "CUBPK76", "CUBPK54", "CUBPK77", "CUBPK47", "CUBPK57", "CUBPK50", "CUBPK51", "CUBPK45", "CUBPK41", "CUBPK53", "CUBPK43", "CUBPK56", "CUBPK59", "CUBPK42", "CUBPK88", "CUBPK58", "CUBPK91", "CUBPK55", "CUBPK48", "CUBPK52", "CUBPK97", "CUBPK98", "CUBPK40", "CUBPK39"),
    "parc_relais" = TRUE
  )
)


utils::globalVariables(c(
  "debut_donnees", "mytimezone", ".", ".SD", "ident",
  "linetype", "nom", "taux_occupation", "time", "tooltip",
  "parkings", "parametres_output_DT", "etat", "type", "lwd",
  "head", "localisation_parking", "parc_relais"
  
))
