mytimezone <- "Europe/Paris"
debut_donnees <- as.Date("2020-03-01")

parametres_output_DT <- list(
  buttons = list(list(extend = "excel", title = NULL)),
  dom = "Bfrtip"
)

utils::globalVariables(c(
  "debut_donnees", "mytimezone", ".", ".SD", "ident",
  "linetype", "nom", "taux_occupation", "time", "tooltip",
  "parkings", "parametres_output_DT", "etat", "type", "lwd"
))
