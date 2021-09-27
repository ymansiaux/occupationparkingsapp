#' R6 Super Class pour donnees occupations et saturation des parkings
#'
#' @description
#' utilisee ensuite par classe Occupation et Saturation
ParkingsStats <- R6::R6Class(
  "ParkingsStats",
  public = list(
    #' @field rangeStart Debut de la periode d'observation
    rangeStart = "",

    #' @field rangeEnd Fin de la periode d'observation
    rangeEnd = "",

    #' @field rangeStep Pas d'aggregation pour requete xtradata
    rangeStep = "",

    #' @field timeStep Fenetre de temps de restitution des graphes (heure, jour, jour de la semaine, mois)
    timeStep = "",

    #' @field plageHoraire plage horaire des donnees à recup
    plageHoraire = 0:23,

    #' @field parkings_list liste des parkings analyses
    parkings_list = NULL,

    #' @field data_xtradata Données issues de l'appel au WS via la fonction download_data
    data_xtradata = NULL,

    #' @field cleaned_data Données nettoyées
    cleaned_data = NULL,

    #' @field download_data_memoise version optimisee (avec cache) de la fonction de telechargement sur xtradata
    download_data_memoise = NULL,

    #' @description
    #' Create a new occupation object.
    #' @param rangeStart rangeStart
    #' @param rangeEnd rangeEnd
    #' @param rangeStep rangeStep
    #' @param timeStep timeStep
    #' @param plageHoraire plageHoraire
    #' @param parkings_list liste des parkings analyses
    #' @return A new `Occupation` object.

    initialize = function(rangeStart = NULL, rangeEnd = NULL, rangeStep = NULL, timeStep = NULL, plageHoraire = NULL, parkings_list = NULL) {
      self$rangeStart <- rangeStart
      self$rangeEnd <- rangeEnd
      self$rangeStep <- rangeStep
      self$timeStep <- timeStep
      self$plageHoraire <- plageHoraire
      # self$localisation_parking <- localisation_parking
      # self$parc_relais <- parc_relais
      self$parkings_list <- parkings_list
    },

    #' @description
    #' Interroge le WS aggregate
    #' @param rangeStart rangeStart xtradata aggregate
    #' @param rangeEnd rangeEnd xtradata aggregate
    #' @param rangeStep rangeStep xtradata aggregate
    #' @param plageHoraire plage horaire d'interet pour les donnees (filtre sur xtradata)
    #' @param parkings_list liste des parkings analyses
    #' @import data.table
    #' @importFrom xtradata xtradata_requete_aggregate
    download_data = function(rangeStart, rangeEnd, rangeStep, plageHoraire, parkings_list) {
      
      if(length(parkings_list) == 1) parkings_list <- list(parkings_list)
      
      download <- try(xtradata_requete_aggregate(
        key = "DATAZBOUBB",
        typename = "ST_PARK_P",
        rangeStart = rangeStart,
        rangeEnd = rangeEnd,
        rangeStep = rangeStep,
        rangeFilter = list(hours = plageHoraire, days = 1:7, publicHolidays = FALSE),
        filter = list(
          "ident" =
            list(
              "$in" =
                parkings_list
            )
        ),
        attributes = list("gid", "time", "nom", "libres", "total", "etat", "ident"),
        showURL = TRUE
      ))

      if (inherits(download, "try-error")) {
        return(NULL)
      } else {
        return(download)
      }
    },

    #' @description
    #' Nettoyage de la sortie xtradata
    #' (application de lubridate et calcul du taux d'occup)
    #' @import data.table
    #' @importFrom lubridate as_datetime

    clean_output = function() {
      self$cleaned_data <- self$data_xtradata %>%
        as.data.table() %>%
        .[, type := NULL] %>%
        .[, `:=`(
          time = as_datetime(time, tz = mytimezone),
          libres = as.integer(ceiling(libres))
        )] %>%
        .[, taux_occupation := 100 * pmax(0, 1 - (libres / total))] %>%
        # merge(., unique(parkings[, c("nom", "ident")]), by = "ident") %>%
        setcolorder(neworder = "time")
    }
  )
)
# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
