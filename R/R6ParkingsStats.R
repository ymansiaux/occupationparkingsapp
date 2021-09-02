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
    
    #' @field localisation_parking Secteur de localisation du parking (hypercentre, centre, peripherie, NA pour les parc relais)
    localisation_parking = "",
    
    #' @field parc_relais Parc relais ou non (boolean)
    parc_relais = "",
    
    #' @field data_xtradata Données issues de l'appel au WS via la fonction download_data
    data_xtradata = NULL,
    
    #' @field cleaned_data Données nettoyées
    cleaned_data = NULL,
    
    download_data_memoise = NULL,
    
    #' @description
    #' Create a new occupation object.
    #' @param rangeStart rangeStart
    #' @param rangeEnd rangeEnd
    #' @param rangeStep rangeStep
    #' @param timeStep timeStep
    #' @param plageHoraire plageHoraire
    #' @param localisation_parking localisation_parking
    #' @param parc_relais parc_relais
    #' @return A new `Occupation` object.
    
    initialize = function(rangeStart, rangeEnd, rangeStep, timeStep, plageHoraire, localisation_parking, parc_relais) {
      self$rangeStart <- rangeStart
      self$rangeEnd <- rangeEnd
      self$rangeStep <- rangeStep
      self$timeStep <- timeStep
      self$plageHoraire <- plageHoraire
      self$localisation_parking <- localisation_parking
      self$parc_relais <- parc_relais
    },
    
    #' @description
    #' Interroge le WS aggregate
    #' @param rangeStep rangeStep xtradata aggregate
    #' @import data.table
    #' @importFrom xtradata xtradata_requete_aggregate
    #' @examples \dontrun{}
    download_data = function(rangeStart, rangeEnd, rangeStep, plageHoraire, localisation_parking, parc_relais) {
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
                parkings[which(parkings$localisation_parking %in% localisation_parking & parkings$parc_relais == parc_relais), "ident"]
            )
        ),
        attributes = list("gid", "time", "libres", "total", "etat", "ident"),
        showURL = TRUE
      ))

      if (inherits(download, "try-error")) {
        self$data_xtradata <- NULL
      } else {
        self$data_xtradata <- download
      }
    },

    #' @description
    #' Nettoyage de la sortie xtradata
    #' (application de lubridate et calcul du taux d'occup)
    #' @import data.table
    #' @importFrom lubridate as_datetime
    #' @examples \dontrun{ clean_output()
    #' }
    clean_output = function() {
      self$cleaned_data <- self$data_xtradata %>%
        as.data.table() %>%
        .[, type := NULL] %>%
        .[, `:=`(
          time = as_datetime(time, tz = mytimezone),
          libres = as.integer(ceiling(libres))
        )] %>%
        .[, taux_occupation := 100 * pmax(0, 1 - (libres / total))] %>%
        merge(., unique(parkings[, c("nom", "ident")]), by = "ident") %>%
        setcolorder(neworder = "time")
    }
  )
)
# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
# ParkingsStats$set("public", "download_data2", memoise::memoise(ParkingsStats$public_methods$download_data))
