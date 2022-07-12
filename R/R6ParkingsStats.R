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

    #' @field aggregation_unit Unite de temps d'aggregation des donnees xtradata (notamment pour l'affichage dans les graphes)
    aggregation_unit = "",

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
    #' @param aggregation_unit aggregation_unit
    #' @param plageHoraire plageHoraire
    #' @param parkings_list liste des parkings analyses
    #' @return A new `Occupation` object.

    initialize = function(rangeStart = NULL, rangeEnd = NULL, rangeStep = NULL, aggregation_unit = NULL, plageHoraire = NULL, parkings_list = NULL) {
      self$rangeStart <- rangeStart
      self$rangeEnd <- rangeEnd
      self$rangeStep <- rangeStep
      self$aggregation_unit <- aggregation_unit
      self$plageHoraire <- plageHoraire
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
      
      # print(class(parkings_list))
      
      download <- try(xtradata_requete_aggregate(
        key = Sys.getenv("XTRADATA_KEY"),
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
            ),
           "etat"=
             list(
               "$in" = c("OUVERT", "LIBRE", "COMPLET")
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
    #' @param parkings_list liste des parkings (permet de recup les noms des pkgs)
    #' @import data.table
    #' @importFrom lubridate as_datetime

    clean_output = function(parkings_list) {
      self$cleaned_data <- self$data_xtradata %>%
        as.data.table() %>%
        .[, type := NULL] %>%
        .[, nom := NULL] %>% 
        .[, `:=`(
          time = as_datetime(time, tz = mytimezone),
          libres = as.integer(ceiling(libres))
        )] %>%
        .[, taux_occupation := 100 * (1 - (libres / total))] %>%
        merge(., unique(parkings_list[, c("nom", "ident")]), by = "ident") %>%
        setcolorder(neworder = "time")
    }
  )
)
# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
