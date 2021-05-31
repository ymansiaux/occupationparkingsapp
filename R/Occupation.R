#' R6 Class pour donnees occupations des parkings
#'
#' @description
#' va gérer la routine API / clean / plot / table / download
Occupation <- R6::R6Class("Occupation",
                          
                          public = list(
                            #' @field rangeStart Debut de la periode d'observation
                            rangeStart = "",
                            
                            #' @field rangeEnd Fin de la periode d'observation
                            rangeEnd = "",
                            
                            #' @field localisation_parking Secteur de localisation du parking (hypercentre, centre, peripherie, NA pour les parc relais)
                            localisation_parking = "",
                            
                            #' @field parc_relais Parc relais ou non (boolean)
                            parc_relais = "",
                            
                            #' @field data_xtradata Données issues de l'appel au WS via la fonction download_data
                            data_xtradata = NULL,
                            
                            #' @description
                            #' Create a new occupation object.
                            #' @param rangeStart rangeStart
                            #' @param rangeEnd rangeEnd.
                            #' @param localisation_parking localisation_parking
                            #' @param parc_relais parc_relais
                            #' @param data_xtradata data_xtradata
                            #' @return A new `Occupation` object.
                            
                            initialize = function(rangeStart, rangeEnd, localisation_parking, parc_relais, data_xtradata) {
                              self$rangeStart <- rangeStart
                              self$rangeEnd <- rangeEnd
                              self$localisation_parking <- localisation_parking
                              self$parc_relais <- parc_relais
                              self$data_xtradata <- NULL
                            },
                            
                            #' @description
                            #' Interroge le WS aggregate
                            #' @param rangeStep rangeStep xtradata aggregate
                            #' @examples \dontrun{
                            #' parc_relais <- Occupation(rangeStart = Sys.Date() - 2, 
                            #' rangeEnd = Sys.Date() - 1, localisation_parking = NA, parc_relais = TRUE)
                            #' parc_relais$download_data()
                            #' parc_relais$data_xtradata }
                            download_data = function(rangeStep) {
                              self$data_xtradata <- try(xtradata_requete_aggregate(
                                key = "DATAZBOUBB",
                                typename = "ST_PARK_P",
                                rangeStart = self$rangeStart,
                                rangeEnd = self$rangeEnd,
                                rangeStep = rangeStep,
                                rangeFilter = list(hours = 0:23, days = 1:7, publicHolidays = FALSE),
                                filter = list(
                                  "ident" =
                                    list(
                                      "$in" =
                                        unlist(copy(parkings)[localisation_parking %in% self$localisation_parking & parc_relais == self$parc_relais, "ident"])
                                    )
                                ),
                                attributes = list("gid", "time", "libres", "total", "etat", "ident"),
                                showURL = TRUE
                              ))
                            },
                            
                            #' @description
                            #' Nettoyage de la sortie xtradata
                            #' (application de lubridate et calcul du taux d'occup)
                            #' @examples \dontrun{ clean_output()
                            #' }
                            clean_output = function() {
                              self$data_xtradata <- as.data.table(self$data_xtradata) %>% 
                                .[, type := NULL] %>% 
                                .[, time := as_datetime(time)] %>% 
                                .[, libres := ceiling(libres)] %>% 
                                .[, taux_occupation := (libres / total)]
                            },
                            
                            #' @description
                            #' Aggregation des données selon une fenetre temporelle
                            #' (application de la fonction summarise_by_time de timetk)
                            #' @param aggregate_step pas d'aggreg (voir timetk)
                            #' @examples \dontrun{ temporal_aggregate("day)
                            #' } 
                            temporal_aggregate = function(aggregate_step) {
                              self$data_xtradata <- self$data_xtradata %>% 
                                summarise_by_time(.date_var = time, 
                                                  .by = aggregate_step,
                                                  taux_occupation = mean(taux_occupation, na.rm = TRUE))
                              
                            }
                            
                          )
)
#https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html