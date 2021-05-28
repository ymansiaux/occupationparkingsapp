Occupation <- R6Class("Occupation",
                      public = list(
                        rangeStart = "",
                        rangeEnd = "",
                        localisation_parking = "",
                        parc_relais = "",
                        data_xtradata = NULL,
                        
                        initialize = function(rangeStart, rangeEnd, localisation_parking, parc_relais, data_xtradata) {
                          self$rangeStart <- rangeStart
                          self$rangeEnd <- rangeEnd
                          self$localisation_parking <- localisation_parking
                          self$parc_relais <- parc_relais
                          self$data_xtradata <- NULL
                        },
                        download_data = function() {
                          toto <- try(xtradata_requete_aggregate(
                            key = "DATAZBOUBB",
                            typename = "ST_PARK_P",
                            rangeStart = self$rangeStart,
                            rangeEnd = self$rangeEnd,
                            rangeStep = "hour",
                            rangeFilter = list(hours = 0:23, days = 1:7, publicHolidays = FALSE),
                            filter = list(
                              "ident" =
                                list(
                                  "$in" =
                                    unlist(copy(parkings)[localisation_parking %in% self$localisation_parking & parc_relais == self$parc_relais, "ident"])
                                )
                            ),
                            showURL = TRUE
                          ))
                          # print("coucou")
                          self$data_xtradata <- toto
                          print(toto)
                          return(self$data_xtradata)
                          # print("coucou2")
                          
                        }
                      )
)