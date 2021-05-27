Occupation <- R6Class("Occupation",
                      public = list(
                        rangeStart = "",
                        rangeEnd = "",
                        localisation_parking = "",
                        parc_relais = "",
                        data_xtradata = data.frame(),
                        initialize = function(rangeStart, rangeEnd, localisation_parking, parc_relais, data_xtradata) {
                          self$rangeStart <- rangeStart
                          self$rangeEnd <- rangeEnd
                          self$localisation_parking <- localisation_parking
                          self$parc_relais <- parc_relais
                        },
                        download_data = function() {
                          self$data_xtradata <- xtradata_requete_aggregate(
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
                          )
                        }
                      )
)