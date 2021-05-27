#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinybm closeWaiter_logoDatalab
#' @importFrom golem app_prod
#' @import R6
#' @noRd
app_server <- function( input, output, session ) {
  observe(closeWaiter_logoDatalab(golem::app_prod()))
  
  Occupation <- R6Class("Occupation", 
                        public = list(
                          rangeStart = "", 
                          rangeEnd = "",
                          localisation_parking = "",
                          parc_relais = "",
                          data_xtradata = data.frame(),
                          
                          initialize = function(rangeStart, rangeEnd, localisation_parking, parc_relais, data_xtradata){
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
                              filter =  list("ident" = list("$in" = unlist(parkings[localisation_parking %in% self$localisation_parking & parc_relais == self$parc_relais, "ident"]))),
                              showURL = TRUE
                            )
                          }
                        ))
  
  colin <- Occupation$new(rangeStart = Sys.Date()-2, rangeEnd = Sys.Date() -1, localisation_parking = "hypercentre", parc_relais = FALSE)
  
  colin$download_data()
  colin$data_xtradata
  

  
}

as.vector(parkings[localisation_parking %in% "hypercentre" & parc_relais == FALSE, "ident"])
