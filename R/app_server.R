#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import data.table
#' @importFrom shinybm closeWaiter_logoDatalab
#' @importFrom golem app_prod
#' @import R6
#' @importFrom xtradata xtradata_requete_aggregate
#' @noRd
app_server <- function(input, output, session) {
  observe(closeWaiter_logoDatalab(golem::app_prod()))

# browser()
  # parc_relais <- Occupation$new(rangeStart = Sys.Date() - 2, rangeEnd = Sys.Date() - 1, localisation_parking = NA, parc_relais = TRUE)
  # parc_relais <- Occupation$new(rangeStart = Sys.Date() - 2, rangeEnd = Sys.Date() - 1, localisation_parking = "hypercentre", parc_relais = FALSE)
  # 
  # parc_relais$download_data()
  # parc_relais$data_xtradata
  mod_occupation_server("occupation_ui_1")
}

