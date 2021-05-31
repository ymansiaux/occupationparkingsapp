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
#' @importFrom timetk summarise_by_time
#' @importFrom lubridate as_datetime
#' @noRd
app_server <- function(input, output, session) {
  observe(closeWaiter_logoDatalab(golem::app_prod()))

  mod_occupation_server("occupation_ui_1")
}

