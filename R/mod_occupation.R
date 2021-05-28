#' occupation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
mod_occupation_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_occupation_appel_WS_ui(ns("occupation_appel_WS_ui_1")),
    mod_occupation_appel_WS_ui(ns("occupation_appel_WS_ui_2"))
  )
}
    
#' occupation Server Functions
#'
#' @noRd 
mod_occupation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    parc_relais <- Occupation$new(rangeStart = Sys.Date() - 2, rangeEnd = Sys.Date() - 1, localisation_parking = NA, parc_relais = TRUE)
    hypercentre <- Occupation$new(rangeStart = Sys.Date() - 2, rangeEnd = Sys.Date() - 1, localisation_parking = "hypercentre", parc_relais = FALSE)
    # observe(    browser())

    mod_occupation_appel_WS_server("occupation_appel_WS_ui_1", r6 = parc_relais)
    mod_occupation_appel_WS_server("occupation_appel_WS_ui_2", r6 = hypercentre)
    
  })
}
    
## To be copied in the UI
# mod_occupation_ui("occupation_ui_1")
    
## To be copied in the server
# mod_occupation_server("occupation_ui_1")
