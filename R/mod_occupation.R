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
    mod_occupation_clean_ui(ns("occupation_clean_ui_1")),
    mod_occupation_graphe_ui(ns("occupation_graphe_ui_1")),
    mod_occupation_table_ui(ns("occupation_table_ui_1")),
    
    mod_occupation_appel_WS_ui(ns("occupation_appel_WS_ui_2")),
    mod_occupation_clean_ui(ns("occupation_clean_ui_2")),
    mod_occupation_graphe_ui(ns("occupation_graphe_ui_2")),
    mod_occupation_table_ui(ns("occupation_table_ui_2")),
    
    mod_occupation_appel_WS_ui(ns("occupation_appel_WS_ui_3")),
    mod_occupation_clean_ui(ns("occupation_clean_ui_3")),
    mod_occupation_graphe_ui(ns("occupation_graphe_ui_3")),
    mod_occupation_table_ui(ns("occupation_table_ui_3")),
    
    mod_occupation_appel_WS_ui(ns("occupation_appel_WS_ui_4")),
    mod_occupation_clean_ui(ns("occupation_clean_ui_4")),
    mod_occupation_graphe_ui(ns("occupation_graphe_ui_4")),
    mod_occupation_table_ui(ns("occupation_table_ui_4")),
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
    centre <- Occupation$new(rangeStart = Sys.Date() - 2, rangeEnd = Sys.Date() - 1, localisation_parking = "centre", parc_relais = FALSE)
    peripherie  <- Occupation$new(rangeStart = Sys.Date() - 2, rangeEnd = Sys.Date() - 1, localisation_parking = "peripherie", parc_relais = FALSE)
    
    # observe(    browser())

    mod_occupation_appel_WS_server("occupation_appel_WS_ui_1", r6 = parc_relais)
    mod_occupation_clean_server("occupation_clean_ui_1", r6 = parc_relais)
    mod_occupation_graphe_server("occupation_graphe_ui_1", r6 = parc_relais)
    mod_occupation_table_server("occupation_table_ui_1", r6 = parc_relais)
    
    mod_occupation_appel_WS_server("occupation_appel_WS_ui_2", r6 = hypercentre)
    mod_occupation_clean_server("occupation_clean_ui_2", r6 = hypercentre)
    mod_occupation_graphe_server("occupation_graphe_ui_2", r6 = hypercentre)
    mod_occupation_table_server("occupation_table_ui_2", r6 = hypercentre)
    
    mod_occupation_appel_WS_server("occupation_appel_WS_ui_3", r6 = centre)
    mod_occupation_clean_server("occupation_clean_ui_3", r6 = centre)
    mod_occupation_graphe_server("occupation_graphe_ui_3", r6 = centre)
    mod_occupation_table_server("occupation_table_ui_3", r6 = centre)
    
    mod_occupation_appel_WS_server("occupation_appel_WS_ui_4", r6 = peripherie)
    mod_occupation_clean_server("occupation_clean_ui_4", r6 = peripherie)
    mod_occupation_graphe_server("occupation_graphe_ui_4", r6 = peripherie)
    mod_occupation_table_server("occupation_table_ui_4", r6 = peripherie)
    
  })
}
    
## To be copied in the UI
# mod_occupation_ui("occupation_ui_1")
    
## To be copied in the server
# mod_occupation_server("occupation_ui_1")
