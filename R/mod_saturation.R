#' saturation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
mod_saturation_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_saturation_appel_WS_ui(ns("saturation_appel_WS_ui_1")),
    mod_saturation_clean_ui(ns("saturation_clean_ui_1")),
    mod_saturation_graphe_ui(ns("saturation_graphe_ui_1")),
    mod_saturation_table_ui(ns("saturation_table_ui_1")),
    
    mod_saturation_appel_WS_ui(ns("saturation_appel_WS_ui_2")),
    mod_saturation_clean_ui(ns("saturation_clean_ui_2")),
    mod_saturation_graphe_ui(ns("saturation_graphe_ui_2")),
    mod_saturation_table_ui(ns("saturation_table_ui_2")),
    
    mod_saturation_appel_WS_ui(ns("saturation_appel_WS_ui_3")),
    mod_saturation_clean_ui(ns("saturation_clean_ui_3")),
    mod_saturation_graphe_ui(ns("saturation_graphe_ui_3")),
    mod_saturation_table_ui(ns("saturation_table_ui_3")),
    
    mod_saturation_appel_WS_ui(ns("saturation_appel_WS_ui_4")),
    mod_saturation_clean_ui(ns("saturation_clean_ui_4")),
    mod_saturation_graphe_ui(ns("saturation_graphe_ui_4")),
    mod_saturation_table_ui(ns("saturation_table_ui_4")),
  )
}

#' saturation Server Functions
#'
#' @noRd 
mod_saturation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    parc_relais <- Saturation$new(rangeStart = as.Date("2021-05-24"), rangeEnd = as.Date("2021-05-31"), localisation_parking = NA, parc_relais = TRUE)
    hypercentre <- Saturation$new(rangeStart = as.Date("2021-05-24"), rangeEnd = as.Date("2021-05-31"), localisation_parking = "hypercentre", parc_relais = FALSE)
    centre <- Saturation$new(rangeStart = as.Date("2021-05-24"), rangeEnd = as.Date("2021-05-31"), localisation_parking = "centre", parc_relais = FALSE)
    peripherie  <- Saturation$new(rangeStart = as.Date("2021-05-24"), rangeEnd = as.Date("2021-05-31"), localisation_parking = "peripherie", parc_relais = FALSE)
    
    # observe(    browser())
    
    mod_saturation_appel_WS_server("saturation_appel_WS_ui_1", r6 = parc_relais)
    mod_saturation_clean_server("saturation_clean_ui_1", r6 = parc_relais)
    mod_saturation_graphe_server("saturation_graphe_ui_1", r6 = parc_relais)
    mod_saturation_table_server("saturation_table_ui_1", r6 = parc_relais)
    
    mod_saturation_appel_WS_server("saturation_appel_WS_ui_2", r6 = hypercentre)
    mod_saturation_clean_server("saturation_clean_ui_2", r6 = hypercentre)
    mod_saturation_graphe_server("saturation_graphe_ui_2", r6 = hypercentre)
    mod_saturation_table_server("saturation_table_ui_2", r6 = hypercentre)
    
    mod_saturation_appel_WS_server("saturation_appel_WS_ui_3", r6 = centre)
    mod_saturation_clean_server("saturation_clean_ui_3", r6 = centre)
    mod_saturation_graphe_server("saturation_graphe_ui_3", r6 = centre)
    mod_saturation_table_server("saturation_table_ui_3", r6 = centre)
    
    mod_saturation_appel_WS_server("saturation_appel_WS_ui_4", r6 = peripherie)
    mod_saturation_clean_server("saturation_clean_ui_4", r6 = peripherie)
    mod_saturation_graphe_server("saturation_graphe_ui_4", r6 = peripherie)
    mod_saturation_table_server("saturation_table_ui_4", r6 = peripherie)
    
  })
}

## To be copied in the UI
# mod_saturation_ui("saturation_ui_1")

## To be copied in the server
# mod_saturation_server("saturation_ui_1")
