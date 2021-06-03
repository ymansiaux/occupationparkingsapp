#' saturation_graphe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @import R6
#' @importFrom ggiraph renderggiraph ggiraphOutput
mod_saturation_graphe_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Graphique"),
    ggiraphOutput(ns("plot"))
  )
}

#' saturation_graphe Server Functions
#'
#' @noRd 
mod_saturation_graphe_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    output$plot <- renderggiraph({
      r6$calendar_heatmap() 
    })
    
  })
}

# parkings %>% tidytable::filter.(parc_relais == r6$parc_relais)

## To be copied in the UI
# mod_saturation_graphe_ui("saturation_graphe_ui_1")

## To be copied in the server
# mod_saturation_graphe_server("saturation_graphe_ui_1")
