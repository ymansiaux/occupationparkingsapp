#' saturation_clean UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @import R6
#' @importFrom DT DTOutput renderDT
mod_saturation_clean_ui <- function(id){
  ns <- NS(id)
  tagList(
  )
}

#' saturation_clean Server Functions
#'
#' @noRd 
mod_saturation_clean_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns
    # observe(browser())
    observe({
      r6$clean_output()
      r6$filter_full_capacity_parkings()
    })
    
  })
}

# parkings %>% tidytable::filter.(parc_relais == r6$parc_relais)

## To be copied in the UI
# mod_saturation_clean_ui("saturation_clean_ui_1")

## To be copied in the server
# mod_saturation_clean_server("saturation_clean_ui_1")
