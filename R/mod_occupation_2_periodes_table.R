#' occupation_table UI Function
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
mod_occupation_2_periodes_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Tableau"),
    DTOutput(ns("table"))
  )
}

#' occupation_table Server Functions
#'
#' @noRd 
mod_occupation_2_periodes_table_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns
    # observe(browser())
    
    output$table <- renderDT({
      # r6$data_xtradata[1:5,]
      r6$cleaned_data
    })
    
  })
}

# parkings %>% tidytable::filter.(parc_relais == r6$parc_relais)

## To be copied in the UI
# mod_occupation_table_ui("occupation_table_ui_1")

## To be copied in the server
# mod_occupation_table_server("occupation_table_ui_1")
