#' occupation_appel_WS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @import R6
#' @import data.table
#' @import DT
mod_occupation_appel_WS_ui <- function(id){
  ns <- NS(id)
  tagList(
    dataTableOutput(ns("table"))
  )
}

#' occupation_appel_WS Server Functions
#'
#' @noRd 
mod_occupation_appel_WS_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns
    # observe(browser())
    
    output$table <- renderDataTable({
      head(r6$download_data())
      head(r6$data_xtradata)
    })

  })
}

## To be copied in the UI
# mod_occupation_appel_WS_ui("occupation_appel_WS_ui_1")

## To be copied in the server
# mod_occupation_appel_WS_server("occupation_appel_WS_ui_1")
