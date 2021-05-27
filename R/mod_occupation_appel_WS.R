#' occupation_appel_WS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
mod_occupation_appel_WS_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("coucou"),
    # tableOutput(ns("table")),
    tableOutput(ns("number"))
  )
}

#' occupation_appel_WS Server Functions
#'
#' @noRd 
mod_occupation_appel_WS_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # observe(browser())
    # output$table <- renderTable({
    #   r6$download_data()$data_xtradata
    # })
    
    output[[ns("number")]] <- renderTable(
      iris[20:23]    )
    
  })
}

## To be copied in the UI
# mod_occupation_appel_WS_ui("occupation_appel_WS_ui_1")

## To be copied in the server
# mod_occupation_appel_WS_server("occupation_appel_WS_ui_1")
