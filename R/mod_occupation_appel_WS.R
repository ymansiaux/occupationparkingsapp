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
    tableOutput(ns("number")),
    tableOutput(ns("table"))
  )
}

#' occupation_appel_WS Server Functions
#'
#' @noRd 
mod_occupation_appel_WS_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns
    # observe(browser())
    
    a <- reactive({
      # browser()
      r6$download_data()
      r6$data_xtradata
      print(head(r6$data_xtradata))
    })
    
    output$table <- renderTable({
      r6$data_xtradata <- r6$download_data()
      (head(r6$data_xtradata))
    })
    # 
    output$number <- renderTable(
      iris[sample(1:50,3,replace=FALSE),]    )
    
  })
}

## To be copied in the UI
# mod_occupation_appel_WS_ui("occupation_appel_WS_ui_1")

## To be copied in the server
# mod_occupation_appel_WS_server("occupation_appel_WS_ui_1")
