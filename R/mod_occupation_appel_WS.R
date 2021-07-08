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
#' @importFrom DT DTOutput renderDT
mod_occupation_appel_WS_ui <- function(id){
  ns <- NS(id)
  tagList(
  )
}

#' occupation_appel_WS Server Functions
#'
#' @noRd 
mod_occupation_appel_WS_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns
    observe({
      r6$download_data(rangeStep = r6$rangeStep)
      
      if(!isTruthy(r6$data_xtradata)) {
        showNotification("La requête n'a pas fonctionné", type = "error", duration = 30)
      }
      
    })
      
  })
  
}

# parkings %>% tidytable::filter.(parc_relais == r6$parc_relais)

## To be copied in the UI
# mod_occupation_appel_WS_ui("occupation_appel_WS_ui_1")

## To be copied in the server
# mod_occupation_appel_WS_server("occupation_appel_WS_ui_1")
