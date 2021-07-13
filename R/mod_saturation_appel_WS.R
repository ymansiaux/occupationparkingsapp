#' saturation_appel_WS UI Function
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
mod_saturation_appel_WS_ui <- function(id){
  ns <- NS(id)
  tagList(
  )
}

#' saturation_appel_WS Server Functions
#'
#' @noRd 
mod_saturation_appel_WS_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns
    observe({
      r6$download_data(rangeStep = "hour")
      
      if(!isTruthy(r6$data_xtradata)) {
        showNotification("La requ\u00eate n\'a pas fonctionn\u00e9", type = "error", duration = 30)
      }

    })
    
  })
  
}

## To be copied in the UI
# mod_saturation_appel_WS_ui("saturation_appel_WS_ui_1")

## To be copied in the server
# mod_saturation_appel_WS_server("saturation_appel_WS_ui_1")
