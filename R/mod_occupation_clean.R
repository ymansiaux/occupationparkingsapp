#' occupation_clean UI Function
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
mod_occupation_clean_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' occupation_clean Server Functions
#'
#' @noRd
mod_occupation_clean_server <- function(id, r6, parkings_list) {
  moduleServer(id, function(input, output, session) {
    # ns <- session$ns
    # observe(browser())
    observe({
      r6$clean_output(parkings_list)
      #r6$mean_by_some_time_unit(time_unit = r6$rangeStep)
      r6$mean_by_some_time_unit(time_unit = r6$aggregation_unit) ##### MODIFI !!!
    })
  })
}

# To be copied in the UI
# mod_occupation_clean_ui("occupation_clean_ui_1")

## To be copied in the server
# mod_occupation_clean_server("occupation_clean_ui_1")
