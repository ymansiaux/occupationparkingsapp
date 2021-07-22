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
mod_saturation_clean_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' saturation_clean Server Functions
#'
#' @noRd
mod_saturation_clean_server <- function(id, r6, seuil_saturation, nb_heures_journalieres_saturation, nb_jours_hebdo_saturation) {
  moduleServer(id, function(input, output, session) {
    # ns <- session$ns
    observe({
      if (isTruthy(r6$data_xtradata)) {
        r6$clean_output()
        r6$filter_full_capacity_parkings(seuil_saturation, nb_heures_journalieres_saturation, nb_jours_hebdo_saturation)
      }
    })
  })
}

## To be copied in the UI
# mod_saturation_clean_ui("saturation_clean_ui_1")

## To be copied in the server
# mod_saturation_clean_server("saturation_clean_ui_1")
