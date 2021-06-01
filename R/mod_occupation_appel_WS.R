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
    DTOutput(ns("table"))
  )
}

#' occupation_appel_WS Server Functions
#'
#' @noRd 
mod_occupation_appel_WS_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns
     # observe(browser())
    
    output$table <- renderDT({
      r6$download_data(rangeStep = "hour")
      r6$clean_output()
      r6$mean_by_some_time_unit(time_unit = "day")
      r6$data_xtradata[1:5,]
    })

    # download_data = function(rangeStep) {
    #   # self$data_xtradata <- 
    #     
    #     try(xtradata_requete_aggregate(
    #     key = "DATAZBOUBB",
    #     typename = "ST_PARK_P",
    #     rangeStart = r6$rangeStart,
    #     rangeEnd = r6$rangeEnd,
    #     rangeStep = "hour",
    #     rangeFilter = list(hours = 0:23, days = 1:7, publicHolidays = FALSE),
    #     filter = list(
    #       "ident" =
    #         list(
    #           "$in" =
    #             parkings %>% filter.(parc_relais == r6$parc_relais & localisation_parking %in% r6$localisation_parking) %>% select.(ident) %>% dplyr::pull()
    #           )
    #     ),
    #     attributes = list("gid", "time", "libres", "total", "etat", "ident"),
    #     showURL = TRUE
    #   ))
    # 
  })
}

# parkings %>% tidytable::filter.(parc_relais == r6$parc_relais)

## To be copied in the UI
# mod_occupation_appel_WS_ui("occupation_appel_WS_ui_1")

## To be copied in the server
# mod_occupation_appel_WS_server("occupation_appel_WS_ui_1")
