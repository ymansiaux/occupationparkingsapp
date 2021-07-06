#' occupation_graphe UI Function
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
#' @importFrom ggiraph renderGirafe girafeOutput girafe  opts_hover_inv opts_sizing opts_hover
#' @importFrom shinybm hidden_div
#' @importFrom shinyjs show hide
#' @importFrom shinycssloaders withSpinner

mod_occupation_2_periodes_graphe_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 8,
             # h3("Graphique"),
             withSpinner(
               girafeOutput(ns("plot"))
             ),
             # actionButton(inputId = ns("pause"), "pause")
             # )
      ),
      # fluidRow(
      
      column(width = 4,
             selectizeInput(inputId = ns("parkings_to_plot"),
                            label = "Parkings \u00e0 afficher",
                            choices = NULL,
                            multiple = TRUE,
                            options = list(maxItems = 5, placeholder = "Choisir au max 5 pkgs", deselectBehavior = "top")
             ),
             
             actionButton(inputId = ns("maj"), "maj")
             
      )
    )
    
  )
}

#' occupation_graphe Server Functions
#'
#' @noRd 
mod_occupation_2_periodes_graphe_server <- function(id, r6_1, r6_2){
  moduleServer( id, function(input, output, session){
    
    observe(updateSelectizeInput(session, 'parkings_to_plot', choices = unique(c(r6_1$cleaned_data$nom, r6_2$cleaned_data$nom)), server = TRUE))
    observeEvent(input$pause, browser())
    
    # observe(      browser())
    output$plot <- renderGirafe({
      observeEvent(input$pause, browser())
      input$maj
      
      r6_1$aggregated_data_by_some_time_unit$nom[is.na(r6_1$aggregated_data_by_some_time_unit$nom)] <- "moyenne"
      r6_2$aggregated_data_by_some_time_unit$nom[is.na(r6_2$aggregated_data_by_some_time_unit$nom)] <- "moyenne"
      
      gg <- r6_1$timeseries_plot_2_curves(r6_1, r6_2, r6_1$timeStep, isolate(unique(parkings$ident[parkings$nom %in% input$parkings_to_plot])))
      
      x <- girafe(ggobj = gg, width_svg = 8, height_svg = 5, 
                  pointsize = 15,
                  options = list(
                    opts_hover_inv(css = "opacity:0.1;"),
                    opts_hover(css = "stroke-width:2;")
                  ))
      x
      
    })
    
  })
}

## To be copied in the UI
# mod_occupation_graphe_ui("occupation_graphe_ui_1")

## To be copied in the server
# mod_occupation_graphe_server("occupation_graphe_ui_1")
