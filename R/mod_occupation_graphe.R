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
#' @import ggiraph
#' @import ggplot2
mod_occupation_graphe_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 8,
             h3("Graphique"),
             girafeOutput(ns("plot")),
             actionButton(inputId = ns("pause"), "pause")
      ),
      column(width = 4,
             selectizeInput(inputId = ns("parkings_to_plot"),
                            label = "Parkings à afficher",
                            choices = NULL,
                            multiple = TRUE,
                            options = list(maxItems = 5, placeholder = "Choisir au max 10 pkgs", deselectBehavior = "top")
             ),
             checkboxInput(inputId = ns("show_average"),
                           label = "Afficher la moyenne", 
                           value = TRUE
                            ),
             actionButton(inputId = ns("maj"), "maj")
             
      )
    )
  )
}

#' occupation_graphe Server Functions
#'
#' @noRd 
mod_occupation_graphe_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    
    observe(updateSelectizeInput(session, 'parkings_to_plot', choices = unique(r6$data_xtradata$ident), server = TRUE))
    observeEvent(input$pause, browser())
    
    output$plot <- renderGirafe({
      # browser()
      # r6$timeseries_plot()
      
      input$maj
      
      gg <- r6$timeseries_plot(isolate(input$parkings_to_plot), isolate(input$show_average))

      x <- girafe(ggobj = gg, width_svg = 8, height_svg = 6,
                  options = list(
                    opts_hover_inv(css = "opacity:0.1;"),
                    opts_hover(css = "stroke-width:2;")
                  ))
      x
      
  
      
    })
    
  })
}

## isolate dans le graphe et bouton MAJ parking

# parkings %>% tidytable::filter.(parc_relais == r6$parc_relais)

## To be copied in the UI
# mod_occupation_graphe_ui("occupation_graphe_ui_1")

## To be copied in the server
# mod_occupation_graphe_server("occupation_graphe_ui_1")
