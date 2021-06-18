#' saturation_graphe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @import R6
#' @importFrom ggiraph renderGirafe girafeOutput girafe opts_sizing opts_tooltip opts_hover  

mod_saturation_graphe_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             h3("Graphique"),
             sliderInput(ns("width"), "width", min = 3, max = 20, value = 12),
             sliderInput(ns("height"), "height", min = 3, max = 20, value = 6),
             sliderInput(ns("pointsize"), "pointsize", min = 3, max = 50, value = 12),
             checkboxInput(ns("with_facet"), "with_facet", value = TRUE)
             
             
      )
    ),
    fluidRow(
      column(width = 6,
             girafeOutput(ns("plot"))
      ),
      column(width = 6,
             girafeOutput(ns("plot2"))
      )
    )
  )
}

#' saturation_graphe Server Functions
#'
#' @noRd 
mod_saturation_graphe_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    output$plot <- renderGirafe({
      r6$add_parkings_names()
      # browser()
      gg <- r6$calendar_heatmap(input$with_facet) 
      
      x <- girafe(ggobj = gg, width_svg = input$width, height_svg = input$height,
                  pointsize = input$pointsize,
                  options = list(
                    # opts_sizing(rescale = TRUE, width = 1) ,
                    opts_hover(css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;")
                  ))
      x
    })
    
    output$plot2 <- renderGirafe({
      r6$add_parkings_names()
      # browser()
      gg <- r6$calendar_heatmap(input$with_facet) 
      
      x <- girafe(ggobj = gg, width_svg = input$width, height_svg = input$height,
                  pointsize = input$pointsize,
                  options = list(
                    # opts_sizing(rescale = TRUE, width = 1) ,
                    opts_hover(css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;")
                  ))
      x
    })
    
  })
}

# parkings %>% tidytable::filter.(parc_relais == r6$parc_relais)

## To be copied in the UI
# mod_saturation_graphe_ui("saturation_graphe_ui_1")

## To be copied in the server
# mod_saturation_graphe_server("saturation_graphe_ui_1")
