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
    # fluidRow(
    #   column(width = 12,
    #          h3("Graphique"),
    #          sliderInput(ns("width"), "width", min = 3, max = 20, value = 12),
    #          sliderInput(ns("height"), "height", min = 3, max = 20, value = 6),
    #          sliderInput(ns("pointsize"), "pointsize", min = 3, max = 50, value = 12),
    #          checkboxInput(ns("with_facet"), "with_facet", value = FALSE)
    #          
    #          
    #   )
    # ),
    fluidRow(
      column(width = 6,
             selectizeInput(ns("selected_satured_parking1"), label = "Choisir un parking à afficher", choices = NULL),
             girafeOutput(ns("plot"))
      ),
      column(width = 6,
             selectizeInput(ns("selected_satured_parking2"), label = "Choisir un parking à afficher", choices = NULL),
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
    
    
    observe({
      updateSelectizeInput(session, 'selected_satured_parking1', 
                                  choices = unique(r6$parkings_satures$ident),
                                  selected = unique(r6$parkings_satures$ident)[1],
                                  server = TRUE)
      updateSelectizeInput(session, 'selected_satured_parking2', 
                           choices = unique(r6$parkings_satures$ident),
                           selected = unique(r6$parkings_satures$ident)[1],
                           server = TRUE)
    })
    
    
    girafe_sizing <- reactiveValues()
    
    observe({
      if(length(unique(as_date(r6$data_xtradata$time)))>7) {
        # si on a un graphe restitué au mois
        girafe_sizing$width_svg <- 10
        girafe_sizing$height_svg <- 9
      }
      
      else {
        girafe_sizing$width_svg <- 12
        girafe_sizing$height_svg <- 6
      }
    })
    
    
    
    output$plot <- renderGirafe({

      gg <- r6$calendar_heatmap(FALSE, selected_parking = input$selected_satured_parking1) 
      
      x <- girafe(ggobj = gg, width_svg =  girafe_sizing$width_svg, height_svg =  girafe_sizing$height_svg,
                  pointsize = 12,
                  options = list(
                    opts_hover(css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;")
                  ))
      x
    })
    
    output$plot2 <- renderGirafe({

      gg <- r6$calendar_heatmap(FALSE, selected_parking = input$selected_satured_parking2) 
      
      x <- girafe(ggobj = gg, width_svg =  girafe_sizing$width_svg, height_svg =  girafe_sizing$height_svg,
                  pointsize = 12,
                  options = list(
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
