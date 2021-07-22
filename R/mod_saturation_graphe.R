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
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT datatable

mod_saturation_graphe_ui <- function(id, title){
  ns <- NS(id)
  tagList(
    fluidRow(
      # actionButton(ns("pause"), "pause"),
      column(width = 12,
             h4(title))
    ),
    fluidRow(
      column(width = 6,
             selectizeInput(ns("selected_satured_parking1"), label = "Choisir un parking \u00e0 afficher", choices = NULL),
             withSpinner(
               girafeOutput(ns("plot"))
             )
      ),
      column(width = 6,
             selectizeInput(ns("selected_satured_parking2"), label = "Choisir un parking \u00e0 afficher", choices = NULL),
             withSpinner(
               girafeOutput(ns("plot2"))
             )
      )
    ),
    
    fluidRow(
      column(width = 12,
             lien_afficher_cacher_div(id_lien = ns("show_plot_data"), 
                                      label_lien = "Afficher les donn\u00e9es du graphe",
                                      id_div = ns("plot_data"), 
                                      contenu_div = tagList(
                                        withSpinner(
                                          DTOutput(ns("table_plot"))
                                        ))
             )
      )
    ),
    fluidRow(column(width = 12,
                    lien_afficher_cacher_div(id_lien = ns("show_raw_data"), 
                                             label_lien = "Afficher les donn\u00e9es brutes",
                                             id_div = ns("raw_data"), 
                                             contenu_div = tagList(
                                               withSpinner(
                                                 DTOutput(ns("table_raw"))
                                               ))
                    )
    )
    )
  )
}

#' saturation_graphe Server Functions
#'
#' @noRd 
mod_saturation_graphe_server <- function(id, r6, app_theme = app_theme){
  moduleServer( id, function(input, output, session){
    
    observeEvent(input$pause, browser())
    
    observe({
      updateSelectizeInput(session, 'selected_satured_parking1', 
                           choices = unique(parkings$nom[parkings$ident %in% r6$parkings_satures$ident]),
                           selected = unique(parkings$nom[parkings$ident %in% r6$parkings_satures$ident])[1],
                           server = TRUE)
      updateSelectizeInput(session, 'selected_satured_parking2', 
                           choices = unique(parkings$nom[parkings$ident %in% r6$parkings_satures$ident]),
                           selected = unique(parkings$nom[parkings$ident %in% r6$parkings_satures$ident])[1],
                           server = TRUE)
    })
    
    girafe_sizing <- reactiveValues()
    
    observe({
      if(r6$timeStep != "Semaine") {
        # si on a un graphe restitué au mois
        girafe_sizing$width_svg <- 10
        girafe_sizing$height_svg <- 9
      }  else {
        girafe_sizing$width_svg <- 12
        girafe_sizing$height_svg <- 6
      }
    })
    
    
    output$plot <- renderGirafe({
      
      validate(
        need(isTruthy(r6$data_xtradata), 'Aucun graphe \u00e0 afficher - v\u00e9rifier la requ\u00eate'),
        need(nrow(r6$parkings_satures) >0, 'Aucun parking ne remplit les crit\u00e8res d\u00e9finis')
      )
      
      
      
      gg <- r6$calendar_heatmap(selected_parking = unique(parkings$ident[parkings$nom %in% input$selected_satured_parking1]),
                                app_theme = app_theme()) 
      
      
      x <- girafe(ggobj = gg, width_svg =  girafe_sizing$width_svg, height_svg =  girafe_sizing$height_svg,
                  # pointsize = 15,
                  options = list(
                    opts_hover(css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;")
                  ))
      x
    })
    
    output$plot2 <- renderGirafe({
      
      validate(
        need(isTruthy(r6$data_xtradata), 'Aucun graphe \u00e0 afficher - v\u00e9rifier la requ\u00eate'),
        need(nrow(r6$parkings_satures) >0, 'Aucun parking ne remplit les crit\u00e8res d\u00e9finis')
      )
      
      gg <- r6$calendar_heatmap(selected_parking = unique(parkings$ident[parkings$nom %in% input$selected_satured_parking2]),
                                app_theme = app_theme())
      
      x <- girafe(ggobj = gg, width_svg =  girafe_sizing$width_svg, height_svg =  girafe_sizing$height_svg,
                  # pointsize = 15,
                  options = list(
                    opts_hover(css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;")
                  ))
      x
    })
    
    
    onclick("show_plot_data",
            toggle(id = "plot_data", anim = TRUE))
    
    onclick("show_raw_data",
            toggle(id = "raw_data", anim = TRUE))
    
    
    output$table_plot <- renderDT({
      validate(
        need(isTruthy(r6$data_xtradata), 'Aucun tableau \u00e0 afficher - v\u00e9rifier la requ\u00eate')
      )
      
      r6$data_plot %>% 
        .[, `:=`(taux_occupation = round(taux_occupation,1),
                 time = as.character(time))] %>% 
        .[,ident:nom] %>% 
        .[,etat:=NULL] %>% 
        datatable(., rownames = FALSE, caption = NULL,
                  extensions = "Buttons", options = parametres_output_DT)
      
    })
    
    output$table_raw <- renderDT({
      validate(
        need(isTruthy(r6$data_xtradata), 'Aucun tableau \u00e0 afficher - v\u00e9rifier la requ\u00eate')
      )
      
      r6$cleaned_data %>% 
        .[, `:=`(taux_occupation = round(taux_occupation,1),
                 time = as.character(time))] %>% 
        .[,etat:=NULL] %>% 
        datatable(., rownames = FALSE, caption = NULL,
                  extensions = "Buttons", options = parametres_output_DT)
    })
    
  })
}

## To be copied in the UI
# mod_saturation_graphe_ui("saturation_graphe_ui_1")

## To be copied in the server
# mod_saturation_graphe_server("saturation_graphe_ui_1")
