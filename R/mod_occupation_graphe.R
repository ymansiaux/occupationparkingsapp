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
#' @importFrom ggiraph renderGirafe girafeOutput girafe  opts_hover_inv
#' @importFrom shinybm hidden_div
#' @importFrom shinyjs show hide
#' 
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
      observeEvent(input$pause, browser())
      # r6$timeseries_plot()
      
      input$maj
      
      r6$add_parkings_names()
      r6$data_xtradata$nom[is.na(r6$data_xtradata$nom)] <- "moyenne"
      
      gg <- r6$timeseries_plot(isolate(input$parkings_to_plot), TRUE)
      
      x <- girafe(ggobj = gg, width_svg = 8, height_svg = 6,
                  options = list(
                    opts_hover_inv(css = "opacity:0.1;"),
                    opts_hover(css = "stroke-width:2;")
                  ))
      x
      # 
      # 
      # 
      # data_plot <-  r6$data_xtradata %>% 
      #   mutate.(tooltip = as.character(
      #     glue_data(.SD, "Date : {as.character(time)}\nnom : {nom}\nVal : {sprintf('%.2f', taux_occupation)}")
      #   )
      #   ) %>% 
      #   mutate.(linetype = ifelse(ident == "moyenne", "dotted", "solid"))
      # 
      # parkings_to_plot <- input$parkings_to_plot
      # 
      # filter.(data_plot, ident %in% parkings_to_plot & ident != "moyenne") %>%  
      #   ggplot(data = ., mapping = aes(x = time, y = taux_occupation, color = nom, group=nom, linetype = nom)) + 
      #   geom_line_interactive(aes(data_id=ident), lwd = 1) + 
      #   geom_point_interactive(aes(tooltip=tooltip, data_id=ident)) + 
      #   theme_minimal() +
      #   theme(legend.position = "bottom") +
      #   geom_line_interactive(data = data_plot %>% filter.(ident == "moyenne"), 
      #                                                          mapping = aes(x = time, y = taux_occupation, tooltip=taux_occupation, data_id = ident, group = nom, color = nom),
      #                                                          lwd = 1.5) +
      #   scale_linetype_manual(
      #     "nom",
      #     values=deframe(data_plot %>% filter.(ident %in% c("moyenne", parkings_to_plot)) %>% select.(nom, linetype) %>% distinct.)) +
      #   scale_color_manual(values = sample(colors(distinct = TRUE), length(parkings_to_plot)+1))
      
    })
    
  })
}

## isolate dans le graphe et bouton MAJ parking

# parkings %>% tidytable::filter.(parc_relais == r6$parc_relais)

## To be copied in the UI
# mod_occupation_graphe_ui("occupation_graphe_ui_1")

## To be copied in the server
# mod_occupation_graphe_server("occupation_graphe_ui_1")
