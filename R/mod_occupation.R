#' occupation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @importFrom lubridate year
#' @importFrom shinybm hidden_div
#' @importFrom purrr walk pluck
#' @importFrom shinyjs show hide
mod_occupation_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 2,
        
        radioButtons(ns("timestep_data"), "Unité de temps",
                     choices = c("Jour", "Semaine", "Mois", "Année"),
                     inline = TRUE),
        
        # Sélection d'un jour
        hidden_div(id_div = ns("selection_timestep_data_day"), 
                   contenu_div = tagList(
                     dateInput(inputId = ns("selected_day"), label = "Sélectionner une journée", 
                               value = Sys.Date()-1, 
                               autoclose = TRUE, weekstart = 1, 
                               min = debut_donnees, max = Sys.Date()-1)
                   )
        ),
        
        # Sélection d'une semaine
        hidden_div(id_div = ns("selection_timestep_data_week"), 
                   contenu_div = tagList(
                     dateInput(inputId = ns("selected_week"), label = "Sélectionner une semaine (lundi)", 
                               daysofweekdisabled = c(0,2:6),
                               autoclose = TRUE, weekstart = 1,
                               min = debut_donnees, max = Sys.Date()-1),
                   )
        ),
        
        # Sélection d'un mois
        hidden_div(id_div = ns("selection_timestep_data_month"), 
                   contenu_div = tagList(
                     sliderInput(inputId = ns("selected_month"), label = "Sélectionner un mois",
                                 min = debut_donnees, max = Sys.Date()-1,
                                 value = debut_donnees, timeFormat = "%Y-%m"
                     )
                   )
        ),
        
        # Sélection d'une année
        hidden_div(id_div = ns("selection_timestep_data_year"), 
                   contenu_div = tagList(
                     radioButtons(inputId = ns("selected_year"), label = "Sélectionner une année",
                                  choices = year(debut_donnees):year(Sys.Date()))
                   )
        ),
        
        actionButton(
          inputId = ns("run_query"),
          label = "Lancer la requ\u00eate"
        )
        
      ),
      
      mainPanel(width = 10,
                fluidRow(
                  column(
                    width = 12,
                    
                    mod_occupation_appel_WS_ui(ns("occupation_appel_WS_ui_1")),
                    mod_occupation_clean_ui(ns("occupation_clean_ui_1")),
                    mod_occupation_graphe_ui(ns("occupation_graphe_ui_1")),
                    mod_occupation_table_ui(ns("occupation_table_ui_1")),
                    
                    mod_occupation_appel_WS_ui(ns("occupation_appel_WS_ui_2")),
                    mod_occupation_clean_ui(ns("occupation_clean_ui_2")),
                    mod_occupation_graphe_ui(ns("occupation_graphe_ui_2")),
                    mod_occupation_table_ui(ns("occupation_table_ui_2")),
                    
                    mod_occupation_appel_WS_ui(ns("occupation_appel_WS_ui_3")),
                    mod_occupation_clean_ui(ns("occupation_clean_ui_3")),
                    mod_occupation_graphe_ui(ns("occupation_graphe_ui_3")),
                    mod_occupation_table_ui(ns("occupation_table_ui_3")),
                    
                    mod_occupation_appel_WS_ui(ns("occupation_appel_WS_ui_4")),
                    mod_occupation_clean_ui(ns("occupation_clean_ui_4")),
                    mod_occupation_graphe_ui(ns("occupation_graphe_ui_4")),
                    mod_occupation_table_ui(ns("occupation_table_ui_4")))
                  
                )
      )         
      
    )
  )
}

#' occupation Server Functions
#'
#' @noRd 
mod_occupation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ids_list <- list("Jour" = "selection_timestep_data_day", 
                     "Semaine" = "selection_timestep_data_week", 
                     "Mois" = "selection_timestep_data_month", 
                     "Année" = "selection_timestep_data_year")
    
    # En fonction de la fenetre temporelle selectionnee, on affiche le selecteur de date approprié et on masque les autres
    observeEvent(input$timestep_data, { 
      # On recupere l'id à afficher
      show(pluck(ids_list, input$timestep_data))
      # On recupere les id à masquer
      walk(ids_list[!names(ids_list) == input$timestep_data], ~hide(.))
      
    })
    

    
    # parc_relais <- Occupation$new(rangeStart = Sys.Date() - 2, rangeEnd = Sys.Date() - 1, localisation_parking = NA, parc_relais = TRUE)
    # hypercentre <- Occupation$new(rangeStart = Sys.Date() - 2, rangeEnd = Sys.Date() - 1, localisation_parking = "hypercentre", parc_relais = FALSE)
    # centre <- Occupation$new(rangeStart = Sys.Date() - 2, rangeEnd = Sys.Date() - 1, localisation_parking = "centre", parc_relais = FALSE)
    # peripherie  <- Occupation$new(rangeStart = Sys.Date() - 2, rangeEnd = Sys.Date() - 1, localisation_parking = "peripherie", parc_relais = FALSE)
    
    observeEvent(input$run_query,{
      
      if(!is.null(input$rangePeriod) & length(input$rangePeriod) == 2)
        
        parc_relais <- Occupation$new(rangeStart = input$rangePeriod[1],
                                      rangeEnd = input$rangePeriod[2],
                                      localisation_parking = NA,
                                      parc_relais = TRUE)
      
      hypercentre <- Occupation$new(rangeStart = input$rangePeriod[1],
                                    rangeEnd = input$rangePeriod[2],
                                    localisation_parking = "hypercentre",
                                    parc_relais = FALSE)
      
      centre <- Occupation$new(rangeStart = input$rangePeriod[1],
                               rangeEnd = input$rangePeriod[2],
                               localisation_parking = "centre",
                               parc_relais = FALSE)
      
      peripherie  <- Occupation$new(rangeStart = input$rangePeriod[1],
                                    rangeEnd = input$rangePeriod[2],
                                    localisation_parking = "peripherie",
                                    parc_relais = FALSE)
      
      # observe(print(input$rangePeriod))
      
      mod_occupation_appel_WS_server("occupation_appel_WS_ui_1", r6 = parc_relais)
      mod_occupation_clean_server("occupation_clean_ui_1", r6 = parc_relais)
      mod_occupation_graphe_server("occupation_graphe_ui_1", r6 = parc_relais)
      mod_occupation_table_server("occupation_table_ui_1", r6 = parc_relais)
      
      mod_occupation_appel_WS_server("occupation_appel_WS_ui_2", r6 = hypercentre)
      mod_occupation_clean_server("occupation_clean_ui_2", r6 = hypercentre)
      mod_occupation_graphe_server("occupation_graphe_ui_2", r6 = hypercentre)
      mod_occupation_table_server("occupation_table_ui_2", r6 = hypercentre)
      
      mod_occupation_appel_WS_server("occupation_appel_WS_ui_3", r6 = centre)
      mod_occupation_clean_server("occupation_clean_ui_3", r6 = centre)
      mod_occupation_graphe_server("occupation_graphe_ui_3", r6 = centre)
      mod_occupation_table_server("occupation_table_ui_3", r6 = centre)
      
      mod_occupation_appel_WS_server("occupation_appel_WS_ui_4", r6 = peripherie)
      mod_occupation_clean_server("occupation_clean_ui_4", r6 = peripherie)
      mod_occupation_graphe_server("occupation_graphe_ui_4", r6 = peripherie)
      mod_occupation_table_server("occupation_table_ui_4", r6 = peripherie)
      
    })
    
  })
}

## To be copied in the UI
# mod_occupation_ui("occupation_ui_1")

## To be copied in the server
# mod_occupation_server("occupation_ui_1")



