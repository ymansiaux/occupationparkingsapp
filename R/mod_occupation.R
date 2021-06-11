#' occupation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @importFrom shinybm hidden_div show_some_ids hide_some_ids
#' @importFrom purrr walk pluck
#' @importFrom shinyjs show hide 
#' @importFrom clock as_date add_days add_weeks add_months date_build add_years get_year date_group

mod_occupation_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 2,
        
        radioButtons(ns("timestep"), "Unité de temps",
                     choices = c("Jour", "Semaine", "Mois", "Année"),
                     inline = TRUE),
        
        # Sélection d'un jour
        hidden_div(id_div = ns("selection_timestep_day"), 
                   contenu_div = tagList(
                     dateInput(inputId = ns("selected_day"), label = "Sélectionner une journée", 
                               value = Sys.Date()-1, 
                               autoclose = TRUE, weekstart = 1, 
                               min = debut_donnees, max = Sys.Date()-1)
                   )
        ),
        
        # Sélection d'une semaine
        hidden_div(id_div = ns("selection_timestep_week"), 
                   contenu_div = tagList(
                     dateInput(inputId = ns("selected_week"), label = "Sélectionner une semaine (lundi)", 
                               daysofweekdisabled = c(0,2:6),
                               autoclose = TRUE, weekstart = 1,
                               min = debut_donnees, max = Sys.Date()-1),
                   )
        ),
        
        # Sélection d'un mois
        hidden_div(id_div = ns("selection_timestep_month"), 
                   contenu_div = tagList(
                     sliderInput(inputId = ns("selected_month"), label = "Sélectionner un mois",
                                 min = date_group(debut_donnees, precision = "month"),
                                 max = date_group(Sys.Date()-1, precision = "month"),
                                 value = debut_donnees, timeFormat = "%Y-%m"
                     )
                   )
        ),
        
        # Sélection d'une année
        hidden_div(id_div = ns("selection_timestep_year"), 
                   contenu_div = tagList(
                     radioButtons(inputId = ns("selected_year"), label = "Sélectionner une année",
                                  choices = get_year(debut_donnees):get_year(Sys.Date()))
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
    
    ids_list <- list("Jour" = "selection_timestep_day", 
                     "Semaine" = "selection_timestep_week", 
                     "Mois" = "selection_timestep_month", 
                     "Année" = "selection_timestep_year")
    
    # En fonction de la fenetre temporelle selectionnee, on affiche le selecteur de date approprié et on masque les autres
    observeEvent(input$timestep, { 
      # On recupere l'id à afficher
      show_some_ids(ids = ids_list[[input$timestep]])
      # On recupere les id à masquer
      hide_some_ids(ids = ids_list[!names(ids_list) == input$timestep])
      
    })
    
    
    observeEvent(input$run_query,{
      xtradata_parameters <-
        switch(input$timestep,
               "Jour" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_day),
               "Semaine" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_week),
               "Mois" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_month),
               "Année" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_year)
        )
 
      parc_relais <- Occupation$new(rangeStart = xtradata_parameters$rangeStart,
                                    rangeEnd = xtradata_parameters$rangeEnd,
                                    rangeStep = xtradata_parameters$rangeStep,
                                    localisation_parking = NA,
                                    parc_relais = TRUE)

      hypercentre <- Occupation$new(rangeStart = xtradata_parameters$rangeStart,
                                    rangeEnd = xtradata_parameters$rangeEnd,
                                    rangeStep = xtradata_parameters$rangeStep,
                                    localisation_parking = "hypercentre",
                                    parc_relais = FALSE)

      centre <- Occupation$new(rangeStart = xtradata_parameters$rangeStart,
                               rangeEnd = xtradata_parameters$rangeEnd,
                               rangeStep = xtradata_parameters$rangeStep,
                               localisation_parking = "centre",
                               parc_relais = FALSE)

      peripherie  <- Occupation$new(rangeStart = xtradata_parameters$rangeStart,
                                    rangeEnd = xtradata_parameters$rangeEnd,
                                    rangeStep = xtradata_parameters$rangeStep,
                                    localisation_parking = "peripherie",
                                    parc_relais = FALSE)


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



