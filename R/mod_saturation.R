#' saturation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
mod_saturation_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 2,
        
        radioButtons(ns("timestep"), "Unit\u00e9 de temps",
                     choices = c("Semaine", "Mois"),
                     inline = TRUE),
        
        # Sélection d'une semaine
        hidden_div(id_div = ns("selection_timestep_week"), 
                   contenu_div = tagList(
                     dateInput(inputId = ns("selected_week"), label = "S\u00e9lectionner une semaine (lundi)", 
                               daysofweekdisabled = c(0,2:6),
                               autoclose = TRUE, weekstart = 1,
                               min = debut_donnees, max = Sys.Date()-1),
                   )
        ),
        
        # Sélection d'un mois
        hidden_div(id_div = ns("selection_timestep_month"), 
                   contenu_div = tagList(
                     sliderInput(inputId = ns("selected_month"), label = "S\u00e9lectionner un mois",
                                 min = floor_date(as_date(debut_donnees, tz = mytimezone), "month"),
                                 max = floor_date(as_date(Sys.Date()-1, tz = mytimezone), "month"),
                                 value = debut_donnees, timeFormat = "%Y-%m"
                     )
                   )
        ),
        
        h4("Définition de la saturation :"),
        sliderInput(inputId = ns("seuil_saturation"), "Seuil de saturation (%)", min = 0, max = 100, value = 90, step = 5),
        sliderInput(inputId = ns("nb_heures_journalieres_saturation"), "Nb heures / j de saturation", min = 0, max = 23, value = 3, step = 1),
        sliderInput(inputId = ns("nb_jours_hebdo_saturation"), "Nb j / semaine de saturation", min = 0, max = 7, value = 2, step = 1),
        
        
        
        actionButton(
          inputId = ns("run_query"),
          label = "Lancer la requ\u00eate"
        )
        
      ),
      
      mainPanel(width = 10,
                fluidRow(
                  column(
                    width = 12,
                    # mod_saturation_appel_WS_ui(ns("saturation_appel_WS_ui_1")),
                    # mod_saturation_clean_ui(ns("saturation_clean_ui_1")),
                    mod_saturation_graphe_ui(ns("saturation_graphe_ui_1")),
                    mod_saturation_table_ui(ns("saturation_table_ui_1"))
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    # mod_saturation_appel_WS_ui(ns("saturation_appel_WS_ui_2")),
                    # mod_saturation_clean_ui(ns("saturation_clean_ui_2")),
                    mod_saturation_graphe_ui(ns("saturation_graphe_ui_2")),
                    mod_saturation_table_ui(ns("saturation_table_ui_2"))
                  )
                ),
                fluidRow(
                  column(
                    width = 12, 
                    # mod_saturation_appel_WS_ui(ns("saturation_appel_WS_ui_3")),
                    # mod_saturation_clean_ui(ns("saturation_clean_ui_3")),
                    mod_saturation_graphe_ui(ns("saturation_graphe_ui_3")),
                    mod_saturation_table_ui(ns("saturation_table_ui_3"))
                  )
                ),
                
                fluidRow(
                  column(
                    width = 12,
                    # mod_saturation_appel_WS_ui(ns("saturation_appel_WS_ui_4")),
                    # mod_saturation_clean_ui(ns("saturation_clean_ui_4")),
                    mod_saturation_graphe_ui(ns("saturation_graphe_ui_4")),
                    mod_saturation_table_ui(ns("saturation_table_ui_4"))
                  )
                )
      )
    
  )
  )
}

#' saturation Server Functions
#'
#' @noRd 
mod_saturation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ids_list <- list("Semaine" = "selection_timestep_week", 
                     "Mois" = "selection_timestep_month")
    
    
    # En fonction de la fenetre temporelle selectionnee, on affiche le selecteur de date approprié et on masque les autres
    observeEvent(input$timestep, { 
      # On recupere l'id à afficher
      show_some_ids(ids = ids_list[[input$timestep]])
      # On recupere les id à masquer
      hide_some_ids(ids = ids_list[!names(ids_list) == input$timestep])
      
    })
    
    observeEvent(input$run_query,{
      xtradata_parameters <- reactive(
        switch(input$timestep,
               "Semaine" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_week),
               "Mois" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_month))
      )
      
      parc_relais <- Saturation$new(rangeStart = xtradata_parameters()$rangeStart,
                                    rangeEnd = xtradata_parameters()$rangeEnd,
                                    rangeStep = "hour",
                                    plageHoraire = 0:23,
                                    timeStep = input$timestep,
                                    localisation_parking = NA,
                                    parc_relais = TRUE)
      
      hypercentre <- Saturation$new(rangeStart = xtradata_parameters()$rangeStart,
                                    rangeEnd = xtradata_parameters()$rangeEnd,
                                    rangeStep = "hour",
                                    plageHoraire = 0:23,
                                    timeStep = input$timestep,
                                    localisation_parking = "hypercentre",
                                    parc_relais = FALSE)
      
      centre <- Saturation$new(rangeStart = xtradata_parameters()$rangeStart,
                               rangeEnd = xtradata_parameters()$rangeEnd,
                               rangeStep = "hour",
                               plageHoraire = 0:23,
                               timeStep = input$timestep,
                               localisation_parking = "centre",
                               parc_relais = FALSE)
      
      peripherie  <- Saturation$new(rangeStart = xtradata_parameters()$rangeStart,
                                    rangeEnd = xtradata_parameters()$rangeEnd,
                                    rangeStep = "hour",
                                    plageHoraire = 0:23,
                                    timeStep = input$timestep,
                                    localisation_parking = "peripherie",
                                    parc_relais = FALSE)
      
      mod_saturation_appel_WS_server("saturation_appel_WS_ui_1", r6 = parc_relais)
      mod_saturation_clean_server("saturation_clean_ui_1", r6 = parc_relais, 
                                  seuil_saturation = input$seuil_saturation, 
                                  nb_heures_journalieres_saturation = input$nb_heures_journalieres_saturation,
                                  nb_jours_hebdo_saturation = input$nb_jours_hebdo_saturation)
      
      mod_saturation_graphe_server("saturation_graphe_ui_1", r6 = parc_relais)
      mod_saturation_table_server("saturation_table_ui_1", r6 = parc_relais)
      
      # mod_saturation_appel_WS_server("saturation_appel_WS_ui_2", r6 = hypercentre)
      # mod_saturation_clean_server("saturation_clean_ui_2", r6 = hypercentre)
      # mod_saturation_graphe_server("saturation_graphe_ui_2", r6 = hypercentre)
      # mod_saturation_table_server("saturation_table_ui_2", r6 = hypercentre)
      # 
      # mod_saturation_appel_WS_server("saturation_appel_WS_ui_3", r6 = centre)
      # mod_saturation_clean_server("saturation_clean_ui_3", r6 = centre)
      # mod_saturation_graphe_server("saturation_graphe_ui_3", r6 = centre)
      # mod_saturation_table_server("saturation_table_ui_3", r6 = centre)
      # 
      # mod_saturation_appel_WS_server("saturation_appel_WS_ui_4", r6 = peripherie)
      # mod_saturation_clean_server("saturation_clean_ui_4", r6 = peripherie)
      # mod_saturation_graphe_server("saturation_graphe_ui_4", r6 = peripherie)
      # mod_saturation_table_server("saturation_table_ui_4", r6 = peripherie)
    })  
  })
}

## To be copied in the UI
# mod_saturation_ui("saturation_ui_1")

## To be copied in the server
# mod_saturation_server("saturation_ui_1")
