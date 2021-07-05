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
#' @importFrom shinyjs show hide 
#' @importFrom lubridate year floor_date as_date

mod_occupation_2_periodes_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 2,
        
        radioButtons(ns("timestep"), "Unit\u00e9 de temps",
                     choices = c("Jour", "Semaine", "Mois", "Ann\u00e9e"),
                     inline = TRUE),
        
        # Plage horaires des donnees
        hidden_div(id_div = ns("selection_plage_horaire"),
                   contenu_div = tagList(
                     radioButtons(inputId = ns("plage_horaire"), 
                                  label = "Plage horaire",
                                  choices = c("Journ\u00e9e (8h-20h)", "Personnalis\u00e9e")), #"Nuit (20h-8h)",
                     hidden_div(id_div = ns("plage_horaire_personnalisee"),
                                contenu_div = tagList(
                                  sliderInput(inputId = ns("plage_horaire_perso"),
                                              label = "Affiner la plage horaire",
                                              min = 0, max = 23, value = c(12,16))
                                )
                     )
                   )
                   
        ),
        
        # Sélection d'un jour
        hidden_div(id_div = ns("selection_timestep_day"), 
                   contenu_div = tagList(
                     dateInput(inputId = ns("selected_day1"), label = "S\u00e9lectionner une premi\u00e8re journ\u00e9e", 
                               value = Sys.Date()-1, 
                               autoclose = TRUE, weekstart = 1, 
                               min = debut_donnees, max = Sys.Date()-1),
                     dateInput(inputId = ns("selected_day2"), label = "S\u00e9lectionner une deuxi\u00e8me journ\u00e9e", 
                               value = Sys.Date()-1, 
                               autoclose = TRUE, weekstart = 1, 
                               min = debut_donnees, max = Sys.Date()-1)
                     
                   )
        ),
        
        # Sélection d'une semaine
        hidden_div(id_div = ns("selection_timestep_week"), 
                   contenu_div = tagList(
                     dateInput(inputId = ns("selected_week1"), label = "S\u00e9lectionner une premi\u00e8re semaine (lundi)", 
                               daysofweekdisabled = c(0,2:6),
                               autoclose = TRUE, weekstart = 1,
                               min = debut_donnees, max = Sys.Date()-1),
                     dateInput(inputId = ns("selected_week2"), label = "S\u00e9lectionner une deuxi\u00e8me semaine (lundi)", 
                               daysofweekdisabled = c(0,2:6),
                               autoclose = TRUE, weekstart = 1,
                               min = debut_donnees, max = Sys.Date()-1)
                   )
        ),
        
        # Sélection d'un mois
        hidden_div(id_div = ns("selection_timestep_month"), 
                   contenu_div = tagList(
                     sliderInput(inputId = ns("selected_month1"), label = "S\u00e9lectionner un premier mois",
                                 min = floor_date(as_date(debut_donnees, tz = mytimezone), "month"),
                                 max = floor_date(as_date(Sys.Date()-1, tz = mytimezone), "month"),
                                 value = debut_donnees, timeFormat = "%Y-%m"),
                     sliderInput(inputId = ns("selected_month2"), label = "S\u00e9lectionner un deuxi\u00e8me mois",
                                 min = floor_date(as_date(debut_donnees, tz = mytimezone), "month"),
                                 max = floor_date(as_date(Sys.Date()-1, tz = mytimezone), "month"),
                                 value = debut_donnees, timeFormat = "%Y-%m")
                     
                   )
        ),
        
        # Sélection d'une année
        hidden_div(id_div = ns("selection_timestep_year"), 
                   contenu_div = tagList(
                     radioButtons(inputId = ns("selected_year1"), label = "S\u00e9lectionner une premi\u00e8re ann\u00e9e",
                                  choices = year(debut_donnees):year(Sys.Date())),
                     radioButtons(inputId = ns("selected_year2"), label = "S\u00e9lectionner une deuxi\u00e8me ann\u00e9e",
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
                    uiOutput(ns("my_Occupation_UI"))
                  )
                )         
      )
    )
  )
}

#' occupation Server Functions
#'
#' @noRd 
mod_occupation_2_periodes_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ids_list <- list("Jour" = "selection_timestep_day", 
                     "Semaine" = "selection_timestep_week", 
                     "Mois" = "selection_timestep_month", 
                     "Ann\u00e9e" = "selection_timestep_year")
    
    # En fonction de la fenetre temporelle selectionnee, on affiche le selecteur de date approprié et on masque les autres
    observeEvent(input$timestep, { 
      # On recupere l'id à afficher
      show_some_ids(ids = ids_list[[input$timestep]])
      # On recupere les id à masquer
      hide_some_ids(ids = ids_list[!names(ids_list) == input$timestep])
      
    })
    
    observeEvent(input$timestep, {
      if(input$timestep == "Jour") {
        show("selection_plage_horaire") 
      } else {
        hide("selection_plage_horaire")
      }
    })
    
    observeEvent(input$plage_horaire, {
      if(input$plage_horaire == "Personnalis\u00e9e") {
        show("plage_horaire_personnalisee") 
      } else {
        hide("plage_horaire_personnalisee")   
      }
      
    })
    
    plageHoraire <- reactive(
      if(input$timestep == "Jour") {
        switch(input$plage_horaire,
               "Journ\u00e9e (8h-20h)" = 8:20,
               "Nuit (20h-8h)" = c(0:7,21:23),
               "Personnalis\u00e9e" = input$plage_horaire_perso[1]:input$plage_horaire_perso[2]
        )
      }  else { 0:23 }
    )
    
    
    
    
    observeEvent(input$run_query,{
      
      # browser()
      
      xtradata_parameters <- reactiveValues(
        periode1 =  switch(input$timestep,
                           "Jour" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_day1),
                           "Semaine" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_week1),
                           "Mois" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_month1),
                           "Ann\u00e9e" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_year1)
        ),
        periode2 =  switch(input$timestep,
                           "Jour" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_day2),
                           "Semaine" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_week2),
                           "Mois" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_month2),
                           "Ann\u00e9e" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_year2)
        )
      )
      # print(xtradata_parameters$periode1)
      
      list_of_Occupation1 <- list(
        parc_relais1 = Occupation$new(rangeStart = xtradata_parameters$periode1$rangeStart,
                                      rangeEnd = xtradata_parameters$periode1$rangeEnd,
                                      rangeStep = xtradata_parameters$periode1$rangeStep,
                                      plageHoraire = plageHoraire(),
                                      localisation_parking = NA,
                                      parc_relais = TRUE)
        ,
        hypercentre1 = Occupation$new(rangeStart = xtradata_parameters$periode1$rangeStart,
                                      rangeEnd = xtradata_parameters$periode1$rangeEnd,
                                      rangeStep = xtradata_parameters$periode1$rangeStep,
                                      plageHoraire = plageHoraire(),
                                      localisation_parking = "hypercentre",
                                      parc_relais = FALSE)
        ,
        centre1 = Occupation$new(rangeStart = xtradata_parameters$periode1$rangeStart,
                                 rangeEnd = xtradata_parameters$periode1$rangeEnd,
                                 rangeStep = xtradata_parameters$periode1$rangeStep,
                                 plageHoraire = plageHoraire(),
                                 localisation_parking = "centre",
                                 parc_relais = FALSE)
        ,
        peripherie1 = Occupation$new(rangeStart = xtradata_parameters$periode1$rangeStart,
                                     rangeEnd = xtradata_parameters$periode1$rangeEnd,
                                     rangeStep = xtradata_parameters$periode1$rangeStep,
                                     plageHoraire = plageHoraire(),
                                     localisation_parking = "peripherie",
                                     parc_relais = FALSE)
      )
      
      
      list_of_Occupation2 <- list(
        parc_relais2 = Occupation$new(rangeStart = xtradata_parameters$periode2$rangeStart,
                                      rangeEnd = xtradata_parameters$periode2$rangeEnd,
                                      rangeStep = xtradata_parameters$periode2$rangeStep,
                                      plageHoraire = plageHoraire(),
                                      localisation_parking = NA,
                                      parc_relais = TRUE)
        ,
        hypercentre2 = Occupation$new(rangeStart = xtradata_parameters$periode2$rangeStart,
                                      rangeEnd = xtradata_parameters$periode2$rangeEnd,
                                      rangeStep = xtradata_parameters$periode2$rangeStep,
                                      plageHoraire = plageHoraire(),
                                      localisation_parking = "hypercentre",
                                      parc_relais = FALSE)
        ,
        centre2 = Occupation$new(rangeStart = xtradata_parameters$periode2$rangeStart,
                                 rangeEnd = xtradata_parameters$periode2$rangeEnd,
                                 rangeStep = xtradata_parameters$periode2$rangeStep,
                                 plageHoraire = plageHoraire(),
                                 localisation_parking = "centre",
                                 parc_relais = FALSE)
        ,
        peripherie2 = Occupation$new(rangeStart = xtradata_parameters$periode2$rangeStart,
                                     rangeEnd = xtradata_parameters$periode2$rangeEnd,
                                     rangeStep = xtradata_parameters$periode2$rangeStep,
                                     plageHoraire = plageHoraire(),
                                     localisation_parking = "peripherie",
                                     parc_relais = FALSE)
      )
      
      
      purrr::imap(c(list_of_Occupation1, list_of_Occupation2), function(.x, .y) {
        mod_occupation_appel_WS_server(paste0("occupation_2_periodes_appel_WS_ui_", .y), r6 = .x)
        mod_occupation_clean_server(paste0("occupation_2_periodes_clean_ui_", .y), r6 = .x)
      })
      
      purrr::pmap(list(list_of_Occupation1, list_of_Occupation2, names(list_of_Occupation1)), function(.x, .y, .z) {
        mod_occupation_2_periodes_graphe_server(paste0("occupation_2_periodes_clean_ui_", .z), r6_1 = .x, r6_2 = .y)
      })
      
      # browser()
      
      output$my_Occupation_UI <- renderUI({
        lapply(names(list_of_Occupation1), function(.y) {
          tagList(
            mod_occupation_2_periodes_graphe_ui(ns(paste0("occupation_2_periodes_graphe_ui_", .y))),
            mod_occupation_2_periodes_table_ui(ns(paste0("occupation_2_periodes_table_ui_",.y)))
          )
        })
      })
    })
    
  })
}
## To be copied in the UI
# mod_occupation_1_periode_ui("occupation_ui_1")

## To be copied in the server
# mod_occupation_1_periode_server("occupation_ui_1")



