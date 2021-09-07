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
#' @importFrom lubridate floor_date as_date
#' @importFrom purrr imap pmap
#' @importFrom memoise memoise

mod_occupation_2_periodes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 2,
        radioButtons(ns("timestep"), "Unit\u00e9 de temps",
                     choices = c("Jour", "Semaine", "Mois", "Ann\u00e9e"),
                     inline = TRUE
        ),
        
        # Plage horaires des donnees
        hidden_div(
          id_div = ns("selection_plage_horaire"),
          contenu_div = tagList(
            radioButtons(
              inputId = ns("plage_horaire"),
              label = "Plage horaire",
              choices = c("Journ\u00e9e (8h-20h)", "Personnalis\u00e9e")
            ),
            hidden_div(
              id_div = ns("plage_horaire_personnalisee"),
              contenu_div = tagList(
                sliderInput(
                  inputId = ns("plage_horaire_perso"),
                  label = "Affiner la plage horaire",
                  min = 0, max = 23, value = c(12, 16)
                )
              )
            )
          )
        ),
        
        # Sélection d'un jour
        hidden_div(
          id_div = ns("selection_timestep_day"),
          contenu_div = tagList(
            dateInput(
              inputId = ns("selected_day1"), label = "S\u00e9lectionner une premi\u00e8re journ\u00e9e",
              value = Sys.Date() - 1,
              autoclose = TRUE, weekstart = 1,
              min = debut_donnees, max = Sys.Date() - 1
            ),
            dateInput(
              inputId = ns("selected_day2"), label = "S\u00e9lectionner une deuxi\u00e8me journ\u00e9e",
              value = Sys.Date() - 1,
              autoclose = TRUE, weekstart = 1,
              min = debut_donnees, max = Sys.Date() - 1
            )
          )
        ),
        
        # Sélection d'une semaine
        hidden_div(
          id_div = ns("selection_timestep_week"),
          contenu_div = tagList(
            dateInput(
              inputId = ns("selected_week1"), label = "S\u00e9lectionner une premi\u00e8re semaine (lundi)",
              daysofweekdisabled = c(0, 2:6),
              autoclose = TRUE, weekstart = 1,
              min = debut_donnees, max = Sys.Date() - 1
            ),
            dateInput(
              inputId = ns("selected_week2"), label = "S\u00e9lectionner une deuxi\u00e8me semaine (lundi)",
              daysofweekdisabled = c(0, 2:6),
              autoclose = TRUE, weekstart = 1,
              min = debut_donnees, max = Sys.Date() - 1
            )
          )
        ),
        
        # Sélection d'un mois
        hidden_div(
          id_div = ns("selection_timestep_month"),
          contenu_div = tagList(
            sliderInput(
              inputId = ns("selected_month1"), label = "S\u00e9lectionner un premier mois",
              min = floor_date(as_date(debut_donnees, tz = mytimezone), "month"),
              max = floor_date(as_date(Sys.Date() - 1, tz = mytimezone), "month"),
              value = debut_donnees, timeFormat = "%Y-%m"
            ),
            sliderInput(
              inputId = ns("selected_month2"), label = "S\u00e9lectionner un deuxi\u00e8me mois",
              min = floor_date(as_date(debut_donnees, tz = mytimezone), "month"),
              max = floor_date(as_date(Sys.Date() - 1, tz = mytimezone), "month"),
              value = debut_donnees, timeFormat = "%Y-%m"
            )
          )
        ),
        
        # Sélection d'une année
        hidden_div(
          id_div = ns("selection_timestep_year"),
          contenu_div = tagList(
            radioButtons(
              inputId = ns("selected_year1"), label = "S\u00e9lectionner une premi\u00e8re ann\u00e9e",
              choices = lubridate::year(debut_donnees):lubridate::year(Sys.Date())
            ),
            radioButtons(
              inputId = ns("selected_year2"), label = "S\u00e9lectionner une deuxi\u00e8me ann\u00e9e",
              choices = lubridate::year(debut_donnees):lubridate::year(Sys.Date())
            )
          )
        ),
        
        checkboxInput(inputId = ns("select_custom_parkings_list"),
                      label = "Sélectionner manuellement des parkings"),
        
        hidden_div(
          id_div = ns("selection_custom_parkings_list"),
          contenu_div = tagList(
            selectizeInput(
              inputId = ns("custom_parkings_list"),
              label = "Parkings \u00e0 analyser",
              choices = NULL,
              multiple = TRUE,
              options = list(deselectBehavior = "top")
            ),
          )
        ),
        
        actionButton(
          inputId = ns("run_query"),
          label = "Lancer la requ\u00eate"
        )
      ),
      mainPanel(
        width = 10,
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
mod_occupation_2_periodes_server <- function(id, app_theme, parkings_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ids_list <- list(
      "Jour" = "selection_timestep_day",
      "Semaine" = "selection_timestep_week",
      "Mois" = "selection_timestep_month",
      "Ann\u00e9e" = "selection_timestep_year"
    )
    
    # En fonction de la fenetre temporelle selectionnee, on affiche le selecteur de date approprié et on masque les autres
    observeEvent(input$timestep, {
      # On recupere l'id à afficher
      show_some_ids(ids = ids_list[[input$timestep]])
      # On recupere les id à masquer
      hide_some_ids(ids = ids_list[!names(ids_list) == input$timestep])
    })
    
    # Si on a omis de sélectionner une journee / une semaine d'interet, le bouton lancer la requete est non cliquable
    observe({
      if (input$timestep == "Jour") {
        if (isTruthy(input$selected_day1) & isTruthy(input$selected_day2)) {
          enable("run_query")
        }
        else {
          disable("run_query")
        }
      } else if (input$timestep == "Semaine") {
        if (isTruthy(input$selected_week1) & isTruthy(input$selected_week2)) {
          enable("run_query")
        }
        else {
          disable("run_query")
        }
      } else {
        enable("run_query")
      }
    })
    
    # La selection de la plage horaire est pour l'instant dispo uniquement au sein d'une journée (pas pour semaine, mois, annee)
    observeEvent(input$timestep, {
      if (input$timestep == "Jour") {
        show("selection_plage_horaire")
      } else {
        hide("selection_plage_horaire")
      }
    })
    
    observeEvent(input$plage_horaire, {
      if (input$plage_horaire == "Personnalis\u00e9e") {
        show("plage_horaire_personnalisee")
      } else {
        hide("plage_horaire_personnalisee")
      }
    })
    
    observeEvent(input$select_custom_parkings_list, {
      if (input$select_custom_parkings_list == TRUE) {
        show("selection_custom_parkings_list")
      } else {
        hide("selection_custom_parkings_list")
      }
    })
    
    observe(updateSelectizeInput(session, "custom_parkings_list", choices = unique(parkings_list()$nom), server = TRUE))
    
    
    plageHoraire <- reactive(
      if (input$timestep == "Jour") {
        switch(input$plage_horaire,
               "Journ\u00e9e (8h-20h)" = 8:20,
               "Nuit (20h-8h)" = c(0:7, 21:23),
               "Personnalis\u00e9e" = input$plage_horaire_perso[1]:input$plage_horaire_perso[2]
        )
        # La selection de la plage horaire est pour l'instant dispo uniquement au sein d'une journée (pas pour semaine, mois, annee)
      } else {
        0:23
      }
    )
    
    
    # On cree la liste d'objets R6 Occupation
    list_of_Occupation1 <- list(
      parc_relais = Occupation$new(parkings_list = parkings[which(parkings$localisation_parking %in% NA & parkings$parc_relais == TRUE), "ident"]),
      hypercentre = Occupation$new(parkings_list = parkings[which(parkings$localisation_parking %in% "hypercentre" & parkings$parc_relais == FALSE), "ident"]),
      centre = Occupation$new(parkings_list = parkings[which(parkings$localisation_parking %in% "centre" & parkings$parc_relais == FALSE), "ident"]),
      peripherie = Occupation$new(parkings_list = parkings[which(parkings$localisation_parking %in% "peripherie" & parkings$parc_relais == FALSE), "ident"]),
      selection_personnalisee = Occupation$new(parkings_list = NULL)
    )
    list_of_Occupation2 <- list(
      parc_relais2 = Occupation$new(parkings_list = parkings[which(parkings$localisation_parking %in% NA & parkings$parc_relais == TRUE), "ident"]),
      hypercentre2 = Occupation$new(parkings_list = parkings[which(parkings$localisation_parking %in% "hypercentre" & parkings$parc_relais == FALSE), "ident"]),
      centre2 = Occupation$new(parkings_list = parkings[which(parkings$localisation_parking %in% "centre" & parkings$parc_relais == FALSE), "ident"]),
      peripherie2 = Occupation$new(parkings_list = parkings[which(parkings$localisation_parking %in% "peripherie" & parkings$parc_relais == FALSE), "ident"]),
      selection_personnalisee2 = Occupation$new(parkings_list = NULL)
    )
    # On appelle memoise pour activer le cache sur les resultats
    list_of_Occupation1 <- lapply(list_of_Occupation1, function(.l) {
      .l$download_data_memoise <- memoise(.l$download_data) 
      .l
    })
    list_of_Occupation2 <- lapply(list_of_Occupation2, function(.l) {
      .l$download_data_memoise <- memoise(.l$download_data) 
      .l
    })
    
    
    observeEvent(input$run_query, {
      # La selection de la plage horaire est pour l'instant dispo uniquement au sein d'une journée (pas pour semaine, mois, annee)
      xtradata_parameters <- reactiveValues(
        periode1 = switch(input$timestep,
                          "Jour" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_day1),
                          "Semaine" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_week1),
                          "Mois" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_month1),
                          "Ann\u00e9e" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_year1)
        ),
        periode2 = switch(input$timestep,
                          "Jour" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_day2),
                          "Semaine" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_week2),
                          "Mois" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_month2),
                          "Ann\u00e9e" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_year2)
        )
      )
      
      # on verifie si la liste des parkings est non nulle, auquel cas soit on ecrase la liste de l'element selection_personnalisee, ou alors on recree une instance R6 si elle n'existe plus
      if(isTruthy(input$custom_parkings_list)) {
        if("selection_personnalisee" %in% names(list_of_Occupation1)) {
          list_of_Occupation1$selection_personnalisee$parkings_list <- parkings_list()[nom %in% input$custom_parkings_list][["ident"]]
          list_of_Occupation2$selection_personnalisee2$parkings_list <- parkings_list()[nom %in% input$custom_parkings_list][["ident"]]
        } else {
          list_of_Occupation1$selection_personnalisee = Occupation$new(parkings_list = parkings_list()[nom %in% input$custom_parkings_list][["ident"]])
          list_of_Occupation2$selection_personnalisee2 = Occupation$new(parkings_list = parkings_list()[nom %in% input$custom_parkings_list][["ident"]])
        }
      } else { # si la selection est nulle on vire la R6 custom selection de la liste des classes R6
        list_of_Occupation1 <- list_of_Occupation1[names(list_of_Occupation1) != "selection_personnalisee"]
        list_of_Occupation2 <- list_of_Occupation2[names(list_of_Occupation2) != "selection_personnalisee2"]
      }
      
      list_of_Occupation1 <- lapply(list_of_Occupation1, function(.l) {
        .l$rangeStart <- xtradata_parameters$periode1$rangeStart
        .l$rangeEnd <-  xtradata_parameters$periode1$rangeEnd
        .l$rangeStep <- xtradata_parameters$periode1$rangeStep
        .l$timeStep <- input$timestep
        .l$plageHoraire <- plageHoraire()
        .l
      })
      
      list_of_Occupation2 <- lapply(list_of_Occupation2, function(.l) {
        .l$rangeStart <- xtradata_parameters$periode2$rangeStart
        .l$rangeEnd <-  xtradata_parameters$periode2$rangeEnd
        .l$rangeStep <- xtradata_parameters$periode2$rangeStep
        .l$timeStep <- input$timestep
        .l$plageHoraire <- plageHoraire()
        .l
      })
      
    # On appelle sur la liste de classes R6, les modules d'appel au WS pour récup les données,
    # le module de nettoyage de l'output, et le module de création du graphique
    imap(c(list_of_Occupation1, list_of_Occupation2), function(.x, .y) {
      mod_occupation_appel_WS_server(paste0("occupation_2_periodes_appel_WS_ui_", .y), r6 = .x)
      mod_occupation_clean_server(paste0("occupation_2_periodes_clean_ui_", .y), r6 = .x)
    })
    
    pmap(list(list_of_Occupation1, list_of_Occupation2, names(list_of_Occupation1)), function(.x, .y, .z) {
      mod_occupation_2_periodes_graphe_server(paste0("occupation_2_periodes_graphe_ui_", .z), r6_1 = .x, r6_2 = .y, app_theme = app_theme, parkings_list = parkings_list)
    })
    
    # On output l'UI qui va contenir le graphique et les tableaux de résultats pour toutes les classes R6
    output$my_Occupation_UI <- renderUI({
      lapply(names(list_of_Occupation1), function(.y) {
        tagList(
          mod_occupation_2_periodes_graphe_ui(ns(paste0("occupation_2_periodes_graphe_ui_", .y)), title = camel(remove_underscore(.y))),
          tags$br(), tags$br()
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
