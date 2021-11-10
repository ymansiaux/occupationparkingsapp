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
#' @importFrom shinyjs show hide enable disable
#' @importFrom lubridate floor_date as_date
#' @importFrom purrr imap
#' @importFrom shinycssloaders withSpinner
#' @importFrom memoise memoise

mod_occupation_1_periode_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 2,
        # actionButton(ns("pause"), "Pause"),
        radioButtons(ns("timestep"), "Unit\u00e9 de temps",
                     choices = c("Jour", "Semaine", "Mois"),#, "Ann\u00e9e"),
                     inline = TRUE
        ),
        
        div(
          tagList(
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
              inputId = ns("selected_day"), label = "S\u00e9lectionner une journ\u00e9e",
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
              inputId = ns("selected_week"), label = "S\u00e9lectionner une semaine (lundi)",
              daysofweekdisabled = c(0, 2:6),
              autoclose = TRUE, weekstart = 1,
              min = debut_donnees, max = Sys.Date() - 1
            ),
          )
        ),
        
        # Sélection d'un mois
        hidden_div(
          id_div = ns("selection_timestep_month"),
          contenu_div = tagList(
            sliderInput(
              inputId = ns("selected_month"), label = "S\u00e9lectionner un mois",
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
              inputId = ns("selected_year"), label = "S\u00e9lectionner une ann\u00e9e",
              choices = lubridate::year(debut_donnees):lubridate::year(Sys.Date())
            )
          )
        ),
        checkboxInput(
          inputId = ns("select_custom_parkings_list"),
          label = "S\u00e9lectionner manuellement des parkings"
        ),
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
mod_occupation_1_periode_server <- function(id, app_theme, parkings_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$pause, browser())
    
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
        if (isTruthy(input$selected_day)) {
          enable("run_query")
        }
        else {
          disable("run_query")
        }
      } else if (input$timestep == "Semaine") {
        if (isTruthy(input$selected_week)) {
          enable("run_query")
        }
        else {
          disable("run_query")
        }
      } else {
        enable("run_query")
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
      switch(input$plage_horaire,
             "Journ\u00e9e (8h-20h)" = 8:20,
             "Personnalis\u00e9e" = input$plage_horaire_perso[1]:input$plage_horaire_perso[2]
      )
    )
    
    # On cree la liste d'objets R6 Occupation
    list_of_Occupation <- list(
      selection_personnalisee = Occupation$new(parkings_list = NULL),
      parc_relais = Occupation$new(parkings_list = parkings[which(parkings$localisation_parking %in% NA & parkings$parc_relais == TRUE), "ident"]),
      hypercentre = Occupation$new(parkings_list = parkings[which(parkings$localisation_parking %in% "hypercentre" & parkings$parc_relais == FALSE), "ident"]),
      centre = Occupation$new(parkings_list = parkings[which(parkings$localisation_parking %in% "centre" & parkings$parc_relais == FALSE), "ident"]),
      peripherie = Occupation$new(parkings_list = parkings[which(parkings$localisation_parking %in% "peripherie" & parkings$parc_relais == FALSE), "ident"])
    )
    # On appelle memoise pour activer le cache sur les resultats
    list_of_Occupation <- lapply(list_of_Occupation, function(.l) {
      .l$download_data_memoise <- memoise(.l$download_data)
      .l
    })
    
    
    observeEvent(input$run_query, {
      # On calcule les parametres rangeStart, rangeEnd, rangeStep pour xtradata en fonction des inputs de l'utilisateur
      xtradata_parameters <- reactive(
        switch(input$timestep,
               "Jour" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_day),
               "Semaine" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_week),
               "Mois" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_month),
               "Ann\u00e9e" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_year)
        )
      )
      
      # on verifie si la liste des parkings est non nulle, auquel cas soit on ecrase la liste de l'element selection_personnalisee, ou alors on recree une instance R6 si elle n'existe plus
      if (isTruthy(input$custom_parkings_list) & input$select_custom_parkings_list == TRUE) {
        if ("selection_personnalisee" %in% names(list_of_Occupation)) {
          list_of_Occupation$selection_personnalisee$parkings_list <- parkings_list()[nom %in% input$custom_parkings_list][["ident"]]
        } else {
          list_of_Occupation$selection_personnalisee <- Occupation$new(parkings_list = parkings_list()[nom %in% input$custom_parkings_list][["ident"]])
        }
      } else { # si la selection est nulle on vire la R6 custom selection de la liste des classes R6
        list_of_Occupation <- list_of_Occupation[names(list_of_Occupation) != "selection_personnalisee"]
      }
      
      list_of_Occupation <- lapply(list_of_Occupation, function(.l) {
        .l$rangeStart <- xtradata_parameters()$rangeStart
        .l$rangeEnd <- xtradata_parameters()$rangeEnd
        .l$rangeStep <- xtradata_parameters()$rangeStep
        .l$aggregation_unit <- xtradata_parameters()$aggregation_unit
        .l$plageHoraire <- plageHoraire()
        
        .l
      })
      
      # On appelle sur la liste de classes R6, les modules d'appel au WS pour récup les données,
      # le module de nettoyage de l'output, et le module de création du graphique
      imap(list_of_Occupation, function(.x, .y) {
        mod_occupation_appel_WS_server(paste0("occupation_appel_WS_ui_", .y), r6 = .x)
        mod_occupation_clean_server(paste0("occupation_clean_ui_", .y), r6 = .x, parkings_list = parkings_list)
        mod_occupation_1_periode_graphe_server(paste0("occupation_graphe_ui_", .y), r6 = .x, app_theme = app_theme, parkings_list = parkings_list)
      })
      
      output$my_Occupation_UI <- renderUI({
        lapply(names(list_of_Occupation), function(.y) {
          tagList(
            mod_occupation_1_periode_graphe_ui(ns(paste0("occupation_graphe_ui_", .y)), title = camel(remove_underscore(.y))),
            tags$br()
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
