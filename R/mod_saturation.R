#' saturation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinyjs enable disable
mod_saturation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 2,
        fluidRow(
          column(
            width = 12,
            radioButtons(ns("timestep"), "Unit\u00e9 de temps",
              choices = c("Semaine", "Mois"),
              inline = TRUE
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
            h4("D\u00e9finition de la saturation :"),
            sliderInput(inputId = ns("seuil_saturation"), "Seuil de saturation (%)", min = 0, max = 100, value = 90, step = 5),
            sliderInput(inputId = ns("nb_heures_journalieres_saturation"), "Nb heures / j de saturation", min = 0, max = 23, value = 3, step = 1),
            sliderInput(inputId = ns("nb_jours_hebdo_saturation"), "Nb j / semaine de saturation", min = 0, max = 7, value = 2, step = 1),
            actionButton(
              inputId = ns("run_query"),
              label = "Lancer la requ\u00eate"
            )
          )
        )
      ),
      mainPanel(
        width = 10,
        fluidRow(
          column(
            width = 12,
            uiOutput(ns("my_Saturation_UI"))
          )
        )
      )
    )
  )
}

#' saturation Server Functions
#'
#' @noRd
mod_saturation_server <- function(id, app_theme, parkings_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ids_list <- list(
      "Semaine" = "selection_timestep_week",
      "Mois" = "selection_timestep_month"
    )


    # En fonction de la fenetre temporelle selectionnee, on affiche le selecteur de date approprié et on masque les autres
    observeEvent(input$timestep, {
      # On recupere l'id à afficher
      show_some_ids(ids = ids_list[[input$timestep]])
      # On recupere les id à masquer
      hide_some_ids(ids = ids_list[!names(ids_list) == input$timestep])
    })

    # si il manque un parametre on empeche l'utilisateur de lancer la requete
    observe({
      if ((input$timestep == "Semaine" & !isTruthy(input$selected_week)) | (input$timestep == "Mois" & !isTruthy(input$selected_month))) {
        disable("run_query")
      } else {
        enable("run_query")
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


    # On cree la liste d'objets R6 Saturation
    list_of_Saturation <- list(
      # selection_personnalisee = Saturation$new(rangeStep = "hour", plageHoraire = 0:23, parkings_list = NULL),
      # parc_relais = Saturation$new(rangeStep = "hour", plageHoraire = 0:23, parkings_list = parkings[which(parkings$localisation_parking %in% NA & parkings$parc_relais == TRUE), "ident"]),
      # hypercentre = Saturation$new(rangeStep = "hour", plageHoraire = 0:23, parkings_list = parkings[which(parkings$localisation_parking %in% "hypercentre" & parkings$parc_relais == FALSE), "ident"]),
      # centre = Saturation$new(rangeStep = "hour", plageHoraire = 0:23, parkings_list = parkings[which(parkings$localisation_parking %in% "centre" & parkings$parc_relais == FALSE), "ident"]),
      peripherie = Saturation$new(rangeStep = "hour", plageHoraire = 0:23, parkings_list = parkings[which(parkings$localisation_parking %in% "peripherie" & parkings$parc_relais == FALSE), "ident"])
    )

    # On appelle memoise pour activer le cache sur les resultats
    list_of_Saturation <- lapply(list_of_Saturation, function(.l) {
      .l$download_data_memoise <- memoise(.l$download_data)
      .l
    })


    observeEvent(input$run_query, {
      xtradata_parameters <- reactive(
        switch(input$timestep,
          "Semaine" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_week),
          "Mois" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_month)
        )
      )

      # si on a sélectionné au moins 1 parking perso et que la case de selection est cochee
      if (isTruthy(input$custom_parkings_list) & input$select_custom_parkings_list == TRUE) {
        # si l'objet R6 selection personnalisee existait deja dans la list_of_Saturation, on ecrase juste la selection de parkings perso, sinon cree l'objet
        if ("selection_personnalisee" %in% names(list_of_Saturation)) {
          list_of_Saturation$selection_personnalisee$parkings_list <- parkings_list()[nom %in% input$custom_parkings_list][["ident"]]
        } else {
          list_of_Saturation$selection_personnalisee <- Occupation$new(parkings_list = parkings_list()[nom %in% input$custom_parkings_list][["ident"]])
        }
      } else { # si la selection est nulle on vire la R6 custom selection de la liste des classes R6
        list_of_Saturation <- list_of_Saturation[names(list_of_Saturation) != "selection_personnalisee"]
      }


      list_of_Saturation <- lapply(list_of_Saturation, function(.l) {
        .l$rangeStart <- xtradata_parameters()$rangeStart
        .l$rangeEnd <- xtradata_parameters()$rangeEnd
        .l$aggregation_unit <- input$timestep
        .l
      })



      imap(list_of_Saturation, function(.x, .y) {
        mod_saturation_appel_WS_server(paste0("saturation_appel_WS_ui_", .y), r6 = .x)
        mod_saturation_clean_server(paste0("saturation_clean_ui_", .y),
          r6 = .x,
          seuil_saturation = input$seuil_saturation,
          nb_heures_journalieres_saturation = input$nb_heures_journalieres_saturation,
          nb_jours_hebdo_saturation = input$nb_jours_hebdo_saturation,
          parkings_list = parkings_list
        )
        mod_saturation_graphe_server(paste0("saturation_graphe_ui_", .y), r6 = .x, app_theme = app_theme, parkings_list = parkings_list)
      })


      output$my_Saturation_UI <- renderUI({
        lapply(names(list_of_Saturation), function(.y) {
          tagList(
            mod_saturation_graphe_ui(ns(paste0("saturation_graphe_ui_", .y)), title = camel(remove_underscore(.y))),
            tags$br(), tags$br()
          )
        })
      })
    })
  })
}

## To be copied in the UI
# mod_saturation_ui("saturation_ui_1")

## To be copied in the server
# mod_saturation_server("saturation_ui_1")
