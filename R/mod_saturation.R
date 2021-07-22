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
mod_saturation_server <- function(id, app_theme) {
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
      if (!isTruthy(input$selected_week)) {
        disable("run_query")
      } else {
        enable("run_query")
      }
    })




    observeEvent(input$run_query, {
      xtradata_parameters <- reactive(
        switch(input$timestep,
          "Semaine" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_week),
          "Mois" = occupation_compute_xtradata_request_parameters(selected_timestep = input$timestep, selected_date = input$selected_month)
        )
      )

      list_of_Saturation <- list(
        parc_relais = Saturation$new(
          rangeStart = xtradata_parameters()$rangeStart,
          rangeEnd = xtradata_parameters()$rangeEnd,
          rangeStep = "hour",
          plageHoraire = 0:23,
          timeStep = input$timestep,
          localisation_parking = NA,
          parc_relais = TRUE
        ),
        hypercentre = Saturation$new(
          rangeStart = xtradata_parameters()$rangeStart,
          rangeEnd = xtradata_parameters()$rangeEnd,
          rangeStep = "hour",
          plageHoraire = 0:23,
          timeStep = input$timestep,
          localisation_parking = "hypercentre",
          parc_relais = FALSE
        ),
        centre = Saturation$new(
          rangeStart = xtradata_parameters()$rangeStart,
          rangeEnd = xtradata_parameters()$rangeEnd,
          rangeStep = "hour",
          plageHoraire = 0:23,
          timeStep = input$timestep,
          localisation_parking = "centre",
          parc_relais = FALSE
        ),
        peripherie = Saturation$new(
          rangeStart = xtradata_parameters()$rangeStart,
          rangeEnd = xtradata_parameters()$rangeEnd,
          rangeStep = "hour",
          plageHoraire = 0:23,
          timeStep = input$timestep,
          localisation_parking = "peripherie",
          parc_relais = FALSE
        )
      )

      imap(list_of_Saturation, function(.x, .y) {
        mod_saturation_appel_WS_server(paste0("saturation_appel_WS_ui_", .y), r6 = .x)
        mod_saturation_clean_server(paste0("saturation_clean_ui_", .y),
          r6 = .x,
          seuil_saturation = input$seuil_saturation,
          nb_heures_journalieres_saturation = input$nb_heures_journalieres_saturation,
          nb_jours_hebdo_saturation = input$nb_jours_hebdo_saturation
        )
        mod_saturation_graphe_server(paste0("saturation_graphe_ui_", .y), r6 = .x, app_theme = app_theme)
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
