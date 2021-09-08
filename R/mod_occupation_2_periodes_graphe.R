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
#' @importFrom ggiraph renderGirafe girafeOutput girafe  opts_hover_inv opts_sizing opts_hover
#' @importFrom shinybm hidden_div lien_afficher_cacher_div
#' @importFrom shinyjs show hide onclick toggle
#' @importFrom shinycssloaders withSpinner

mod_occupation_2_periodes_graphe_ui <- function(id, title) {
  ns <- NS(id)
  tagList(
    fluidRow(
      span(
        h4(title),
        actionButton(inputId = ns("show_hide_panel"), label = "afficher / masquer le secteur", class = "btn btn-info")
      )
    ),
    div(
      id = ns("show_results"),
      fluidRow(
        column(
          width = 8,
          withSpinner(
            girafeOutput(ns("plot"))
          )
        ),
        column(
          width = 4,
          selectizeInput(
            inputId = ns("parkings_to_plot"),
            label = "Parkings \u00e0 afficher",
            choices = NULL,
            multiple = TRUE,
            options = list(maxItems = 5, placeholder = "Choisir au max 5 pkgs", deselectBehavior = "top")
          ),
          actionButton(inputId = ns("maj"), "MAJ graphes et tableaux")
        )
      ),
      fluidRow(
        column(
          width = 12,
          lien_afficher_cacher_div(
            id_lien = ns("show_plot_data"),
            label_lien = "Afficher les donn\u00e9es du graphe",
            id_div = ns("plot_data"),
            contenu_div = tagList(
              withSpinner(
                DTOutput(ns("table_plot"))
              )
            )
          )
        )
      ),
      fluidRow(column(
        width = 12,
        lien_afficher_cacher_div(
          id_lien = ns("show_raw_data"),
          label_lien = "Afficher les donn\u00e9es brutes",
          id_div = ns("raw_data"),
          contenu_div = tagList(
            withSpinner(
              DTOutput(ns("table_raw"))
            )
          )
        )
      ))
    )
  )
}

#' occupation_graphe Server Functions
#'
#' @noRd
mod_occupation_2_periodes_graphe_server <- function(id, r6_1, r6_2, app_theme, parkings_list) {
  moduleServer(id, function(input, output, session) {
    observe(updateSelectizeInput(session, "parkings_to_plot", choices = unique(c(r6_1$cleaned_data$nom, r6_2$cleaned_data$nom)), server = TRUE))
    observeEvent(input$pause, browser())

    observeEvent(input$show_hide_panel, {
      toggle(id = "show_results", anim = TRUE)
    })

    output$plot <- renderGirafe({
      input$maj

      validate(
        need(isTruthy(r6_1$data_xtradata) & isTruthy(r6_2$data_xtradata), "Aucun graphe \u00e0 afficher - v\u00e9rifier la requ\u00eate")
      )
      r6_1$aggregated_data_by_some_time_unit$nom[is.na(r6_1$aggregated_data_by_some_time_unit$nom)] <- "moyenne"
      r6_2$aggregated_data_by_some_time_unit$nom[is.na(r6_2$aggregated_data_by_some_time_unit$nom)] <- "moyenne"

      gg <- r6_1$timeseries_plot_2_periods(
        data_occupation_1 = r6_1,
        data_occupation_2 = r6_2,
        timeStep = r6_1$timeStep,
        parkings_to_plot = isolate(unique(parkings_list()$ident[parkings_list()$nom %in% input$parkings_to_plot])),
        app_theme = app_theme()
      )

      x <- girafe(
        ggobj = gg, width_svg = 8, height_svg = 5,
        pointsize = 15,
        options = list(
          opts_hover_inv(css = "opacity:0.1;"),
          opts_hover(css = "stroke-width:2;")
        )
      )
      x
    })


    onclick(
      "show_plot_data",
      toggle(id = "plot_data", anim = TRUE)
    )

    onclick(
      "show_raw_data",
      toggle(id = "raw_data", anim = TRUE)
    )


    output$table_plot <- renderDT({
      input$maj

      validate(
        need(isTruthy(r6_1$data_xtradata) & isTruthy(r6_2$data_xtradata), "Aucun graphe \u00e0 afficher - v\u00e9rifier la requ\u00eate")
      )

      r6_1$data_plot_2_periods %>%
        .[, `:=`(
          taux_occupation = round(taux_occupation, 1),
          time = as.character(time)
        )] %>%
        .[, tooltip := NULL] %>%
        .[, linetype := NULL] %>%
        datatable(.,
          rownames = FALSE, caption = NULL,
          extensions = "Buttons", options = parametres_output_DT
        )
    })

    output$table_raw <- renderDT({
      validate(
        need(isTruthy(r6_1$data_xtradata) & isTruthy(r6_2$data_xtradata), "Aucun graphe \u00e0 afficher - v\u00e9rifier la requ\u00eate")
      )

      rbind(
        r6_1$cleaned_data %>%
          .[, `:=`(
            taux_occupation = round(taux_occupation, 1),
            time = as.character(time)
          )],
        r6_2$cleaned_data %>%
          .[, `:=`(
            taux_occupation = round(taux_occupation, 1),
            time = as.character(time)
          )]
      ) %>%
        .[, etat := NULL] %>%
        datatable(.,
          rownames = FALSE, caption = NULL,
          extensions = "Buttons", options = parametres_output_DT
        )
    })
  })
}

## To be copied in the UI
# mod_occupation_1_periode_graphe_ui("occupation_graphe_ui_1")

## To be copied in the server
# mod_occupation_1_periode_graphe_server("occupation_graphe_ui_1")
