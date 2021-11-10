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
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom ggiraph renderGirafe girafeOutput girafe  opts_hover_inv opts_sizing opts_hover
#' @importFrom shinybm hidden_div lien_afficher_cacher_div
#' @importFrom shinyjs show hide onclick toggle
#' @importFrom shinycssloaders withSpinner
#' @importFrom grDevices dev.off tiff

mod_occupation_1_periode_graphe_ui <- function(id, title) {
  ns <- NS(id)
  tagList(
    fluidRow(
      span(
        h4(title),
        actionButton(inputId = ns("show_hide_panel"), label = "afficher / masquer le secteur", class = "btn btn-info", style = "margin: 0 0 5% 0")
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
          # ,actionButton(inputId = ns("pause"), "pause")
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
          tags$div(
            actionButton(inputId = ns("maj"), "MAJ graphes et tableaux", style = "margin: 0 0 5% 0")
          ),
          tags$div(
            downloadButton(outputId = ns("down"), label = "T\u00e9l\u00e9charger le graphique", class = "btn btn-warning", style = "margin: 0 0 5% 0")
          )
        )
      ),
      fluidRow(
        tags$span(
          actionButton(inputId = ns("show_plot_data"), label = "Afficher / masquer les donn\u00e9es du graphe", class = "btn btn-warning", style = "margin: 0 0 5% 0"),
          actionButton(inputId = ns("show_raw_data"), label = "Afficher / masquer les donn\u00e9es de la requ\u00eate", class = "btn btn-warning", style = "margin: 0 0 5% 0")
        )
      ),
      fluidRow(
        column(
          width = 12,
          hidden_div(
            id_div = ns("plot_data"),
            contenu_div = tagList(
              withSpinner(
                DTOutput(ns("table_plot"))
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        hidden_div(
          id_div = ns("raw_data"),
          contenu_div = tagList(
            tagList(
              withSpinner(
                DTOutput(ns("table_raw"))
              )
            )
          )
        )
      )
    )
  )
}

#' occupation_graphe Server Functions
#'
#' @noRd
mod_occupation_1_periode_graphe_server <- function(id, r6, app_theme, parkings_list) {
  moduleServer(id, function(input, output, session) {
    observe(updateSelectizeInput(session, "parkings_to_plot", choices = unique(r6$cleaned_data$nom), server = TRUE))
    # observeEvent(input$pause, browser())
    
    observeEvent(input$show_hide_panel, {
      toggle(id = "show_results", anim = TRUE)
    })
    
    
    ### GRAPHE
    
    # Creation d'une reactive pour le graphique
    graphique <- reactive({
      
      req(isTruthy(r6$data_xtradata))
      
      input$maj
      
      r6$aggregated_data_by_some_time_unit$nom[is.na(r6$aggregated_data_by_some_time_unit$nom)] <- "moyenne"
      
      gg <- r6$timeseries_plot_1_period(
        parkings_to_plot = isolate(unique(parkings_list()$ident[parkings_list()$nom %in% input$parkings_to_plot])),
        aggregation_unit = r6$aggregation_unit,
        app_theme = app_theme()
      )
      
      gg
      
    })
    
    # Affichage du graphe
    output$plot <- renderGirafe({
      
      validate(
        need(isTruthy(r6$data_xtradata), "Aucun graphe \u00e0 afficher - v\u00e9rifier la requ\u00eate")
      )
      
      x <- girafe(
        ggobj = graphique(), width_svg = 8, height_svg = 5,
        pointsize = 15,
        options = list(
          opts_hover_inv(css = "opacity:0.1;"),
          opts_hover(css = "stroke-width:2;"),
          opts_toolbar(saveaspng = FALSE)
        )
      )
      x
    })
    
    # Telechargement du graphe
    output$down <- downloadHandler(
      filename =  function() {
        "graphique.tiff"
      },
      content = function(file) {
        tiff(file, units="in", width=8, height=5, res=300)
        print(graphique())
        dev.off() 
      } 
    )
    
    
    ### TABLEAU
    onclick(
      "show_plot_data",
      toggle(id = "plot_data", anim = TRUE)
    )
    
    onclick(
      "show_raw_data",
      toggle(id = "raw_data", anim = TRUE)
    )
    
    
    
    output$table_plot <- renderDT(server = FALSE, {
      input$maj
      
      validate(
        need(isTruthy(r6$data_xtradata), "Aucun tableau \u00e0 afficher - v\u00e9rifier la requ\u00eate")
      )
      
      r6$data_plot_1_period %>%
        .[, `:=`(
          taux_occupation = round(taux_occupation, 1),
          time = as.character(time)
        )] %>%
        .[, tooltip := NULL] %>%
        .[, linetype := NULL] %>%
        .[, lwd := NULL] %>%
        datatable(.,
                  rownames = FALSE, caption = NULL,
                  extensions = "Buttons", options = parametres_output_DT
        )
    })
    
    output$table_raw <- renderDT(server = FALSE, {
      validate(
        need(isTruthy(r6$data_xtradata), "Aucun tableau \u00e0 afficher - v\u00e9rifier la requ\u00eate")
      )
      
      r6$cleaned_data %>%
        .[, `:=`(
          taux_occupation = round(taux_occupation, 1),
          time = as.character(time)
        )] %>%
        .[, etat := NULL] %>%
        datatable(., extensions = "Buttons", options = parametres_output_DT)
    })
  })
}

## isolate dans le graphe et bouton MAJ parking

## To be copied in the UI
# mod_occupation_1_periode_graphe_ui("occupation_graphe_ui_1")

## To be copied in the server
# mod_occupation_1_periode_graphe_server("occupation_graphe_ui_1")
