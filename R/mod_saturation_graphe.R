#' saturation_graphe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @import R6
#' @importFrom ggiraph renderGirafe girafeOutput girafe opts_sizing opts_tooltip opts_hover
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT datatable
#' @importFrom shinybm hidden_div
#' @importFrom grDevices dev.off tiff

mod_saturation_graphe_ui <- function(id, title) {
  ns <- NS(id)
  tagList(
    fluidRow(
      span(
        h4(title),
        actionButton(inputId = ns("show_hide_panel"), label = "afficher / masquer le secteur", class = "btn btn-info", style = "margin: 0 0 5% 0"),
        actionButton(ns("pause"),"Pause")
      )
    ),
    div(
      id = ns("show_results"),
      fluidRow(
        column(
          width = 6,
          selectizeInput(ns("selected_satured_parking1"), label = "Choisir un parking \u00e0 afficher", choices = NULL),
          withSpinner(
            girafeOutput(ns("plot"))
          ),
          tags$div(
            downloadButton(outputId = ns("down"), label = "T\u00e9l\u00e9charger le graphique", class = "btn btn-warning", style = "margin: 0 0 5% 0")
          )
        ),
        column(
          width = 6,
          selectizeInput(ns("selected_satured_parking2"), label = "Choisir un parking \u00e0 afficher", choices = NULL),
          withSpinner(
            girafeOutput(ns("plot2"))
          ),
          tags$div(
            downloadButton(outputId = ns("down2"), label = "T\u00e9l\u00e9charger le graphique", class = "btn btn-warning", style = "margin: 0 0 5% 0")
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          hidden_div(id_div = ns("div_bouton_affichage_plot_data"),
                     contenu_div = 
                       tagList(actionButton(inputId = ns("show_plot_data"), label = "Afficher / masquer les donn\u00e9es du graphe", class = "btn btn-warning", style = "margin: 0 0 1em 0")
                       )
          ),
          hidden_div(
            id_div = ns("plot_data"),
            contenu_div = tagList(
              withSpinner(
                DTOutput(ns("table_plot"))
              )
            )
          )
        ),
      ),
      #),
      fluidRow(
        column(
          width = 12,
          actionButton(inputId = ns("show_raw_data"), label = "Afficher / masquer les donn\u00e9es de la requ\u00eate", class = "btn btn-warning", style = "margin: 0 0 1em 0"),
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
  )
}

#' saturation_graphe Server Functions
#'
#' @noRd
mod_saturation_graphe_server <- function(id, r6, app_theme, parkings_list) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$pause, browser())
    
    observeEvent(input$show_hide_panel, {
      toggle(id = "show_results", anim = TRUE)
    })
    
    observe({
      updateSelectizeInput(session, "selected_satured_parking1",
                           choices = unique(parkings_list()$nom[parkings_list()$ident %in% r6$parkings_satures$ident]),
                           selected = unique(parkings_list()$nom[parkings_list()$ident %in% r6$parkings_satures$ident])[1],
                           server = TRUE
      )
      updateSelectizeInput(session, "selected_satured_parking2",
                           choices = unique(parkings_list()$nom[parkings_list()$ident %in% r6$parkings_satures$ident]),
                           selected = unique(parkings_list()$nom[parkings_list()$ident %in% r6$parkings_satures$ident])[1],
                           server = TRUE
      )
    })
    
    girafe_sizing <- reactiveValues()
    
    observe({
      if (r6$aggregation_unit != "Semaine") {
        # si on a un graphe restitué au mois
        girafe_sizing$width_svg <- 10
        girafe_sizing$height_svg <- 9
      } else {
        girafe_sizing$width_svg <- 8
        girafe_sizing$height_svg <- 5
      }
    })
    
    
    ### GRAPHE
    
    # Creation d'une reactive pour le graphique
    graphique1 <- reactive({
      req(isTruthy(r6$data_xtradata))
      req(nrow(r6$parkings_satures) > 0)
      
      gg <- r6$calendar_heatmap(
        selected_parking = unique(parkings_list()$ident[parkings_list()$nom %in% input$selected_satured_parking1]),
        app_theme = app_theme()
      )
      
      gg
      
    })
    
    graphique2 <- reactive({
      req(isTruthy(r6$data_xtradata))
      req(nrow(r6$parkings_satures) > 0)
      
      gg <- r6$calendar_heatmap(
        selected_parking = unique(parkings_list()$ident[parkings_list()$nom %in% input$selected_satured_parking2]),
        app_theme = app_theme()
      )
      
      gg
      
    })
    
    # Affichage du graphe
    output$plot <- renderGirafe({
      validate(
        need(isTruthy(r6$data_xtradata), "Aucun graphe \u00e0 afficher - v\u00e9rifier la requ\u00eate"),
        need(nrow(r6$parkings_satures) > 0, "Aucun parking ne remplit les crit\u00e8res d\u00e9finis")
      )
      
      observe(
        if(nrow(r6$parkings_satures) >0) {
          shinyjs::showElement("div_bouton_affichage_plot_data")
        } else {
          shinyjs::hideElement("div_bouton_affichage_plot_data")
        }
        
      )
      
      x <- girafe(
        ggobj = graphique1(), width_svg = girafe_sizing$width_svg, height_svg = girafe_sizing$height_svg,
        pointsize = 15,
        options = list(
          opts_hover(css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;")
        )
      )
      x
    })
    
    
    output$plot2 <- renderGirafe({
      validate(
        need(isTruthy(r6$data_xtradata), "Aucun graphe \u00e0 afficher - v\u00e9rifier la requ\u00eate"),
        need(nrow(r6$parkings_satures) > 0, "Aucun parking ne remplit les crit\u00e8res d\u00e9finis")
      )
      
      x <- girafe(
        ggobj = graphique2(), width_svg = girafe_sizing$width_svg, height_svg = girafe_sizing$height_svg,
        pointsize = 15,
        options = list(
          opts_hover(css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;")
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
        print(graphique1())
        dev.off() 
      } 
    )
    
    output$down2 <- downloadHandler(
      filename =  function() {
        "graphique.tiff"
      },
      content = function(file) {
        tiff(file, units="in", width=8, height=5, res=300)
        print(graphique2())
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
      validate(
        need(isTruthy(r6$data_xtradata), "Aucun tableau \u00e0 afficher - v\u00e9rifier la requ\u00eate")
      )
      
      r6$data_plot %>%
        .[, `:=`(
          taux_occupation = round(taux_occupation, 1),
          time = as.character(time)
        )] %>%
        .[, list(time, ident, nom, taux_occupation)] %>%
        .[, etat := NULL] %>%
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
        datatable(.,
                  rownames = FALSE, caption = NULL,
                  extensions = "Buttons", options = parametres_output_DT
        )
    })
  })
}

## To be copied in the UI
# mod_saturation_graphe_ui("saturation_graphe_ui_1")

## To be copied in the server
# mod_saturation_graphe_server("saturation_graphe_ui_1")
