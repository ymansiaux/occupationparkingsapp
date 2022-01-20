#' accueil UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom shinyjs toggle onclick
mod_accueil_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    div("Bienvenue dans cet outil de suivi de l\'occupation et de la saturation des parkings hors voirie de Bordeaux M\u00e9tropole"),
    br(),
    div("Les parkings \u00e9tudi\u00e9s sont group\u00e9s en 4 secteurs : Parc Relais, Hypercentre, Centre et P\u00e9riph\u00e9rie"),
    br(),
    
    ## Afficher masquer la liste des parkings
    fluidRow(
      tags$span(
        actionButton(inputId = ns("show_parkings_list"), label = "Afficher / masquer les parkings \u00e9tudi\u00e9s", class = "btn btn-warning", style = "margin: 0 0 5% 0")
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        hidden_div(
          id_div = ns("parkings_list"),
          contenu_div = tagList(
            withSpinner(
              DTOutput(ns("studied_parkings"))
            )
          )
        )
      )
    ),
    
    br(),
    div("Les donn\u00e9es sources sont issues du portail Open Data de Bordeaux M\u00e9tropole"),
    br(), br(),
    div("Ci-dessous une vid\u00e9o tuto pour prendre en main l\'application :"),
    tags$a(href = "https://bdx.sharepoint.com/:v:/r/sites/MET-UVD/Documents%20partages/General/Vid%C3%A9os_Tutos/Datalab/Application_parkings/Video%20appli%20parking.avi?csf=1&web=1&e=Sebund", target = "_blank", "Pr\u00e9sentation de l\'appli"),
    br()
    
  )
}

#' accueil Server Functions
#'
#' @noRd
mod_accueil_server <- function(id, parkings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # observe(browser())
    
    output$studied_parkings <- renderDT(server = FALSE, {
      pkg <- parkings()[, c("localisation_parking", "ident", "nom")]
      pkg[is.na(pkg$localisation_parking), "localisation_parking"] <- "parc relais"
      colnames(pkg) <- c("localisation", "identifiant", "nom")
      
      datatable(pkg,
                rownames = FALSE, caption = NULL,
                extensions = "Buttons", options = parametres_output_DT)
    })
    
    onclick(
      "show_parkings_list",
      toggle(id = "parkings_list", anim = TRUE)
    )
  })
}

## To be copied in the UI
# mod_accueil_ui("accueil_ui_1")

## To be copied in the server
# mod_accueil_server("accueil_ui_1")
