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
    div("Ci-dessous des vid\u00e9os tuto pour prendre en main l\'application :"),
    tags$a(href = "https://www.loom.com/share/c5076fee3d654a46bcfb9cfe2314f1d7?sharedAppSource=personal_library", target = "_blank", "Pr\u00e9sentation de l\'appli"),
    br(),
    tags$a(href = "https://www.loom.com/share/ca777fde7545454bb025d571f4e49226", target = "_blank", "Onglet Occupation - observer 1 p\u00e9riode"),
    br(),
    tags$a(href = "https://www.loom.com/share/fb13dd94385f431e8e1999fcf2afccd3", target = "_blank", "Onglet Occupation - comparer 2 p\u00e9riodes"),
    br(),
    tags$a(href = "https://www.loom.com/share/b457c0e8f8854ebd8c6ec1905153028b?sharedAppSource=personal_library", target = "_blank", "Onglet Saturation"),
    br()
  )
}

#' accueil Server Functions
#'
#' @noRd
mod_accueil_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # observe(browser())
    
    output$studied_parkings <- renderDT(server = FALSE, {
      pkg <- parkings[, c("localisation_parking", "ident", "nom")]
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
