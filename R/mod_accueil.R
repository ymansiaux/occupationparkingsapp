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
mod_accueil_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    div("Bienvenue dans cet outil de suivi de l'occupation et de la saturation des parkings hors voirie de Bordeaux Métropole"),
    br(),
    div("Les parkings étudiés sont groupés en 4 secteurs : Parc Relais, Hypercentre, Centre et Périphérie"),
    br(),
    lien_afficher_cacher_div(id_lien = ns("show_parkings_list"), 
                             label_lien = "Cliquer pour afficher les parkings étudiés", 
                             id_div = ns("parkings_list"), 
                             contenu_div = tagList(
                               br(),
                               DTOutput(ns("studied_parkings")),
                               br(), 
                               br()
                             )
    ),
    div("Les données sources sont issues du portail Open Data de Bordeaux Métropole")
    
  )
}

#' accueil Server Functions
#'
#' @noRd 
mod_accueil_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # observe(browser())
    
    output$studied_parkings <- renderDT(server = FALSE, {
      
      pkg <- parkings[, c("localisation_parking", "ident", "nom")]
      pkg[is.na(pkg$localisation_parking), "localisation_parking"] <- "parc relais"
      colnames(pkg) <- c("localisation", "identifiant", "nom")  
      
      datatable(pkg)
      
    })
    
    onclick("show_parkings_list",
            toggle(id = "parkings_list", anim = TRUE))
    
    
  })
}

## To be copied in the UI
# mod_accueil_ui("accueil_ui_1")

## To be copied in the server
# mod_accueil_server("accueil_ui_1")
