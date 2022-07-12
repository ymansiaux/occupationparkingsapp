#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import R6
#' @importFrom shinybm closeWaiter_logoDatalab
#' @importFrom golem app_prod
#' @import data.table
#' @importFrom xtradata xtradata_requete_features


#' @noRd
app_server <- function(input, output, session) {
  options(datatable.print.class = TRUE)
  options(bitmapType = "cairo")
  
  observe(closeWaiter_logoDatalab(golem::app_prod()))

  rv <- reactiveValues()
  rv$theme <- "light"
  rv$parkings_list_is_updated <- FALSE
  rv$list_of_Occupation_1periode <- NULL
  rv$list_of_Occupation_2periodes_1 <- NULL
  rv$list_of_Occupation_2periodes_2 <- NULL
  
  observeEvent(session,
               {
                 init_cookie_theme(input$dark_mode)
                 
                 # Au lancement de l'appli on met à jour la liste des parkings (parfois les noms changent ...)
                 parkings_names <- 
                   xtradata_requete_features(
                     key = Sys.getenv("XTRADATA_KEY"),
                     typename = "ST_PARK_P",
                     attributes = list("ident", "nom")
                   ) %>%
                   setDT() %>%
                   .[, type := NULL] %>%
                   .[order(nom)]
                 
                 rv$parkings <- merge(parkings, parkings_names, by = "ident") %>% as.data.table()
                 rv$parkings_list_is_updated <- TRUE
                 
                 #############################################################################
                 # Création de la liste des objets "Occupation" pour l'analyse d'une période #
                 #############################################################################
                 
                 rv$list_of_Occupation_1periode <- list(
                   parc_relais = Occupation$new(parkings_list = rv$parkings[localisation_parking %in% NA & parc_relais == TRUE][["ident"]]),
                   hypercentre = Occupation$new(parkings_list = rv$parkings[localisation_parking %in% "hypercentre" & parc_relais == FALSE][["ident"]]),
                   centre = Occupation$new(parkings_list = rv$parkings[localisation_parking %in% "centre" & parc_relais == FALSE][["ident"]]),
                   peripherie = Occupation$new(parkings_list = rv$parkings[localisation_parking %in% "peripherie" & parc_relais == FALSE][["ident"]]),
                   selection_personnalisee = Occupation$new(parkings_list = NULL)
                 )
                 
                 # On appelle memoise pour activer le cache sur les resultats
                 rv$list_of_Occupation_1periode <- lapply(rv$list_of_Occupation_1periode, function(.l) {
                   .l$download_data_memoise <- memoise(.l$download_data)
                   .l
                 })
                 
                 #############################################################################
                 # Création de la liste des objets "Occupation" pour l'analyse de 2 périodes #
                 #############################################################################
                 
                 rv$list_of_Occupation_2periodes_1 <- list(
                   parc_relais = Occupation$new(parkings_list = rv$parkings[localisation_parking %in% NA & parc_relais == TRUE][["ident"]]),
                   hypercentre = Occupation$new(parkings_list = rv$parkings[localisation_parking %in% "hypercentre" & parc_relais == FALSE][["ident"]]),
                   centre = Occupation$new(parkings_list = rv$parkings[localisation_parking %in% "centre" & parc_relais == FALSE][["ident"]]),
                   peripherie = Occupation$new(parkings_list = rv$parkings[localisation_parking %in% "peripherie" & parc_relais == FALSE][["ident"]]),
                   selection_personnalisee = Occupation$new(parkings_list = NULL)
                 )
                 
                 rv$list_of_Occupation_2periodes_2 <- list(
                   parc_relais2 = Occupation$new(parkings_list = rv$parkings[localisation_parking %in% NA & parc_relais == TRUE][["ident"]]),
                   hypercentre2 = Occupation$new(parkings_list = rv$parkings[localisation_parking %in% "hypercentre" & parc_relais == FALSE][["ident"]]),
                   centre2 = Occupation$new(parkings_list = rv$parkings[localisation_parking %in% "centre" & parc_relais == FALSE][["ident"]]),
                   peripherie2 = Occupation$new(parkings_list = rv$parkings[localisation_parking %in% "peripherie" & parc_relais == FALSE][["ident"]]),
                   selection_personnalisee = Occupation$new(parkings_list = NULL)
                 )
                 
                 rv$list_of_Occupation_2periodes_1 <- lapply(rv$list_of_Occupation_2periodes_1, function(.l) {
                   .l$download_data_memoise <- memoise(.l$download_data)
                   .l
                 })
                 
                 rv$list_of_Occupation_2periodes_2  <- lapply(rv$list_of_Occupation_2periodes_2, function(.l) {
                   .l$download_data_memoise <- memoise(.l$download_data)
                   .l
                 })
                 
               },
               once = TRUE
  )
  
  
  ##########################
  # Dark mode / Light mode #
  ##########################
  
  observeEvent(input$dark_mode,
               {
                 change_theme(input$dark_mode)
                 
                 # theme to be used for the graphs
                 if (isTRUE(input$dark_mode)) {
                   rv$theme <- "dark"
                 } else {
                   rv$theme <- "light"
                 }
               },
               ignoreInit = TRUE
  )
  
  output$my_logo <- renderUI({
    if (rv$theme == "light") {
      tags$img(src = "www/datalab-logo-lightmode.png", width = "150px")
    } else if (rv$theme == "dark") {
      tags$img(src = "www/datalab-logo-darkmode.png", width = "150px")
    }
  })
  
  
  #####################
  # Appel des modules #
  #####################
  
  observeEvent(rv$parkings_list_is_updated, {
    if(rv$parkings_list_is_updated == TRUE) {
      
      mod_occupation_1_periode_server("occupation_ui_1", 
                                      app_theme = reactive(rv$theme), 
                                      parkings = rv$parkings, 
                                      list_of_Occupation = rv$list_of_Occupation_1periode)
      
      mod_occupation_2_periodes_server("occupation_ui_2", 
                                       app_theme = reactive(rv$theme),
                                       parkings = rv$parkings, 
                                       list_of_Occupation1 = rv$list_of_Occupation_2periodes_1,
                                       list_of_Occupation2 = rv$list_of_Occupation_2periodes_2)
      
      mod_saturation_server("saturation_ui_1",
                            app_theme = reactive(rv$theme),
                            parkings = rv$parkings)
      
      mod_accueil_server("accueil_ui_1",
                         parkings = reactive(rv$parkings))
    }
    
  })
  
}
