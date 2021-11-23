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
  
  data("parkings")
  
  # MAJ de la liste des parkings au demarrage
  parkings_list <- xtradata_requete_features(
    key = Sys.getenv("XTRADATA_KEY"),
    typename = "ST_PARK_P",
    filter = list("connecte" = 1),
    attributes = list("ident", "nom")
  ) %>%
    setDT() %>%
    .[, type := NULL] %>%
    .[order(nom)]
  
  parkings_list <- unique(
    as.data.table(
      rbind(
        parkings_list,
        parkings[,c("ident","nom")]
      )
    ),
    by = "ident"
  )
  
  
  # Appel des modules #
  mod_occupation_1_periode_server("occupation_ui_1", app_theme = reactive(rv$theme), parkings_list = reactive(parkings_list))
  mod_occupation_2_periodes_server("occupation_ui_2", app_theme = reactive(rv$theme), parkings_list = reactive(parkings_list))
  mod_saturation_server("saturation_ui_1", app_theme = reactive(rv$theme), parkings_list = reactive(parkings_list))
  mod_accueil_server("accueil_ui_1")
  
  ### PARTIE BDXMETROIDENTITY ###
  rv <- reactiveValues()
  rv$theme <- "light"
  
  observeEvent(session,
               {
                 init_cookie_theme(input$dark_mode)
               },
               once = TRUE
  )
  
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
}
