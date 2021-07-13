#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bdxmetroidentity
#' @importFrom shinyjs useShinyjs
#' @importFrom shinybm waiter_logoDatalab title_with_logoDatalab 
#' @importFrom shinybusy add_busy_spinner
#' @noRd
#' 

app_ui <- function(request) {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    waiter_logoDatalab(golem::app_prod()),
    add_busy_spinner(spin = "fading-circle", color = "#ff4d3e", height = "150px", width = "150px"),
    
    fluidPage(
      navbarpage_bdx(title = "Parkings",
                     collapsible = TRUE,
                     tabPanel("Accueil",
                              titlePanel("Bienvenue"),
                              uiOutput(outputId = "my_logo"),
                              h5("Blabla à compléter")),
                     tabPanel("Occupation - observer 1 p\u00e9riode",
                              mod_occupation_1_periode_ui("occupation_ui_1")),
                     tabPanel("Occupation - comparer 2 p\u00e9riodes",
                              mod_occupation_2_periodes_ui("occupation_ui_2")),
                     tabPanel("Saturation",
                              mod_saturation_ui("saturation_ui_1"))
      )
    )
    
  )
  
  
  
  
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )
  
  golem::use_internal_file(path = system.file("shiny/www", "datalab-logo-lightmode.png", package = "bdxmetroidentity"))
  golem::use_internal_file(path = system.file("shiny/www", "datalab-logo-darkmode.png", package = "bdxmetroidentity"))
  
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "occupationsparkingsapp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
