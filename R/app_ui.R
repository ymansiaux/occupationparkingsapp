#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinybm waiter_logoDatalab title_with_logoDatalab 
#' @noRd
#' 

app_ui <- function(request) {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    waiter_logoDatalab(golem::app_prod()),
    fluidPage(
      navbarPage(title = title_with_logoDatalab(main_title = "Coucou"),
                 id = "navbarpage",
                 collapsible = TRUE,
                 tabPanel("Tab1",
                          mod_occupation_ui("occupation_ui_1")),
                 tabPanel("Tab2"),
                 tabPanel("Tab3")
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
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "kanotapp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
