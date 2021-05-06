#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    waiter_fun(golem::app_prod()),
    navbarPage(title = div(img(src = "www/LogoDataLab.png", height = "35px", width = "35px"), "Kanot",
      style = "position:absolute;left:0%; top:20%;"
    )),
    tabPanel(
      title = "Acc\u00e8s aux donn\u00e9es",
      tabPanel(
        "Visualisation"
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

waiter_fun <- function(isinProd) {
  if (isinProd) {
    shiny::tagList(
      waiter::use_waiter(),
      waiter::waiter_show_on_load(html = img(src = "www/LogoDataLab.png"))
    )
  }
}
