#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinybm closeWaiter_logoDatalab
#' @importFrom golem app_prod
#' @noRd
app_server <- function( input, output, session ) {
  observe(closeWaiter_logoDatalab(golem::app_prod()))
  
}
