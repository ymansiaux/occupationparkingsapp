#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import tidytable
#' @import R6
#' @importFrom shinybm closeWaiter_logoDatalab
#' @importFrom golem app_prod
#' @importFrom data.table :=


#' @noRd
app_server <- function(input, output, session) {
  
  options(datatable.print.class = TRUE)
  options(bitmapType='cairo')

  observe(closeWaiter_logoDatalab(golem::app_prod()))

  mod_occupation_1_periode_server("occupation_ui_1")
  mod_occupation_2_periodes_server("occupation_ui_2")
  mod_saturation_server("saturation_ui_1")
}

