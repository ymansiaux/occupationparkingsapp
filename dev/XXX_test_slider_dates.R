

library(shiny)
library(shinyWidgets)
library(clock)


x <- date_build(2020, 1, 31)
x <- add_months(x, 1:12, invalid = "previous")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Sélection d'un jour
      dateInput("select_day", "day:", value = "2012-02-29", autoclose = FALSE, weekstart = 1),
      
      # Sélection d'une semaine
      dateInput("select_week", "week:", daysofweekdisabled = c(0,2:6), autoclose = FALSE, weekstart = 1),
      
      sliderTextInput(
        inputId    = "select_month",
        label      = "Mois",
        choices    = unique(date_format(x, format = "%Y %m")),
        selected   = date_format(min(x), format = "%Y %m"),
        grid       = TRUE,
        width      = "100%"
      ),
      
      
      radioButtons("select_year", "year",
                   choices = unique(get_year(x)))
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput(("jour")),
      textOutput(("semaine")),
      textOutput(("mois")),
      textOutput(("annee"))
      
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$jour <- renderText({
input$select_day
    })
  output$semaine <- renderText({
    input$select_week
  })
  output$mois <- renderText({
    input$select_month
  })
  
  output$annee <- renderText({
    input$select_year
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
