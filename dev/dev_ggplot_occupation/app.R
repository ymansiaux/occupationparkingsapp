#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           actionButton(inputId = "maj", "maj graph"),
           selectizeInput(inputId = "selected_parkings", label = "pkg selec", choices = parkings$ident[parkings$parc_relais == TRUE], multiple = TRUE,
                          options = list(maxItems = 5, placeholder = "Choisir au max 10 pkgs",highlight="true", deselectBehavior = "top")),
           textOutput("pkg_selectionnes"),
           actionButton(inputId = "stop", "stop")
           
        ),

        # Show a plot of the generated distribution
        mainPanel(
            ggiraph::girafeOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    pkgload::load_all()
    library(ggiraph)
    library(ggplot2)
    observeEvent(input$stop,browser())
    xtradata_parameters <- occupation_compute_xtradata_request_parameters(selected_timestep = "Jour", selected_date = Sys.Date()-1)
    parc_relais <- Occupation$new(rangeStart = xtradata_parameters$rangeStart,
                                  rangeEnd = xtradata_parameters$rangeEnd,
                                  rangeStep = xtradata_parameters$rangeStep,
                                  localisation_parking = NA,
                                  parc_relais = TRUE)
    

    
    ## voir pour isolate
    ## select input des parkings (limite à 10)
    ## select_all et deselect_all (future fonction shinybm ?)
    output$pkg_selectionnes <- renderText(input$selected_parkings)
    
    output$distPlot <- renderGirafe({
        # generate bins based on input$bins from ui.R

        # draw the histogram with the specified number of bins
        # hist(rnorm(10), col = 'darkgray', border = 'white')
        parc_relais$download_data(rangeStep = "hour")
        parc_relais$clean_output()
        parc_relais$mean_by_some_time_unit(time_unit = "hour")
        # parc_relais$timeseries_plot()
        input$maj
        
    
        
        data_plot <-  parc_relais$data_xtradata %>% mutate.(tooltip = as.character(glue::glue("Date : {.[,time]}\nIdent : {.[,ident]}\nVal : {.[,taux_occupation]}")))
        
        gg <- filter.(data_plot, ident %in% input$selected_parkings) %>%  
        ggplot(data = isolate(.), mapping = aes(x = time, y = taux_occupation, color = ident, group=ident)) + 
            geom_line_interactive(aes(tooltip=tooltip, data_id=ident)) + 
            scale_color_viridis_d() +
            theme_minimal() +
            theme(legend.position = "bottom")
            
        x <- girafe(ggobj = gg, width_svg = 8, height_svg = 6,
                    options = list(
                        opts_hover_inv(css = "opacity:0.1;"),
                        opts_hover(css = "stroke-width:2;")
                    ))
        x
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


iris2 %>% 
    filter(Species != "setosa") %>% 
    ggplot(data = ., aes(Sepal.Length, Sepal.Width, group = Species, color = Species, linetype = Species)) +
    geom_line() +
    geom_line(data = filter(iris2, Species == "setosa"), aes(Sepal.Length, Sepal.Width, lwd = 2))  +
    scale_color_manual(
        "Legend",
        values=c("setosa"="black", "virginica"="blue", "versicolor"="orange")
    ) +
    scale_linetype_manual(
        "Legend",
        values=c("setosa"="dashed", "virginica"="solid", "versicolor"="solid")
    )
# voir guides 
# https://ggplot2.tidyverse.org/reference/guides.html

iris %>% 
    filter(Species != "setosa") %>% 
    ggplot(data = ., aes(Sepal.Length, Sepal.Width, group = Species, color = Species, linetype = Species)) +
    geom_line() +
    geom_line(data = filter(iris, Species == "setosa"), aes(Sepal.Length, Sepal.Width), lwd = 1.1)  +
    # scale_color_manual(
    #     "Legend",
    #     values=c("setosa"="black", "virginica"="blue", "versicolor"="orange")
    # ) +
     scale_linetype_manual(
         "Species",
         values=c("setosa"="dashed", "virginica"="solid", "versicolor"="solid")
     ) +
guides(size = "none")
