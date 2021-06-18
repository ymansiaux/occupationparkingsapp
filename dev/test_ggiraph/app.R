library(shiny)
library(ggplot2)
library(ggiraph)

gg_no_facet <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
    geom_tile_interactive(aes(fill = Petal.Length), colour = "white") +
    scale_fill_distiller(palette = "Spectral", direction = -1) +
    scale_y_continuous() +
    scale_x_continuous() +
    theme_minimal() + 
    theme(
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
    coord_equal()

gg_facet <- gg_no_facet +  facet_wrap(~ Species, ncol = 3, strip.position = "right")


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel( width = 2,
                      sliderInput("width_svg", "width_svg", min = 4, max = 12, value = 12),
                      sliderInput("height_svg", "height_svg", min = 4, max = 12, value = 4)
        ),
        
        mainPanel(width = 10,
                  fluidRow(
                      column(width =12,
                             h3("height svg et width svg à la main"),
                             girafeOutput("plot")
                      )
                  ),
                  fluidRow(
                      column(width =12,
                             h3("en utilisant opts_sizing(rescale = TRUE, width = 1)"),
                             girafeOutput("plot2")
                      )
                  ),
                  
                  fluidRow(
                      column(width =12,
                             h3("graphe simple sans facet"),
                             girafeOutput("plot3")
                      )
                  )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot <- renderGirafe({
        girafe(ggobj = gg_facet, width_svg = input$width_svg, height_svg = input$height_svg)
    })
    
    output$plot2 <- renderGirafe({
        girafe(ggobj = gg_facet, 
               options = list(
                   opts_sizing(rescale = TRUE, width = 1)
               ))
    })
    
    output$plot3 <- renderGirafe({
        girafe(ggobj = gg_no_facet, 
               options = list(
                   opts_sizing(rescale = TRUE, width = 1)
               ))
        
    })
    
}

shinyApp(ui = ui, server = server)
