library(shiny)
library(tidyverse)
library(nycflights13)

# Define UI for application that draws a plot of NYC flight data
ui <- fluidPage(# Application title
    titlePanel("NYC Flights"),
    
    # Sidebar with multiple UI options
    sidebarLayout(
        sidebarPanel(
            #Select which variable is plotted to the X axis
            selectInput(
                "x",
                "X axis",
                c(
                    "Arrival Time" = "arr_time",
                    "Departure Time" = "dep_time",
                    "Arrival Delay" = "arr_delay",
                    "Departure Delay" = "dep_delay",
                    "Air Time" = "air_time",
                    "Distance" = "distance",
                    "Month" = "month",
                    "Carrier" = "carrier",
                    "Origin" = "origin"
                )
            ),
            
            #Select which variable is plotted to the Y axis
            selectInput(
                "y",
                "Y axis",
                c(
                    "Arrival Time" = "arr_time",
                    "Departure Time" = "dep_time",
                    "Arrival Delay" = "arr_delay",
                    "Departure Delay" = "dep_delay",
                    "Air Time" = "air_time",
                    "Distance" = "distance",
                    "Month" = "month",
                    "Carrier" = "carrier",
                    "Origin" = "origin"
                )
            ),
            #Select how the graph is colored and grouped
            selectInput("color", "Color by:", c("NULL", "
                                            month",
                                            "origin",
                                            "carrier")),
            #Choose the type of plot used in the graph
            radioButtons(
                "plot",
                "Plot Type",
                c("boxplot", "scatter", "count", "Freq Poly")
            )
        ),
        
        #Plot the data
        mainPanel(plotOutput("flightPlot"))
    ))




# Define server logic required to draw plot
server <- function(input, output) {
    output$flightPlot <- renderPlot({
        #Plots different graph based on user input
        if (input$plot == "boxplot") {
            p <-
                ggplot(flights,
                       aes_string(
                           x = input$x,
                           y = input$y,
                           fill = input$color
                       )) +
                geom_boxplot()
        }
        if (input$plot == "scatter") {
            p <-
                ggplot(flights,
                       aes_string(
                           x = input$x,
                           y = input$y,
                           color = input$color
                       )) +
                geom_point()
        }
        if (input$plot == "count") {
            p <-
                ggplot(flights,
                       aes_string(
                           x = input$x,
                           y = input$y,
                           color = input$color
                       )) +
                geom_count()
        }
        if (input$plot == "Freq Poly") {
            p <- ggplot(flights,
                        aes_string(x = input$x, color = input$color)) +
                geom_freqpoly()
        }
        p
    })
}

# Run the application
shinyApp(ui = ui, server = server)
