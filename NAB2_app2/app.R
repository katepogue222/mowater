#
## packages
library(shiny)
library(ggplot2)
library(readr)



# data
setwd("/Users/katepogue/Desktop/MOWATER")
NAB2 <- read_csv("NAB2_app2/NAB2_copy.csv")

#login to shiny server
#rsconnect::setAccountInfo(name='ftk79j-kate-pogue',
#                          token='1480DE6D8C4FA9D51608EA1B3B267A1A',
#                          secret='2oN7sIcc7T5I/4duRg8Ycx78GvW6CsPsA8T0t0kv')

# Define UI
ui <- fluidPage(
  fluidRow(
    column(
      width = 6,
      selectInput(inputId = "variable1", label = "Select Variable 1", choices = colnames(NAB2)[24:38]),
      dateRangeInput(inputId = "timeframe1", label = "Select Timeframe 1", start = min(NAB2$timestamp), end = max(NAB2$timestamp))
    ),
    column(
      width = 6,
      selectInput(inputId = "variable2", label = "Select Variable 2", choices = colnames(NAB2)[24:38]),
      dateRangeInput(inputId = "timeframe2", label = "Select Timeframe 2", start = min(NAB2$timestamp), end = max(NAB2$timestamp))
    )
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput(outputId = "plot1")
    ),
    column(
      width = 6,
      plotOutput(outputId = "plot2")
    )
  )
)

# Define server
server <- function(input, output) {
  # Generate plot 1 based on user inputs
  output$plot1 <- renderPlot({
    # Filter the data based on selected variable and timeframe
    filtered_data <- subset(NAB2, timestamp >= input$timeframe1[1] & timestamp <= input$timeframe1[2])

    # Create the plot
    plot_data <- filtered_data[, c("timestamp", input$variable1)]
    plot_data$timestamp <- as.Date(plot_data$timestamp)
    plot <- ggplot(plot_data, aes(x = timestamp, y = .data[[input$variable1]], color = .data[[input$variable1]])) +
      geom_point(size = 3) +
      labs(x = "Timestamp", y = input$variable1) +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal()

    plot
  })




  # Generate plot 2 based on user inputs
  output$plot2 <- renderPlot({
    # Filter the data based on selected variable and timeframe
    filtered_data <- subset(NAB2, timestamp >= input$timeframe2[1] & timestamp <= input$timeframe2[2])

    # Create the plot
    plot_data <- filtered_data[, c("timestamp", input$variable2)]
    plot_data$timestamp <- as.Date(plot_data$timestamp)
    plot <- ggplot(plot_data, aes(x = timestamp, y = .data[[input$variable2]], color = .data[[input$variable2]])) +
      geom_point(size = 3) +
      labs(x = "Timestamp", y = input$variable2) +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal()

    plot
  })
}

# Run the app
shinyApp(ui, server)

#deploy

