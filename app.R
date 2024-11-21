################################################################
# Shiny app by Mason Padgett
# Nov 4, 2024
#
# Creates Grouped Bar Charts by State
#
# Deployed at https://masonpadgett.shinyapps.io/Assignment10/
# Source code at GitHub: https://github.com/masonpadgett/STAT-442-Assignent-10/tree/main
################################################################

# This is a test
# This is another test
# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(shiny)

accident <- read_csv("accident.csv")
accident <- subset(accident, HOUR <= 24)
accident$DAY_WEEKNAME <- factor(accident$DAY_WEEKNAME, 
                                levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                           "Thursday", "Friday", "Saturday"))
accident$MONTHNAME <- factor(accident$MONTHNAME, 
                             levels = c("January", "February", "March", "April", 
                                        "May", "June", "July", "August", 
                                        "September", "October", "November", "December"))

# Summarize data by state and year for plotting
state_summary <- accident %>%
  group_by(STATENAME, YEAR) %>%
  summarise(total_accidents = n(), .groups = 'drop')


# Shiny UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Accidents Breakdown by Day of Week or Month and Factor"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Select State
      selectInput("state", "Select State:", 
                  choices = c("All States", unique(accident$STATENAME)), 
                  selected = "All States"),
      # Select Factor
      selectInput("factor", "Select Breakdown Factor:", 
                  choices = c("Time of Day" = "HOUR",
                              "Rural or Urban" = "RUR_URBNAME",
                              "Weather" = "WEATHERNAME",
                              "Lighting" = "LGT_CONDNAME")),
      # Select Day of Week or Month
      radioButtons("timePeriod", "Select Time Period:",
                   choices = c("Day of the Week" = "DAY_WEEKNAME", "Month" = "MONTHNAME"))
    ),
    
    # Show the grouped bar plot
    mainPanel(
      plotOutput("groupedBarPlot")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  output$groupedBarPlot <- renderPlot({
    # Conditionally filter data by state
    if (input$state == "All States") {
      plot_data <- accident %>%
        group_by(.data[[input$timePeriod]], .data[[input$factor]]) %>%
        summarise(accident_count = n(), .groups = 'drop')
    } else {
      plot_data <- accident %>%
        filter(STATENAME == input$state) %>%
        group_by(.data[[input$timePeriod]], .data[[input$factor]]) %>%
        summarise(accident_count = n(), .groups = 'drop')
    }
    
    # Exclude clear weather if weather is selected as the factor
    if (input$factor == "WEATHERNAME") {
      plot_data <- plot_data %>% filter(.data[[input$factor]] != "Clear")
    }
    
    # Plot a grouped bar chart
    ggplot(plot_data, aes(x = .data[[input$timePeriod]], y = accident_count, fill = as.factor(.data[[input$factor]]))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
      labs(title = if (input$state == "All States") {
        "Accidents in the US by Selected Time Period and Factor"
      } else {
        paste("Accidents in", input$state, "by Selected Time Period and Factor")
      },
      x = if (input$timePeriod == "DAY_WEEKNAME") {
        "Day of the Week"
      } else {
        "Month"
      }, 
      y = "Number of Accidents", fill = input$factor) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
