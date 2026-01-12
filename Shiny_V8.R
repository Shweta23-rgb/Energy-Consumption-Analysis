library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(arrow)
library(readxl)  # Ensure this package is installed and loaded

# Load your datasets
Test_Optimied_Variables <- read_excel("/Users/USER/Desktop/IDS Files/Test_Optimised.xlsx")
Weather_Energy <- read_excel("/Users/USER/Desktop/IDS Files/Weather_Energy.xlsx")

# Extract column names and create a filtered list
colnames_Test_Optimied <- colnames(Test_Optimied_Variables)
exclude_variables <- c("Dry Bulb Temperature [°C]", "Relative Humidity [%]", 
                       "Wind Speed [m/s]", "hour", "in.county", 
                       "Direct Normal Radiation [W/m2]", "Wind Direction [Deg]", 
                       "Diffuse Horizontal Radiation [W/m2]", 
                       "Global Horizontal Radiation [W/m2]", "in.sqft")
columns_with_slider_names_filtered <- colnames_Test_Optimied[!colnames_Test_Optimied %in% exclude_variables]

# Custom CSS to change background color
customCSS <- "
body {
  background-color: #f2f2f2;
}
.tab-content:last-child {
  background-color: #f2f2f2;
}
"

# Shiny UI definition
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML(customCSS))),
  titlePanel("Energynomics"),
  
  tabsetPanel(
    tabPanel("Energy Comparison",
             sidebarLayout(
               sidebarPanel(
                 selectInput("county_name", "Select a County", 
                             choices = c("All Counties" = "all", unique(Test_Optimied_Variables$county_name))),
               ),
               mainPanel(
                 plotOutput("energyComparisonPlot")
               )
             )
    ),
    tabPanel("Weather vs Energy Consumption in July",
             selectInput("selectedMetric", "Select a Metric", 
                         choices = c("Dry Bulb Temperature [°C]", "Relative Humidity [%]",
                                     "Wind Direction [Deg]", "Global Horizontal Radiation [W/m2]",
                                     "Direct Normal Radiation [W/m2]", "Diffuse Horizontal Radiation [W/m2]")),
             plotOutput("metricVsEnergyPlot")
    ),
    tabPanel("Averages Calculator",
             sidebarLayout(
               sidebarPanel(
                 selectInput("column_select", "Select a Column", choices = columns_with_slider_names_filtered)
               ),
               mainPanel(
                 plotOutput("averagesPlot")
               )
             )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Relationship with Metrics Plot
  output$metricVsEnergyPlot <- renderPlot({
    selected_metric <- input$selectedMetric
    if (is.null(selected_metric)) {
      return(NULL)
    }
    
    plot_data <- Weather_Energy %>%
      select(selected_metric, Final_Energy_KWH)
    
    ggplot(data = plot_data, aes(x = .data[[selected_metric]], y = Final_Energy_KWH)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "lightblue") +
      labs(x = selected_metric, y = "Final Energy (KWH)") +
      ggtitle(paste(selected_metric, "vs Energy Consumption")) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "#f2f2f2"),
        plot.background = element_rect(fill = "#f2f2f2"),
        plot.margin = margin(10, 10, 10, 10)
      )
  })
  
  # Averages Calculator - Bar Chart with Tilted X-axis Text
  output$averagesPlot <- renderPlot({
    req(input$column_select)
    selected_column <- input$column_select
    
    averages <- Test_Optimied_Variables %>%
      group_by_at(vars(selected_column)) %>%
      summarise(mean_value = mean(Final_Energy_KWH, na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(averages, aes_string(x = selected_column, y = "mean_value")) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(x = selected_column, y = "Average Energy Consumption (KWH)") +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "#f2f2f2"),
        plot.background = element_rect(fill = "#f2f2f2"),
        plot.margin = margin(10, 10, 10, 10),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$energyComparisonPlot <- renderPlot({
    req(input$county_name)
    
    # Adjust logic for 'All Counties' selection
    filtered_data <- if(input$county_name == "all") {
      Test_Optimied_Variables
    } else {
      filtered_subset <- Test_Optimied_Variables %>%
        filter(county_name == input$county_name)
      
      # Diagnostics: Check if the filtered data is empty
      if(nrow(filtered_subset) == 0) {
        print(paste("No data available for county:", input$county_name))
        return(NULL)
      }
      
      filtered_subset
    }
    
    # Summarize the data
    Predictions_hour <- filtered_data %>%
      group_by(hour) %>%
      summarise(total_energy = sum(Final_Energy_KWH, na.rm = TRUE), 
                predicted_energy = sum(predictions1, na.rm = TRUE))
    
    # Calculate percentage difference
    Predictions_hour <- Predictions_hour %>%
      mutate(percentage_diff = 
               ((predicted_energy - total_energy) / total_energy) * 100)
    
    # Check if summarization results in data
    if(nrow(Predictions_hour) == 0) {
      print("No data available after summarization")
      return(NULL)
    }
    
    # Plotting
    ggplot(data = Predictions_hour, aes(x = hour)) +
      geom_bar(aes(y = total_energy), stat = "identity", fill = "blue", alpha = 0.6) +
      geom_bar(aes(y = predicted_energy), stat = "identity", fill = "red", alpha = 0.6) +
      geom_text(aes(y = pmax(predicted_energy, total_energy), label = paste0(round(percentage_diff, 2), "%")), 
                position = position_stack(vjust = 0.5), 
                size = 4, 
                color = "black", 
                angle = 90, 
                hjust = -0.5) +  
      labs(title = "Total Energy vs Predicted Energy by Hour",
           x = "Hour", y = "Energy") +
      theme(panel.background = element_rect(fill = "#f2f2f2", colour = "#f2f2f2"),
            plot.background = element_rect(fill = "#f2f2f2", colour = NA),  # NA for plot border
            axis.line = element_line(color = "#f2f2f2"),  # Customize axis line color
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.text = element_text(color = "black"),  # Adjust text color if needed
            axis.ticks = element_line(color = "#f2f2f2")  # Customize tick color
      )
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
