library(shiny)
library(dplyr)
library(arrow)

# Define your Subset_V4 dataset here
# Replace this with your actual dataset
Subset_V4 <- read_parquet("/Users/USER/Desktop/IDS Files/Subset_V4.parquet")


# Shiny UI
ui <- fluidPage(
  titlePanel("Averages Calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("column_select", "Select a Column", choices = colnames(Subset_V4))
    ),
    mainPanel(
      tableOutput("averages_table")
    )
  ),
  tags$style(HTML(".nav-tabs li.active a { background-color: #f2f2f2; }")),
  tags$style(HTML(".tab-content { background-color: #f2f2f2; }"))
)

# Shiny Server
server <- function(input, output) {
  output$averages_table <- renderTable({
    selected_column <- input$column_select
    if (is.null(selected_column)) {
      return(NULL)
    }
    
    averages <- Subset_V4 %>%
      group_by_at(selected_column) %>%
      summarise(mean_value = mean(Final_Energy_KWH, na.rm = TRUE))
    
    colnames(averages) <- c("Category", "Mean_Value")
    
    return(averages)
  })
}

# Run the app
shinyApp(ui, server)
