library(esquisse)
library(shinydashboard)
library(reactable)
library(tidyverse)
ui <- dashboardPage(
  dashboardHeader(
    title = div("Esquisse with Shiny",
                
    )
  ),
  dashboardSidebar(
    selectInput(
      inputId = "data", 
      label = "Select data to use:", 
      choices = c("starwars", "storms", "economics"),
      selected = "starwars"
    )
  ),
  dashboardBody(
    fluidRow(
      infoBoxOutput("dataset_info_box"),   # Info box for dataset name
      infoBoxOutput("num_rows_info_box"),  # Info box for number of rows
      infoBoxOutput("num_cols_info_box"),  # Info box for number of columns
      width = 12  # Adjust the width of the fluid row
    ),
    tabsetPanel(
      tabPanel(
        title = "esquisse",
        esquisse_ui(
          id = "esquisse", 
          header = FALSE # don't display gadget title
        )
      ),
      tabPanel(
        title = "output",
        tags$b("Data:"),
        verbatimTextOutput("data_glimpse"),  # Output for glimpse of the selected dataset
        reactableOutput("data_table")  # Output for the formatted dataset table
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive function to get the selected dataset
  selected_data <- reactive({
    switch(input$data,
           "starwars" = starwars,
           "storms" = storms,
           "economics" = economics)
  })
  
  data_r <- reactiveValues(data = starwars, name = "starwars")  # Set the default dataset
  
  observe({
    data_r$data <- get(input$data)
    data_r$name <- input$data
  })
  
  results <- esquisse_server(
    id = "esquisse",
    data_rv = data_r
  )
  
  output$data_glimpse <- renderPrint({
    dplyr::glimpse(results$data)  # Display glimpse output of the selected dataset
  })
  
  output$data_table <- renderReactable({
    reactable(results$data)  # Create the formatted dataset table
  })
  
  # Info box for dataset name
  output$dataset_info_box <- renderInfoBox({
    infoBox(
      "Dataset Name",
      data_r$name,
      icon = icon("database"),
      color = "teal"
    )
  })
  
  # Info box for number of rows
  output$num_rows_info_box <- renderInfoBox({
    infoBox(
      "Number of Rows",
      nrow(data_r$data),
      icon = icon("table"),
      color = "purple"
    )
  })
  
  # Info box for number of columns
  output$num_cols_info_box <- renderInfoBox({
    infoBox(
      "Number of Columns",
      ncol(data_r$data),
      icon = icon("th"),
      color = "orange"
    )
  })
}

shinyApp(ui, server)