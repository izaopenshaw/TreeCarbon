library(shiny)
library(devtools)
library(TreeCarbon)

# Source the custom functions file
source("functions.R")

# Define UI
ui <- fluidPage(
  titlePanel("Carbon and Error Calculator"),

  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload a CSV file with tree data",
                accept = c(".csv")),
      textInput("species_name", "For single tree, Species Name:"),
      selectInput("type", "Type", choices = c("broadleaf", "conifer", "NA"), selected = "NA"),
      numericInput("dbh", "DBH (cm):", value = NA, min = 0),
      numericInput("height", "Height (m):", value = NA, min = 0),
      textInput("method", "Method:", value = "IPCC2"),
      selectInput("biome", "Biome:", choices = c("temperate", "boreal", "tropical", "subtropical", "mediterranean")),
      actionButton("calculate", "Calculate")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Results", tableOutput("results")),
        tabPanel("Instructions",
                 p("To use the app:"),
                 p("1. Upload a CSV file with columns for `name`, `dbh`, and `height`."),
                 p("2. Or input single tree data in the fields and click Calculate.")
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {

  calculate_results <- reactive({
    req(input$calculate) # Wait for button click

    # Check if a file was uploaded
    if (!is.null(input$datafile)) {
      # Load data from file
      tree_data <- read.csv(input$datafile$datapath)

      # Ensure required columns are present
      if (!all(c("name", "dbh", "height") %in% colnames(tree_data))) {
        showNotification("Uploaded file must have columns: name, dbh, and height.", type = "error")
        return(NULL)
      }

      # Calculate results for all trees in the file
      return(fc_agc_error(
        name = tree_data$name,
        type = if ("type" %in% colnames(tree_data)) tree_data$type else rep(input$type, nrow(tree_data)),
        dbh = tree_data$dbh,
        height = tree_data$height,
        method = input$method,
        biome = input$biome,
        returnv = "All"
      ))

    } else {
      # Calculate for single tree
      return(fc_agc_error(
        name = input$species_name,
        type = input$type,
        dbh = input$dbh,
        height = input$height,
        method = input$method,
        biome = input$biome,
        returnv = "All"
      ))
    }
  })

  # Display results
  output$results <- renderTable({
    req(calculate_results())
    calculate_results()
  })
}

# Run the app
shinyApp(ui, server)
