library(shiny)

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
if (!requireNamespace("TreeCarbon", quietly = TRUE)) {
  remotes::install_github("izaopenshaw/TreeCarbon")
}
library(TreeCarbon)

# Define UI
ui <- fluidPage(
  titlePanel("Tree Above Ground Carbon and Error Calculator"),

  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload a CSV file with tree data",
                accept = c(".csv")),
      textInput("species_name", "Species Name:", value = ""),
      selectInput("type", "Type", choices = c("broadleaf", "conifer", "NA"), selected = "NA"),
      #numericInput("dbh", "DBH (cm):", value = NA, min = 0),
      sliderInput("dbh", "DBH (cm):", min = 0, max = 500, value = 25, step = 1),
      numericInput("height", "Height (m):", value = NA, min = 0),
      selectInput("method", "Carbon to Biomass Method:", choices = c("Thomas", "Matthews1", "Matthews2", "IPCC1", "IPCC2")),
      selectInput("biome", "Biome:", choices = c("temperate", "boreal", "tropical", "subtropical", "mediterranean")),
      selectInput("output_type", "Output Type:", choices = c("AGC and sig_AGC","All Data")),
      actionButton("calculate", "Calculate"),
      downloadButton("download", "Download Results")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Results", tableOutput("results")),
        tabPanel("Instructions",
                 p("To use the app:"),
                 p("1. Upload a CSV file with columns for `name`, `dbh`, and `height`."),
                 p("2. Or input single tree data in the fields and click Calculate."),
                 p("3. Use the 'Output Type' dropdown to select which columns to show.")
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  calculate_results <- reactive({
    req(input$calculate) # Wait for button click

    if (!is.null(input$datafile)) {
      # Load data from file
      tree_data <- read.csv(input$datafile$datapath)

      # Ensure required columns are present
      if (!all(c("name", "dbh", "height") %in% colnames(tree_data))) {
        showNotification("Uploaded file must have columns: name, dbh, and height.", type = "error")
        return(NULL)
      }

      # Calculate results for all trees
      results <- fc_agc_error(
        name = tree_data$name,
        dbh = tree_data$dbh,
        height = tree_data$height,
        method = input$method,
        biome = input$biome,
        returnv = "All"
      )
    } else {
      # Single tree calculation
      results <- fc_agc_error(
        name = input$species_name,
        dbh = input$dbh,
        height = input$height,
        method = input$method,
        biome = input$biome,
        returnv = "All"
      )
    }

    # Filter output if necessary
    if (input$output_type == "AGC and sig_AGC") {
      results <- results[, c("AGC_t", "sig_AGC"), drop = FALSE]
    }

    return(results)
  })

  # Render results table
  output$results <- renderTable({
    req(calculate_results())
    calculate_results()
  })

  # Handle file download
  output$download <- downloadHandler(
    filename = function() {
      paste("results-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(calculate_results(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)
