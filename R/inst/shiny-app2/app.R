# App2 to compare different allometries

library(shiny)
library(ggplot2)

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
if (!requireNamespace("TreeCarbon", quietly = TRUE)) {
  remotes::install_github("izaopenshaw/TreeCarbon")
}
library(TreeCarbon)

# Define UI
ui <- fluidPage(
  titlePanel("Tree Above Ground Carbon Calculator using WCC"),

  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload a CSV file with tree data",
                accept = c(".csv")),
      textInput("genus", "Enter Genus:", value = ""),
      textInput("species", "Enter Species:", value = ""),
      selectInput("type", "Type (optional):", choices = c("broadleaf", "conifer", "NA"), selected = "NA"),
      sliderInput("dbh", "DBH (cm):", min = 0, max = 150, value = 20, step = 1),
      sliderInput("height", "Height (m):", min = 0, max = 60, value = 15, step = 1),
      selectInput("method", "Carbon to Biomass Conversion Method:", choices = c("IPCC1", "IPCC2", "Matthews1", "Matthews2", "Thomas")),
      checkboxInput("output_all", "Output all data", value = TRUE),
      checkboxInput("returnv", "Output Biomass", value = "AGB"),
      actionButton("calculate", "Calculate"),
      downloadButton("download", "Download Results")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Results and Plot", plotOutput("dbhPlot"), tableOutput("results")),
        tabPanel("Instructions",
                 p("To use the app:"),
                 p("1a. Either upload a csv file with columns for `name` (either botanical or common), `dbh` (cm), `height` (m) and type (broadleaf or confier. This is optional as it is a fall back if the species name is not found)."),
                 p("b. Or input single tree data in the fields and click Calculate."),
                 p("2. Use the 'Output all data' checkbox to toggle all output data or just carbon estimates. Above-ground carbon (AGC) is outputted in tonnes, along with the propagated error estimate, sig_AGC which is an estimate for sigma of AGC. The plotted error bars represent AGC + or - sig_AGC."))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Helper function for selected name
  selected_name <- reactive({
    if (nzchar(input$common_name)) {
      input$common_name
    } else if (nzchar(input$botanical_name)) {
      input$botanical_name
    } else {
      "Unknown" # Default value
    }
  })

  # Reactive to calculate results
  calculate_results <- reactive({

    req(input$calculate)

    # Initialize warnings
    warnings <- character()

    # Function to capture warnings
    capture_warning <- function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }

    # Run fc_agc_error
    results <- tryCatch({
      withCallingHandlers({

        if (!is.null(input$datafile)) {
          # Load data from file
          tree_data <- read.csv(input$datafile$datapath)

          if (!all(c("name", "dbh", "height") %in% colnames(tree_data))) {
            stop("CSV file must contain columns: name, dbh, height")
          }
          print("checks1")

          # Assign default values for optional columns if missing
          if (!"type" %in% colnames(tree_data)) tree_data$type <- NA
          tree_data$type[!tree_data$type %in% c("broadleaf", "conifer")] <- NA
          print("checks2")

          print(tree_data$type)

          # Calculate results for all trees
          allometries(tree_data$genus, tree_data$species, tree_data$dbh, tree_data$height, tree_data$type,
                      method = input$method, output.all = input$output_all , returnv = input$returnv)

        } else {
          # Single tree calculation
          allometries(input$genus, input$species, input$dbh, input$height, input$type, method =input$method,
                      output.all = input$output_all , returnv = input$returnv)
        }
      }, warning = capture_warning)
    }, error = function(e) {
      showNotification("An error occurred while calculating results", type = "error")    # paste("Error:", conditionMessage(e)), type = "error")
      return(NULL)
      result
    })

    # Display all warnings as notifications
    for (warning_msg in warnings) {
      showNotification(paste("Warning:", warning_msg), type = "warning")
    }

    return(results)
  })

  # Render DBH bar plot
  output$dbhPlot <- renderPlot({
    results <- req(calculate_results())

      plot_data <- results[!is.na(results$dbh) & !is.na(results$Height), ]

      if(input$returnv == "AGB"){
        agb_cols <- grep("^AGB_", names(plot_data), value = TRUE)
        plot_data_long <- data.frame(
          Method = rep(agb_cols, each = nrow(plot_data)),
          Biomass = unlist(plot_data[agb_cols])
        )

        # Create bar plot
        ggplot(plot_data_long, aes(x = Method, y = Biomass, fill = Method)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Comparison of Biomass Calculation Methods",
               x = "Biomass Calculation Method",
               y = "Biomass Value") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

      } else {
        agc_cols <- grep("^AGC_", names(plot_data), value = TRUE)
        plot_data_long <- data.frame(
          Method = rep(agc_cols, each = nrow(plot_data)),
          Carbon = unlist(plot_data[agc_cols])
        )

        # Create bar plot
        ggplot(plot_data_long, aes(x = Method, y = Carbon, fill = Method)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Comparison of Carbon Calculation Methods",
               x = "Carbon Calculation Method",
               y = "Carbon Value") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }

  })

  # Render results table
  output$results <- renderTable({
    results <- req(calculate_results())

    results

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



