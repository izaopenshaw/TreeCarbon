library(shiny)
library(ggplot2)

# errors: graph not appearing with output all False, cant get re inputs to work

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
if (!requireNamespace("TreeCarbon", quietly = TRUE)) {
  remotes::install_github("izaopenshaw/TreeCarbon")
}
library(TreeCarbon)

lookup_latin_names <- c("", "Abies", "Abies alba", "Abies grandis", "Abies nordmanniana", "Abies procera",
                        "Abies sibirica", "Acer campestre", "Acer platanoides", "Acer pseudoplatanus",
                        "Aesculus hippocastanum", "Alnus", "Alnus cordata", "Alnus glutinosa", "Alnus incana",
                        "Betula", "Callitropsis nootkatensis", "Carpinus betulus", "Castanea sativa",
                        "Chamaecyparis lawsoniana", "Corylus avellana", "Cupressocyparis leylandii",
                        "Cupressus macrocarpa", "Cryptomeria japonica", "Fagus sylvatica", "Fraxinus",
                        "Larix decidua", "Larix kaempferi", "Larix × eurolepis", "Metasequoia glyptostroboides",
                        "Nothofagus nervosa", "Nothofagus obliqua", "Nothofagus procera", "Picea", "Picea abies",
                        "Picea engelmannii", "Picea glauca", "Picea omorika", "Picea orientalis", "Picea pungens",
                        "Picea sitchensis", "Pinus", "Pinus contorta", "Pinus muricata", "Pinus nigra",
                        "Pinus pinaster", "Pinus ponderosa", "Pinus radiata", "Pinus resinosa", "Pinus strobus",
                        "Pinus sylvestris", "Platanus × hispanica", "Populus", "Populus canadensis", "Populus nigra",
                        "Prunus avium", "Prunus padus", "Pseudotsuga menziesii", "Quercus", "Quercus cerris",
                        "Quercus petraea", "Quercus robur", "Quercus rubra", "Sequoiadendron giganteum",
                        "Sequoia sempervirens", "Thuja plicata", "Tilia", "Tsuga heterophylla", "Ulmus", "Ulmus glabra", "")

# Define UI
ui <- fluidPage(
  titlePanel("Tree Above Ground Carbon and Error Calculator"),

  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload a CSV file with tree data",
                accept = c(".csv")),
      selectInput("botanical_name", "Select Botanical Name:",
                  choices = c("", lookup_latin_names), selected = NULL),
      textInput("common_name", "Or enter a common name", value = ""),
      selectInput("type", "Type", choices = c("broadleaf", "conifer", "NA"), selected = "NA"),
      sliderInput("dbh", "DBH (cm):", min = 0, max = 150, value = 20, step = 1),
      sliderInput("height", "Height (m):", min = 0, max = 60, value = 15, step = 1),
      selectInput("method", "Carbon to Biomass Method:", choices = c("Thomas", "IPCC2")),
      #textInput("re_dbh", "Relative error for dbh (%)", value = "0.05"),
      #textInput("re_h", "Relative error for height (%)", value = "0.1"),
      #textInput("re", "Relative error for coefficients (%)", value = "0.025"),
      checkboxInput("output_all", "Output all data", value = TRUE), # Updated to toggle button
      actionButton("calculate", "Calculate"),
      downloadButton("download", "Download Results")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Results and Plot", plotOutput("dbhPlot"), tableOutput("results")),
        tabPanel("Instructions",
                 p("To use the app:"),
                 p("1a. Upload a CSV file with columns for `name` (either botanical or common), `dbh` (cm), and `height` (m)."),
                 p("b. Or input single tree data in the fields and click Calculate."),
                 p("2. Use the 'Output Type' checkbox to toggle all output data or just carbon estimates."))
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

          # Ensure required columns are present
          if (!all(c("name", "dbh", "height") %in% colnames(tree_data))) {
            showNotification("Uploaded file must have columns: name, dbh, and height.", type = "error")
            return(NULL)
          }

          # Assign default values for optional columns if missing
          if (!"type" %in% colnames(tree_data)) tree_data$type <- "NA"
          #if (!"nsg" %in% colnames(tree_data)) tree_data$nsg <- NA
          #if (!"sig_nsg" %in% colnames(tree_data)) tree_data$sig_nsg <- NA
          #if (!"re_dbh" %in% colnames(tree_data)) tree_data$re_dbh <- 0.05
          #if (!"re_h" %in% colnames(tree_data)) tree_data$re_h <- 0.1
          #if (!"re" %in% colnames(tree_data)) tree_data$re <- 0.025

          # Calculate results for all trees
          fc_agc_error(
            name = tree_data$name,
            dbh = tree_data$dbh,
            height = tree_data$height,
            type = tree_data$type,
            method = input$method,
            #biome = input$biome,
            #nsg = tree_data$nsg,
            #sig_nsg = tree_data$sig_nsg,
            #re_dbh = as.numeric(input$re_dbh),
            #re_h = as.numeric(input$re_h),
            #re = as.numeric(input$re),
            output.all = input$output_all
          )
        } else {
          # Single tree calculation
          fc_agc_error(
            name = selected_name(),
            dbh = input$dbh,
            height = input$height,
            method = input$method,
            #biome = input$biome,
            #re_dbh = input$re_dbh,
            #re_h = input$re_h,
            #re = input$re,
            output.all = input$output_all
          )
        }
      }, warning = capture_warning)
    }, error = function(e) {
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
      return(NULL)
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

    # Filter rows with missing values
    plot_data <- results[!is.na(results$dbh) & !is.na(results$AGC_t), ]

    # Conditional plotting
    if (input$output_all) {
      ggplot(plot_data, aes(x = dbh, y = AGC_t)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        geom_errorbar(
          aes(ymin = AGC_t - sig_AGC, ymax = AGC_t + sig_AGC),
          width = 0.5
        ) +
        labs(
          title = "Above Ground Carbon (AGC) with DBH on X-Axis",
          x = "DBH (cm)",
          y = "AGC (t)"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    } else {
      ggplot(plot_data, aes(x = dbh, y = AGC_t)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(
          title = "Above Ground Carbon (AGC) with DBH on X-Axis",
          x = "DBH (cm)",
          y = "AGC (t)"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    }
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




