# App1 - WCC Tree Carbon Calculator (Improved Version)
# Features: Dashboard layout, interactive plots, value boxes, validation, etc.

library(shiny)
library(ggplot2)

# Check for optional packages
if (!requireNamespace("shinydashboard", quietly = TRUE)) {
  stop("Please install 'shinydashboard' package: install.packages('shinydashboard')")
}
library(shinydashboard)

if (!requireNamespace("plotly", quietly = TRUE)) {
  warning("plotly not installed. Interactive plots will not work. Install with: install.packages('plotly')")
}
if (!requireNamespace("DT", quietly = TRUE)) {
  warning("DT not installed. Enhanced tables will not work. Install with: install.packages('DT')")
}

# Load optional packages if available
if (requireNamespace("plotly", quietly = TRUE)) library(plotly)
if (requireNamespace("DT", quietly = TRUE)) library(DT)

# Species lookup list
lookup_latin_names <- c("Quercus robur", "Abies", "Abies alba", "Abies grandis", "Abies nordmanniana", "Abies procera",
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
                        "Quercus petraea", "Quercus rubra", "Sequoiadendron giganteum",
                        "Sequoia sempervirens", "Thuja plicata", "Tilia", "Tsuga heterophylla", "Ulmus", "Ulmus glabra")

# Helper function to strip ANSI color codes from strings
strip_ansi <- function(x) {
  gsub("\033\\[[0-9;]*m", "", x)
}

# UI - Dashboard Layout
ui <- dashboardPage(
  dashboardHeader(title = "WCC Carbon Calculator"),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Calculator", tabName = "main", icon = icon("calculator"), selected = TRUE),
      menuItem("Instructions", tabName = "instructions", icon = icon("book")),
      menuItem("Method Info", tabName = "info", icon = icon("info-circle"))
    )
  ),

  dashboardBody(
    # Custom CSS with mobile responsiveness
    tags$head(
      # Viewport meta tag for proper mobile scaling
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1"),
      tags$style(HTML("
        .shiny-notification { position: fixed; top: 10%; left: 50%; transform: translateX(-50%); }
        .btn-warning { margin-left: 10px; }
        .content-wrapper { background-color: #f4f6f9; }

        /* General improvements */
        .box { overflow-x: auto; }
        .form-control { font-size: 16px; } /* Prevents zoom on iOS */

        /* Tablet view */
        @media (max-width: 992px) {
          .col-lg-4, .col-lg-8, .col-md-4, .col-md-8 {
            width: 100% !important;
            float: none !important;
          }
          .box {
            width: 100% !important;
            margin-bottom: 15px;
          }
          .value-box { margin-bottom: 10px; }
          .sidebar-toggle { display: block !important; }
        }

        /* Sidebar menu items - ensure visibility on all screen sizes */
        .main-sidebar {
          z-index: 1040;
        }
        .main-sidebar .sidebar-menu {
          display: block !important;
          visibility: visible !important;
          overflow: visible !important;
        }
        .main-sidebar .sidebar-menu > li {
          display: block !important;
          visibility: visible !important;
        }
        .main-sidebar .sidebar-menu > li > a {
          display: block !important;
          visibility: visible !important;
          padding: 12px 15px 12px 15px;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .main-sidebar .sidebar-menu > li > a > .fa,
        .main-sidebar .sidebar-menu > li > a > .glyphicon,
        .main-sidebar .sidebar-menu > li > a > .ion {
          margin-right: 10px;
          display: inline-block !important;
        }
        .main-sidebar .sidebar-menu > li > a > span {
          display: inline-block !important;
        }
        /* Ensure sidebar content is visible when collapsed on mobile */
        .sidebar-collapse .main-sidebar .sidebar-menu > li > a > span {
          display: inline-block !important;
        }
        .left-side, .main-sidebar {
          overflow-y: auto;
          overflow-x: hidden;
        }

        /* Mobile view */
        @media (max-width: 768px) {
          .shiny-notification {
            left: 5% !important;
            right: 5% !important;
            width: 90% !important;
            transform: none;
          }
          .main-header .logo {
            width: 100% !important;
            font-size: 14px !important;
          }
          .main-header .navbar { margin-left: 0 !important; }
          .btn {
            margin-bottom: 8px;
            min-height: 44px; /* Touch-friendly size */
          }
          .form-group { margin-bottom: 12px; }
          .col-sm-4, .col-sm-6, .col-sm-8, .col-sm-3, .col-sm-2, .col-sm-12 {
            width: 100% !important;
            padding: 5px 15px;
            float: none !important;
          }
          .box-body { padding: 10px; }
          .content { padding: 10px; }

          /* Stack value boxes vertically */
          .small-box { margin-bottom: 10px; }

          /* Make sliders touch-friendly */
          .irs--shiny .irs-handle {
            width: 26px;
            height: 26px;
            top: 22px;
          }

          /* Larger select dropdowns */
          .selectize-input { min-height: 44px; padding-top: 10px; }

          /* Mobile sidebar fixes */
          .main-sidebar {
            position: fixed !important;
            height: 100% !important;
            padding-top: 50px;
          }
          .main-sidebar .sidebar-menu > li > a {
            padding: 15px 15px;
            font-size: 16px;
          }
          .main-sidebar .sidebar-menu > li > a > span {
            display: inline !important;
            visibility: visible !important;
            opacity: 1 !important;
          }
          .sidebar-mini.sidebar-collapse .main-sidebar .sidebar-menu > li > a > span {
            display: inline !important;
          }
        }

        /* Very small screens */
        @media (max-width: 480px) {
          .main-header .logo { font-size: 12px !important; }
          h4 { font-size: 16px; }
          h5 { font-size: 14px; }
          .box-title { font-size: 14px; }
        }

        /* Make plot options wrap nicely */
        .plot-options-row .form-group { margin-bottom: 10px; }

        /* Ensure tables scroll horizontally on small screens */
        .dataTables_wrapper { overflow-x: auto; }
      "))
    ),

    tabItems(
      # Main Tab - Input AND Results on same page
      tabItem(tabName = "main",
              # Value Boxes at top
              fluidRow(
                valueBoxOutput("total_trees", width = 4),
                valueBoxOutput("total_carbon", width = 4),
                valueBoxOutput("avg_carbon", width = 4)
              ),

              # Input and Plot side by side
              fluidRow(
                # Left column - Input
                box(title = "Data Input", width = 4, status = "primary", solidHeader = TRUE,
                    fileInput("datafile", "Upload CSV file (name, dbh, height columns)",
                              accept = c(".csv"),
                              placeholder = "Select CSV file"),
                    downloadButton("download_template", "Download Template CSV", class = "btn-sm btn-info"),
                    hr(),

                    h5("OR Input Single Tree", style = "font-weight: bold;"),
                    selectInput("botanical_name", "Select Species:",
                                choices = c("", lookup_latin_names),
                                selected = "Quercus robur"),
                    textInput("common_name", "Or enter a name to search:", value = ""),
                    selectInput("type", "Type (fallback if species not found):",
                                choices = c("broadleaf", "conifer", "NA"),
                                selected = "NA"),
                    sliderInput("dbh", "DBH (cm):", min = 1, max = 150, value = 20, step = 1),
                    sliderInput("height", "Height (m):", min = 1, max = 60, value = 15, step = 1),
                    hr(),
                    selectInput("method", "Carbon Conversion Method:",
                                choices = c("Thomas", "IPCC2", "Matthews1", "Matthews2", "IPCC1"),
                                selected = "Thomas"),
                    hr(),
                    actionButton("calculate", "Calculate", class = "btn-primary btn-lg", icon = icon("calculator")),
                    actionButton("clear", "Clear Results", class = "btn-warning", icon = icon("trash"))
                ),

                # Right column - Plot
                box(title = "Carbon/Biomass Plot", width = 8, status = "primary", solidHeader = TRUE,
                    fluidRow(class = "plot-options-row",
                             column(6,
                                    selectInput("plot_type", "Plot Type:",
                                                choices = c("Bar Chart (Carbon)" = "carbon_bar",
                                                            "Stacked Biomass" = "biomass_stack",
                                                            "Scatter by DBH" = "scatter_dbh",
                                                            "Scatter by Height" = "scatter_height"),
                                                selected = "carbon_bar")
                             ),
                             column(6,
                                    selectInput("color_scheme", "Colors:",
                                                choices = c("Default" = "default",
                                                            "Viridis" = "viridis",
                                                            "Colorblind" = "colorblind"),
                                                selected = "default")
                             )
                    ),
                    fluidRow(class = "plot-options-row",
                             column(4,
                                    checkboxInput("show_errors", "Show error bars", value = TRUE)
                             ),
                             column(4,
                                    checkboxInput("interactive_plot", "Interactive plot", value = FALSE)
                             ),
                             column(4,
                                    selectInput("point_size_by", "Point size by (scatter only):",
                                                choices = c("None" = "none",
                                                            "DBH" = "dbh",
                                                            "Height" = "height"),
                                                selected = "none")
                             )
                    ),
                    fluidRow(
                      column(12,
                             downloadButton("download_plot_png", "Download PNG", class = "btn-sm")
                      )
                    ),
                    hr(),
                    uiOutput("plot_ui")
                )
              ),

              # Warnings Panel
              uiOutput("warnings_panel"),

              # Results Table
              fluidRow(
                box(title = "Results Table", width = 12, status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(6, checkboxInput("output_all", "Show all columns", value = FALSE)),
                      column(6,
                             downloadButton("download_csv", "CSV", class = "btn-primary btn-sm"),
                             downloadButton("download_excel", "Excel", class = "btn-success btn-sm")
                      )
                    ),
                    DT::dataTableOutput("results_table")
                )
              )
      ),

      # Instructions Tab
      tabItem(tabName = "instructions",
              fluidRow(
                box(title = "How to Use This App", width = 12, status = "primary", solidHeader = TRUE,
                    h4("1. Input Data"),
                    tags$ul(
                      tags$li("Upload a CSV file with columns: ", tags$code("name"), " (botanical or common name), ",
                              tags$code("dbh"), " (cm), ", tags$code("height"), " (m)"),
                      tags$li("Optional column: ", tags$code("type"), " (broadleaf or conifer) - used as fallback if species not recognized"),
                      tags$li("Maximum file size: 10MB, Maximum rows: 1000"),
                      tags$li("OR enter single tree data manually using the dropdown and sliders"),
                      tags$li("Download the template CSV to see the correct format")
                    ),
                    hr(),
                    h4("2. Set Parameters"),
                    tags$ul(
                      tags$li("Choose carbon conversion method (Thomas is recommended for UK species)"),
                      tags$li("Use the 'Show all columns' checkbox to see all calculation details")
                    ),
                    hr(),
                    h4("3. Calculate"),
                    tags$ul(
                      tags$li("Click 'Calculate' to process data"),
                      tags$li("Results accumulate - add more trees or upload another file"),
                      tags$li("Click 'Clear Results' to start fresh")
                    ),
                    hr(),
                    h4("4. View Results"),
                    tags$ul(
                      tags$li("Above-ground carbon is output in tonnes (AGC_WCC_t)"),
                      tags$li("Error estimate (sig_AGC) represents the propagated uncertainty"),
                      tags$li("Error bars represent AGC_WCC_t ± sig_AGC")
                    ),
                    hr(),
                    h4("5. Export"),
                    tags$ul(
                      tags$li("Download results as CSV or Excel"),
                      tags$li("Export plots as PNG")
                    )
                )
              )
      ),

      # Method Info Tab
      tabItem(tabName = "info",
              fluidRow(
                box(title = "Woodland Carbon Code (WCC) Method", width = 12, status = "primary", solidHeader = TRUE,
                    h4("Overview"),
                    p("This app calculates above-ground carbon (AGC) using the UK Forestry Commission's Woodland Carbon Code protocol.
                      The method uses species-specific tariff numbers and volume equations developed for UK forestry."),
                    p(tags$strong("Reference:"), " Jenkins, Thomas AR, et al. 'FC Woodland Carbon Code: Carbon Assessment Protocol (v2.0).' (2018)."),
                    hr(),

                    h4("Calculation Steps"),
                    tags$ol(
                      tags$li(tags$strong("Stem Volume: "), "Calculated using species-specific tariff equations"),
                      tags$li(tags$strong("Stem Biomass: "), "Volume × Wood Density (Nominal Specific Gravity)"),
                      tags$li(tags$strong("Crown Biomass: "), "Estimated from stem biomass using crown:stem ratios"),
                      tags$li(tags$strong("Root Biomass: "), "Estimated from stem biomass using root:shoot ratios"),
                      tags$li(tags$strong("Carbon Content: "), "Total biomass × Carbon Fraction")
                    ),
                    hr(),

                    h4("Carbon Conversion Methods"),
                    tags$div(
                      tags$h5("Thomas & Martin (2012)", style = "font-weight: bold;"),
                      p("Biome and type-specific carbon fractions. Most detailed method, recommended for accuracy."),
                      p(tags$em("Reference: Thomas, S.C., & Martin, A.R. (2012). Carbon content of tree tissues: A synthesis. Forests, 3(2), 332-352.")),

                      tags$h5("IPCC2", style = "font-weight: bold;"),
                      p("IPCC default values by biome and tree type."),
                      p(tags$em("Reference: IPCC (2006). Forest lands. IPCC Guidelines for National Greenhouse Gas Inventories, Vol. 4, p. 83.")),

                      tags$h5("Matthews1", style = "font-weight: bold;"),
                      p("Simple 50% carbon fraction for all trees."),

                      tags$h5("Matthews2", style = "font-weight: bold;"),
                      p("Type-specific carbon fractions (broadleaf vs conifer)."),
                      p(tags$em("Reference: Matthews, G.A.R. (1993). The Carbon Content of Trees. Forestry Commission Technical Paper 4.")),

                      tags$h5("IPCC1", style = "font-weight: bold;"),
                      p("Single default value: 47.7% carbon fraction.")
                    ),
                    hr(),

                    h4("Error Propagation"),
                    p("Uncertainty is propagated through all calculation steps using standard error propagation methods."),
                    p("The output ", tags$code("sig_AGC"), " represents the estimated standard deviation of the AGC estimate."),
                    p("Default relative errors used: DBH (5%), Height (10%), Coefficients (2.5%)")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive values
  appData <- reactiveValues(results = data.frame())
  calculation_warnings <- reactiveValues(warnings = character())

  # File size and row limits
  max_file_size <- 10 * 1024 * 1024  # 10MB
  max_rows <- 1000

  # Helper function for selected name
  selected_name <- reactive({
    if (nzchar(input$common_name)) {
      input$common_name
    } else if (nzchar(input$botanical_name)) {
      input$botanical_name
    } else {
      "Quercus robur"  # Default
    }
  })

  # CSV data reactive with validation
  csv_data <- reactive({
    if (!is.null(input$datafile)) {
      # Check file size
      if (input$datafile$size > max_file_size) {
        showNotification(paste("File too large. Maximum size is", max_file_size / 1024 / 1024, "MB"),
                         type = "error", duration = 10)
        return(NULL)
      }
      tryCatch({
        df <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE)

        # Check required columns
        if (!all(c("name", "dbh", "height") %in% colnames(df))) {
          showNotification("CSV file must contain columns: name, dbh, height", type = "error", duration = 10)
          return(NULL)
        }

        # Check row limit
        if (nrow(df) > max_rows) {
          showNotification(paste("Too many rows. Maximum is", max_rows, ". Using first", max_rows, "rows."),
                           type = "warning", duration = 5)
          df <- df[1:max_rows, ]
        }

        # Validate numeric columns
        if (!is.numeric(df$dbh)) {
          df$dbh <- as.numeric(df$dbh)
        }
        if (!is.numeric(df$height)) {
          df$height <- as.numeric(df$height)
        }

        if (any(is.na(df$dbh)) || any(df$dbh <= 0, na.rm = TRUE)) {
          showNotification("Warning: 'dbh' column contains invalid values (NA or <= 0)", type = "warning", duration = 10)
        }
        if (any(is.na(df$height)) || any(df$height <= 0, na.rm = TRUE)) {
          showNotification("Warning: 'height' column contains invalid values (NA or <= 0)", type = "warning", duration = 10)
        }

        return(df)
      }, error = function(e) {
        showNotification(paste("Error reading CSV:", e$message), type = "error", duration = 10)
        return(NULL)
      })
    } else {
      return(NULL)
    }
  })

  # CSV uploaded flag
  output$csv_uploaded <- reactive({
    !is.null(input$datafile)
  })
  outputOptions(output, "csv_uploaded", suspendWhenHidden = FALSE)

  # CSV Preview
  output$csv_preview <- DT::renderDataTable({
    df <- csv_data()
    if (!is.null(df) && nrow(df) > 0) {
      DT::datatable(head(df, 10),
                    options = list(pageLength = 5, scrollX = TRUE, dom = 't'),
                    caption = paste("Showing first", min(10, nrow(df)), "of", nrow(df), "rows"))
    }
  })

  # Template CSV download
  output$download_template <- downloadHandler(
    filename = "template_tree_data.csv",
    content = function(file) {
      template <- data.frame(
        name = c("Quercus robur", "Fagus sylvatica", "Pinus sylvestris", "Betula"),
        dbh = c(25, 30, 35, 20),
        height = c(15, 18, 20, 12),
        type = c("broadleaf", "broadleaf", "conifer", "broadleaf")
      )
      write.csv(template, file, row.names = FALSE)
    }
  )

  # Calculate button
  observeEvent(input$calculate, {
    req(input$calculate)

    # Clear previous warnings
    calculation_warnings$warnings <- character()

    # Environment to capture warnings
    warn_env <- new.env()
    warn_env$warnings <- character()

    withProgress(message = "Calculating...", value = 0, {

      newResult <- tryCatch({
        withCallingHandlers({

          if (!is.null(input$datafile)) {
            # CSV file processing
            incProgress(0.2, detail = "Reading CSV data...")
            tree_data <- csv_data()

            if (is.null(tree_data)) {
              showNotification("Error: Could not read CSV file", type = "error")
              return(NULL)
            }

            # Assign default values for optional columns
            if (!"type" %in% colnames(tree_data)) tree_data$type <- NA
            tree_data$type[!tree_data$type %in% c("broadleaf", "conifer")] <- NA

            incProgress(0.5, detail = paste("Calculating for", nrow(tree_data), "trees..."))

            # Calculate results
            fc_agc_error(
              name = tree_data$name,
              dbh = tree_data$dbh,
              height = tree_data$height,
              type = tree_data$type,
              method = input$method
            )
          } else {
            # Single tree calculation
            incProgress(0.5, detail = "Processing single tree...")

            type_val <- if (input$type == "NA") NA else input$type

            fc_agc_error(
              name = selected_name(),
              dbh = input$dbh,
              height = input$height,
              type = type_val,
              method = input$method
            )
          }
        }, warning = function(w) {
          warn_env$warnings <- c(warn_env$warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }, message = function(m) {
          warn_env$warnings <- c(warn_env$warnings, paste("Note:", conditionMessage(m)))
          invokeRestart("muffleMessage")
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        return(NULL)
      })

      incProgress(1, detail = "Done!")

      # Store warnings
      if (length(warn_env$warnings) > 0) {
        clean_warnings <- sapply(warn_env$warnings, strip_ansi, USE.NAMES = FALSE)
        calculation_warnings$warnings <- unique(clean_warnings)
      }

      # Append results
      if (!is.null(newResult)) {
        appData$results <- rbind(appData$results, newResult)
        n_trees <- if (!is.null(input$datafile)) nrow(csv_data()) else 1
        showNotification(paste("Successfully calculated for", n_trees, "tree(s)"), type = "message", duration = 3)
      }
    })
  })

  # Clear results
  observeEvent(input$clear, {
    appData$results <- data.frame()
    calculation_warnings$warnings <- character()
    showNotification("Results cleared", type = "message", duration = 2)
  })

  # Value Boxes
  output$total_trees <- renderValueBox({
    n <- if (nrow(appData$results) > 0) nrow(appData$results) else 0
    valueBox(n, "Total Trees", icon = icon("tree"), color = "green")
  })

  output$total_carbon <- renderValueBox({
    if (nrow(appData$results) > 0 && "AGC_WCC_t" %in% colnames(appData$results)) {
      total <- sum(appData$results$AGC_WCC_t, na.rm = TRUE)
      valueBox(round(total, 3), "Total Carbon (t)", icon = icon("leaf"), color = "blue")
    } else {
      valueBox(0, "Total Carbon (t)", icon = icon("leaf"), color = "blue")
    }
  })

  output$avg_carbon <- renderValueBox({
    if (nrow(appData$results) > 0 && "AGC_WCC_t" %in% colnames(appData$results)) {
      avg <- mean(appData$results$AGC_WCC_t, na.rm = TRUE)
      valueBox(round(avg, 4), "Avg Carbon/Tree (t)", icon = icon("chart-line"), color = "yellow")
    } else {
      valueBox(0, "Avg Carbon/Tree (t)", icon = icon("chart-line"), color = "yellow")
    }
  })

  # Warnings Panel
  output$warnings_panel <- renderUI({
    warnings <- calculation_warnings$warnings
    if (length(warnings) == 0) return(NULL)

    warning_items <- lapply(warnings, function(w) {
      tags$li(
        tags$span(icon("exclamation-triangle"), style = "color: #f39c12; margin-right: 8px;"),
        w
      )
    })

    fluidRow(
      box(title = paste(length(warnings), "Warning(s) from Last Calculation"),
          width = 12, status = "warning", solidHeader = TRUE,
          collapsible = TRUE, collapsed = FALSE,
          tags$ul(warning_items, style = "list-style-type: none; padding-left: 0;"),
          tags$p(
            tags$em("These warnings may indicate data quality issues. Review results carefully."),
            style = "color: #666; font-size: 0.9em; margin-top: 10px;"
          )
      )
    )
  })

  # Plot UI (interactive or static)
  output$plot_ui <- renderUI({
    if (nrow(appData$results) == 0) {
      return(div("No data to plot. Enter tree data and click 'Calculate'.",
                 style = "padding: 50px; text-align: center; color: #666;"))
    }

    if (input$interactive_plot && requireNamespace("plotly", quietly = TRUE)) {
      plotlyOutput("main_plot", height = "450px")
    } else {
      plotOutput("main_plot_static", height = "450px")
    }
  })

  # Create base plot
  create_plot <- reactive({
    req(nrow(appData$results) > 0)

    plot_data <- appData$results
    plot_data$ID <- seq_len(nrow(plot_data))

    # Color scheme
    colors <- switch(input$color_scheme,
                     "default" = c("AGC" = "skyblue", "stem" = "#1f77b4", "crown" = "#ff7f0e", "root" = "#2ca02c"),
                     "viridis" = c("AGC" = "#440154", "stem" = "#21918c", "crown" = "#fde725", "root" = "#5ec962"),
                     "colorblind" = c("AGC" = "#0072B2", "stem" = "#E69F00", "crown" = "#56B4E9", "root" = "#009E73"))

    if (input$plot_type == "carbon_bar") {
      # Bar chart of carbon
      p <- ggplot(plot_data, aes(x = factor(ID), y = AGC_WCC_t)) +
        geom_bar(stat = "identity", fill = colors["AGC"]) +
        labs(title = "Above Ground Carbon (AGC)", x = "Tree Index", y = "AGC (tonnes)") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      if (input$show_errors && "sig_AGC" %in% colnames(plot_data)) {
        p <- p + geom_errorbar(aes(ymin = AGC_WCC_t - sig_AGC, ymax = AGC_WCC_t + sig_AGC),
                               width = 0.3, color = "darkgray")
      }

    } else if (input$plot_type == "biomass_stack") {
      # Stacked biomass
      req(all(c("crownbiomass_t", "stembiomass_t", "rootbiomass_t") %in% colnames(plot_data)))

      library(tidyr)
      plot_data_long <- plot_data %>%
        pivot_longer(
          cols = c(crownbiomass_t, stembiomass_t, rootbiomass_t),
          names_to = "component",
          values_to = "biomass"
        )

      plot_data_long$component <- factor(plot_data_long$component,
                                         levels = c("rootbiomass_t", "stembiomass_t", "crownbiomass_t"),
                                         labels = c("Root", "Stem", "Crown"))

      p <- ggplot(plot_data_long, aes(x = factor(ID), y = biomass, fill = component)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = c("Root" = colors["root"], "Stem" = colors["stem"], "Crown" = colors["crown"])) +
        labs(title = "Biomass Components", x = "Tree Index", y = "Biomass (tonnes)", fill = "Component") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      if (input$show_errors) {
        # Calculate total biomass error
        if (all(c("sig_stembiomass", "sig_crownbiomass", "sig_rootbiomass") %in% colnames(plot_data))) {
          plot_data$total_biomass <- plot_data$crownbiomass_t + plot_data$stembiomass_t + plot_data$rootbiomass_t
          plot_data$total_sigma <- sqrt(plot_data$sig_stembiomass^2 + plot_data$sig_crownbiomass^2 + plot_data$sig_rootbiomass^2)

          p <- p + geom_errorbar(data = plot_data,
                                 aes(x = factor(ID),
                                     ymin = total_biomass - total_sigma,
                                     ymax = total_biomass + total_sigma),
                                 width = 0.3, color = "darkgray", inherit.aes = FALSE)
        }
      }

    } else if (input$plot_type == "scatter_dbh") {
      # Scatter by DBH
      size_by <- if (!is.null(input$point_size_by)) input$point_size_by else "none"

      if (size_by == "dbh" && "dbh" %in% colnames(plot_data)) {
        p <- ggplot(plot_data, aes(x = dbh, y = AGC_WCC_t, size = dbh,
                                   text = paste("Tree:", ID, "<br>DBH:", dbh, "cm<br>Carbon:", round(AGC_WCC_t, 4), "t"))) +
          geom_point(color = colors["AGC"], alpha = 0.7) +
          scale_size_continuous(range = c(2, 8), name = "DBH (cm)")
      } else if (size_by == "height" && "height" %in% colnames(plot_data)) {
        p <- ggplot(plot_data, aes(x = dbh, y = AGC_WCC_t, size = height,
                                   text = paste("Tree:", ID, "<br>DBH:", dbh, "cm<br>Height:", height, "m<br>Carbon:", round(AGC_WCC_t, 4), "t"))) +
          geom_point(color = colors["AGC"], alpha = 0.7) +
          scale_size_continuous(range = c(2, 8), name = "Height (m)")
      } else {
        p <- ggplot(plot_data, aes(x = dbh, y = AGC_WCC_t,
                                   text = paste("Tree:", ID, "<br>DBH:", dbh, "cm<br>Carbon:", round(AGC_WCC_t, 4), "t"))) +
          geom_point(size = 3, color = colors["AGC"], alpha = 0.7)
      }

      p <- p + labs(title = "Carbon vs DBH", x = "DBH (cm)", y = "AGC (tonnes)") +
        theme_minimal(base_size = 14)

      if (input$show_errors && "sig_AGC" %in% colnames(plot_data)) {
        p <- p + geom_errorbar(data = plot_data,
                               aes(x = dbh, ymin = AGC_WCC_t - sig_AGC, ymax = AGC_WCC_t + sig_AGC),
                               width = 1, color = "gray", alpha = 0.5, inherit.aes = FALSE)
      }

    } else if (input$plot_type == "scatter_height") {
      # Scatter by Height
      size_by <- if (!is.null(input$point_size_by)) input$point_size_by else "none"

      if (size_by == "dbh" && "dbh" %in% colnames(plot_data)) {
        p <- ggplot(plot_data, aes(x = height, y = AGC_WCC_t, size = dbh,
                                   text = paste("Tree:", ID, "<br>Height:", height, "m<br>DBH:", dbh, "cm<br>Carbon:", round(AGC_WCC_t, 4), "t"))) +
          geom_point(color = colors["AGC"], alpha = 0.7) +
          scale_size_continuous(range = c(2, 8), name = "DBH (cm)")
      } else if (size_by == "height" && "height" %in% colnames(plot_data)) {
        p <- ggplot(plot_data, aes(x = height, y = AGC_WCC_t, size = height,
                                   text = paste("Tree:", ID, "<br>Height:", height, "m<br>Carbon:", round(AGC_WCC_t, 4), "t"))) +
          geom_point(color = colors["AGC"], alpha = 0.7) +
          scale_size_continuous(range = c(2, 8), name = "Height (m)")
      } else {
        p <- ggplot(plot_data, aes(x = height, y = AGC_WCC_t,
                                   text = paste("Tree:", ID, "<br>Height:", height, "m<br>Carbon:", round(AGC_WCC_t, 4), "t"))) +
          geom_point(size = 3, color = colors["AGC"], alpha = 0.7)
      }

      p <- p + labs(title = "Carbon vs Height", x = "Height (m)", y = "AGC (tonnes)") +
        theme_minimal(base_size = 14)

      if (input$show_errors && "sig_AGC" %in% colnames(plot_data)) {
        p <- p + geom_errorbar(data = plot_data,
                               aes(x = height, ymin = AGC_WCC_t - sig_AGC, ymax = AGC_WCC_t + sig_AGC),
                               width = 0.5, color = "gray", alpha = 0.5, inherit.aes = FALSE)
      }
    }

    p
  })

  # Interactive plot
  output$main_plot <- renderPlotly({
    p <- create_plot()
    if (is.null(p)) return(NULL)

    if (input$plot_type %in% c("scatter_dbh", "scatter_height")) {
      ggplotly(p, tooltip = "text")
    } else {
      ggplotly(p)
    }
  })

  # Static plot
  output$main_plot_static <- renderPlot({
    create_plot()
  })

  # Results Table
  output$results_table <- DT::renderDataTable({
    if (nrow(appData$results) == 0) {
      return(data.frame(Message = "No data yet. Enter tree data and click 'Calculate'."))
    }

    if (input$output_all) {
      df <- appData$results
    } else {
      cols <- c("name", "dbh", "height", "AGC_WCC_t", "sig_AGC", "matchtype")
      avail_cols <- intersect(cols, colnames(appData$results))
      df <- appData$results[, avail_cols, drop = FALSE]
    }

    # Round numeric columns
    numeric_cols <- sapply(df, is.numeric)
    df[numeric_cols] <- round(df[numeric_cols], 4)

    DT::datatable(df,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })

  # Download handlers
  output$download_csv <- downloadHandler(
    filename = function() paste0("WCC_carbon_results_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(appData$results, file, row.names = FALSE)
    }
  )

  output$download_excel <- downloadHandler(
    filename = function() paste0("WCC_carbon_results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      if (requireNamespace("writexl", quietly = TRUE)) {
        writexl::write_xlsx(appData$results, file)
      } else {
        showNotification("Please install 'writexl' package for Excel export", type = "error")
        # Fallback to CSV
        write.csv(appData$results, file, row.names = FALSE)
      }
    }
  )

  # Plot download
  output$download_plot_png <- downloadHandler(
    filename = function() paste0("WCC_carbon_plot_", Sys.Date(), ".png"),
    content = function(file) {
      p <- create_plot()
      if (!is.null(p)) {
        ggsave(file, p, width = 10, height = 6, dpi = 300)
      }
    }
  )
}

# Run the app
shinyApp(ui, server)
