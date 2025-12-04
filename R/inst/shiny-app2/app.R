# App2 to compare different allometries - Improved Version
# Features: Interactive plots, comparison stats, export options, responsive design, etc.

library(shiny)
library(ggplot2)
library(TreeCarbon)

# Optional packages for enhanced features
if (requireNamespace("shinydashboard", quietly = TRUE)) {
  library(shinydashboard)
} else {
  stop("Please install 'shinydashboard' package: install.packages('shinydashboard')")
}
if (requireNamespace("plotly", quietly = TRUE)) {
  library(plotly)
} else {
  warning("plotly not installed. Interactive plots will not work.")
}
if (requireNamespace("DT", quietly = TRUE)) {
  library(DT)
} else {
  warning("DT not installed. Enhanced tables will not work.")
}
if (requireNamespace("shinyBS", quietly = TRUE)) {
  library(shinyBS)
} else {
  warning("shinyBS not installed. Tooltips will not work.")
}
if (requireNamespace("RColorBrewer", quietly = TRUE)) {
  library(RColorBrewer)
}
if (requireNamespace("scales", quietly = TRUE)) {
  library(scales)
}

# Helper function for tooltips
helpIcon <- function(id) {
  tags$span(
    shiny::icon("question-circle"),
    id = id,
    style = "cursor: pointer; color: #337ab7; margin-left: 5px; display: inline-block;"
  )
}

# Helper function to create input with inline help icon
inputWithHelp <- function(input_element, help_id, tooltip_text = NULL) {
  tags$div(
    style = "display: flex; align-items: center; gap: 5px;",
    tags$div(style = "flex: 1;", input_element),
    helpIcon(help_id),
    if (!is.null(tooltip_text) && requireNamespace("shinyBS", quietly = TRUE)) {
      shinyBS::bsTooltip(help_id, tooltip_text, placement = "right")
    }
  )
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Tree Carbon Allometry Comparison"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input & Calculation", tabName = "input", icon = icon("calculator")),
      menuItem("Results & Plots", tabName = "results", icon = icon("chart-line")),
      menuItem("Statistics", tabName = "stats", icon = icon("table")),
      menuItem("Method Info", tabName = "info", icon = icon("info-circle")),
      menuItem("Instructions", tabName = "instructions", icon = icon("book"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .help-icon { cursor: pointer; color: #337ab7; }
        .shiny-notification { position: fixed; top: calc(50% - 50px); left: calc(50% - 200px); }
        .tooltip-inner { white-space: nowrap; max-width: 500px; }
      "))
    ),
    tabItems(
      # Input Tab
      tabItem(tabName = "input",
              fluidRow(
                box(title = "Data", width = 6, status = "primary", solidHeader = TRUE,
                    fileInput("datafile", "Upload CSV file",
                              accept = c(".csv"),
                              placeholder = "Select CSV file"),
                    helpIcon("help_csv"),
                    if (requireNamespace("shinyBS", quietly = TRUE)) {
                      shinyBS::bsTooltip("help_csv", "CSV must have columns: genus, species, dbh (cm), height (m). Optional: type. Max 1000 rows, 10MB.",
                                         placement = "right")
                    },
                    downloadButton("download_template", "Download Template CSV", class = "btn-sm"),
                    hr(),
                    h5("OR Input Trees one at a time"),
                    tags$div(
                      style = "display: flex; align-items: center; gap: 5px;",
                      tags$div(style = "flex: 1;",
                               textInput("genus", "Genus", value = "Quercus")
                      ),
                      tags$div(style = "margin-top: 25px;",
                               helpIcon("help_genus_species"),
                               if (requireNamespace("shinyBS", quietly = TRUE)) {
                                 shinyBS::bsTooltip("help_genus_species", "Genus and species can be empty if using CSV")
                               }
                      )
                    ),
                    textInput("species", "Species", value = "robur"),
                    selectInput("type", "Type:",
                                choices = c("broadleaf", "conifer", "NA"),
                                selected = "NA"),
                    sliderInput("dbh", "DBH (cm):", min = 1, max = 150, value = 20, step = 1),
                    sliderInput("height", "Height (m):", min = 1, max = 60, value = 15, step = 1)
                ),
                box(title = "Parameters", width = 6, status = "primary", solidHeader = TRUE,
                    tags$div(
                      style = "display: flex; align-items: center; gap: 5px;",
                      tags$div(style = "flex: 1;",
                               selectInput("method", "Carbon Conversion Method:",
                                           choices = c("Thomas", "IPCC2", "Matthews1", "Matthews2", "IPCC1"),
                                           selected = "IPCC2")
                      ),
                      tags$div(style = "margin-top: 25px;",
                               helpIcon("help_method"),
                               if (requireNamespace("shinyBS", quietly = TRUE)) {
                                 shinyBS::bsTooltip("help_method", "Method for converting biomass to carbon. See Method Info tab for details.")
                               }
                      )
                    ),
                    selectInput("returnv", "Output Type:",
                                choices = c("Carbon" = "AGC", "Biomass" = "AGB"),
                                selected = "AGC"),
                    selectInput("biome", "Biome",
                                choices = c("temperate", "boreal", "mediterranean", "tropical", "subtropical"),
                                selected = "temperate"),
                    selectInput("region", "Region",
                                choices = c(
                                  "Africa (extratropical)" = "Africa (extratropical)",
                                  "Africa (tropical)" = "Africa (tropical)",
                                  "Australia" = "Australia",
                                  "Australia/PNG (tropical)" = "Australia/PNG (tropical)",
                                  "Central America (tropical)" = "Central America (tropical)",
                                  "China" = "China",
                                  "Europe" = "Europe",
                                  "India" = "India",
                                  "Madagascar" = "Madagascar",
                                  "Mexico" = "Mexico",
                                  "North America" = "NorthAmerica",
                                  "Oceania" = "Oceania",
                                  "South-East Asia" = "South-East Asia",
                                  "South-East Asia (tropical)" = "South-East Asia (tropical)",
                                  "South America (extratropical)" = "South America (extratropical)",
                                  "South America (tropical)" = "South America (tropical)",
                                  "World" = "World"
                                ), selected = "Europe"),
                    numericInput("longitude", "Longitude", value = -0.088837, step = 0.0001, min = -180, max = 180),
                    numericInput("latitude", "Latitude", value = 51.071610, step = 0.0001, min = -90, max = 90),
                    checkboxInput("checkTaxo", "Check taxonomy spelling", value = FALSE),
                    hr(),
                    h5("Advanced Options", style = "font-weight: bold;"),
                    if (requireNamespace("shinyBS", quietly = TRUE)) {
                      shinyBS::bsCollapse(
                        id = "advanced_collapse",
                        shinyBS::bsCollapsePanel(
                          title = "Error Propagation Parameters",
                          numericInput("re_dbh", "Relative error DBH (%)", value = 5, min = 0, max = 100, step = 0.1),
                          numericInput("re_h", "Relative error Height (%)", value = 10, min = 0, max = 100, step = 0.1),
                          numericInput("re", "Relative error coefficients (%)", value = 2.5, min = 0, max = 100, step = 0.1),
                          numericInput("sig_nsg", "Sigma NSG", value = 0.09413391, min = 0, step = 0.001),
                          textInput("nsg", "Nominal Specific Gravity (optional)", value = "", placeholder = "Leave empty for default")
                        )
                      )
                    } else {
                      wellPanel(
                        h6("Error Propagation Parameters"),
                        numericInput("re_dbh", "Relative error DBH (%)", value = 5, min = 0, max = 100, step = 0.1),
                        numericInput("re_h", "Relative error Height (%)", value = 10, min = 0, max = 100, step = 0.1),
                        numericInput("re", "Relative error coefficients (%)", value = 2.5, min = 0, max = 100, step = 0.1),
                        numericInput("sig_nsg", "Sigma NSG", value = 0.09413391, min = 0, step = 0.001),
                        textInput("nsg", "Nominal Specific Gravity (optional)", value = "", placeholder = "Leave empty for default")
                      )
                    },
                    hr(),
                    actionButton("calculate", "Calculate", class = "btn-primary btn-lg"),
                    actionButton("clear", "Clear Results", class = "btn-warning")
                )
              ),
              fluidRow(
                box(title = "CSV Preview", width = 12, status = "info", solidHeader = TRUE,
                    conditionalPanel(condition = "output.csv_uploaded",
                                     DT::dataTableOutput("csv_preview")
                    )
                )
              )
      ),
      # Results Tab
      tabItem(tabName = "results",
              fluidRow(
                valueBoxOutput("total_trees"),
                valueBoxOutput("total_carbon"),
                valueBoxOutput("avg_carbon")
              ),
              # Warnings panel - only shown when there are warnings
              uiOutput("warnings_panel"),
              fluidRow(
                box(title = "Plot Options", width = 3, status = "info", solidHeader = TRUE,
                    selectInput("plot_type", "Plot Type:",
                                choices = c("Bar graph (sum)" = "bar",
                                            "Scatter with index" = "tree_index",
                                            "Scatter with DBH" = "dbh",
                                            "Scatter with height" = "height"),
                                selected = "bar"),
                    hr(),
                    h5("Display Options", style = "font-weight: bold;"),
                    checkboxInput("show_errors", "Show error bars", value = FALSE),
                    checkboxInput("interactive_plot", "Interactive plot", value = FALSE),
                    conditionalPanel(
                      condition = "input.plot_type != 'bar' && input.plot_type != 'tree_index'",
                      checkboxInput("log_scale", "Log scale", value = FALSE)
                    ),
                    hr(),
                    conditionalPanel(
                      condition = "input.plot_type == 'dbh' || input.plot_type == 'height' || input.plot_type == 'tree_index'",
                      checkboxInput("jitter", "Jitter points", value = TRUE)
                    ),
                    conditionalPanel(
                      condition = "input.plot_type == 'tree_index' || input.plot_type == 'dbh' || input.plot_type == 'height'",
                      selectInput("size_scale", "Point size mapping:",
                                  choices = c("None" = "none",
                                              "Height" = "Height",
                                              "DBH" = "DBH"),
                                  selected = "none"),
                      conditionalPanel(
                        condition = "input.size_scale == 'none'",
                        numericInput("point_size", "Point size:", value = 2, min = 0.5, max = 10, step = 0.5)
                      )
                    ),
                    hr(),
                    if (requireNamespace("shinyBS", quietly = TRUE)) {
                      shinyBS::bsCollapse(
                        id = "appearance_collapse",
                        shinyBS::bsCollapsePanel(
                          title = "Appearance",
                          numericInput("font_size", "Font size", value = 12, min = 8, max = 20, step = 1),
                          selectInput("color_scheme", "Color scheme:",
                                      choices = c("Default" = "default",
                                                  "Viridis" = "viridis",
                                                  "ColorBrewer" = "brewer"),
                                      selected = "default"),
                          selectInput("plot_theme", "Plot theme:",
                                      choices = c("Minimal" = "minimal",
                                                  "Classic" = "classic",
                                                  "Gray" = "gray",
                                                  "Light" = "light",
                                                  "Dark" = "dark"),
                                      selected = "minimal")
                        )
                      )
                    } else {
                      wellPanel(
                        h6("Appearance"),
                        numericInput("font_size", "Font size", value = 12, min = 8, max = 20, step = 1),
                        selectInput("color_scheme", "Color scheme:",
                                    choices = c("Default" = "default",
                                                "Viridis" = "viridis",
                                                "ColorBrewer" = "brewer"),
                                    selected = "default"),
                        selectInput("plot_theme", "Plot theme:",
                                    choices = c("Minimal" = "minimal",
                                                "Classic" = "classic",
                                                "Gray" = "gray",
                                                "Light" = "light",
                                                "Dark" = "dark"),
                                    selected = "minimal")
                      )
                    },
                    hr(),
                    downloadButton("download_plot_png", "Download PNG", class = "btn-sm"),
                    downloadButton("download_plot_svg", "Download SVG", class = "btn-sm")
                ),
                box(title = uiOutput("plot_title"), width = 9, status = "primary", solidHeader = TRUE,
                    uiOutput("plot_ui")
                )
              ),
              fluidRow(
                box(title = "Data Filters & Plot Dimensions", width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    fluidRow(
                      column(3,
                             textInput("filter_genus", "Filter by Genus:", value = "", placeholder = "Leave empty for all")
                      ),
                      column(3,
                             textInput("filter_species", "Filter by Species:", value = "", placeholder = "Leave empty for all")
                      ),
                      column(3,
                             checkboxGroupInput("filter_methods", "Filter Methods:",
                                                choices = c("WCC" = "WCC", "Biomass" = "Biomass",
                                                            "Allodb" = "Allodb", "Bunce" = "Bunce"),
                                                selected = c("WCC", "Biomass", "Allodb", "Bunce"),
                                                inline = TRUE)
                      )
                    ),
                    hr(),
                    h5("Plot Dimensions", style = "font-weight: bold;"),
                    fluidRow(
                      column(3,
                             numericInput("plot_width", "Plot width (px)", value = NULL, min = 300, max = 2000, step = 50)
                      ),
                      column(3,
                             numericInput("plot_height", "Plot height (px)", value = 500, min = 300, max = 1000, step = 50)
                      )
                    ),
                    hr(),
                    h5("Axis Limits", style = "font-weight: bold;"),
                    fluidRow(
                      column(3,
                             numericInput("filter_x_min", "X-axis Min:", value = NA, step = 0.1)
                      ),
                      column(3,
                             numericInput("filter_x_max", "X-axis Max:", value = NA, step = 0.1)
                      ),
                      column(3,
                             numericInput("filter_y_min", "Y-axis Min:", value = NA, step = 0.1)
                      ),
                      column(3,
                             numericInput("filter_y_max", "Y-axis Max:", value = NA, step = 0.1)
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Results Table", width = 12, status = "primary", solidHeader = TRUE,
                    DT::dataTableOutput("results_table"),
                    br(),
                    downloadButton("download_csv", "Download CSV", class = "btn-primary"),
                    downloadButton("download_excel", "Download Excel", class = "btn-success"),
                    downloadButton("download_pdf", "Download PDF Report", class = "btn-info")
                )
              )
      ),
      # Statistics Tab
      tabItem(tabName = "stats",
              fluidRow(
                box(title = "Summary Statistics by Method", width = 6, status = "primary", solidHeader = TRUE,
                    DT::dataTableOutput("summary_stats")
                ),
                box(title = "Method Agreement", width = 6, status = "primary", solidHeader = TRUE,
                    DT::dataTableOutput("method_agreement")
                )
              ),
              fluidRow(
                box(title = "Mean Differences Matrix", width = 6, status = "info", solidHeader = TRUE,
                    DT::dataTableOutput("mean_differences")
                ),
                box(title = "Correlation Matrix", width = 6, status = "info", solidHeader = TRUE,
                    helpText("Note: Correlation matrix requires multiple trees for meaningful comparison. For single trees, correlation is always 1."),
                    DT::dataTableOutput("correlation_matrix")
                )
              )
      ),
      # Method Info Tab
      tabItem(tabName = "info",
              fluidRow(
                box(title = "Method Descriptions", width = 12, status = "primary", solidHeader = TRUE,
                    h4("WCC (Woodland Carbon Code)"),
                    p("UK Forestry Commission method for estimating tree carbon. Uses species-specific tariff numbers and volume equations."),
                    p(tags$strong("Reference:"), "Jenkins, Thomas AR, et al. 'FC Woodland Carbon Code: Carbon Assessment Protocol (v2. 0).' (2018)."),
                    hr(),
                    h4("BIOMASS (R Package)"),
                    p("Global allometric equations from Chave et al. Uses wood density and tree dimensions. Suitable for tropical and temperate forests."),
                    p(tags$strong("Reference:"), "Réjou-Méchain, M., Tanguy, A., Piponiot, C., Chave, J., & Hérault, B. (2017). BIOMASS: an R package for estimating above-ground biomass and its uncertainty in tropical forests. Methods in Ecology and Evolution, 8(9), 1163-1167."),
                    p(tags$strong("R Package:"), tags$a(href = "https://cran.r-project.org/package=BIOMASS", "BIOMASS", target = "_blank")),
                    hr(),
                    h4("Allodb (R Package)"),
                    p("Extratropical forest allometric database. Provides species-specific equations based on location."),
                    p(tags$strong("Reference:"), "Gonzalez-Akre, E., Piponiot, C., Lepore, M., & Anderson-Teixeira, K. (2020). allodb: An R package for biomass estimation at globally distributed extratropical forest plots. Methods in Ecology and Evolution, 11(10), 1273-1280."),
                    p(tags$strong("R Package:"), tags$a(href = "https://github.com/ropensci/allodb", "allodb", target = "_blank")),
                    hr(),
                    h4("Bunce"),
                    p("Simple power-law equation: biomass = exp(a + b * log(π * dbh)). Suitable for UK broadleaf species."),
                    p(tags$strong("Reference:"), "Bunce, R. G. H. 'Biomass and Production of Trees in a Mixed Deciduous Woodland: I. Girth and height as Parameters for the Estimation of Tree Dry Weight' (1968)."),
                    hr(),
                    h4("Carbon Conversion Methods"),
                    h5("Thomas & Martin (2012)"),
                    p("Biome and type-specific carbon fractions. Most detailed method."),
                    p(tags$strong("Reference:"), "Thomas, S.C., & Martin, A.R. (2012). Carbon content of tree tissues: A synthesis. Forests, 3(2), 332-352. doi:10.3390/f3020332"),
                    h5("IPCC2"),
                    p("IPCC default values by biome and tree type."),
                    p(tags$strong("Reference:"), "IPCC. (2006). Forest lands. Intergovernmental Panel on Climate Change Guidelines for National Greenhouse Gas Inventories, Volume 4, p. 83."),
                    h5("Matthews1"),
                    p("Simple 50% carbon fraction for all trees."),
                    p(tags$strong("Reference:"), "Matthews, G.A.R. (1993). The Carbon Content of Trees. Forestry Commission Technical Paper 4, Forestry Commission, Edinburgh, 21 pp. ISBN: 0-85538-317-8."),
                    h5("Matthews2"),
                    p("Type-specific carbon fractions (broadleaf vs conifer)."),
                    p(tags$strong("Reference:"), "Matthews, G.A.R. (1993). The Carbon Content of Trees. Forestry Commission Technical Paper 4, Forestry Commission, Edinburgh, 21 pp. ISBN: 0-85538-317-8."),
                    h5("IPCC1"),
                    p("Single default value: 47.7% carbon fraction."),
                    p(tags$strong("Reference:"), "IPCC. (2006). Forest lands. Intergovernmental Panel on Climate Change Guidelines for National Greenhouse Gas Inventories, Volume 4, p. 83.")
                )
              )
      ),
      # Instructions Tab
      tabItem(tabName = "instructions",
              fluidRow(
                box(title = "How to Use This App", width = 12, status = "primary", solidHeader = TRUE,
                    h4("1. Input Data"),
                    tags$ul(
                      tags$li("Upload a CSV file with columns: genus, species, dbh (cm), height (m). Optional: type"),
                      tags$li("Maximum file size: 10MB, Maximum rows: 1000 (to ensure app performance and prevent timeouts)"),
                      tags$li("OR enter single tree data manually"),
                      tags$li("Download template CSV for correct format")
                    ),
                    h4("2. Set Parameters"),
                    tags$ul(
                      tags$li("Choose carbon conversion method"),
                      tags$li("Select biome and region"),
                      tags$li("Enter coordinates (longitude, latitude)"),
                      tags$li("Adjust advanced error parameters if needed")
                    ),
                    h4("3. Calculate"),
                    tags$ul(
                      tags$li("Click 'Calculate' to process data"),
                      tags$li("View results in Results & Plots tab"),
                      tags$li("Check Statistics tab for method comparisons")
                    ),
                    h4("4. Export"),
                    tags$ul(
                      tags$li("Download results as CSV or Excel"),
                      tags$li("Export plots as PNG or SVG"),
                      tags$li("Generate PDF report with all results")
                    )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Add tooltips (if shinyBS is available)
  if (requireNamespace("shinyBS", quietly = TRUE)) {
    shinyBS::addTooltip(session, "help_csv", "CSV format help", placement = "right")
  }

  # Store cumulative data
  cumulative_data <- reactiveValues(data = NULL)

  # Store warnings from calculations
  calculation_warnings <- reactiveValues(warnings = character())

  # CSV size limit: 10MB, 1000 rows
  max_file_size <- 10 * 1024 * 1024  # 10MB
  max_rows <- 1000

  # CSV preview
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
        # Check row limit
        if (nrow(df) > max_rows) {
          showNotification(paste("Too many rows. Maximum is", max_rows, ". Showing first", max_rows),
                           type = "warning", duration = 5)
          df <- df[1:max_rows, ]
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

  output$csv_preview <- DT::renderDataTable({
    df <- csv_data()
    if (!is.null(df) && nrow(df) > 0) {
      DT::datatable(head(df, 10), options = list(pageLength = 5, scrollX = TRUE))
    }
  })

  output$csv_uploaded <- reactive({
    !is.null(input$datafile)
  })
  outputOptions(output, "csv_uploaded", suspendWhenHidden = FALSE)

  # Template CSV download
  output$download_template <- downloadHandler(
    filename = "template_tree_data.csv",
    content = function(file) {
      template <- data.frame(
        genus = c("Quercus", "Fagus", "Pinus"),
        species = c("robur", "sylvatica", "sylvestris"),
        dbh = c(25, 30, 35),
        height = c(15, 18, 20),
        type = c("broadleaf", "broadleaf", "conifer")
      )
      write.csv(template, file, row.names = FALSE)
    }
  )

  # Calculate results
  new_results <- eventReactive(input$calculate, {
    validate(
      need(input$dbh > 0, "DBH must be greater than 0"),
      need(input$height > 0, "Height must be greater than 0"),
      need(input$longitude >= -180 && input$longitude <= 180, "Longitude must be between -180 and 180"),
      need(input$latitude >= -90 && input$latitude <= 90, "Latitude must be between -90 and 90")
    )

    coords <- c(input$longitude, input$latitude)
    type_val <- if (input$type == "NA") NULL else input$type

    # Handle NSG input
    nsg_val <- if (input$nsg == "" || is.na(as.numeric(input$nsg))) NULL else as.numeric(input$nsg)

    if (!is.null(input$datafile)) {
      # CSV processing
      withProgress(message = "Processing CSV file...", value = 0, {
        df <- csv_data()
        if (is.null(df)) return(NULL)

        # Validate columns
        required_cols <- c("genus", "species", "dbh", "height")
        missing_cols <- setdiff(required_cols, colnames(df))
        if (length(missing_cols) > 0) {
          showNotification(paste("Missing columns:", paste(missing_cols, collapse = ", ")),
                           type = "error", duration = 10)
          return(NULL)
        }

        # Validate numeric columns
        if (!is.numeric(df$dbh) || any(df$dbh <= 0, na.rm = TRUE)) {
          showNotification("'dbh' must contain positive numeric values", type = "error", duration = 10)
          return(NULL)
        }
        if (!is.numeric(df$height) || any(df$height <= 0, na.rm = TRUE)) {
          showNotification("'height' must contain positive numeric values", type = "error", duration = 10)
          return(NULL)
        }

        # Handle type column - use from CSV if available, otherwise use user-selected type
        if ("type" %in% colnames(df)) {
          tree_types <- df$type
        } else {
          tree_types <- type_val
        }

        # Process all rows at once (vectorized) instead of row-by-row
        n_rows <- nrow(df)

        incProgress(0.5, detail = "Calculating allometries for all trees...")

        # Use environment to capture warnings reliably
        warn_env <- new.env()
        warn_env$warnings <- character()

        # Set warning option to capture all warnings
        old_warn <- getOption("warn")
        options(warn = 1)  # Immediate warnings

        result <- tryCatch(
          withCallingHandlers({
            allometries(genus = as.character(df$genus),
                        species = as.character(df$species),
                        dbh = df$dbh,
                        height = df$height,
                        type = tree_types,
                        method = input$method,
                        returnv = input$returnv,
                        region = input$region,
                        biome = input$biome,
                        coords = coords,
                        re_dbh = input$re_dbh / 100,
                        re_h = input$re_h / 100,
                        re = input$re / 100,
                        nsg = nsg_val,
                        sig_nsg = input$sig_nsg,
                        checkTaxo = input$checkTaxo)
          },
          warning = function(w) {
            warn_env$warnings <- c(warn_env$warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          },
          message = function(m) {
            warn_env$warnings <- c(warn_env$warnings, paste("Note:", conditionMessage(m)))
            invokeRestart("muffleMessage")
          }),
          error = function(e) {
            warn_env$warnings <- c(warn_env$warnings, paste("Error:", e$message))
            showNotification(paste("Error processing data:", e$message), type = "error", duration = 10)
            return(NULL)
          }
        )

        # Restore warning option
        options(warn = old_warn)

        # Store unique warnings and show notifications
        captured_warnings <- warn_env$warnings
        if (length(captured_warnings) > 0) {
          unique_warnings <- unique(captured_warnings)
          calculation_warnings$warnings <- unique_warnings
          # Show each unique warning as a notification
          for (w in unique_warnings) {
            showNotification(w, type = "warning", duration = 15)
          }
        } else {
          calculation_warnings$warnings <- character()
        }

        incProgress(1, detail = "Done!")

        if (!is.null(result)) {
          showNotification(paste("Successfully processed", n_rows, "tree(s)"), type = "message", duration = 3)
        }
        return(result)
      })
    } else {
      # Single tree
      withProgress(message = "Calculating...", value = 0, {
        incProgress(0.5)

        # Use environment to capture warnings reliably
        warn_env <- new.env()
        warn_env$warnings <- character()

        # Set warning option to capture all warnings
        old_warn <- getOption("warn")
        options(warn = 1)  # Immediate warnings

        result <- tryCatch(
          withCallingHandlers({
            allometries(genus = input$genus,
                        species = input$species,
                        dbh = input$dbh,
                        height = input$height,
                        type = type_val,
                        method = input$method,
                        returnv = input$returnv,
                        region = input$region,
                        biome = input$biome,
                        coords = coords,
                        re_dbh = input$re_dbh / 100,
                        re_h = input$re_h / 100,
                        re = input$re / 100,
                        nsg = nsg_val,
                        sig_nsg = input$sig_nsg,
                        checkTaxo = input$checkTaxo)
          },
          warning = function(w) {
            warn_env$warnings <- c(warn_env$warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          },
          message = function(m) {
            warn_env$warnings <- c(warn_env$warnings, paste("Note:", conditionMessage(m)))
            invokeRestart("muffleMessage")
          }),
          error = function(e) {
            warn_env$warnings <- c(warn_env$warnings, paste("Error:", e$message))
            showNotification(paste("Error:", e$message), type = "error", duration = 10)
            return(NULL)
          }
        )

        # Restore warning option
        options(warn = old_warn)

        # Store unique warnings and show notifications
        captured_warnings <- warn_env$warnings
        if (length(captured_warnings) > 0) {
          unique_warnings <- unique(captured_warnings)
          calculation_warnings$warnings <- unique_warnings
          for (w in unique_warnings) {
            showNotification(w, type = "warning", duration = 15)
          }
        } else {
          calculation_warnings$warnings <- character()
        }

        incProgress(1)
        if (!is.null(result)) {
          showNotification("Calculation complete!", type = "message", duration = 2)
        }
        return(result)
      })
    }
  })

  # Update cumulative data
  observeEvent(input$calculate, {
    new_data <- new_results()
    if (!is.null(new_data)) {
      if (is.null(cumulative_data$data)) {
        cumulative_data$data <- new_data
      } else {
        cumulative_data$data <- rbind(cumulative_data$data, new_data)
      }
    }
  })

  observeEvent(input$clear, {
    cumulative_data$data <- NULL
    calculation_warnings$warnings <- character()
  })

  # Current data
  # Base data (unfiltered)
  base_data <- reactive({
    if (is.null(cumulative_data$data)) {
      new_results()
    } else {
      cumulative_data$data
    }
  })

  # Filtered data based on user filters
  filtered_data <- reactive({
    df <- base_data()
    if (is.null(df) || nrow(df) == 0) return(df)

    # Filter by genus
    if (!is.null(input$filter_genus) && input$filter_genus != "") {
      df <- df[grepl(input$filter_genus, df$genus, ignore.case = TRUE), ]
    }

    # Filter by species
    if (!is.null(input$filter_species) && input$filter_species != "") {
      df <- df[grepl(input$filter_species, df$species, ignore.case = TRUE), ]
    }

    # Note: X/Y axis limits are handled in plot options, not as data filters

    # Filter by methods (keep only selected method columns)
    if (!is.null(input$filter_methods) && length(input$filter_methods) < 4) {
      method_map <- c("WCC" = if(input$returnv == "AGC") "WCC_C_t" else "WCC_B_t",
                      "Biomass" = if(input$returnv == "AGC") "biomass_C_t" else "biomass_B_t",
                      "Allodb" = if(input$returnv == "AGC") "allodb_C_t" else "allodb_B_t",
                      "Bunce" = if(input$returnv == "AGC") "Bunce_C_t" else "Bunce_B_t")
      cols_to_keep <- c("genus", "species", "dbh", "height", "family", "type")
      for (m in input$filter_methods) {
        if (method_map[m] %in% colnames(df)) {
          cols_to_keep <- c(cols_to_keep, method_map[m])
          # Also keep error columns if they exist
          err_col <- gsub("_t$", "_sig", method_map[m])
          if (err_col %in% colnames(df)) {
            cols_to_keep <- c(cols_to_keep, err_col)
          }
        }
      }
      df <- df[, colnames(df) %in% cols_to_keep, drop = FALSE]
    }

    df
  })

  # Use filtered_data as current_data for display
  current_data <- filtered_data

  output$has_data <- reactive({
    df <- current_data()
    !is.null(df) && is.data.frame(df) && nrow(df) > 0
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)

  # Warnings panel display
  output$warnings_panel <- renderUI({
    warnings <- calculation_warnings$warnings
    if (length(warnings) == 0) return(NULL)

    # Create a formatted list of warnings
    warning_items <- lapply(warnings, function(w) {
      tags$li(
        tags$span(icon("exclamation-circle"), style = "color: #f39c12; margin-right: 8px;"),
        w
      )
    })

    fluidRow(
      box(title = "Warnings & Notes", width = 12, status = "warning", solidHeader = TRUE,
          collapsible = TRUE, collapsed = FALSE,
          tags$div(
            tags$p(tags$strong(paste(length(warnings), "warning(s) from last calculation:")),
                   style = "margin-bottom: 10px;"),
            tags$ul(warning_items, style = "list-style-type: none; padding-left: 0;"),
            tags$hr(),
            tags$p(
              tags$em("These warnings indicate potential data quality issues. Results may still be valid, but review carefully."),
              style = "color: #666; font-size: 0.9em;"
            )
          )
      )
    )
  })

  # Summary boxes
  output$total_trees <- renderValueBox({
    df <- current_data()
    n <- if (is.null(df) || !is.data.frame(df)) 0 else nrow(df)
    valueBox(n, "Total Trees", icon = icon("tree"), color = "green")
  })

  output$total_carbon <- renderValueBox({
    df <- current_data()
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(valueBox(0, "Total (t)", icon = icon("calculator"), color = "blue"))
    }
    col_name <- if (input$returnv == "AGC") "WCC_C_t" else "WCC_B_t"
    if (col_name %in% colnames(df)) {
      total <- sum(df[[col_name]], na.rm = TRUE)
      valueBox(round(total, 2), "Total Carbon/Biomass (t)", icon = icon("calculator"), color = "blue")
    } else {
      valueBox(0, "Total Carbon/Biomass (t)", icon = icon("calculator"), color = "blue")
    }
  })

  output$avg_carbon <- renderValueBox({
    df <- current_data()
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(valueBox(0, "Average per Tree (t)", icon = icon("chart-line"), color = "yellow"))
    }
    col_name <- if (input$returnv == "AGC") "WCC_C_t" else "WCC_B_t"
    if (col_name %in% colnames(df)) {
      avg <- mean(df[[col_name]], na.rm = TRUE)
      valueBox(round(avg, 3), "Average per Tree (t)", icon = icon("chart-line"), color = "yellow")
    } else {
      valueBox(0, "Average per Tree (t)", icon = icon("chart-line"), color = "yellow")
    }
  })

  # Plot title (dynamic based on return type)
  output$plot_title <- renderUI({
    plot_type_label <- switch(input$plot_type,
                              "bar" = "Bar Plot",
                              "dbh" = "DBH vs",
                              "height" = "Height vs",
                              "tree_index" = "Tree Index vs",
                              "Plot")
    return_type_label <- if (input$returnv == "AGC") "Carbon" else "Biomass"
    title_text <- if (input$plot_type == "bar") {
      paste(plot_type_label, "-", return_type_label)
    } else {
      paste(plot_type_label, return_type_label)
    }
    return(h4(title_text))
  })

  # Plot
  output$plot_ui <- renderUI({
    df <- current_data()
    if (is.null(df) || nrow(df) == 0) {
      return(div("No data to plot. Click 'Calculate' to add data."))
    }

    if (input$interactive_plot && requireNamespace("plotly", quietly = TRUE)) {
      plotlyOutput("interactive_plot", height = paste0(input$plot_height, "px"))
    } else {
      plotOutput("static_plot", height = paste0(input$plot_height, "px"))
    }
  })

  output$interactive_plot <- renderPlotly({
    df <- current_data()
    if (is.null(df) || nrow(df) == 0) {
      if (requireNamespace("plotly", quietly = TRUE)) {
        return(plotly::layout(plotly::plotly_empty(), title = "No data to plot. Click 'Calculate' to add data."))
      }
      return(NULL)
    }
    # Convert empty/NA values to NULL for optional parameters
    x_min_val <- if (is.null(input$filter_x_min) || is.na(input$filter_x_min) || input$filter_x_min == "") NULL else input$filter_x_min
    x_max_val <- if (is.null(input$filter_x_max) || is.na(input$filter_x_max) || input$filter_x_max == "") NULL else input$filter_x_max
    y_min_val <- if (is.null(input$filter_y_min) || is.na(input$filter_y_min) || input$filter_y_min == "") NULL else input$filter_y_min
    y_max_val <- if (is.null(input$filter_y_max) || is.na(input$filter_y_max) || input$filter_y_max == "") NULL else input$filter_y_max
    size_scale_val <- if (is.null(input$size_scale) || input$size_scale == "none" ||
                          (input$plot_type != "dbh" && input$plot_type != "height" && input$plot_type != "tree_index")) {
      NULL
    } else {
      input$size_scale
    }
    theme_val <- switch(input$plot_theme,
                        "minimal" = ggplot2::theme_minimal,
                        "classic" = ggplot2::theme_classic,
                        "gray" = ggplot2::theme_gray,
                        "light" = ggplot2::theme_light,
                        "dark" = ggplot2::theme_dark,
                        ggplot2::theme_minimal)
    jitter_val <- if (input$plot_type == "dbh" || input$plot_type == "height" || input$plot_type == "tree_index") {
      if (is.null(input$jitter)) TRUE else input$jitter
    } else TRUE

    # Create plot object
    p <- plot_allometries(df, input$plot_type, input$returnv,
                          input$show_errors, TRUE,
                          log_scale = input$log_scale,
                          color_scheme = input$color_scheme,
                          font_size = input$font_size,
                          size_scale = size_scale_val,
                          theme = theme_val,
                          jitter = jitter_val,
                          point_size = input$point_size,
                          x_min = x_min_val, x_max = x_max_val,
                          y_min = y_min_val, y_max = y_max_val)

    if (is.null(p)) {
      if (requireNamespace("plotly", quietly = TRUE)) {
        return(plotly::layout(plotly::plotly_empty(), title = "No data available to plot"))
      }
      return(NULL)
    }

    # Capture warnings during plotly conversion/rendering
    plot_warnings <- character()
    result <- withCallingHandlers({
      # For plotly, the warnings happen during ggplotly build
      if (inherits(p, "plotly")) {
        p
      } else {
        # Build the plot to trigger any ggplot warnings
        ggplot2::ggplot_build(p)
        p
      }
    }, warning = function(w) {
      plot_warnings <<- c(plot_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

    # Show plot warnings as notifications
    if (length(plot_warnings) > 0) {
      unique_warnings <- unique(plot_warnings)
      # Append to existing warnings
      all_warnings <- c(calculation_warnings$warnings, paste("[Plot]", unique_warnings))
      calculation_warnings$warnings <- unique(all_warnings)
      for (w in unique_warnings) {
        showNotification(paste("Plot:", w), type = "warning", duration = 10)
      }
    }

    # Log scale is handled in plot_allometries function (only for scatter plots)
    p
  })

  output$static_plot <- renderPlot({
    df <- current_data()
    if (is.null(df) || nrow(df) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data to plot. Click 'Calculate' to add data.") +
               theme_void())
    }
    # Convert empty/NA values to NULL for optional parameters
    x_min_val <- if (is.null(input$filter_x_min) || is.na(input$filter_x_min) || input$filter_x_min == "") NULL else input$filter_x_min
    x_max_val <- if (is.null(input$filter_x_max) || is.na(input$filter_x_max) || input$filter_x_max == "") NULL else input$filter_x_max
    y_min_val <- if (is.null(input$filter_y_min) || is.na(input$filter_y_min) || input$filter_y_min == "") NULL else input$filter_y_min
    y_max_val <- if (is.null(input$filter_y_max) || is.na(input$filter_y_max) || input$filter_y_max == "") NULL else input$filter_y_max
    size_scale_val <- if (is.null(input$size_scale) || input$size_scale == "none" ||
                          (input$plot_type != "dbh" && input$plot_type != "height" && input$plot_type != "tree_index")) {
      NULL
    } else {
      input$size_scale
    }
    theme_val <- switch(input$plot_theme,
                        "minimal" = ggplot2::theme_minimal,
                        "classic" = ggplot2::theme_classic,
                        "gray" = ggplot2::theme_gray,
                        "light" = ggplot2::theme_light,
                        "dark" = ggplot2::theme_dark,
                        ggplot2::theme_minimal)
    jitter_val <- if (input$plot_type == "dbh" || input$plot_type == "height" || input$plot_type == "tree_index") {
      if (is.null(input$jitter)) TRUE else input$jitter
    } else TRUE

    # Create plot object first
    p <- plot_allometries(df, input$plot_type, input$returnv,
                          input$show_errors, FALSE,
                          log_scale = input$log_scale,
                          color_scheme = input$color_scheme,
                          font_size = input$font_size,
                          size_scale = size_scale_val,
                          theme = theme_val,
                          jitter = jitter_val,
                          point_size = input$point_size,
                          x_min = x_min_val, x_max = x_max_val,
                          y_min = y_min_val, y_max = y_max_val)

    if (is.null(p)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data available to plot") +
               theme_void())
    }

    # Capture warnings during plot BUILD (ggplot_build triggers the warnings)
    plot_warnings <- character()
    withCallingHandlers({
      # Build the plot - this triggers "Removed X rows" warnings
      ggplot2::ggplot_build(p)
    }, warning = function(w) {
      plot_warnings <<- c(plot_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

    # Show plot warnings as notifications
    if (length(plot_warnings) > 0) {
      unique_warnings <- unique(plot_warnings)
      # Append to existing warnings
      all_warnings <- c(calculation_warnings$warnings, paste("[Plot]", unique_warnings))
      calculation_warnings$warnings <- unique(all_warnings)
      for (w in unique_warnings) {
        showNotification(paste("Plot:", w), type = "warning", duration = 10)
      }
    }

    # Return the plot for Shiny to render
    p
  })

  # Plot downloads
  output$download_plot_png <- downloadHandler(
    filename = function() paste0("allometry_plot_", Sys.Date(), ".png"),
    content = function(file) {
      df <- current_data()
      # Convert empty/NA values to NULL for optional parameters
      x_min_val <- if (is.null(input$filter_x_min) || is.na(input$filter_x_min) || input$filter_x_min == "") NULL else input$filter_x_min
      x_max_val <- if (is.null(input$filter_x_max) || is.na(input$filter_x_max) || input$filter_x_max == "") NULL else input$filter_x_max
      y_min_val <- if (is.null(input$filter_y_min) || is.na(input$filter_y_min) || input$filter_y_min == "") NULL else input$filter_y_min
      y_max_val <- if (is.null(input$filter_y_max) || is.na(input$filter_y_max) || input$filter_y_max == "") NULL else input$filter_y_max
      size_scale_val <- if (is.null(input$size_scale) || input$size_scale == "none" ||
                            (input$plot_type != "dbh" && input$plot_type != "height" && input$plot_type != "tree_index")) {
        NULL
      } else {
        input$size_scale
      }
      theme_val <- switch(input$plot_theme,
                          "minimal" = ggplot2::theme_minimal,
                          "classic" = ggplot2::theme_classic,
                          "gray" = ggplot2::theme_gray,
                          "light" = ggplot2::theme_light,
                          "dark" = ggplot2::theme_dark,
                          ggplot2::theme_minimal)
      jitter_val <- if (input$plot_type == "dbh" || input$plot_type == "height") {
        if (is.null(input$jitter)) TRUE else input$jitter
      } else TRUE

      p <- plot_allometries(df, input$plot_type, input$returnv,
                            input$show_errors, FALSE,
                            log_scale = input$log_scale,
                            color_scheme = input$color_scheme,
                            font_size = input$font_size,
                            size_scale = size_scale_val,
                            theme = theme_val,
                            jitter = jitter_val,
                            point_size = input$point_size,
                            x_min = x_min_val, x_max = x_max_val,
                            y_min = y_min_val, y_max = y_max_val)
      plot_width_in <- if (!is.null(input$plot_width) && input$plot_width > 0) input$plot_width / 100 else 10
      plot_height_in <- input$plot_height / 100
      ggsave(file, p, width = plot_width_in, height = plot_height_in, dpi = 300)
    }
  )

  output$download_plot_svg <- downloadHandler(
    filename = function() paste0("allometry_plot_", Sys.Date(), ".svg"),
    content = function(file) {
      df <- current_data()
      # Convert empty/NA values to NULL for optional parameters
      x_min_val <- if (is.null(input$filter_x_min) || is.na(input$filter_x_min) || input$filter_x_min == "") NULL else input$filter_x_min
      x_max_val <- if (is.null(input$filter_x_max) || is.na(input$filter_x_max) || input$filter_x_max == "") NULL else input$filter_x_max
      y_min_val <- if (is.null(input$filter_y_min) || is.na(input$filter_y_min) || input$filter_y_min == "") NULL else input$filter_y_min
      y_max_val <- if (is.null(input$filter_y_max) || is.na(input$filter_y_max) || input$filter_y_max == "") NULL else input$filter_y_max
      size_scale_val <- if (is.null(input$size_scale) || input$size_scale == "none" ||
                            (input$plot_type != "dbh" && input$plot_type != "height" && input$plot_type != "tree_index")) {
        NULL
      } else {
        input$size_scale
      }
      theme_val <- switch(input$plot_theme,
                          "minimal" = ggplot2::theme_minimal,
                          "classic" = ggplot2::theme_classic,
                          "gray" = ggplot2::theme_gray,
                          "light" = ggplot2::theme_light,
                          "dark" = ggplot2::theme_dark,
                          ggplot2::theme_minimal)
      jitter_val <- if (input$plot_type == "dbh" || input$plot_type == "height") {
        if (is.null(input$jitter)) TRUE else input$jitter
      } else TRUE

      p <- plot_allometries(df, input$plot_type, input$returnv,
                            input$show_errors, FALSE,
                            log_scale = input$log_scale,
                            color_scheme = input$color_scheme,
                            font_size = input$font_size,
                            size_scale = size_scale_val,
                            theme = theme_val,
                            jitter = jitter_val,
                            point_size = input$point_size,
                            x_min = x_min_val, x_max = x_max_val,
                            y_min = y_min_val, y_max = y_max_val)
      plot_width_in <- if (!is.null(input$plot_width) && input$plot_width > 0) input$plot_width / 100 else 10
      plot_height_in <- input$plot_height / 100
      ggsave(file, p, width = plot_width_in, height = plot_height_in)
    }
  )

  # Results table
  output$results_table <- DT::renderDataTable({
    df <- current_data()
    if (is.null(df) || nrow(df) == 0) {
      return(data.frame(Message = "No data. Click 'Calculate' to add data."))
    }
    basic_cols <- c("genus", "species", "dbh", "height")
    if (input$returnv == "AGC") {
      carbon_cols <- colnames(df)[grepl("_C_t$|_C_sig$", colnames(df))]
    } else {
      carbon_cols <- colnames(df)[grepl("_B_t$|_B_sig$", colnames(df))]
    }
    avail_cols <- intersect(c(basic_cols, carbon_cols), colnames(df))
    if (length(avail_cols) > 0) {
      result <- df[, avail_cols, drop = FALSE]
      numeric_cols <- sapply(result, is.numeric)
      result[numeric_cols] <- round(result[numeric_cols], 4)
      DT::datatable(result, options = list(pageLength = 10, scrollX = TRUE))
    } else {
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
    }
  })

  # Comparison statistics
  comparison_stats <- reactive({
    df <- current_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    compare_methods(df, input$returnv)
  })

  output$summary_stats <- DT::renderDataTable({
    stats <- comparison_stats()
    if (is.null(stats)) return(data.frame(Message = "No statistics available"))
    # Round to 4 decimal places
    stats$summary_stats$Value <- round(stats$summary_stats$Value, 4)
    DT::datatable(stats$summary_stats, options = list(pageLength = 10))
  })

  output$method_agreement <- DT::renderDataTable({
    stats <- comparison_stats()
    if (is.null(stats)) return(data.frame(Message = "No statistics available"))
    # Round to 4 decimal places
    stats$method_agreement$Mean_Abs_Diff <- round(stats$method_agreement$Mean_Abs_Diff, 4)
    stats$method_agreement$Correlation <- round(stats$method_agreement$Correlation, 4)
    DT::datatable(stats$method_agreement, options = list(pageLength = 10, digits = 4))
  })

  output$mean_differences <- DT::renderDataTable({
    stats <- comparison_stats()
    if (is.null(stats)) return(data.frame(Message = "No statistics available"))
    # Round to 4 decimal places and convert to data frame
    diff_df <- as.data.frame(round(stats$mean_differences, 4))
    DT::datatable(diff_df, options = list(pageLength = 10, digits = 4))
  })

  output$correlation_matrix <- DT::renderDataTable({
    stats <- comparison_stats()
    if (is.null(stats)) return(data.frame(Message = "No statistics available"))
    # Note: Correlation matrix is often = 1 for single tree comparisons, less informative
    # Only show if we have multiple trees
    df <- current_data()
    if (is.null(df) || nrow(df) <= 1) {
      return(data.frame(Note = "Correlation matrix requires multiple trees for meaningful comparison"))
    }
    cor_df <- as.data.frame(round(stats$correlation_matrix, 4))
    DT::datatable(cor_df, options = list(pageLength = 10, digits = 4))
  })


  # Export functions
  output$download_csv <- downloadHandler(
    filename = function() paste0("allometry_results_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- current_data()
      if (is.null(df)) df <- new_results()
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_excel <- downloadHandler(
    filename = function() paste0("allometry_results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      if (!requireNamespace("writexl", quietly = TRUE)) {
        showNotification("Please install 'writexl' package for Excel export", type = "error")
        return()
      }
      df <- current_data()
      if (is.null(df)) df <- new_results()
      writexl::write_xlsx(df, file)
    }
  )

  output$download_pdf <- downloadHandler(
    filename = function() paste0("allometry_report_", Sys.Date(), ".pdf"),
    content = function(file) {
      if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        showNotification("Please install 'rmarkdown' package for PDF export", type = "error")
        return()
      }
      # Create temporary Rmd file
      temp_rmd <- tempfile(fileext = ".Rmd")
      writeLines(c(
        "---",
        "title: 'Allometry Comparison Report'",
        "output: pdf_document",
        "---",
        "",
        "## Summary",
        "",
        "Total trees: `r nrow(df)`",
        "",
        "## Results",
        "",
        "```{r, echo=FALSE}",
        "knitr::kable(df)",
        "```"
      ), temp_rmd)

      df <- current_data()
      if (is.null(df)) df <- new_results()

      # This would need a proper Rmd template - simplified for now
      showNotification("PDF export requires rmarkdown template. CSV export recommended.", type = "warning")
    }
  )
}

shinyApp(ui, server)

