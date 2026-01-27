# App2 to compare different allometries
# Features: Interactive plots, comparison stats, export options, responsive design,
#           sensitivity analysis, Monte Carlo uncertainty estimation

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
      menuItem("Carbon per Area", tabName = "per_area", icon = icon("map")),
      menuItem("Sensitivity Analysis", tabName = "sensitivity", icon = icon("balance-scale")),
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
        .fa-question-circle {
          color: #6c757d;
          cursor: help;
          margin-left: 5px;
          font-size: 0.9em;
        }
        .fa-question-circle:hover { color: #007bff; }
        .box-title .fa-question-circle { font-weight: normal; }
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
                          title = "WCC Error Propagation",
                          numericInput("re_dbh", "Relative error DBH (%)", value = 5, min = 0, max = 100, step = 0.1),
                          numericInput("re_h", "Relative error Height (%)", value = 10, min = 0, max = 100, step = 0.1),
                          numericInput("re", "Relative error coefficients (%)", value = 2.5, min = 0, max = 100, step = 0.1),
                          numericInput("sig_nsg", "Sigma NSG", value = 0.09413391, min = 0, step = 0.001),
                          textInput("nsg", "Nominal Specific Gravity (optional)", value = "", placeholder = "Leave empty for default")
                        ),
                        shinyBS::bsCollapsePanel(
                          title = "BIOMASS Monte Carlo Uncertainty",
                          checkboxInput("biomass_uncertainty", "Enable Monte Carlo uncertainty", value = FALSE),
                          helpIcon("help_biomass_mc"),
                          if (requireNamespace("shinyBS", quietly = TRUE)) {
                            shinyBS::bsTooltip("help_biomass_mc",
                              "Uses AGBmonteCarlo to propagate DBH, height, and wood density errors")
                          },
                          conditionalPanel(
                            condition = "input.biomass_uncertainty == true",
                            numericInput("n_mc", "Monte Carlo iterations", value = 1000, min = 100, max = 10000, step = 100),
                            numericInput("errH", "Height error (%)", value = 5, min = 0, max = 50, step = 1),
                            helpText("Height error as % of measured height (e.g., 5 = ±5%)")
                          )
                        )
                      )
                    } else {
                      wellPanel(
                        h6("WCC Error Propagation Parameters"),
                        numericInput("re_dbh", "Relative error DBH (%)", value = 5, min = 0, max = 100, step = 0.1),
                        numericInput("re_h", "Relative error Height (%)", value = 10, min = 0, max = 100, step = 0.1),
                        numericInput("re", "Relative error coefficients (%)", value = 2.5, min = 0, max = 100, step = 0.1),
                        numericInput("sig_nsg", "Sigma NSG", value = 0.09413391, min = 0, step = 0.001),
                        textInput("nsg", "Nominal Specific Gravity (optional)", value = "", placeholder = "Leave empty for default"),
                        hr(),
                        h6("BIOMASS Monte Carlo Uncertainty"),
                        checkboxInput("biomass_uncertainty", "Enable Monte Carlo uncertainty", value = FALSE),
                        conditionalPanel(
                          condition = "input.biomass_uncertainty == true",
                          numericInput("n_mc", "Monte Carlo iterations", value = 1000, min = 100, max = 10000, step = 100),
                          numericInput("errH", "Height error (%)", value = 5, min = 0, max = 50, step = 1)
                        )
                      )
                    },
                    hr(),
                    checkboxInput("calc_stats", "Calculate comparison statistics", value = FALSE),
                    helpText("Compare estimates across allometric methods (WCC, BIOMASS, allodb, Bunce)"),
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
              fluidRow(
                box(title = "Plot Options", width = 3, status = "info", solidHeader = TRUE,
                    selectInput("plot_type", "Plot Type:",
                                choices = c("Bar graph (sum)" = "bar",
                                            "Boxplot by method" = "boxplot",
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
                                                  "Colorblind-friendly" = "colorblind"),
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
              # Warnings panel - shown below the graph when there are warnings
              uiOutput("warnings_panel"),
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
              conditionalPanel(
                condition = "!input.calc_stats",
                fluidRow(
                  box(title = "Statistics Not Enabled", width = 12, status = "warning", solidHeader = TRUE,
                      p("To view comparison statistics, enable 'Calculate comparison statistics' in the Input & Calculation tab and click Calculate.")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.calc_stats",
                fluidRow(
                  box(title = "Filter Methods for Comparison", width = 12, status = "warning", solidHeader = TRUE,
                      checkboxGroupInput("stats_filter_methods", "Select methods to compare:",
                                         choices = c("WCC" = "WCC", "BIOMASS" = "BIOMASS",
                                                     "allodb" = "allodb", "Bunce" = "Bunce"),
                                         selected = c("WCC", "BIOMASS", "allodb", "Bunce"),
                                         inline = TRUE)
                  )
                ),
                fluidRow(
                  box(title = tags$span("Method Comparison Boxplot ",
                                        tags$i(class = "fa fa-question-circle",
                                               title = "Shows the distribution of estimates from each allometric method. Compare medians (middle line), spread (box height = IQR), and outliers (points). Methods with similar distributions produce comparable estimates.")),
                      width = 12, status = "primary", solidHeader = TRUE,
                      helpText("Distribution of carbon/biomass estimates across all trees for each allometric method. Wider boxes indicate more variability."),
                      plotOutput("method_boxplot", height = "400px")
                  )
                ),
                fluidRow(
                  box(title = tags$span("Bland-Altman Plots ",
                                        tags$i(class = "fa fa-question-circle",
                                               title = "Bland-Altman plots visualize agreement between two methods. X-axis: mean of both methods. Y-axis: difference between methods. The blue line shows bias (systematic difference). Red dashed lines show 95% limits of agreement. Points should scatter randomly around zero if methods agree well.")),
                      width = 12, status = "success", solidHeader = TRUE,
                      helpText(tags$strong("How to interpret:"), " Each point is one tree. The blue line = mean bias. Red dashed lines = 95% limits of agreement (\u00B11.96 SD). Good agreement: points scattered evenly around zero, narrow limits."),
                      helpText(tags$strong("Warning signs:"), " Trend in points = proportional bias (methods disagree more at higher/lower values). Wide limits = poor reliability."),
                      uiOutput("ba_comparison_ui"),
                      plotOutput("bland_altman_plot", height = "400px")
                  )
                ),
                fluidRow(
                  box(title = tags$span("Bland-Altman Metrics ",
                                        tags$i(class = "fa fa-question-circle",
                                               title = "Bias: Average difference between methods (positive = Method 1 higher). Limits of Agreement (LoA): 95% of differences fall within these bounds. Narrower LoA = better agreement. % Within LoA: Should be ~95% if data is normally distributed.")),
                      width = 12, status = "success", solidHeader = TRUE,
                      helpText(tags$strong("Bias:"), " Mean difference between methods. Ideally close to 0."),
                      helpText(tags$strong("Lower/Upper LoA:"), " 95% limits of agreement (Bias \u00B1 1.96\u00D7SD). Narrower = more consistent agreement."),
                      DT::dataTableOutput("bland_altman_table")
                  )
                ),
                fluidRow(
                  box(title = tags$span("Normalized Differences ",
                                        tags$i(class = "fa fa-question-circle",
                                               title = "Normalized difference scales the comparison relative to estimate magnitude: (Method1 - Method2)/(Method1 + Method2). Values range -1 to +1. Near 0 = good agreement. Useful when comparing trees of different sizes.")),
                      width = 12, status = "info", solidHeader = TRUE,
                      helpText("Normalized difference: (AGB\u2081 - AGB\u2082) / (AGB\u2081 + AGB\u2082). Values range from -1 to 1, where 0 = perfect agreement."),
                      helpText("Reference: Neumann et al. (2022) European Journal of Forest Research"),
                      plotOutput("normalized_diff_plot", height = "400px")
                  )
                ),
                fluidRow(
                  box(title = tags$span("Normalized Differences Summary ",
                                        tags$i(class = "fa fa-question-circle",
                                               title = "Mean: Average normalized difference (0 = no systematic bias). SD: Spread of differences (lower = more consistent). Interpretation thresholds: <0.05 good, 0.05-0.15 moderate, >0.15 large difference.")),
                      width = 12, status = "info", solidHeader = TRUE,
                      helpText("Mean and SD of normalized differences for each method pair. Interpretation: |Mean| < 0.05 = Good, < 0.15 = Moderate, \u2265 0.15 = Large difference."),
                      DT::dataTableOutput("normalized_diff_table")
                  )
                )
              )
      ),
      # Carbon per Area Tab
      tabItem(tabName = "per_area",
              fluidRow(
                box(title = "Carbon per Unit Area", width = 12, status = "primary", solidHeader = TRUE,
                    p("Calculate carbon density (carbon per unit area) for your tree data using the ",
                      tags$code("summary_per_area()"), " function."),
                    p("This is useful for reporting carbon stocks at the plot, stand, or landscape level."),
                    hr(),
                    fluidRow(
                      column(4,
                             numericInput("plot_area", "Plot/Stand Area:", value = 1, min = 0.001, step = 0.1),
                             selectInput("area_unit", "Area Unit:",
                                         choices = c("hectares (ha)" = "ha",
                                                     "square metres (m²)" = "m2",
                                                     "acres" = "acres",
                                                     "square kilometres (km²)" = "km2"),
                                         selected = "ha"),
                             helpText("Enter the total area surveyed. Carbon will be scaled to this area.")
                      ),
                      column(4,
                             selectInput("per_area_methods", "Methods to Include:",
                                         choices = c("WCC" = "WCC", "BIOMASS" = "BIOMASS",
                                                     "allodb" = "allodb", "Bunce" = "Bunce"),
                                         selected = c("WCC", "BIOMASS", "allodb", "Bunce"),
                                         multiple = TRUE),
                             checkboxInput("per_area_include_error", "Include uncertainty estimates", value = TRUE),
                             helpText("Calculate error propagation for carbon density")
                      ),
                      column(4,
                             selectInput("output_unit", "Output Carbon Unit:",
                                         choices = c("tonnes C / ha" = "t_ha",
                                                     "kg C / ha" = "kg_ha",
                                                     "tonnes C / acre" = "t_acre",
                                                     "Mg C / ha" = "Mg_ha"),
                                         selected = "t_ha"),
                             br(),
                             actionButton("calc_per_area", "Calculate Carbon Density",
                                          class = "btn-primary btn-lg", style = "margin-top: 10px;")
                      )
                    )
                )
              ),
              fluidRow(
                valueBoxOutput("total_carbon_per_area"),
                valueBoxOutput("carbon_density_wcc"),
                valueBoxOutput("method_range_per_area")
              ),
              fluidRow(
                box(title = "Carbon Density by Method", width = 6, status = "success", solidHeader = TRUE,
                    plotOutput("per_area_bar_plot", height = "400px")
                ),
                box(title = "Method Comparison", width = 6, status = "info", solidHeader = TRUE,
                    DT::dataTableOutput("per_area_table")
                )
              ),
              fluidRow(
                box(title = "Summary Statistics", width = 12, status = "warning", solidHeader = TRUE,
                    verbatimTextOutput("per_area_summary"),
                    hr(),
                    fluidRow(
                      column(6,
                             downloadButton("download_per_area_csv", "Download Results (CSV)", class = "btn-success")
                      ),
                      column(6,
                             downloadButton("download_per_area_report", "Download Report (TXT)", class = "btn-info")
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Unit Conversion Reference", width = 12, status = "info", solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    tags$table(class = "table table-striped table-sm",
                               tags$thead(
                                 tags$tr(tags$th("From"), tags$th("To"), tags$th("Multiply by"))
                               ),
                               tags$tbody(
                                 tags$tr(tags$td("hectares"), tags$td("m²"), tags$td("10,000")),
                                 tags$tr(tags$td("hectares"), tags$td("acres"), tags$td("2.471")),
                                 tags$tr(tags$td("km²"), tags$td("hectares"), tags$td("100")),
                                 tags$tr(tags$td("tonnes C"), tags$td("tonnes CO₂e"), tags$td("3.67")),
                                 tags$tr(tags$td("tonnes"), tags$td("Mg (megagrams)"), tags$td("1 (equivalent)")),
                                 tags$tr(tags$td("tonnes"), tags$td("kg"), tags$td("1,000"))
                               )
                    )
                )
              )
      ),
      # Sensitivity Analysis Tab
      tabItem(tabName = "sensitivity",
              fluidRow(
                box(title = "Sensitivity Analysis: How Sensitive is Carbon to Method Choice?",
                    width = 12, status = "primary", solidHeader = TRUE,
                    p("This analysis quantifies how much your carbon estimate varies depending on which allometric method you use."),
                    p(tags$strong("Key question:"), " If I report carbon using method X, how different would my estimate be using method Y?"),
                    hr(),
                    fluidRow(
                      column(4,
                             selectInput("sens_reference", "Reference Method:",
                                         choices = c("WCC", "BIOMASS", "allodb", "Bunce"),
                                         selected = "WCC"),
                             helpText("Compare all methods against this reference")
                      ),
                      column(4,
                             checkboxGroupInput("sens_methods", "Methods to Compare:",
                                                choices = c("WCC" = "WCC", "BIOMASS" = "BIOMASS",
                                                            "allodb" = "allodb", "Bunce" = "Bunce"),
                                                selected = c("WCC", "BIOMASS", "allodb", "Bunce"),
                                                inline = FALSE)
                      ),
                      column(4,
                             actionButton("run_sensitivity", "Run Sensitivity Analysis",
                                         class = "btn-primary btn-lg", style = "margin-top: 25px;"),
                             br(), br(),
                             checkboxInput("sens_aggregate", "Aggregate results (total)", value = TRUE),
                             helpText("Uncheck for per-tree sensitivity"),
                             hr(),
                             checkboxInput("sens_show_tree_dist", "Show per-tree CV distribution", value = TRUE),
                             helpText("Visualize which trees are most sensitive to method choice")
                      )
                    )
                )
              ),
              fluidRow(
                valueBoxOutput("sens_range_ratio"),
                valueBoxOutput("sens_cv"),
                valueBoxOutput("sens_level")
              ),
              fluidRow(
                box(title = "Method Comparison Plot", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("sensitivity_comparison_plot", height = "400px")
                ),
                box(title = "Deviation from Reference", width = 6, status = "success", solidHeader = TRUE,
                    plotOutput("sensitivity_deviation_plot", height = "400px")
                )
              ),
              # Per-tree CV distribution (Option A) - conditional on checkbox
              conditionalPanel(
                condition = "input.sens_show_tree_dist == true",
                fluidRow(
                  box(title = "Per-Tree Sensitivity Distribution (Option A)", width = 12,
                      status = "info", solidHeader = TRUE,
                      p("This shows how sensitivity to method choice varies across individual trees. ",
                        "Trees with higher CV have more disagreement between allometric methods."),
                      fluidRow(
                        column(6,
                               plotOutput("tree_cv_histogram", height = "350px")
                        ),
                        column(6,
                               plotOutput("tree_cv_vs_dbh", height = "350px")
                        )
                      ),
                      hr(),
                      fluidRow(
                        column(4,
                               valueBoxOutput("sens_tree_median_cv", width = 12)
                        ),
                        column(4,
                               valueBoxOutput("sens_tree_high_cv_pct", width = 12)
                        ),
                        column(4,
                               valueBoxOutput("sens_tree_low_cv_pct", width = 12)
                        )
                      ),
                      helpText("Interpretation: Trees with CV > 25% are highly sensitive to method choice. ",
                               "Large trees often show higher absolute differences but may have similar relative (%) sensitivity.")
                  )
                )
              ),
              fluidRow(
                box(title = "Sensitivity Summary", width = 6, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("sensitivity_summary")
                ),
                box(title = "Interpretation Guide", width = 6, status = "warning", solidHeader = TRUE,
                    h5("Sensitivity Levels", style = "font-weight: bold;"),
                    tags$table(class = "table table-striped table-sm",
                               tags$thead(
                                 tags$tr(
                                   tags$th("CV (%)"), tags$th("Level"), tags$th("Interpretation")
                                 )
                               ),
                               tags$tbody(
                                 tags$tr(tags$td("< 10"), tags$td(tags$span(class = "label label-success", "LOW")),
                                         tags$td("Method choice has minimal impact")),
                                 tags$tr(tags$td("10-25"), tags$td(tags$span(class = "label label-info", "MODERATE")),
                                         tags$td("Typical variation; report range")),
                                 tags$tr(tags$td("25-50"), tags$td(tags$span(class = "label label-warning", "HIGH")),
                                         tags$td("Significant impact; justify method choice")),
                                 tags$tr(tags$td("> 50"), tags$td(tags$span(class = "label label-danger", "VERY HIGH")),
                                         tags$td("Investigate data quality/applicability"))
                               )
                    ),
                    hr(),
                    h5("Key Metric: Range Ratio", style = "font-weight: bold; color: #605ca8;"),
                    p("The ", tags$strong("Range Ratio (max/min)"), " is the most robust sensitivity metric. ",
                      "A ratio of 1.5x means the highest method gives 50% more than the lowest."),
                    hr(),
                    h5("Statistical Note", style = "font-weight: bold;"),
                    p(style = "font-size: 0.9em; color: #666;",
                      "These 4 methods (WCC, BIOMASS, allodb, Bunce) are ", tags$em("fixed, commonly-used approaches"),
                      " - not random samples from a population of methods. The CV describes how much ",
                      tags$em("these specific methods"), " disagree, not sampling uncertainty. ",
                      "The Range Ratio is more robust with n=4 methods as it doesn't assume any distribution.")
                )
              ),
              fluidRow(
                box(title = "Method Results Table", width = 12, status = "primary", solidHeader = TRUE,
                    DT::dataTableOutput("sensitivity_table"),
                    br(),
                    downloadButton("download_sensitivity", "Download Sensitivity Report", class = "btn-info")
                )
              ),
              conditionalPanel(
                condition = "input.sens_aggregate == false",
                fluidRow(
                  box(title = "Per-Tree Sensitivity", width = 12, status = "info", solidHeader = TRUE,
                      helpText("Trees ranked by method sensitivity (highest CV first)"),
                      DT::dataTableOutput("sensitivity_per_tree_table")
                  )
                )
              ),
              # Method comparison scatter plots
              fluidRow(
                box(title = "Method Comparison: Reference vs Other Methods", width = 12, 
                    status = "primary", solidHeader = TRUE,
                    p("Compare how each method's estimates relate to the reference method. ",
                      "Points on the 1:1 line indicate perfect agreement."),
                    fluidRow(
                      column(4,
                             radioButtons("scatter_view", "Display:",
                                          choices = c("Combined (single plot)" = "single",
                                                      "Faceted (one per method)" = "faceted"),
                                          selected = "single", inline = TRUE)
                      ),
                      column(8,
                             helpText("The reference method is selected above. Other methods are compared against it.")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.scatter_view == 'single'",
                      plotOutput("method_scatter_single", height = "450px")
                    ),
                    conditionalPanel(
                      condition = "input.scatter_view == 'faceted'",
                      plotOutput("method_scatter_faceted", height = "500px")
                    )
                )
              ),
              # Method correlation heatmap
              fluidRow(
                box(title = "Method Correlation Heatmap", width = 6, status = "success", solidHeader = TRUE,
                    p("Shows correlation between per-tree carbon estimates across methods. ",
                      "High correlation indicates methods rank trees similarly."),
                    plotOutput("method_correlation_heatmap", height = "400px")
                ),
                box(title = "Correlation Interpretation", width = 6, status = "info", solidHeader = TRUE,
                    h5("Understanding the Heatmap"),
                    tags$ul(
                      tags$li(tags$strong("r > 0.95:"), " Very strong agreement - methods rank trees nearly identically"),
                      tags$li(tags$strong("r = 0.80-0.95:"), " Strong agreement - general pattern preserved"),
                      tags$li(tags$strong("r = 0.60-0.80:"), " Moderate agreement - some disagreement in ranking"),
                      tags$li(tags$strong("r < 0.60:"), " Weak agreement - methods fundamentally differ in how they rank trees")
                    ),
                    hr(),
                    p(style = "font-size: 0.9em; color: #666;",
                      tags$strong("Note:"), " High correlation does not mean methods give the same ",
                      tags$em("values"), " - it means they ", tags$em("rank"), " trees similarly. ",
                      "Two methods can be perfectly correlated (r=1) but still have different absolute values.")
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
                      tags$li("Adjust advanced error parameters if needed"),
                      tags$li(tags$strong("NEW:"), " Enable BIOMASS Monte Carlo uncertainty for robust confidence intervals")
                    ),
                    h4("3. Calculate"),
                    tags$ul(
                      tags$li("Click 'Calculate' to process data"),
                      tags$li("View results in Results & Plots tab"),
                      tags$li("Check Statistics tab for method comparisons")
                    ),
                    h4("4. Sensitivity Analysis (NEW)"),
                    tags$ul(
                      tags$li("After calculating, go to the 'Sensitivity Analysis' tab"),
                      tags$li("Select reference method and methods to compare"),
                      tags$li("Click 'Run Sensitivity Analysis' to see how estimates vary by method"),
                      tags$li("Interpret results: CV < 10% = LOW sensitivity, 10-25% = MODERATE, etc."),
                      tags$li("Use this to decide if method choice significantly affects your results")
                    ),
                    h4("5. Export"),
                    tags$ul(
                      tags$li("Download results as CSV or Excel"),
                      tags$li("Export plots as PNG or SVG"),
                      tags$li("Download sensitivity analysis report")
                    ),
                    hr(),
                    h4("Understanding Sensitivity Analysis"),
                    p("The sensitivity analysis answers: ", tags$em("'How much does my carbon estimate change based on which allometric method I choose?'")),
                    p("Key metrics:"),
                    tags$ul(
                      tags$li(tags$strong("CV (Coefficient of Variation):"), " Standard deviation as percentage of mean. Higher CV = more variation between methods."),
                      tags$li(tags$strong("Spread:"), " Range (max - min) as percentage of mean. Shows total variation."),
                      tags$li(tags$strong("Range Ratio:"), " Max estimate / Min estimate. E.g., 1.5x means highest is 50% more than lowest.")
                    )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Helper function to strip ANSI color codes from strings
  strip_ansi <- function(x) {
    gsub("\033\\[[0-9;]*m", "", x)
  }

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

        # Store unique warnings (displayed in warnings panel only, not as pop-ups)
        captured_warnings <- warn_env$warnings
        if (length(captured_warnings) > 0) {
          unique_warnings <- unique(captured_warnings)
          # Strip ANSI codes before storing
          clean_warnings <- sapply(unique_warnings, strip_ansi, USE.NAMES = FALSE)
          calculation_warnings$warnings <- clean_warnings
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

        # Store unique warnings (displayed in warnings panel only, not as pop-ups)
        captured_warnings <- warn_env$warnings
        if (length(captured_warnings) > 0) {
          unique_warnings <- unique(captured_warnings)
          # Strip ANSI codes before storing
          clean_warnings <- sapply(unique_warnings, strip_ansi, USE.NAMES = FALSE)
          calculation_warnings$warnings <- clean_warnings
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

    # Warnings are already cleaned of ANSI codes when stored
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

    # Capture all warnings from plot creation and rendering
    plot_warnings <- character()

    p <- withCallingHandlers({
      plot_allometries(df, input$plot_type, input$returnv,
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
    }, warning = function(w) {
      plot_warnings <<- c(plot_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

    if (is.null(p)) {
      if (requireNamespace("plotly", quietly = TRUE)) {
        return(plotly::layout(plotly::plotly_empty(), title = "No data available to plot"))
      }
      return(NULL)
    }

    # Also capture warnings during plot build/render
    withCallingHandlers({
      if (!inherits(p, "plotly")) {
        # Build the plot to trigger any ggplot warnings
        ggplot2::ggplot_build(p)
      }
    }, warning = function(w) {
      plot_warnings <<- c(plot_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

    # Store plot warnings in panel (no pop-up notifications)
    if (length(plot_warnings) > 0) {
      unique_warnings <- unique(plot_warnings)
      # Strip ANSI codes
      clean_warnings <- sapply(unique_warnings, strip_ansi, USE.NAMES = FALSE)
      # Append to existing warnings
      all_warnings <- c(calculation_warnings$warnings, paste("[Plot]", clean_warnings))
      calculation_warnings$warnings <- unique(all_warnings)
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

    # Capture all warnings from plot creation and rendering
    plot_warnings <- character()

    p <- withCallingHandlers({
      plot_allometries(df, input$plot_type, input$returnv,
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
    }, warning = function(w) {
      plot_warnings <<- c(plot_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

    if (is.null(p)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data available to plot") +
               theme_void())
    }

    # Also capture warnings during plot BUILD (ggplot_build triggers the "Removed X rows" warnings)
    withCallingHandlers({
      ggplot2::ggplot_build(p)
    }, warning = function(w) {
      plot_warnings <<- c(plot_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

    # Store plot warnings in panel (no pop-up notifications)
    if (length(plot_warnings) > 0) {
      unique_warnings <- unique(plot_warnings)
      # Strip ANSI codes
      clean_warnings <- sapply(unique_warnings, strip_ansi, USE.NAMES = FALSE)
      # Append to existing warnings
      all_warnings <- c(calculation_warnings$warnings, paste("[Plot]", clean_warnings))
      calculation_warnings$warnings <- unique(all_warnings)
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

  # Comparison statistics - only calculate when checkbox is enabled
  comparison_stats <- reactive({
    if (!input$calc_stats) return(NULL)
    df <- current_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    compare_methods(df, input$returnv)
  })

  # Boxplot comparing methods
  output$method_boxplot <- renderPlot({
    if (!input$calc_stats) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Enable 'Calculate comparison statistics' to view") +
               theme_void())
    }

    df <- current_data()
    if (is.null(df) || nrow(df) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data available") +
               theme_void())
    }

    # Get selected methods from filter
    selected_methods <- input$stats_filter_methods
    if (is.null(selected_methods) || length(selected_methods) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Select at least one method to compare") +
               theme_void())
    }

    # Determine which columns to use based on selection
    if (input$returnv == "AGC") {
      method_map <- c("WCC" = "WCC_C_t", "BIOMASS" = "biomass_C_t", "allodb" = "allodb_C_t", "Bunce" = "Bunce_C_t")
      y_label <- "Carbon (tonnes)"
    } else {
      method_map <- c("WCC" = "WCC_B_t", "BIOMASS" = "biomass_B_t", "allodb" = "allodb_B_t", "Bunce" = "Bunce_B_t")
      y_label <- "Biomass (tonnes)"
    }

    # Filter to selected methods
    method_cols <- method_map[selected_methods]

    # Get available columns
    avail_cols <- intersect(method_cols, colnames(df))
    if (length(avail_cols) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No method data available") +
               theme_void())
    }

    # Reshape data for boxplot
    method_names <- c("WCC_C_t" = "WCC", "biomass_C_t" = "BIOMASS", "allodb_C_t" = "allodb", "Bunce_C_t" = "Bunce",
                      "WCC_B_t" = "WCC", "biomass_B_t" = "BIOMASS", "allodb_B_t" = "allodb", "Bunce_B_t" = "Bunce")

    plot_data <- data.frame()
    for (col in avail_cols) {
      temp <- data.frame(
        Method = method_names[col],
        Value = df[[col]]
      )
      plot_data <- rbind(plot_data, temp)
    }

    # Remove NAs
    plot_data <- plot_data[!is.na(plot_data$Value), ]

    if (nrow(plot_data) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No valid data to plot") +
               theme_void())
    }

    # Create boxplot
    ggplot(plot_data, aes(x = Method, y = Value, fill = Method)) +
      geom_boxplot(alpha = 0.7, outlier.shape = 21) +
      geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
      scale_fill_manual(values = c("WCC" = "#E69F00", "BIOMASS" = "#56B4E9",
                                   "allodb" = "#009E73", "Bunce" = "#CC79A7")) +
      labs(x = "Allometric Method", y = y_label,
           title = "Distribution of Estimates by Method") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"))
  })

  # Calculate normalized differences for all method pairs
  normalized_diffs <- reactive({
    if (!input$calc_stats) return(NULL)
    df <- current_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)

    # Get selected methods from filter
    selected_methods <- input$stats_filter_methods
    if (is.null(selected_methods) || length(selected_methods) < 2) return(NULL)

    # Determine which columns to use based on selection
    if (input$returnv == "AGC") {
      method_map <- c("WCC" = "WCC_C_t", "BIOMASS" = "biomass_C_t", "allodb" = "allodb_C_t", "Bunce" = "Bunce_C_t")
    } else {
      method_map <- c("WCC" = "WCC_B_t", "BIOMASS" = "biomass_B_t", "allodb" = "allodb_B_t", "Bunce" = "Bunce_B_t")
    }

    # Filter to selected methods
    method_cols <- method_map[selected_methods]

    method_names <- c("WCC_C_t" = "WCC", "biomass_C_t" = "BIOMASS", "allodb_C_t" = "allodb", "Bunce_C_t" = "Bunce",
                      "WCC_B_t" = "WCC", "biomass_B_t" = "BIOMASS", "allodb_B_t" = "allodb", "Bunce_B_t" = "Bunce")

    avail_cols <- intersect(method_cols, colnames(df))
    if (length(avail_cols) < 2) return(NULL)

    # Calculate normalized differences for all pairs
    diff_data <- data.frame()
    for (i in 1:(length(avail_cols) - 1)) {
      for (j in (i + 1):length(avail_cols)) {
        col1 <- avail_cols[i]
        col2 <- avail_cols[j]

        vals1 <- df[[col1]]
        vals2 <- df[[col2]]

        # Calculate normalized difference: (AGB1 - AGB2) / (AGB1 + AGB2)
        sum_vals <- vals1 + vals2
        # Avoid division by zero
        valid_idx <- !is.na(vals1) & !is.na(vals2) & sum_vals != 0

        if (sum(valid_idx) > 0) {
          norm_diff <- (vals1[valid_idx] - vals2[valid_idx]) / sum_vals[valid_idx]

          pair_name <- paste0(method_names[col1], " vs ", method_names[col2])
          temp <- data.frame(
            Comparison = pair_name,
            Method1 = method_names[col1],
            Method2 = method_names[col2],
            Normalized_Diff = norm_diff
          )
          diff_data <- rbind(diff_data, temp)
        }
      }
    }

    diff_data
  })

  # Plot normalized differences
  output$normalized_diff_plot <- renderPlot({
    # Check if at least 2 methods are selected
    selected_methods <- input$stats_filter_methods
    if (is.null(selected_methods) || length(selected_methods) < 2) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Select at least 2 methods to compare") +
               theme_void())
    }

    diff_data <- normalized_diffs()

    if (is.null(diff_data) || nrow(diff_data) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Not enough data to compare methods") +
               theme_void())
    }

    # Create boxplot with reference line at 0
    ggplot(diff_data, aes(x = Comparison, y = Normalized_Diff, fill = Comparison)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
      geom_boxplot(alpha = 0.7, outlier.shape = 21) +
      geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
      scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")) +
      labs(x = "Method Comparison",
           y = "Normalized Difference",
           title = "Normalized Differences Between Allometric Methods",
           subtitle = "Diff = (Method\u2081 - Method\u2082) / (Method\u2081 + Method\u2082)") +
      coord_flip() +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, color = "gray40")) +
      annotate("text", x = 0.5, y = -0.8, label = "\u2190 Method\u2082 higher", color = "gray50", size = 3) +
      annotate("text", x = 0.5, y = 0.8, label = "Method\u2081 higher \u2192", color = "gray50", size = 3)
  })

  # Summary table of normalized differences
  output$normalized_diff_table <- DT::renderDataTable({
    # Check if at least 2 methods are selected
    selected_methods <- input$stats_filter_methods
    if (is.null(selected_methods) || length(selected_methods) < 2) {
      return(data.frame(Message = "Select at least 2 methods to compare"))
    }

    diff_data <- normalized_diffs()

    if (is.null(diff_data) || nrow(diff_data) == 0) {
      return(data.frame(Message = "Not enough data to compare methods"))
    }

    # Calculate summary statistics for each comparison
    summary_df <- aggregate(Normalized_Diff ~ Comparison, data = diff_data, FUN = function(x) {
      c(Mean = mean(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        N = length(x))
    })

    # Flatten the matrix result
    summary_df <- cbind(summary_df[, 1, drop = FALSE], as.data.frame(summary_df$Normalized_Diff))
    colnames(summary_df) <- c("Comparison", "Mean", "SD", "Median", "N")

    # Round values
    summary_df$Mean <- round(summary_df$Mean, 4)
    summary_df$SD <- round(summary_df$SD, 4)
    summary_df$Median <- round(summary_df$Median, 4)

    # Add interpretation
    summary_df$Interpretation <- ifelse(abs(summary_df$Mean) < 0.05, "Good agreement",
                                        ifelse(abs(summary_df$Mean) < 0.15, "Moderate difference",
                                               "Large difference"))

    DT::datatable(summary_df, options = list(pageLength = 10, dom = 't'),
                  rownames = FALSE)
  })

  # Dynamic dropdown for Bland-Altman comparison based on selected methods
  output$ba_comparison_ui <- renderUI({
    selected_methods <- input$stats_filter_methods
    if (is.null(selected_methods) || length(selected_methods) < 2) {
      return(helpText("Select at least 2 methods above to enable comparisons"))
    }

    # Generate all pairwise comparisons
    comparisons <- c()
    for (i in 1:(length(selected_methods) - 1)) {
      for (j in (i + 1):length(selected_methods)) {
        comparisons <- c(comparisons, paste0(selected_methods[i], " vs ", selected_methods[j]))
      }
    }

    selectInput("ba_comparison", "Select method pair to compare:",
                choices = comparisons,
                selected = comparisons[1])
  })

  # Bland-Altman data calculation
  bland_altman_data <- reactive({
    if (!input$calc_stats) return(NULL)
    df <- current_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)

    # Parse selected comparison
    comparison <- input$ba_comparison
    methods <- strsplit(comparison, " vs ")[[1]]
    if (length(methods) != 2) return(NULL)

    # Map method names to column names
    if (input$returnv == "AGC") {
      method_map <- c("WCC" = "WCC_C_t", "BIOMASS" = "biomass_C_t", "allodb" = "allodb_C_t", "Bunce" = "Bunce_C_t")
    } else {
      method_map <- c("WCC" = "WCC_B_t", "BIOMASS" = "biomass_B_t", "allodb" = "allodb_B_t", "Bunce" = "Bunce_B_t")
    }

    col1 <- method_map[methods[1]]
    col2 <- method_map[methods[2]]

    if (!(col1 %in% colnames(df)) || !(col2 %in% colnames(df))) return(NULL)

    vals1 <- df[[col1]]
    vals2 <- df[[col2]]

    # Calculate Bland-Altman values
    valid_idx <- !is.na(vals1) & !is.na(vals2)
    if (sum(valid_idx) < 2) return(NULL)

    vals1 <- vals1[valid_idx]
    vals2 <- vals2[valid_idx]

    mean_vals <- (vals1 + vals2) / 2
    diff_vals <- vals1 - vals2

    # Calculate metrics
    bias <- mean(diff_vals, na.rm = TRUE)
    sd_diff <- sd(diff_vals, na.rm = TRUE)
    lower_loa <- bias - 1.96 * sd_diff
    upper_loa <- bias + 1.96 * sd_diff

    # Count within limits
    within_loa <- sum(diff_vals >= lower_loa & diff_vals <= upper_loa)
    pct_within <- 100 * within_loa / length(diff_vals)

    list(
      mean_vals = mean_vals,
      diff_vals = diff_vals,
      bias = bias,
      sd_diff = sd_diff,
      lower_loa = lower_loa,
      upper_loa = upper_loa,
      pct_within = pct_within,
      n = length(diff_vals),
      method1 = methods[1],
      method2 = methods[2]
    )
  })

  # Bland-Altman plot
  output$bland_altman_plot <- renderPlot({
    ba_data <- bland_altman_data()

    if (is.null(ba_data)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Not enough data for Bland-Altman analysis") +
               theme_void())
    }

    plot_df <- data.frame(
      Mean = ba_data$mean_vals,
      Difference = ba_data$diff_vals
    )

    # Create Bland-Altman plot
    ggplot(plot_df, aes(x = Mean, y = Difference)) +
      # Reference line at zero
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50", linewidth = 0.5) +
      # Bias line
      geom_hline(yintercept = ba_data$bias, color = "#2171b5", linewidth = 1) +
      # Limits of agreement
      geom_hline(yintercept = ba_data$lower_loa, linetype = "dashed", color = "#cb181d", linewidth = 0.8) +
      geom_hline(yintercept = ba_data$upper_loa, linetype = "dashed", color = "#cb181d", linewidth = 0.8) +
      # Points
      geom_point(alpha = 0.6, size = 3, color = "#2c7bb6") +
      # Labels for lines
      annotate("text", x = max(plot_df$Mean) * 0.98, y = ba_data$bias,
               label = paste0("Bias: ", round(ba_data$bias, 4)),
               hjust = 1, vjust = -0.5, color = "#2171b5", fontface = "bold", size = 4) +
      annotate("text", x = max(plot_df$Mean) * 0.98, y = ba_data$upper_loa,
               label = paste0("+1.96 SD: ", round(ba_data$upper_loa, 4)),
               hjust = 1, vjust = -0.5, color = "#cb181d", size = 3.5) +
      annotate("text", x = max(plot_df$Mean) * 0.98, y = ba_data$lower_loa,
               label = paste0("-1.96 SD: ", round(ba_data$lower_loa, 4)),
               hjust = 1, vjust = 1.5, color = "#cb181d", size = 3.5) +
      # Shaded region for limits of agreement
      annotate("rect", xmin = -Inf, xmax = Inf,
               ymin = ba_data$lower_loa, ymax = ba_data$upper_loa,
               alpha = 0.1, fill = "#2171b5") +
      labs(
        x = paste0("Mean of ", ba_data$method1, " and ", ba_data$method2, " (tonnes)"),
        y = paste0("Difference (", ba_data$method1, " - ", ba_data$method2, ") (tonnes)"),
        title = paste0("Bland-Altman Plot: ", ba_data$method1, " vs ", ba_data$method2),
        subtitle = paste0("n = ", ba_data$n, " trees | ", round(ba_data$pct_within, 1), "% within limits of agreement")
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40")
      )
  })

  # Bland-Altman metrics table
  output$bland_altman_table <- DT::renderDataTable({
    if (!input$calc_stats) {
      return(data.frame(Message = "Enable statistics to view"))
    }

    df <- current_data()
    if (is.null(df) || nrow(df) == 0) {
      return(data.frame(Message = "No data available"))
    }

    # Get selected methods
    selected_methods <- input$stats_filter_methods
    if (is.null(selected_methods) || length(selected_methods) < 2) {
      return(data.frame(Message = "Select at least 2 methods"))
    }

    # Map method names to column names
    if (input$returnv == "AGC") {
      method_map <- c("WCC" = "WCC_C_t", "BIOMASS" = "biomass_C_t", "allodb" = "allodb_C_t", "Bunce" = "Bunce_C_t")
    } else {
      method_map <- c("WCC" = "WCC_B_t", "BIOMASS" = "biomass_B_t", "allodb" = "allodb_B_t", "Bunce" = "Bunce_B_t")
    }

    # Calculate Bland-Altman metrics for all selected pairs
    results <- data.frame()
    for (i in 1:(length(selected_methods) - 1)) {
      for (j in (i + 1):length(selected_methods)) {
        m1 <- selected_methods[i]
        m2 <- selected_methods[j]
        col1 <- method_map[m1]
        col2 <- method_map[m2]

        if (!(col1 %in% colnames(df)) || !(col2 %in% colnames(df))) next

        vals1 <- df[[col1]]
        vals2 <- df[[col2]]

        valid_idx <- !is.na(vals1) & !is.na(vals2)
        if (sum(valid_idx) < 2) next

        vals1 <- vals1[valid_idx]
        vals2 <- vals2[valid_idx]

        diff_vals <- vals1 - vals2
        bias <- mean(diff_vals, na.rm = TRUE)
        sd_diff <- sd(diff_vals, na.rm = TRUE)
        lower_loa <- bias - 1.96 * sd_diff
        upper_loa <- bias + 1.96 * sd_diff

        within_loa <- sum(diff_vals >= lower_loa & diff_vals <= upper_loa)
        pct_within <- 100 * within_loa / length(diff_vals)

        results <- rbind(results, data.frame(
          Comparison = paste0(m1, " vs ", m2),
          N = length(diff_vals),
          Bias = round(bias, 4),
          SD = round(sd_diff, 4),
          Lower_LoA = round(lower_loa, 4),
          Upper_LoA = round(upper_loa, 4),
          Pct_Within_LoA = round(pct_within, 1)
        ))
      }
    }

    if (nrow(results) == 0) {
      return(data.frame(Message = "No valid comparisons available"))
    }

    colnames(results) <- c("Comparison", "N", "Bias", "SD", "Lower LoA", "Upper LoA", "% Within LoA")

    DT::datatable(results, options = list(pageLength = 10, dom = 't'),
                  rownames = FALSE)
  })

  # ========== CARBON PER AREA ==========

  # Store per-area results
  per_area_results <- reactiveValues(data = NULL)

  # Calculate carbon per area when button clicked
  observeEvent(input$calc_per_area, {
    df <- current_data()
    if (is.null(df) || nrow(df) == 0) {
      showNotification("No data available. Calculate allometries first.", type = "error")
      return()
    }

    # Get area and convert to hectares for internal calculations
    area_ha <- switch(input$area_unit,
                      "ha" = input$plot_area,
                      "m2" = input$plot_area / 10000,
                      "acres" = input$plot_area / 2.471,
                      "km2" = input$plot_area * 100,
                      input$plot_area)

    if (area_ha <= 0) {
      showNotification("Area must be greater than 0", type = "error")
      return()
    }

    # Determine column suffix based on return type
    suffix <- if (input$returnv == "AGC") "_C_t" else "_B_t"
    suffix_sig <- if (input$returnv == "AGC") "_C_sig" else "_B_sig"

    # Map method names to column names
    col_map <- c(
      "WCC" = paste0("WCC", suffix),
      "BIOMASS" = paste0("biomass", suffix),
      "allodb" = paste0("allodb", suffix),
      "Bunce" = paste0("Bunce", suffix)
    )
    sig_map <- c(
      "WCC" = paste0("WCC", suffix_sig),
      "BIOMASS" = paste0("biomass", suffix_sig),
      "allodb" = paste0("allodb", suffix_sig),
      "Bunce" = paste0("Bunce", suffix_sig)
    )

    # Calculate per-area values for each selected method
    results <- data.frame()

    for (m in input$per_area_methods) {
      col_name <- col_map[m]
      sig_col <- sig_map[m]

      if (col_name %in% names(df)) {
        carbon_vals <- df[[col_name]]
        total_carbon <- sum(carbon_vals, na.rm = TRUE)
        carbon_per_ha <- total_carbon / area_ha

        # Calculate error if available and requested
        if (input$per_area_include_error && sig_col %in% names(df)) {
          error_vals <- df[[sig_col]]
          # Propagate errors in quadrature for sum
          total_error <- sqrt(sum(error_vals^2, na.rm = TRUE))
          error_per_ha <- total_error / area_ha
          cv_pct <- 100 * total_error / total_carbon
        } else {
          total_error <- NA
          error_per_ha <- NA
          cv_pct <- NA
        }

        # Convert to output units
        output_multiplier <- switch(input$output_unit,
                                    "t_ha" = 1,
                                    "kg_ha" = 1000,
                                    "t_acre" = 2.471,
                                    "Mg_ha" = 1,
                                    1)

        results <- rbind(results, data.frame(
          Method = m,
          Total_Carbon_t = round(total_carbon, 4),
          Total_Error_t = round(total_error, 4),
          Carbon_per_ha = round(carbon_per_ha * output_multiplier, 4),
          Error_per_ha = round(error_per_ha * output_multiplier, 4),
          CV_pct = round(cv_pct, 2),
          N_trees = sum(!is.na(carbon_vals)),
          stringsAsFactors = FALSE
        ))
      }
    }

    if (nrow(results) > 0) {
      # Add ranking
      results <- results[order(-results$Carbon_per_ha), ]
      results$Rank <- seq_len(nrow(results))

      # Store metadata
      attr(results, "area_ha") <- area_ha
      attr(results, "area_input") <- input$plot_area
      attr(results, "area_unit") <- input$area_unit
      attr(results, "output_unit") <- input$output_unit
      attr(results, "n_trees") <- nrow(df)

      per_area_results$data <- results
      showNotification("Carbon density calculated!", type = "message", duration = 3)
    } else {
      showNotification("No valid data found for selected methods", type = "error")
    }
  })

  # Value boxes for per-area tab
  output$total_carbon_per_area <- renderValueBox({
    results <- per_area_results$data
    if (is.null(results)) {
      return(valueBox("--", "Total Carbon (all methods avg)", icon = icon("tree"), color = "green"))
    }
    avg_total <- mean(results$Total_Carbon_t, na.rm = TRUE)
    valueBox(paste0(round(avg_total, 2), " t"), "Total Carbon (avg)", icon = icon("tree"), color = "green")
  })

  output$carbon_density_wcc <- renderValueBox({
    results <- per_area_results$data
    if (is.null(results)) {
      return(valueBox("--", "Carbon Density", icon = icon("chart-area"), color = "blue"))
    }
    # Get WCC value or first available
    wcc_row <- results[results$Method == "WCC", ]
    if (nrow(wcc_row) > 0) {
      val <- wcc_row$Carbon_per_ha[1]
      method_label <- "WCC"
    } else {
      val <- results$Carbon_per_ha[1]
      method_label <- results$Method[1]
    }
    unit_label <- switch(attr(results, "output_unit"),
                         "t_ha" = "t C/ha",
                         "kg_ha" = "kg C/ha",
                         "t_acre" = "t C/acre",
                         "Mg_ha" = "Mg C/ha",
                         "t C/ha")
    valueBox(paste0(round(val, 2), " ", unit_label),
             paste0("Carbon Density (", method_label, ")"),
             icon = icon("chart-area"), color = "blue")
  })

  output$method_range_per_area <- renderValueBox({
    results <- per_area_results$data
    if (is.null(results) || nrow(results) < 2) {
      return(valueBox("--", "Method Range", icon = icon("arrows-alt-h"), color = "purple"))
    }
    range_ratio <- max(results$Carbon_per_ha, na.rm = TRUE) / min(results$Carbon_per_ha, na.rm = TRUE)
    valueBox(paste0(round(range_ratio, 2), "x"), "Method Range (max/min)",
             icon = icon("arrows-alt-h"), color = "purple")
  })

  # Bar plot for per-area results
  output$per_area_bar_plot <- renderPlot({
    results <- per_area_results$data
    if (is.null(results) || nrow(results) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Click 'Calculate Carbon Density' to see results") +
               theme_void())
    }

    unit_label <- switch(attr(results, "output_unit"),
                         "t_ha" = "t C/ha",
                         "kg_ha" = "kg C/ha",
                         "t_acre" = "t C/acre",
                         "Mg_ha" = "Mg C/ha",
                         "t C/ha")

    p <- ggplot(results, aes(x = reorder(Method, -Carbon_per_ha), y = Carbon_per_ha, fill = Method)) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = round(Carbon_per_ha, 2)), vjust = -0.5, size = 4)

    # Add error bars if available
    if (!all(is.na(results$Error_per_ha))) {
      # Cap error bars at zero
      results$ymin <- pmax(0, results$Carbon_per_ha - results$Error_per_ha)
      results$ymax <- results$Carbon_per_ha + results$Error_per_ha
      p <- p + geom_errorbar(data = results,
                             aes(ymin = ymin, ymax = ymax),
                             width = 0.2, linewidth = 0.8)
    }

    p +
      scale_fill_manual(values = c("WCC" = "#E69F00", "BIOMASS" = "#56B4E9",
                                   "allodb" = "#009E73", "Bunce" = "#CC79A7")) +
      labs(x = "Allometric Method",
           y = paste0("Carbon Density (", unit_label, ")"),
           title = "Carbon Density by Allometric Method",
           subtitle = sprintf("Area: %.2f %s | Trees: %d",
                              attr(results, "area_input"),
                              attr(results, "area_unit"),
                              attr(results, "n_trees"))) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, color = "gray40"))
  })

  # Results table for per-area
  output$per_area_table <- DT::renderDataTable({
    results <- per_area_results$data
    if (is.null(results)) {
      return(data.frame(Message = "Calculate carbon density to see results"))
    }

    # Format for display
    display_df <- results[, c("Method", "Total_Carbon_t", "Carbon_per_ha", "Error_per_ha", "CV_pct", "Rank")]
    colnames(display_df) <- c("Method", "Total (t)", "Density", "± Error", "CV (%)", "Rank")

    DT::datatable(display_df, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })

  # Summary text for per-area
  output$per_area_summary <- renderPrint({
    results <- per_area_results$data
    if (is.null(results)) {
      cat("Click 'Calculate Carbon Density' after calculating allometries.\n")
      return()
    }

    cat("=== CARBON DENSITY SUMMARY ===\n\n")
    cat(sprintf("Survey area: %.2f %s (%.4f ha)\n",
                attr(results, "area_input"),
                attr(results, "area_unit"),
                attr(results, "area_ha")))
    cat(sprintf("Number of trees: %d\n", attr(results, "n_trees")))
    cat(sprintf("Methods compared: %d\n\n", nrow(results)))

    unit_label <- switch(attr(results, "output_unit"),
                         "t_ha" = "t C/ha",
                         "kg_ha" = "kg C/ha",
                         "t_acre" = "t C/acre",
                         "Mg_ha" = "Mg C/ha",
                         "t C/ha")

    cat("--- Carbon Density by Method ---\n")
    for (i in seq_len(nrow(results))) {
      r <- results[i, ]
      if (!is.na(r$Error_per_ha)) {
        cat(sprintf("%s: %.2f ± %.2f %s (CV: %.1f%%)\n",
                    r$Method, r$Carbon_per_ha, r$Error_per_ha, unit_label, r$CV_pct))
      } else {
        cat(sprintf("%s: %.2f %s\n", r$Method, r$Carbon_per_ha, unit_label))
      }
    }

    cat("\n--- Method Agreement ---\n")
    if (nrow(results) >= 2) {
      range_ratio <- max(results$Carbon_per_ha) / min(results$Carbon_per_ha)
      cv_methods <- 100 * sd(results$Carbon_per_ha) / mean(results$Carbon_per_ha)
      cat(sprintf("Range ratio: %.2fx (max/min)\n", range_ratio))
      cat(sprintf("CV across methods: %.1f%%\n", cv_methods))
      cat(sprintf("Mean density: %.2f %s\n", mean(results$Carbon_per_ha), unit_label))
    }

    # CO2e conversion note
    cat("\n--- CO₂ Equivalent ---\n")
    mean_carbon <- mean(results$Carbon_per_ha)
    co2e <- mean_carbon * 3.67
    cat(sprintf("Mean CO₂e: %.2f t CO₂e/%s (carbon × 3.67)\n",
                co2e, gsub("t C/|kg C/|Mg C/", "", unit_label)))
  })

  # Download handlers for per-area
  output$download_per_area_csv <- downloadHandler(
    filename = function() paste0("carbon_density_", Sys.Date(), ".csv"),
    content = function(file) {
      results <- per_area_results$data
      if (is.null(results)) {
        write.csv(data.frame(Message = "No data"), file, row.names = FALSE)
        return()
      }
      write.csv(results, file, row.names = FALSE)
    }
  )

  output$download_per_area_report <- downloadHandler(
    filename = function() paste0("carbon_density_report_", Sys.Date(), ".txt"),
    content = function(file) {
      results <- per_area_results$data
      if (is.null(results)) {
        writeLines("No data available", file)
        return()
      }

      # Generate report text
      lines <- c(
        "CARBON DENSITY REPORT",
        paste0("Generated: ", Sys.time()),
        "",
        sprintf("Survey area: %.2f %s", attr(results, "area_input"), attr(results, "area_unit")),
        sprintf("Number of trees: %d", attr(results, "n_trees")),
        "",
        "RESULTS BY METHOD:",
        "-------------------"
      )

      unit_label <- attr(results, "output_unit")
      for (i in seq_len(nrow(results))) {
        r <- results[i, ]
        lines <- c(lines, sprintf("%s: %.4f (± %.4f) %s",
                                  r$Method, r$Carbon_per_ha, r$Error_per_ha, unit_label))
      }

      lines <- c(lines, "",
                 sprintf("Range ratio: %.2fx", max(results$Carbon_per_ha) / min(results$Carbon_per_ha)),
                 sprintf("CV across methods: %.1f%%", 100 * sd(results$Carbon_per_ha) / mean(results$Carbon_per_ha)))

      writeLines(lines, file)
    }
  )

  # ========== SENSITIVITY ANALYSIS ==========

  # Store sensitivity results
  sensitivity_results <- reactiveValues(result = NULL)

  # Run sensitivity analysis when button clicked
  observeEvent(input$run_sensitivity, {
    df <- current_data()
    if (is.null(df) || nrow(df) == 0) {
      showNotification("No data available. Calculate allometries first.", type = "error")
      return()
    }

    withProgress(message = "Running sensitivity analysis...", value = 0, {
      incProgress(0.3, detail = "Analyzing method variation...")

      # Use the allometries() output directly for consistency
      # This ensures sensitivity analysis uses the SAME values as the Results tab
      result <- tryCatch({
        sensitivity_analysis(
          data = df,  # Pass allometries() output directly
          methods = input$sens_methods,
          returnv = input$returnv,  # Use same return type (AGC/AGB)
          aggregate = input$sens_aggregate,
          reference = input$sens_reference
        )
      }, error = function(e) {
        showNotification(paste("Sensitivity analysis error:", e$message), type = "error", duration = 10)
        NULL
      })

      incProgress(1, detail = "Done!")

      if (!is.null(result)) {
        sensitivity_results$result <- result
        showNotification("Sensitivity analysis complete!", type = "message", duration = 3)
      }
    })
  })

  # Sensitivity value boxes
  output$sens_cv <- renderValueBox({
    result <- sensitivity_results$result
    if (is.null(result)) {
      return(valueBox("--", "CV Across Methods", icon = icon("percent"), color = "blue"))
    }
    cv <- result$sensitivity_metrics$cv_across_methods
    valueBox(paste0(round(cv, 1), "%"), "CV Across Methods", icon = icon("percent"), color = "blue")
  })

  output$sens_range_ratio <- renderValueBox({
    result <- sensitivity_results$result
    if (is.null(result)) {
      return(valueBox("--", "Range Ratio", icon = icon("arrows-alt-h"), color = "purple"))
    }
    ratio <- result$sensitivity_metrics$range_ratio
    valueBox(paste0(round(ratio, 2), "x"), "Range Ratio (max/min)", icon = icon("arrows-alt-h"), color = "purple")
  })

  output$sens_level <- renderValueBox({
    result <- sensitivity_results$result
    if (is.null(result)) {
      return(valueBox("--", "Sensitivity Level", icon = icon("thermometer-half"), color = "green"))
    }
    level <- result$sensitivity_level
    color <- switch(level,
                    "LOW" = "green",
                    "MODERATE" = "blue",
                    "HIGH" = "orange",
                    "VERY HIGH" = "red",
                    "purple")
    valueBox(level, "Sensitivity Level", icon = icon("thermometer-half"), color = color)
  })

  # Sensitivity summary text
  output$sensitivity_summary <- renderPrint({
    result <- sensitivity_results$result
    if (is.null(result)) {
      cat("Run sensitivity analysis to see results.\n")
      cat("\nClick 'Run Sensitivity Analysis' after calculating allometries.")
      return()
    }

    cat("=== SENSITIVITY ANALYSIS RESULTS ===\n\n")

    cat("--- Key Metrics ---\n")
    cat(sprintf("Range ratio: %.2fx (max/min) <- most robust\n", result$sensitivity_metrics$range_ratio))
    cat(sprintf("CV across methods: %.1f%%\n", result$sensitivity_metrics$cv_across_methods))
    cat(sprintf("Spread: %.1f%% of mean\n", result$sensitivity_metrics$spread_pct))
    cat(sprintf("Methods compared: %d\n", result$sensitivity_metrics$n_methods))
    cat(sprintf("Trees analyzed: %d\n", result$sensitivity_metrics$n_trees))

    cat("\n--- Interpretation ---\n")
    cat(strwrap(result$interpretation, width = 60), sep = "\n")
    cat("\n\n--- Recommendation ---\n")
    cat(strwrap(result$recommendation, width = 60), sep = "\n")
  })

  # Sensitivity comparison plot
  output$sensitivity_comparison_plot <- renderPlot({
    result <- sensitivity_results$result
    if (is.null(result)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Run sensitivity analysis to see plot") +
               theme_void())
    }

    # Create bar chart of method totals
    method_summary <- result$method_summary
    if (is.null(method_summary) || nrow(method_summary) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No method data available") +
               theme_void())
    }

    ggplot(method_summary, aes(x = reorder(method, -total), y = total, fill = method)) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = round(total, 3)), vjust = -0.5, size = 4) +
      scale_fill_manual(values = c("WCC" = "#E69F00", "BIOMASS" = "#56B4E9",
                                   "allodb" = "#009E73", "Bunce" = "#CC79A7")) +
      labs(x = "Method", y = "Total Carbon (tonnes)",
           title = "Carbon Estimates by Method") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"))
  })

  # Sensitivity deviation plot
  output$sensitivity_deviation_plot <- renderPlot({
    result <- sensitivity_results$result
    if (is.null(result)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Run sensitivity analysis to see plot") +
               theme_void())
    }

    method_summary <- result$method_summary
    ref_method <- result$sensitivity_metrics$reference

    if (is.null(method_summary) || nrow(method_summary) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No method data available") +
               theme_void())
    }

    # Calculate deviation from reference
    ref_value <- method_summary$total[method_summary$method == ref_method]
    if (length(ref_value) == 0) ref_value <- mean(method_summary$total, na.rm = TRUE)

    method_summary$deviation <- method_summary$total - ref_value
    method_summary$deviation_pct <- 100 * method_summary$deviation / ref_value

    ggplot(method_summary, aes(x = reorder(method, deviation_pct), y = deviation_pct, fill = deviation_pct > 0)) +
      geom_col(alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_text(aes(label = paste0(ifelse(deviation_pct > 0, "+", ""), round(deviation_pct, 1), "%")),
                hjust = ifelse(method_summary$deviation_pct > 0, -0.1, 1.1), size = 4) +
      scale_fill_manual(values = c("TRUE" = "#2ca02c", "FALSE" = "#d62728"), guide = "none") +
      coord_flip() +
      labs(x = "Method", y = paste0("Deviation from ", ref_method, " (%)"),
           title = paste0("Deviation from Reference (", ref_method, ")")) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })

  # ========== PER-TREE CV DISTRIBUTION (OPTION A) ==========

  # Histogram of per-tree CVs
  output$tree_cv_histogram <- renderPlot({
    result <- sensitivity_results$result
    if (is.null(result) || is.null(result$by_tree)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Run sensitivity analysis first") +
               theme_void())
    }

    tree_data <- result$by_tree
    if (!"cv_pct" %in% names(tree_data) || all(is.na(tree_data$cv_pct))) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "CV data not available") +
               theme_void())
    }

    # Calculate summary stats for annotations
    median_cv <- median(tree_data$cv_pct, na.rm = TRUE)
    mean_cv <- mean(tree_data$cv_pct, na.rm = TRUE)

    ggplot(tree_data, aes(x = cv_pct)) +
      geom_histogram(bins = 25, fill = "steelblue", alpha = 0.7, color = "white") +
      geom_vline(xintercept = median_cv, color = "red", linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = 10, color = "green", linetype = "dotted", linewidth = 0.8) +
      geom_vline(xintercept = 25, color = "orange", linetype = "dotted", linewidth = 0.8) +
      annotate("text", x = median_cv, y = Inf, label = paste0("Median: ", round(median_cv, 1), "%"),
               vjust = 2, hjust = -0.1, color = "red", fontface = "bold") +
      annotate("text", x = 10, y = Inf, label = "Low", vjust = 3, hjust = 1.1, color = "green", size = 3) +
      annotate("text", x = 25, y = Inf, label = "High", vjust = 3, hjust = -0.1, color = "orange", size = 3) +
      labs(
        x = "Coefficient of Variation (%)",
        y = "Number of Trees",
        title = "Per-Tree Sensitivity Distribution",
        subtitle = sprintf("n = %d trees | Mean CV = %.1f%% | Median CV = %.1f%%",
                           sum(!is.na(tree_data$cv_pct)), mean_cv, median_cv)
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, color = "gray40"))
  })

  # CV vs DBH scatter plot
  output$tree_cv_vs_dbh <- renderPlot({
    result <- sensitivity_results$result
    df <- current_data()

    if (is.null(result) || is.null(result$by_tree) || is.null(df)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Run sensitivity analysis first") +
               theme_void())
    }

    tree_data <- result$by_tree
    if (!"cv_pct" %in% names(tree_data)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "CV data not available") +
               theme_void())
    }

    # Add DBH from original data
    if ("dbh" %in% names(df) && nrow(df) == nrow(tree_data)) {
      tree_data$dbh <- df$dbh
    } else {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "DBH data not aligned") +
               theme_void())
    }

    # Color by sensitivity level
    tree_data$sensitivity_level <- cut(tree_data$cv_pct,
                                       breaks = c(-Inf, 10, 25, 50, Inf),
                                       labels = c("Low (<10%)", "Moderate (10-25%)",
                                                  "High (25-50%)", "Very High (>50%)"))

    ggplot(tree_data, aes(x = dbh, y = cv_pct, color = sensitivity_level)) +
      geom_point(alpha = 0.7, size = 3) +
      geom_smooth(method = "loess", se = TRUE, color = "gray40", linetype = "dashed") +
      geom_hline(yintercept = c(10, 25), linetype = "dotted", color = c("green", "orange")) +
      scale_color_manual(values = c("Low (<10%)" = "#2ca02c",
                                    "Moderate (10-25%)" = "#1f77b4",
                                    "High (25-50%)" = "#ff7f0e",
                                    "Very High (>50%)" = "#d62728"),
                         name = "Sensitivity") +
      labs(
        x = "DBH (cm)",
        y = "CV across methods (%)",
        title = "Sensitivity vs Tree Size",
        subtitle = "Does method disagreement vary with tree size?"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
            legend.position = "bottom")
  })

  # Value boxes for per-tree sensitivity
  output$sens_tree_median_cv <- renderValueBox({
    result <- sensitivity_results$result
    if (is.null(result) || is.null(result$by_tree)) {
      return(valueBox("--", "Median Tree CV", icon = icon("chart-line"), color = "blue"))
    }
    median_cv <- median(result$by_tree$cv_pct, na.rm = TRUE)
    color <- if (median_cv < 10) "green" else if (median_cv < 25) "blue" else if (median_cv < 50) "orange" else "red"
    valueBox(paste0(round(median_cv, 1), "%"), "Median Tree CV", icon = icon("chart-line"), color = color)
  })

  output$sens_tree_high_cv_pct <- renderValueBox({
    result <- sensitivity_results$result
    if (is.null(result) || is.null(result$by_tree)) {
      return(valueBox("--", "High Sensitivity Trees", icon = icon("exclamation-triangle"), color = "orange"))
    }
    high_pct <- 100 * sum(result$by_tree$cv_pct > 25, na.rm = TRUE) / sum(!is.na(result$by_tree$cv_pct))
    valueBox(paste0(round(high_pct, 1), "%"), "Trees with CV > 25%", icon = icon("exclamation-triangle"), color = "orange")
  })

  output$sens_tree_low_cv_pct <- renderValueBox({
    result <- sensitivity_results$result
    if (is.null(result) || is.null(result$by_tree)) {
      return(valueBox("--", "Low Sensitivity Trees", icon = icon("check-circle"), color = "green"))
    }
    low_pct <- 100 * sum(result$by_tree$cv_pct < 10, na.rm = TRUE) / sum(!is.na(result$by_tree$cv_pct))
    valueBox(paste0(round(low_pct, 1), "%"), "Trees with CV < 10%", icon = icon("check-circle"), color = "green")
  })

  # Sensitivity table
  output$sensitivity_table <- DT::renderDataTable({
    result <- sensitivity_results$result
    if (is.null(result)) {
      return(data.frame(Message = "Run sensitivity analysis to see results"))
    }

    method_summary <- result$method_summary
    if (is.null(method_summary)) {
      return(data.frame(Message = "No method data available"))
    }

    # Round numeric columns
    numeric_cols <- sapply(method_summary, is.numeric)
    method_summary[numeric_cols] <- lapply(method_summary[numeric_cols], round, 4)

    DT::datatable(method_summary, options = list(pageLength = 10, dom = 't'),
                  rownames = FALSE)
  })

  # Per-tree sensitivity table
  output$sensitivity_per_tree_table <- DT::renderDataTable({
    result <- sensitivity_results$result
    if (is.null(result) || is.null(result$by_tree)) {
      return(data.frame(Message = "Run sensitivity analysis with 'Aggregate results' unchecked"))
    }

    by_tree <- result$by_tree

    # Select and round relevant columns
    cols_to_show <- c("tree_id", "mean_estimate", "sd_across_methods", "cv_pct", "spread_pct", "range_ratio")
    cols_avail <- intersect(cols_to_show, colnames(by_tree))

    if (length(cols_avail) == 0) {
      return(data.frame(Message = "No per-tree data available"))
    }

    by_tree_display <- by_tree[, cols_avail, drop = FALSE]

    # Round numeric columns
    numeric_cols <- sapply(by_tree_display, is.numeric)
    by_tree_display[numeric_cols] <- lapply(by_tree_display[numeric_cols], round, 4)

    # Sort by CV descending
    if ("cv_pct" %in% colnames(by_tree_display)) {
      by_tree_display <- by_tree_display[order(-by_tree_display$cv_pct), ]
    }

    DT::datatable(by_tree_display, options = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE)
  })

  # Download sensitivity report
  output$download_sensitivity <- downloadHandler(
    filename = function() paste0("sensitivity_report_", Sys.Date(), ".csv"),
    content = function(file) {
      result <- sensitivity_results$result
      if (is.null(result)) {
        write.csv(data.frame(Message = "No sensitivity analysis results"), file, row.names = FALSE)
        return()
      }

      # Combine summary and metrics into one report
      summary_df <- result$method_summary
      summary_df$sensitivity_level <- result$sensitivity_level
      summary_df$cv_across_methods <- result$sensitivity_metrics$cv_across_methods
      summary_df$spread_pct <- result$sensitivity_metrics$spread_pct
      summary_df$range_ratio <- result$sensitivity_metrics$range_ratio

      write.csv(summary_df, file, row.names = FALSE)
    }
  )

  # ========== METHOD COMPARISON SCATTER PLOTS ==========
  
  # Method colors for consistency
  method_colors <- c("WCC" = "#E69F00", "BIOMASS" = "#56B4E9",
                     "allodb" = "#009E73", "Bunce" = "#CC79A7")
  
  # Single scatter plot: Reference method vs all others
  output$method_scatter_single <- renderPlot({
    result <- sensitivity_results$result
    df <- current_data()
    
    if (is.null(result) || is.null(df)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Run sensitivity analysis first") +
               theme_void())
    }
    
    ref_method <- input$sens_reference
    all_methods <- c("WCC", "BIOMASS", "allodb", "Bunce")
    other_methods <- setdiff(all_methods, ref_method)
    
    # Map method names to column names
    col_map <- c("WCC" = "WCC_C_t", "BIOMASS" = "biomass_C_t", 
                 "allodb" = "allodb_C_t", "Bunce" = "Bunce_C_t")
    
    ref_col <- col_map[ref_method]
    if (!ref_col %in% colnames(df)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = paste("Reference method", ref_method, "not in data")) +
               theme_void())
    }
    
    # Build comparison data
    scatter_data <- data.frame()
    for (m in other_methods) {
      m_col <- col_map[m]
      if (m_col %in% colnames(df)) {
        temp <- data.frame(
          Reference = df[[ref_col]],
          Comparison = df[[m_col]],
          Method = m
        )
        scatter_data <- rbind(scatter_data, temp)
      }
    }
    
    if (nrow(scatter_data) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No comparison data available") +
               theme_void())
    }
    
    scatter_data <- scatter_data[!is.na(scatter_data$Reference) & !is.na(scatter_data$Comparison), ]
    
    # Get max for equal axis
    max_val <- max(c(scatter_data$Reference, scatter_data$Comparison), na.rm = TRUE) * 1.1
    
    ggplot(scatter_data, aes(x = Reference, y = Comparison, color = Method)) +
      geom_point(alpha = 0.5, size = 2.5) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40", linewidth = 1) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
      scale_color_manual(values = method_colors[other_methods]) +
      coord_fixed(ratio = 1, xlim = c(0, max_val), ylim = c(0, max_val)) +
      labs(
        title = paste("Method Comparison:", ref_method, "vs Other Methods"),
        subtitle = "Dashed line = 1:1 (perfect agreement)",
        x = paste(ref_method, "Carbon (tonnes)"),
        y = "Other Method Carbon (tonnes)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
        legend.position = "bottom"
      )
  })
  
  # Faceted scatter plot: One panel per comparison method
  output$method_scatter_faceted <- renderPlot({
    result <- sensitivity_results$result
    df <- current_data()
    
    if (is.null(result) || is.null(df)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Run sensitivity analysis first") +
               theme_void())
    }
    
    ref_method <- input$sens_reference
    all_methods <- c("WCC", "BIOMASS", "allodb", "Bunce")
    other_methods <- setdiff(all_methods, ref_method)
    
    col_map <- c("WCC" = "WCC_C_t", "BIOMASS" = "biomass_C_t", 
                 "allodb" = "allodb_C_t", "Bunce" = "Bunce_C_t")
    
    ref_col <- col_map[ref_method]
    if (!ref_col %in% colnames(df)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = paste("Reference method", ref_method, "not in data")) +
               theme_void())
    }
    
    scatter_data <- data.frame()
    for (m in other_methods) {
      m_col <- col_map[m]
      if (m_col %in% colnames(df)) {
        temp <- data.frame(
          Reference = df[[ref_col]],
          Comparison = df[[m_col]],
          Method = m
        )
        scatter_data <- rbind(scatter_data, temp)
      }
    }
    
    if (nrow(scatter_data) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No comparison data available") +
               theme_void())
    }
    
    scatter_data <- scatter_data[!is.na(scatter_data$Reference) & !is.na(scatter_data$Comparison), ]
    
    ggplot(scatter_data, aes(x = Reference, y = Comparison)) +
      geom_point(alpha = 0.5, size = 2, color = "#0072B2") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
      geom_smooth(method = "lm", se = TRUE, color = "#D55E00", fill = "#D55E00", alpha = 0.2) +
      facet_wrap(~ Method, scales = "free") +
      labs(
        title = paste("Method Comparison:", ref_method, "vs Each Method"),
        subtitle = "Red dashed = 1:1 line; Orange = linear fit with 95% CI",
        x = paste(ref_method, "Carbon (tonnes)"),
        y = "Comparison Method Carbon (tonnes)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
        strip.text = element_text(face = "bold", size = 11)
      )
  })
  
  # Method correlation heatmap
  output$method_correlation_heatmap <- renderPlot({
    df <- current_data()
    
    if (is.null(df)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Calculate results first") +
               theme_void())
    }
    
    # Get carbon columns
    carbon_cols <- c("WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")
    available_cols <- carbon_cols[carbon_cols %in% colnames(df)]
    
    if (length(available_cols) < 2) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Need at least 2 methods for correlation") +
               theme_void())
    }
    
    # Build correlation matrix
    carbon_matrix <- df[, available_cols, drop = FALSE]
    
    # Rename columns for display
    method_names <- c("WCC_C_t" = "WCC", "biomass_C_t" = "BIOMASS", 
                      "allodb_C_t" = "allodb", "Bunce_C_t" = "Bunce")
    colnames(carbon_matrix) <- method_names[colnames(carbon_matrix)]
    
    cor_matrix <- cor(carbon_matrix, use = "pairwise.complete.obs")
    
    # Convert to long format
    cor_long <- as.data.frame(as.table(cor_matrix))
    colnames(cor_long) <- c("Method1", "Method2", "Correlation")
    
    ggplot(cor_long, aes(x = Method1, y = Method2, fill = Correlation)) +
      geom_tile(color = "white", linewidth = 1) +
      geom_text(aes(label = sprintf("%.2f", Correlation)), size = 5, fontface = "bold") +
      scale_fill_gradient2(low = "#d62728", mid = "white", high = "#2ca02c",
                           midpoint = 0.5, limits = c(0, 1), name = "r") +
      labs(
        title = "Correlation Between Methods",
        subtitle = "Per-tree carbon estimates",
        x = "", y = ""
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        legend.position = "right"
      ) +
      coord_fixed()
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

