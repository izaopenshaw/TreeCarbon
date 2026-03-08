# App2 - Global: libraries and shared helpers
# Features: Interactive plots, comparison stats, export options, sensitivity analysis

library(shiny)
library(ggplot2)
library(TreeCarbon)

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

# Helper to strip ANSI color codes (used in server)
strip_ansi <- function(x) {
  gsub("\033\\[[0-9;]*m", "", x)
}
