# App2 to compare different allometries

library(shiny)
library(ggplot2)
library(TreeCarbon)

# Example lookup
lookup_latin_names <- c("", "Abies alba", "Quercus robur", "Fagus sylvatica")

ui <- fluidPage(
  titlePanel("Tree Above Ground Carbon: Allometry Comparison"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload CSV with columns: genus, species, dbh, height",
                accept = c(".csv")),
      selectInput("botanical_name", "Select Botanical Name:", choices = lookup_latin_names),
      textInput("common_name", "Or enter a name to search:", value = ""),
      selectInput("type", "Type (optional):", choices = c("broadleaf", "conifer", "NA"), selected = "NA"),
      sliderInput("dbh", "DBH (cm):", min = 0, max = 150, value = 20),
      sliderInput("height", "Height (m):", min = 0, max = 60, value = 15),
      selectInput("method", "Carbon conversion method:", choices = c("Thomas", "IPCC2")),
      checkboxInput("output_all", "Output all data", value = TRUE),
      actionButton("calculate", "Calculate"),
      downloadButton("download", "Download Results")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Results & Plot", plotOutput("allometryPlot"), tableOutput("results")),
        tabPanel("Instructions",
                 p("Upload a CSV or input single tree data."),
                 p("Plot compares AGC from WCC, allodb, and Bunce methods."))
      )
    )
  )
)

server <- function(input, output, session) {

  selected_name <- reactive({
    if (nzchar(input$common_name)) input$common_name else input$botanical_name
  })

  results_data <- eventReactive(input$calculate, {
    if (!is.null(input$datafile)) {
      df <- read.csv(input$datafile$datapath)
      if (!all(c("genus","species","dbh","height") %in% colnames(df)))
        stop("CSV must have columns: genus, species, dbh, height")
      df$type <- ifelse("type" %in% colnames(df), df$type, NA)
      df <- df %>% rowwise() %>% mutate(allo = list(allometries(genus, species, dbh, height, type, method=input$method)))
      do.call(rbind, df$allo)
    } else {
      allometries(genus = selected_name(), species = selected_name(),
                  dbh = input$dbh, height = input$height,
                  type = input$type, method = input$method)
    }
  })

  output$allometryPlot <- renderPlot({
    df <- req(results_data())
    df_long <- reshape2::melt(df, measure.vars = c("AGC_WCC_t","AGC_allodb_t","AGC_Bunce_t"),
                              variable.name = "Method", value.name = "AGC_t")
    ggplot(df_long, aes(x = dbh, y = AGC_t, color = Method)) +
      geom_point() +
      geom_line() +
      labs(x = "DBH (cm)", y = "AGC (t)", title = "AGC Comparison: WCC vs Allodb vs Bunce") +
      theme_minimal()
  })

  output$results <- renderTable({
    df <- req(results_data())
    if (input$output_all) df else df[, c("genus","species","dbh","height","AGC_WCC_t","AGC_allodb_t","AGC_Bunce_t")]
  })

  output$download <- downloadHandler(
    filename = function() paste0("allometry_results_", Sys.Date(), ".csv"),
    content = function(file) write.csv(results_data(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)


