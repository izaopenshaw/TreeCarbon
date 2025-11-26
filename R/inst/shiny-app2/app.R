# App2 to compare different allometries

library(shiny)
library(ggplot2)
library(TreeCarbon)

ui <- fluidPage(
  titlePanel("Tree Above Ground Carbon: Allometry Comparison"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload a CSV file with tree data",
                accept = c(".csv")),
      textInput("genus", "Genus", value = "Quercus"),
      textInput("species", "Species", value = "robur"),
      selectInput("type", "Type (optional):", choices = c("broadleaf", "conifer", "NA"), selected = "broadleaf"),
      sliderInput("dbh", "DBH (cm):", min = 0, max = 150, value = 20, step = 1),
      sliderInput("height", "Height (m):", min = 0, max = 60, value = 15, step = 1),
      selectInput("method", "Carbon to Biomass Conversion Method:", choices = c("Thomas", "IPCC2", "Matthews1", "Matthews2", "IPCC1"), selected = "IPCC2"),
      selectInput("plot_type", "Plot Type:", choices = c(
        "Bar plot (sum)" = "bar",
        "DBH vs Carbon/Biomass" = "dbh",
        "Height vs Carbon/Biomass" = "height"
      ), selected = "bar"),
      checkboxInput("checkTaxo", "Check taxonomy spelling", value = FALSE),
      selectInput("returnv", "Return carbon or biomass:", choices = c("carbon" = "AGC", "biomass" = "AGB"), selected = "AGC"),
      selectInput("biome", "Biome", choices = c("temperate", "boreal", "mediterranean", "tropical", "subtropical"), selected = "temperate"),
      selectInput("region", "Region", choices = c(
        "Africa (extra tropical)" = "Africa (extratropical)",
        "Africa (tropical)" = "Africa (tropical)",
        "Australia" = "Australia",
        "Australia (tropical)" = "Australia/PNG (tropical)",
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
        "South America (extra tropical)" = "South America (extratropical)",
        "South America (tropical)" = "South America (tropical)",
        "World" = "World"
      ), selected = "Europe"),
      numericInput("longitude", "Longitude", value = -0.088837, step = 0.0001),
      numericInput("latitude", "Latitude", value = 51.071610, step = 0.0001),
      actionButton("calculate", "Calculate"),
      actionButton("clear", "Clear Plot"),
      downloadButton("download", "Download Results")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Results and Plot", plotOutput("dbhPlot"), tableOutput("results")),
        tabPanel("Instructions",
                 h4("How to use this app:"),
                 p("1. Upload a CSV file OR enter single tree data:"),
                 tags$ul(
                   tags$li("CSV file: Must contain columns 'genus', 'species', 'dbh' (cm), 'height' (m), and optionally 'type' (broadleaf/conifer)"),
                   tags$li("Single tree: Enter genus, species, DBH, and height, then click 'Calculate'")
                 ),
                 p("2. Select plot type:"),
                 tags$ul(
                   tags$li("Bar plot: Shows total carbon/biomass summed across all methods (WCC, Biomass, Allodb, Bunce)"),
                   tags$li("DBH vs Carbon/Biomass: Scatter plot showing relationship between DBH and carbon/biomass estimates"),
                   tags$li("Height vs Carbon/Biomass: Scatter plot showing relationship between height and carbon/biomass estimates")
                 ),
                 p("3. Cumulative plotting: Each time you click 'Calculate' with new tree data, it adds to the existing plot. Use 'Clear Plot' to start over."),
                 p("4. Results table shows carbon/biomass estimates from four methods:"),
                 tags$ul(
                   tags$li("WCC: Woodland Carbon Code method"),
                   tags$li("Biomass: BIOMASS package method"),
                   tags$li("Allodb: Allometric database method"),
                   tags$li("Bunce: Bunce equation method")
                 ),
                 p("5. Error bars on plots represent uncertainty estimates (sig_AGC or sig_AGB) when available."))
      )
    )
  )
)

server <- function(input, output, session) {

  # Store cumulative data for plotting
  cumulative_data <- reactiveValues(data = NULL)


  # Calculate new results
  new_results <- eventReactive(input$calculate, {
    coords <- c(input$longitude, input$latitude)
    type_val <- if (input$type == "NA") NULL else input$type

    if (!is.null(input$datafile)) {
      # CSV file upload
      df <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE)

      # Check for required columns
      required_cols <- c("genus", "species", "dbh", "height")
      missing_cols <- setdiff(required_cols, colnames(df))
      if (length(missing_cols) > 0) {
        stop(paste("CSV must have the following columns:", paste(required_cols, collapse = ", "),
                   ". Missing:", paste(missing_cols, collapse = ", ")))
      }

      # Check for empty values in required columns
      if (any(is.na(df$genus) | df$genus == "", na.rm = TRUE) ||
          any(is.na(df$species) | df$species == "", na.rm = TRUE)) {
        stop("CSV must have non-empty values for 'genus' and 'species' columns")
      }

      # Handle type column (optional)
      if ("type" %in% colnames(df)) {
        df$type <- ifelse(df$type == "NA" | is.na(df$type), NA, df$type)
      } else {
        df$type <- NA
      }

      # Apply allometries function row by row
      result_list <- list()
      for (i in 1:nrow(df)) {
        tree_type <- if (is.na(df$type[i]) || df$type[i] == "NA") type_val else df$type[i]
        if (!is.null(tree_type) && tree_type == "NA") tree_type <- NULL

        result_list[[i]] <- tryCatch({
          allometries(genus = as.character(df$genus[i]),
                      species = as.character(df$species[i]),
                      dbh = df$dbh[i],
                      height = df$height[i],
                      type = tree_type,
                      method = input$method,
                      returnv = input$returnv,
                      region = input$region,
                      biome = input$biome,
                      coords = coords,
                      checkTaxo = input$checkTaxo)
        }, error = function(e) {
          # Create minimal data frame with NAs - include all expected columns
          result_cols <- if (input$returnv == "AGC") {
            c("genus", "species", "dbh", "height", "WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")
          } else {
            c("genus", "species", "dbh", "height", "WCC_B_t", "biomass_B_t", "allodb_B_t", "Bunce_B_t")
          }
          result_df <- data.frame(matrix(NA, nrow = 1, ncol = length(result_cols)))
          colnames(result_df) <- result_cols
          result_df$genus <- df$genus[i]
          result_df$species <- df$species[i]
          result_df$dbh <- df$dbh[i]
          result_df$height <- df$height[i]
          result_df
        })
      }

      # Combine results
      do.call(rbind, result_list)
    } else {
      # Single tree calculation
      if (input$genus == "" || input$species == "") {
        stop("Please enter both genus and species")
      }

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
                  checkTaxo = input$checkTaxo)
    }
  })

  # Update cumulative data when new results are calculated
  observeEvent(input$calculate, {
    new_data <- new_results()
    if (is.null(cumulative_data$data)) {
      cumulative_data$data <- new_data
    } else {
      cumulative_data$data <- rbind(cumulative_data$data, new_data)
    }
  })

  # Clear cumulative data
  observeEvent(input$clear, {
    cumulative_data$data <- NULL
  })

  # Get current data for display (cumulative or new)
  current_data <- reactive({
    if (is.null(cumulative_data$data)) {
      new_results()
    } else {
      cumulative_data$data
    }
  })

  output$dbhPlot <- renderPlot({
    out <- current_data()

    if (is.null(out) || nrow(out) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data to plot. Click 'Calculate' to add data.") +
               theme_void())
    }

    # Determine which carbon/biomass columns to use
    all_cols <- colnames(out)
    if (input$returnv == "AGC") {
      # Map columns based on actual function output: WCC_C_t, biomass_C_t, allodb_C_t, Bunce_C_t
      methods_cols <- c()
      if ("WCC_C_t" %in% all_cols) methods_cols["WCC"] <- "WCC_C_t"
      if ("biomass_C_t" %in% all_cols) methods_cols["biomass"] <- "biomass_C_t"
      if ("allodb_C_t" %in% all_cols) methods_cols["allodb"] <- "allodb_C_t"
      if ("Bunce_C_t" %in% all_cols) methods_cols["Bunce"] <- "Bunce_C_t"

      # Error columns: WCC_C_sig, biomass_C_sig, allodb_C_sig, Bunce_C_sig
      err_cols <- c()
      if ("WCC_C_sig" %in% all_cols) err_cols["WCC"] <- "WCC_C_sig"
      if ("biomass_C_sig" %in% all_cols) err_cols["biomass"] <- "biomass_C_sig"
      if ("allodb_C_sig" %in% all_cols) err_cols["allodb"] <- "allodb_C_sig"
      if ("Bunce_C_sig" %in% all_cols) err_cols["Bunce"] <- "Bunce_C_sig"

      carbon_label <- "Carbon (t)"
    } else {
      # For biomass: WCC_B_t, biomass_B_t, allodb_B_t, Bunce_B_t
      methods_cols <- c()
      if ("WCC_B_t" %in% all_cols) methods_cols["WCC"] <- "WCC_B_t"
      if ("biomass_B_t" %in% all_cols) methods_cols["biomass"] <- "biomass_B_t"
      if ("allodb_B_t" %in% all_cols) methods_cols["allodb"] <- "allodb_B_t"
      if ("Bunce_B_t" %in% all_cols) methods_cols["Bunce"] <- "Bunce_B_t"

      # Error columns for biomass: WCC_B_sig, biomass_B_sig, allodb_B_sig, Bunce_B_sig
      err_cols <- c()
      if ("WCC_B_sig" %in% all_cols) err_cols["WCC"] <- "WCC_B_sig"
      if ("biomass_B_sig" %in% all_cols) err_cols["biomass"] <- "biomass_B_sig"
      if ("allodb_B_sig" %in% all_cols) err_cols["allodb"] <- "allodb_B_sig"
      if ("Bunce_B_sig" %in% all_cols) err_cols["Bunce"] <- "Bunce_B_sig"

      carbon_label <- "Biomass (t)"
    }

    if (length(methods_cols) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No carbon/biomass data available") +
               theme_void())
    }

    n <- nrow(out)

    # Build long dataframe
    df_list <- lapply(names(methods_cols), function(m) {
      col_val <- out[[methods_cols[m]]]
      data.frame(
        tree = seq_len(n),
        DBH = out$dbh,
        Height = out$height,
        method = m,
        value = col_val,
        stringsAsFactors = FALSE
      )
    })

    plot_df <- do.call(rbind, df_list)

    # Attach SE
    plot_df$se <- NA_real_
    for (m in names(err_cols)) {
      if (m %in% plot_df$method) {
        plot_df$se[plot_df$method == m] <- out[[err_cols[m]]]
      }
    }

    plot_df <- plot_df[!is.na(plot_df$value), ]

    plot_df$ymin <- pmax(plot_df$value - plot_df$se, 1e-6)
    plot_df$ymax <- plot_df$value + plot_df$se

    # IF ONLY ONE TREE
    if (length(unique(plot_df$tree)) == 1) {
      if (input$plot_type == "bar") {
        ggplot(plot_df, aes(method, value, fill = method)) +
          geom_col(width = 0.6) +
          geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1) +
          geom_text(aes(label = signif(value, 2)), vjust = -0.4, size = 3.5) +
          labs(x = "Method", y = carbon_label,
               title = paste("Carbon/Biomass Estimates for Single Tree")) +
          theme_minimal() +
          theme(legend.position = "none")
      } else if (input$plot_type == "dbh") {
        ggplot(plot_df, aes(DBH, value, colour = method)) +
          geom_point(aes(size = Height), position = position_dodge(width = 0.2)) +
          geom_errorbar(aes(ymin = ymin, ymax = value + se), position = position_dodge(width = 0.2), width = 0.2) +
          scale_alpha_continuous(range = c(0.3, 1)) +
          labs(x = "DBH (cm)", y = carbon_label, title = "Carbon/Biomass Estimates Across Allometric Methods") +
          theme_minimal()
      } else if (input$plot_type == "height") {
        ggplot(plot_df, aes(Height, value, colour = method)) +
          geom_point(aes(size = DBH), position = position_dodge(width = 0.2)) +
          geom_errorbar(aes(ymin = ymin, ymax = value + se), position = position_dodge(width = 0.2), width = 0.2) +
          scale_alpha_continuous(range = c(0.3, 1)) +
          labs(x = "Height (m)", y = carbon_label, title = "Carbon/Biomass Estimates Across Allometric Methods") +
          theme_minimal()
      }
    } else {
      # Multiple trees
      if (input$plot_type == "bar") {
        # Sum of values and sum of errors (sqrt(x^2))
        sum_df <- merge(
          aggregate(value ~ method, plot_df, sum, na.rm = TRUE),
          aggregate(se ~ method, plot_df, function(x) sqrt(sum(x^2, na.rm = TRUE))),
          by = "method"
        )

        # Compute ymin / ymax for plotting
        sum_df$ymin <- pmax(sum_df$value - sum_df$se, 1e-6)
        sum_df$ymax <- sum_df$value + sum_df$se

        ggplot(sum_df, aes(method, value, fill = method)) +
          geom_col(width = 0.6) +
          geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1) +
          geom_text(aes(label = signif(value, 2)), vjust = -0.4, size = 3.5) +
          labs(x = "Method", y = carbon_label,
               title = paste("Total", carbon_label, "Estimates per Method")) +
          theme_minimal() +
          theme(legend.position = "none")
      } else if (input$plot_type == "dbh") {
        ggplot(plot_df, aes(factor(tree), value, colour = method)) +
          geom_point(aes(size = DBH, alpha = Height), position = position_dodge(width = 0.2)) +
          geom_errorbar(aes(ymin = ymin, ymax = value + se), position = position_dodge(width = 0.2), width = 0.2) +
          scale_alpha_continuous(range = c(0.3, 1)) +
          labs(x = "Tree", y = carbon_label, title = "Carbon/Biomass Estimates Across Allometric Methods") +
          theme_minimal()
      } else if (input$plot_type == "height") {
        ggplot(plot_df, aes(factor(tree), value, colour = method)) +
          geom_point(aes(size = Height, alpha = DBH), position = position_dodge(width = 0.2)) +
          geom_errorbar(aes(ymin = ymin, ymax = value + se), position = position_dodge(width = 0.2), width = 0.2) +
          scale_alpha_continuous(range = c(0.3, 1)) +
          labs(x = "Tree", y = carbon_label, title = "Carbon/Biomass Estimates Across Allometric Methods") +
          theme_minimal()
      }
    }
  })

  output$results <- renderTable({
    df <- current_data()
    if (is.null(df)) {
      return(data.frame(Message = "No data calculated yet. Click 'Calculate' to add data."))
    }

    # Select available columns - basic info plus carbon/biomass columns
    basic_cols <- c("genus", "species", "dbh", "height")
    if (input$returnv == "AGC") {
      carbon_cols <- colnames(df)[grepl("_C_t$|_C_sig$", colnames(df))]
    } else {
      carbon_cols <- colnames(df)[grepl("_B_t$|_B_sig$", colnames(df))]
    }
    avail_cols <- intersect(c(basic_cols, carbon_cols), colnames(df))
    if (length(avail_cols) > 0) {
      df[, avail_cols, drop = FALSE]
    } else {
      df
    }
  })

  output$download <- downloadHandler(
    filename = function() paste0("allometry_results_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- current_data()
      if (is.null(df)) {
        df <- new_results()
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

