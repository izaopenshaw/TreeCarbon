############# Comparison Statistics for Allometry Methods ################
#'
#' @title Calculate comparison statistics for allometry methods
#' @description Calculates summary statistics comparing different allometric methods
#' including mean differences, coefficient of variation, and method agreement metrics
#' @author Isabel Openshaw. I.Openshaw@kew.org
#' @param results_df Data frame with results from allometries function
#' @param returnv Either "AGC" for carbon or "AGB" for biomass
#' @return List with comparison statistics
#' @examples
#' results <- allometries(c("Quercus","Quercus"), c("robur","robur"), c(20, 30), c(10, 15))
#' compare_methods(results, "AGC")
#' @export
#'
compare_methods <- function(results_df, returnv = "AGC") {
  if (returnv == "AGC") {
    method_cols <- c("WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")
    sig_cols <- c("WCC_C_sig", "biomass_C_sig", "allodb_C_sig", "Bunce_C_sig")
    method_names <- c("WCC", "Biomass", "Allodb", "Bunce")
  } else {
    method_cols <- c("WCC_B_t", "biomass_B_t", "allodb_B_t", "Bunce_B_t")
    sig_cols <- c("WCC_B_sig", "biomass_B_sig", "allodb_B_sig", "Bunce_B_sig")
    method_names <- c("WCC", "Biomass", "Allodb", "Bunce")
  }

  # Extract method values
  method_data <- results_df[, intersect(method_cols, colnames(results_df)), drop = FALSE]
  if (ncol(method_data) == 0) {
    return(list(summary_stats = data.frame(Message = "No method data available")))
  }
  colnames(method_data) <- method_names[method_cols %in% colnames(results_df)]

  # Remove rows with all NAs
  valid_rows <- rowSums(!is.na(method_data)) > 0
  method_data <- method_data[valid_rows, , drop = FALSE]

  if (nrow(method_data) == 0) {
    return(list(summary_stats = data.frame(Statistic = "No valid data", Value = NA)))
  }

  # Calculate statistics
  stats_list <- list()

  # Mean values per method
  means <- colMeans(method_data, na.rm = TRUE)
  stats_list$mean <- data.frame(
    Method = names(means),
    Value = means,
    Statistic = "Mean"
  )

  # Standard deviation per method
  sds <- apply(method_data, 2, function(x) sd(x, na.rm = TRUE))
  stats_list$sd <- data.frame(
    Method = names(sds),
    Value = sds,
    Statistic = "SD"
  )

  # Coefficient of variation
  cv <- (sds / means) * 100
  stats_list$cv <- data.frame(
    Method = names(cv),
    Value = cv,
    Statistic = "CV (%)"
  )

  # Mean difference matrix
  n_methods <- length(colnames(method_data))
  method_names_actual <- colnames(method_data)
  diff_matrix <- matrix(NA, nrow = n_methods, ncol = n_methods)
  rownames(diff_matrix) <- colnames(diff_matrix) <- method_names_actual

  for (i in 1:n_methods) {
    for (j in 1:n_methods) {
      if (i != j) {
        diff_vals <- method_data[, i] - method_data[, j]
        diff_matrix[i, j] <- mean(diff_vals, na.rm = TRUE)
      }
    }
  }

  # Correlation matrix
  cor_matrix <- cor(method_data, use = "pairwise.complete.obs")

  # Method agreement (mean absolute difference)
  agreement <- data.frame(
    Method1 = character(),
    Method2 = character(),
    Mean_Abs_Diff = numeric(),
    Correlation = numeric()
  )

  for (i in 1:(n_methods - 1)) {
    for (j in (i + 1):n_methods) {
      diff_vals <- abs(method_data[, i] - method_data[, j])
      agreement <- rbind(agreement, data.frame(
        Method1 = method_names_actual[i],
        Method2 = method_names_actual[j],
        Mean_Abs_Diff = mean(diff_vals, na.rm = TRUE),
        Correlation = cor_matrix[i, j]
      ))
    }
  }

  # Round all statistics to 4 decimal places
  stats_list$mean$Value <- round(stats_list$mean$Value, 4)
  stats_list$sd$Value <- round(stats_list$sd$Value, 4)
  stats_list$cv$Value <- round(stats_list$cv$Value, 4)
  diff_matrix <- round(diff_matrix, 4)
  agreement$Mean_Abs_Diff <- round(agreement$Mean_Abs_Diff, 4)
  agreement$Correlation <- round(agreement$Correlation, 4)

  return(list(
    summary_stats = rbind(stats_list$mean, stats_list$sd, stats_list$cv),
    mean_differences = diff_matrix,
    correlation_matrix = cor_matrix,  # Note: correlation often = 1 for single tree, less useful
    method_agreement = agreement
  ))
}

############# Plot Allometry Comparison ################
#'
#' @title Create comparison plots for allometry methods
#' @description Creates ggplot2 or plotly plots comparing different allometric methods
#' @author Isabel Openshaw. I.Openshaw@kew.org
#' @param results_df Data frame with results from allometries function
#' @param plot_type Type of plot: "bar", "dbh", or "height" (dbh and height are scatter plots)
#' @param returnv Either "AGC" for carbon or "AGB" for biomass
#' @param show_errors Logical, whether to show error bars
#' @param interactive Logical, whether to return plotly plot
#' @return ggplot or plotly object
#' @import ggplot2
#' @importFrom scales viridis_pal
#' @importFrom RColorBrewer brewer.pal
#' @examples
#' results <- allometries(c("Quercus","Quercus"), c("robur","robur"), c(20, 30), c(10, 15))
#' plot_allometry_comparison(results, "bar", "AGC", interactive = FALSE)
#' @export
#'
plot_allometry_comparison <- function(results_df, plot_type = "bar", returnv = "AGC",
                                      show_errors = TRUE, interactive = FALSE,
                                      log_scale = FALSE, color_scheme = "default",
                                      font_size = 12, plot_width = NULL,
                                      x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL) {

  # Auto-detect data type if returnv doesn't match available columns
  has_AGC <- any(grepl("_C_t$", colnames(results_df)))
  has_AGB <- any(grepl("_B_t$", colnames(results_df)))

  # If returnv doesn't match data, auto-detect
  if (returnv == "AGC" && !has_AGC && has_AGB) {
    returnv <- "AGB"
    warning("Data contains AGB columns but returnv='AGC' was specified. Using AGB instead.")
  } else if (returnv == "AGB" && !has_AGB && has_AGC) {
    returnv <- "AGC"
    warning("Data contains AGC columns but returnv='AGB' was specified. Using AGC instead.")
  }

  # Determine columns
  if (returnv == "AGC") {
    methods_cols <- c("WCC" = "WCC_C_t", "Biomass" = "biomass_C_t",
                      "Allodb" = "allodb_C_t", "Bunce" = "Bunce_C_t")
    err_cols <- c("WCC" = "WCC_C_sig", "Biomass" = "biomass_C_sig",
                  "Allodb" = "allodb_C_sig", "Bunce" = "Bunce_C_sig")
    y_label <- "Carbon (t)"
  } else {
    methods_cols <- c("WCC" = "WCC_B_t", "Biomass" = "biomass_B_t",
                      "Allodb" = "allodb_B_t", "Bunce" = "Bunce_B_t")
    err_cols <- c("WCC" = "WCC_B_sig", "Biomass" = "biomass_B_sig",
                  "Allodb" = "allodb_B_sig", "Bunce" = "Bunce_B_sig")
    y_label <- "Biomass (t)"
  }

  # Check which columns actually exist in the data
  available_method_cols <- methods_cols[methods_cols %in% colnames(results_df)]

  if (length(available_method_cols) == 0) {
    # Try to provide helpful error message
    if (!has_AGC && !has_AGB) {
      warning("No method columns found in results_df (neither _C_t nor _B_t columns). Available columns: ",
              paste(colnames(results_df), collapse = ", "))
    } else {
      warning("No matching method columns found for returnv='", returnv, "'. Available columns: ",
              paste(colnames(results_df), collapse = ", "),
              ". Try using returnv='", ifelse(has_AGC, "AGC", "AGB"), "' instead.")
    }
    return(NULL)
  }

  # Build long dataframe
  n <- nrow(results_df)
  plot_df_list <- lapply(names(available_method_cols), function(m) {
    col_name <- available_method_cols[m]
    if (col_name %in% colnames(results_df)) {
      data.frame(
        tree = seq_len(n),
        DBH = results_df$dbh,
        Height = results_df$height,
        method = m,
        value = results_df[[col_name]],
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  plot_df <- do.call(rbind, Filter(Negate(is.null), plot_df_list))

  if (is.null(plot_df) || nrow(plot_df) == 0) {
    warning("No data to plot after filtering")
    return(NULL)
  }

  # Remove rows with all NA values
  plot_df <- plot_df[!is.na(plot_df$value), ]

  if (nrow(plot_df) == 0) {
    warning("No non-NA values to plot")
    return(NULL)
  }

  # Add error bars if available
  if (show_errors) {
    plot_df$se <- NA_real_
    available_err_cols <- err_cols[err_cols %in% colnames(results_df)]
    for (m in names(available_err_cols)) {
      if (m %in% plot_df$method) {
        err_vals <- results_df[[available_err_cols[m]]]
        if (!all(is.na(err_vals))) {
          plot_df$se[plot_df$method == m] <- err_vals
        }
      }
    }
    # For log scale, allow ymin to go below 0 (will be handled by scale)
    # For linear scale, ensure ymin >= small positive value
    if (log_scale) {
      plot_df$ymin <- plot_df$value - plot_df$se
      plot_df$ymax <- plot_df$value + plot_df$se
    } else {
      plot_df$ymin <- pmax(plot_df$value - plot_df$se, 1e-6, na.rm = TRUE)
      plot_df$ymax <- plot_df$value + plot_df$se
    }
  }

  # Define color schemes
  if (color_scheme == "viridis") {
    method_colors <- scales::viridis_pal()(length(unique(plot_df$method)))
  } else if (color_scheme == "brewer") {
    method_colors <- RColorBrewer::brewer.pal(max(3, length(unique(plot_df$method))), "Set2")[1:length(unique(plot_df$method))]
  } else {
    # Default colors
    method_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
  }
  names(method_colors) <- unique(plot_df$method)

  # Create plots
  if (plot_type == "bar") {
    if (length(unique(plot_df$tree)) == 1) {
      p <- ggplot(plot_df, aes(method, value, fill = method)) +
        geom_col(width = 0.6) +
        scale_fill_manual(values = method_colors) +
        geom_text(aes(label = signif(value, 2)), vjust = -0.4, size = font_size * 0.3) +
        labs(x = "Method", y = y_label, title = "Carbon/Biomass Estimates for Single Tree") +
        theme_minimal(base_size = font_size) +
        theme(legend.position = "none")
      if (show_errors && !all(is.na(plot_df$se))) {
        p <- p + geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1, na.rm = TRUE)
      }
    } else {
      sum_df <- merge(
        aggregate(value ~ method, plot_df, sum, na.rm = TRUE),
        aggregate(se ~ method, plot_df, function(x) sqrt(sum(x^2, na.rm = TRUE))),
        by = "method"
      )
      # For log scale, allow ymin to go below 0
      if (log_scale) {
        sum_df$ymin <- sum_df$value - sum_df$se
        sum_df$ymax <- sum_df$value + sum_df$se
      } else {
        sum_df$ymin <- pmax(sum_df$value - sum_df$se, 1e-6, na.rm = TRUE)
        sum_df$ymax <- sum_df$value + sum_df$se
      }

      p <- ggplot(sum_df, aes(method, value, fill = method)) +
        geom_col(width = 0.6) +
        scale_fill_manual(values = method_colors[names(method_colors) %in% sum_df$method]) +
        geom_text(aes(label = signif(value, 2)), vjust = -0.4, size = font_size * 0.3) +
        labs(x = "Method", y = y_label, title = paste("Total", y_label, "Estimates per Method")) +
        theme_minimal(base_size = font_size) +
        theme(legend.position = "none")
      if (show_errors && !all(is.na(sum_df$se))) {
        p <- p + geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1, na.rm = TRUE)
      }
    }
  } else if (plot_type == "dbh") {
    # Calculate proportional jitter based on data range
    dbh_range <- diff(range(plot_df$DBH, na.rm = TRUE))
    jitter_width <- if (dbh_range > 0) dbh_range * 0.02 else 0.1  # 2% of range, min 0.1

    p <- ggplot(plot_df, aes(DBH, value, colour = method)) +
      geom_point(aes(size = Height), position = position_jitter(width = jitter_width, seed = 123)) +
      scale_color_manual(values = method_colors) +
      labs(x = "DBH (cm)", y = y_label, title = "Carbon/Biomass Estimates Across Allometric Methods") +
      theme_minimal(base_size = font_size)
    if (show_errors && !all(is.na(plot_df$se))) {
      # For error bars, use smaller jitter to keep them aligned with points
      p <- p + geom_errorbar(aes(ymin = ymin, ymax = ymax),
                             position = position_jitter(width = jitter_width, seed = 123),
                             width = jitter_width * 2, na.rm = TRUE)
    }
  } else if (plot_type == "height") {
    # Calculate proportional jitter based on data range
    height_range <- diff(range(plot_df$Height, na.rm = TRUE))
    jitter_width <- if (height_range > 0) height_range * 0.02 else 0.1  # 2% of range, min 0.1

    p <- ggplot(plot_df, aes(Height, value, colour = method)) +
      geom_point(aes(size = DBH), position = position_jitter(width = jitter_width, seed = 123)) +
      scale_color_manual(values = method_colors) +
      labs(x = "Height (m)", y = y_label, title = "Carbon/Biomass Estimates Across Allometric Methods") +
      theme_minimal(base_size = font_size)
    if (show_errors && !all(is.na(plot_df$se)) && any(!is.na(plot_df$ymin))) {
      # For error bars, use smaller jitter to keep them aligned with points
      p <- p + geom_errorbar(aes(ymin = ymin, ymax = ymax),
                             position = position_jitter(width = jitter_width, seed = 123),
                             width = jitter_width * 2, na.rm = TRUE)
    }
  } else {
    return(NULL)
  }

  # Apply axis limits if specified
  x_limits <- NULL
  if ((!is.null(x_min) && !is.na(x_min)) || (!is.null(x_max) && !is.na(x_max))) {
    x_limits <- c(if (!is.null(x_min) && !is.na(x_min)) x_min else -Inf,
                  if (!is.null(x_max) && !is.na(x_max)) x_max else Inf)
    p <- p + xlim(x_limits)
  }

  y_limits <- NULL
  if ((!is.null(y_min) && !is.na(y_min)) || (!is.null(y_max) && !is.na(y_max))) {
    y_limits <- c(if (!is.null(y_min) && !is.na(y_min)) y_min else -Inf,
                  if (!is.null(y_max) && !is.na(y_max)) y_max else Inf)
    p <- p + ylim(y_limits)
  }

  # Apply log scale if requested (only for scatter plots, not bar plots)
  # Bar plots should always use linear scale as they represent absolute values from 0
  if (log_scale && plot_type != "bar") {
    if (!interactive) {
      p <- p + scale_y_log10()
    }
  }

  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    p_plotly <- plotly::ggplotly(p)
    # Only apply log scale to scatter plots, not bar plots
    if (log_scale && plot_type != "bar") {
      p_plotly <- p_plotly %>% plotly::layout(yaxis = list(type = "log"))
    }
    # Apply axis limits for plotly (if not already applied via xlim/ylim)
    if (!is.null(x_limits)) {
      p_plotly <- p_plotly %>% plotly::layout(xaxis = list(range = x_limits))
    }
    if (!is.null(y_limits)) {
      p_plotly <- p_plotly %>% plotly::layout(yaxis = list(range = y_limits))
    }
    return(p_plotly)
  } else {
    return(p)
  }
}
