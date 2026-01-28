# ==============================================================================
# TreeCarbon - Uncertainty Propagation Module
# ==============================================================================
#
# This file provides uncertainty quantification for allometric biomass and
# carbon estimates.
# Four main sources of uncertainty:
#   1. Measurement error (DBH ± mm, height ± m)
#   2. Equation residual error (model prediction error)
#   3. Parameter uncertainty (regression coefficient uncertainty)
#   4. Method choice uncertainty (variation across allometric methods)
#
# Functions included:
#   - Analytical error propagation (error_product)
#   - Monte Carlo uncertainty simulation (mc_uncertainty)
#   - Comprehensive uncertainty analysis (comprehensive_uncertainty)
#
# Authors: Justin Moat (J.Moat@kew.org), Isabel Openshaw (I.Openshaw@kew.org)
#
# References:
#   - Taylor, J.R. (1997). An Introduction to Error Analysis (2nd ed.).
#     University Science Books.
#
# Suppress R CMD check notes for non-standard evaluation
utils::globalVariables(c("source", "contribution_pct", "cv", "rr"))

############# Propagation of error for a product ################
#' @title Analytical error progression for a product or a quotient
#' @description Calculates sigma squared for f when either f = a * b, f = a / b
#'  or f = a * b * c, f = a / b * c, f = a * b / c
#' @author Isabel Openshaw. I.Openshaw@kew.org, Justin Moat. J.Moat@kew.org
#' @param a first variable in function
#' @param sig_a sigma for a
#' @param b second variable in function
#' @param sig_b sigma for b
#' @param c (optional) third variable in function
#' @param sig_c (optional) sigma for c
#' @param returnv return value, either 'sigma' (standard deviation) or
#' 'sigma squared' (variance)
#' @param fn function describing how the variables are related. If not specified
#'  assumes that the variables are related by a product.
#' @return either an estimate for sigma or sigma squared error propagation for a
#' product or a quotient of two or three variables.
#' @references Taylor, J. R. (1997). An Introduction to Error Analysis: The
#' Study of Uncertainties in Physical Measurements (2nd ed.). University
#' Science Books.
#' @examples
#' a = 5 ; sig_a = 0.01
#' b = 10 ; sig_b = 0.1
#' error_product(a, sig_a, b, sig_b)
#' c = 12 ; sig_c = 0.05
#' error_product(a, sig_a, b, sig_b, c, sig_c)
#' # error for a quotient
#' error_product(a, sig_a, b, sig_b, fn = a/b)
#' @export
#' @aliases error_product
#'
error_product <- function(a, sig_a, b, sig_b, c = NULL, sig_c = NULL,
                          returnv = "sigmasquared", fn = NULL){
  if (!is.numeric(a) || !is.numeric(b) || !is.numeric(sig_a) ||
      !is.numeric(sig_b)) {
    stop("inputs must be numeric")
  }
  if (!is.character(returnv) || !(returnv %in% c("sigma", "sigmasquared"))) {
    stop("returnv must be either 'sigma' or 'sigmasquared'")
  }

  if(is.null(c)) { # function is either a*b or a/b

    if(is.null(fn)){ fn <- a * b }

    if(returnv == "sigma") {
      # sigma = |f| * sqrt((sig_a/a)^2 + (sig_b/b)^2)
      result <- abs(fn) * sqrt((sig_a / a)^2 + (sig_b / b)^2)
    } else {
      # sigma_squared = f^2 * ((sig_a/a)^2 + (sig_b/b)^2)
      # Fixed: was missing outer parentheses around the sum
      result <- (fn)^2 * ((sig_a / a)^2 + (sig_b / b)^2)
    }

  } else { # if c is not null, and function is a*b*c or a*b/c or a/b*c
    if (!is.numeric(c) || !is.numeric(sig_c)) {
      stop("c and sig_c must be numeric")
    }
    if(is.null(fn)){ fn <- a * b * c}


    if(returnv == "sigma") {
      # sigma
      result <- abs(fn) * sqrt((sig_a / a)^2 + (sig_b / b)^2 + (sig_c / c)^2)
    } else {
      # sigma_squared
      result <- (fn)^2 * ( (sig_a / a)^2 + (sig_b / b)^2 + (sig_c / c)^2 )
    }
  }

  return(result)
}



############# Core Monte Carlo Uncertainty Function #############
#' Monte Carlo Uncertainty Propagation
#' @description Propagates multiple sources of uncertainty through allometric fn
#' using Monte Carlo simulation. Reports comprehensive uncertainty statistics
#' including median, quantile intervals, and distribution shape metrics.
#'
#' \itemize{
#'   \item Captures non-linear effects that first-order approximations miss
#'   \item Handles correlated inputs
#'   \item Reports asymmetric uncertainty (common in log-transformed allometries)
#'   \item Allows decomposition of uncertainty by source
#' }
#' @param fn A function that computes the biomass/carbon estimate.
#'   Must accept named arguments matching the names in \code{inputs}.
#' @param inputs A named list where each element is itself a list with:
#'   \describe{
#'     \item{mean}{Central value (required)}
#'     \item{sd}{Standard deviation (required)}
#'     \item{dist}{Distribution type: "normal" (default), "lognormal", "uniform"}
#'     \item{source}{Uncertainty source label: "measurement", "parameter",
#'       "residual", "other"}
#'   }
#' @param N Number of Monte Carlo samples (default: 1000). Higher values give
#'   more stable estimates but take longer. Recommended: 1000 for quick checks,
#'   10000 for final results.
#' @param corr_matrix Optional correlation matrix for inputs. If NULL, inputs
#'   are assumed independent.
#' @param conf Confidence level for intervals (default: 0.90 for 0.5-0.95).
#' @param seed Random seed for reproducibility. Set to NULL for no seed.
#' @param decompose Logical. If TRUE, also compute variance decomposition by
#'   uncertainty source. Default FALSE.
#' @param extra_args Named list of additional fixed arguments to pass to fn.
#'
#' @return An S3 object of class "mc_uncertainty" containing:
#' \describe{
#'   \item{estimate}{Point estimate (mean of simulations)}
#'   \item{median}{Median of simulations (often better for skewed distributions)}
#'   \item{sd}{Standard deviation of simulations}
#'   \item{cv}{Coefficient of variation (percent)}
#'   \item{ci_low}{Lower confidence bound}
#'   \item{ci_high}{Upper confidence bound}
#'   \item{conf}{Confidence level used}
#'   \item{skewness}{Distribution skewness (0 = symmetric)}
#'   \item{kurtosis}{Distribution kurtosis (3 = normal)}
#'   \item{samples}{All simulation samples (for custom analysis)}
#'   \item{n_samples}{Number of valid samples used}
#'   \item{inputs_summary}{Summary of input distributions}
#'   \item{decomposition}{Variance decomposition by source (if decompose=TRUE)}
#' }
#'
#' @details
#' \strong{Uncertainty Sources:}
#' \enumerate{
#'   \item \strong{Measurement error}: Uncertainty in field measurements
#'     (DBH typically +/- 1-5 mm, height +/- 0.5-2 m)
#'   \item \strong{Parameter uncertainty}: Uncertainty in allometric equation
#'     coefficients (from regression standard errors)
#'   \item \strong{Residual error}: Prediction error of the allometric model
#'     (from model RSE/RMSE)
#'   \item \strong{Method choice}: Variation across different allometric methods
#' }
#'
#' \strong{Interpretation Guidelines:}
#'
#' \itemize{
#'   \item CV < 10 percent: Low uncertainty, estimates reliable
#'   \item CV 10-30 percent: Moderate uncertainty, typical for field data
#'   \item CV > 30 percent: High uncertainty, interpret with caution
#'   \item Absolute skewness > 1: Highly asymmetric, use median not mean
#' }
#'
#' @examples
#' # Simple example: Bunce equation with measurement + parameter uncertainty
#' bunce_fn <- function(dbh, a, b) {
#'   exp(a + b * log(pi * dbh))
#' }
#'
#' inputs <- list(
#'   dbh = list(mean = 30, sd = 0.75, source = "measurement"),
#'   a = list(mean = 1.868, sd = 0.047, source = "parameter"),
#'   b = list(mean = 2.268, sd = 0.057, source = "parameter")
#' )
#'
#' result <- mc_uncertainty(bunce_fn, inputs, N = 1000, decompose = TRUE)
#' print(result)
#' summary(result)
#'
#' # With lognormal distribution for DBH (accounts for positive constraint)
#' inputs_ln <- list(
#'   dbh = list(mean = 30, sd = 2, dist = "lognormal", source = "measurement"),
#'   a = list(mean = 1.868, sd = 0.047, source = "parameter"),
#'   b = list(mean = 2.268, sd = 0.057, source = "parameter")
#' )
#' result_ln <- mc_uncertainty(bunce_fn, inputs_ln, N = 1000)
#'
#' @references
#' \itemize{
#'   \item Chave, J. et al. (2004). Error propagation and scaling for tropical
#'     forest biomass estimates. Phil. Trans. R. Soc. Lond. B 359, 409-420.
#'   \item Molto, Q. et al. (2013). Error propagation in biomass estimation in
#'     tropical forests. Methods in Ecology and Evolution 4, 175-183.
#' }
#'
#' @export
mc_uncertainty <- function(fn, inputs, N = 1000, corr_matrix = NULL,
                           conf = 0.90, seed = 42, decompose = FALSE,
                           extra_args = list()) {

  # Validate inputs
  if (!is.function(fn)) {
    stop("'fn' must be a function")
  }
  if (!is.list(inputs) || length(inputs) == 0) {
    stop("'inputs' must be a non-empty named list")
  }
  if (is.null(names(inputs)) || any(names(inputs) == "")) {
    stop("All elements of 'inputs' must be named")
  }

  # Set seed for reproducibility

if (!is.null(seed)) set.seed(seed)

  input_names <- names(inputs)
  n_inputs <- length(inputs)

  # Parse input specifications
  means <- sapply(inputs, function(x) x$mean)
  sds <- sapply(inputs, function(x) x$sd)
  dists <- sapply(inputs, function(x) if (!is.null(x$dist)) x$dist else "normal")
  sources <- sapply(inputs, function(x) if (!is.null(x$source)) x$source else "other")

  # Validate
  if (any(is.na(means)) || any(is.na(sds))) {
    stop("All inputs must have 'mean' and 'sd' specified")
  }
  if (any(sds < 0)) {
    stop("Standard deviations must be non-negative")
  }

  # Generate samples based on distribution type
  samples <- matrix(NA, nrow = N, ncol = n_inputs)
  colnames(samples) <- input_names

  for (i in seq_len(n_inputs)) {
    samples[, i] <- switch(dists[i],
      "normal" = stats::rnorm(N, mean = means[i], sd = sds[i]),
      "lognormal" = {
        # Convert mean/sd to lognormal parameters
        mu_ln <- log(means[i]^2 / sqrt(sds[i]^2 + means[i]^2))
        sigma_ln <- sqrt(log(1 + (sds[i]^2 / means[i]^2)))
        stats::rlnorm(N, meanlog = mu_ln, sdlog = sigma_ln)
      },
      "uniform" = {
        # sd of uniform = (b-a)/sqrt(12), so range = sd*sqrt(12)
        half_range <- sds[i] * sqrt(3)
        stats::runif(N, min = means[i] - half_range, max = means[i] + half_range)
      },
      stats::rnorm(N, mean = means[i], sd = sds[i])  # default to normal
    )
  }

  # Apply correlation structure if provided
  if (!is.null(corr_matrix)) {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      warning("MASS package not available, ignoring correlation structure")
    } else {
      # For correlated sampling, regenerate with mvrnorm (normal only)
      cov_matrix <- diag(sds) %*% corr_matrix %*% diag(sds)
      samples <- MASS::mvrnorm(N, mu = means, Sigma = cov_matrix)
      colnames(samples) <- input_names
    }
  }

  # Evaluate function for each Monte Carlo sample
  results <- numeric(N)
  for (i in seq_len(N)) {
    arg_list <- as.list(samples[i, ])
    names(arg_list) <- input_names
    arg_list <- c(arg_list, extra_args)
    tryCatch({
      results[i] <- do.call(fn, arg_list)
    }, error = function(e) {
      results[i] <- NA
    })
  }

  # Remove NA/Inf values
  valid_idx <- is.finite(results)
  valid_results <- results[valid_idx]
  n_valid <- length(valid_results)

  if (n_valid < N * 0.9) {
    warning(sprintf("%.1f%% of simulations failed or returned non-finite values",
                    100 * (1 - n_valid / N)))
  }

  # Calculate summary statistics
  alpha <- (1 - conf) / 2
  ci_probs <- c(alpha, 1 - alpha)

  estimate <- mean(valid_results)
  median_val <- stats::median(valid_results)
  sd_val <- stats::sd(valid_results)
  cv_val <- 100 * sd_val / abs(estimate)
  ci <- stats::quantile(valid_results, probs = ci_probs, names = FALSE)

  # Skewness and kurtosis
  m3 <- mean((valid_results - estimate)^3)
  m4 <- mean((valid_results - estimate)^4)
  skewness <- m3 / (sd_val^3)
  kurtosis <- m4 / (sd_val^4)

  # Variance decomposition by source (if requested)
  decomposition <- NULL
  if (decompose && n_inputs > 1) {
    decomposition <- .decompose_variance(fn, inputs, samples, results,
                                         valid_idx, sources, extra_args)
  }

  # Build result object
  result <- list(
    estimate = estimate,
    median = median_val,
    sd = sd_val,
    cv = cv_val,
    ci_low = ci[1],
    ci_high = ci[2],
    conf = conf,
    skewness = skewness,
    kurtosis = kurtosis,
    samples = valid_results,
    n_samples = n_valid,
    n_requested = N,
    inputs_summary = data.frame(
      parameter = input_names,
      mean = means,
      sd = sds,
      distribution = dists,
      source = sources,
      stringsAsFactors = FALSE
    ),
    decomposition = decomposition
  )

  class(result) <- c("mc_uncertainty", "list")
  return(result)
}

#' @keywords internal function
.decompose_variance <- function(fn, inputs, samples, results, valid_idx,
                                sources, extra_args) {

  # Sobol-like variance decomposition: compute variance contribution
  # by fixing each input at its mean and varying others

  input_names <- names(inputs)
  n_inputs <- length(inputs)
  means <- sapply(inputs, function(x) x$mean)

  total_var <- stats::var(results[valid_idx])

  # First-order effects: variance when only input i varies
  var_contributions <- numeric(n_inputs)
  names(var_contributions) <- input_names

  for (i in seq_len(n_inputs)) {
    # Fix all inputs except i at their means
    partial_samples <- samples
    for (j in seq_len(n_inputs)) {
      if (j != i) {
        partial_samples[, j] <- means[j]
      }
    }

    # Evaluate function
    partial_results <- numeric(nrow(samples))
    for (k in seq_len(nrow(samples))) {
      arg_list <- as.list(partial_samples[k, ])
      names(arg_list) <- input_names
      arg_list <- c(arg_list, extra_args)
      tryCatch({
        partial_results[k] <- do.call(fn, arg_list)
      }, error = function(e) {
        partial_results[k] <- NA
      })
    }
    var_contributions[i] <- stats::var(partial_results, na.rm = TRUE)
  }

  # Normalize to percentages
  total_first_order <- sum(var_contributions)
  if (total_first_order > 0) {
    pct_contributions <- 100 * var_contributions / total_first_order
  } else {
    pct_contributions <- rep(0, n_inputs)
  }

  # Aggregate by source
  unique_sources <- unique(sources)
  source_contributions <- sapply(unique_sources, function(s) {
    sum(pct_contributions[sources == s])
  })

  list(
    by_input = data.frame(
      input = input_names,
      variance = var_contributions,
      contribution_pct = pct_contributions,
      source = sources,
      stringsAsFactors = FALSE
    ),
    by_source = data.frame(
      source = unique_sources,
      contribution_pct = source_contributions,
      stringsAsFactors = FALSE
    )
  )
}


########### Print Monte Carlo Uncertainty Results ###########
#' @title Print Monte Carlo Uncertainty Results
#' @description Print method for mc_uncertainty objects showing key statistics
#'   and interpretation.
#'
#' @param x An mc_uncertainty object from \code{\link{mc_uncertainty}}
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#'
#' @method print mc_uncertainty
#' @export
print.mc_uncertainty <- function(x, ...) {
  cat("\n=== Monte Carlo Uncertainty Analysis ===\n\n")

  cat(sprintf("Point Estimate:  %.4f\n", x$estimate))
  cat(sprintf("Median:          %.4f\n", x$median))
  cat(sprintf("Std. Deviation:  %.4f\n", x$sd))
  cat(sprintf("CV:              %.1f%%\n", x$cv))
  cat(sprintf("\n%.0f%% Interval:    [%.4f, %.4f]\n",
              100 * x$conf, x$ci_low, x$ci_high))

  cat(sprintf("\nSkewness:        %.2f", x$skewness))
  if (abs(x$skewness) > 1) {
    cat("  [HIGH - use median]")
  } else if (abs(x$skewness) > 0.5) {
    cat("  [moderate]")
  }
  cat("\n")

  cat(sprintf("Kurtosis:        %.2f", x$kurtosis))
  if (x$kurtosis > 4) cat("  [heavy-tailed]")
  cat("\n")

  cat(sprintf("\nSamples used:    %d / %d (%.1f%%)\n",
              x$n_samples, x$n_requested, 100 * x$n_samples / x$n_requested))

  # Interpretation
  cat("\n--- Interpretation ---\n")
  if (x$cv < 10) {
    cat("LOW uncertainty: estimates are reliable\n")
  } else if (x$cv < 30) {
    cat("MODERATE uncertainty: typical for field measurements\n")
  } else {
    cat("HIGH uncertainty: interpret estimates with caution\n")
  }

  invisible(x)
}


########### Summary Monte Carlo Uncertainty Results ###########
#' @title Summary of Monte Carlo Uncertainty Results
#' @description Summary method for mc_uncertainty objects providing detailed
#'   statistics and variance decomposition.
#'
#' @param object An mc_uncertainty object from \code{\link{mc_uncertainty}}
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#'
#' @method summary mc_uncertainty
#' @export
summary.mc_uncertainty <- function(object, ...) {
  cat("---------- MONTE CARLO UNCERTAINTY SUMMARY ----------\n")

  # Main results
  cat("\nRESULTS:\n")
  cat(sprintf("  Estimate (mean): %.4f\n", object$estimate))
  cat(sprintf("  Median:          %.4f\n", object$median))
  cat(sprintf("  Standard Dev:    %.4f (CV = %.1f%%)\n", object$sd, object$cv))
  cat(sprintf("  %.0f%% CI:         [%.4f, %.4f]\n",
              100 * object$conf, object$ci_low, object$ci_high))

  # Distribution shape
  cat("\nDISTRIBUTION SHAPE:\n")
  cat(sprintf("  Skewness:  %.3f  ", object$skewness))
  if (object$skewness > 0.5) cat("(right-skewed)")
  else if (object$skewness < -0.5) cat("(left-skewed)")
  else cat("(approximately symmetric)")
  cat("\n")

  cat(sprintf("  Kurtosis:  %.3f  ", object$kurtosis))
  if (object$kurtosis > 3.5) cat("(heavy tails)")
  else if (object$kurtosis < 2.5) cat("(light tails)")
  else cat("(near-normal)")
  cat("\n")

  # Input summary
  cat("\nINPUT PARAMETERS:\n")
  print(object$inputs_summary, row.names = FALSE)

  # Variance decomposition
  if (!is.null(object$decomposition)) {
    cat("\nVARIANCE DECOMPOSITION BY SOURCE:\n")
    by_source <- object$decomposition$by_source
    by_source <- by_source[order(-by_source$contribution_pct), ]
    for (i in seq_len(nrow(by_source))) {
      bar <- paste(rep("=", round(by_source$contribution_pct[i] / 5)), collapse = "")
      cat(sprintf("  %-12s %5.1f%% %s\n",
                  by_source$source[i], by_source$contribution_pct[i], bar))
    }

    cat("\nBY INPUT PARAMETER:\n")
    by_input <- object$decomposition$by_input
    by_input <- by_input[order(-by_input$contribution_pct), ]
    for (i in seq_len(nrow(by_input))) {
      cat(sprintf("  %-10s %5.1f%% (%s)\n",
                  by_input$input[i], by_input$contribution_pct[i],
                  by_input$source[i]))
    }
  }

  invisible(object)
}


########### Plot Monte Carlo Uncertainty Results ###########
#'
#' @title Plot Monte Carlo Uncertainty Results
#' @description Visualize the distribution of Monte Carlo simulation results
#'   from an mc_uncertainty object.
#'
#' @param x An mc_uncertainty object from \code{\link{mc_uncertainty}}
#' @param type Plot type: "histogram" (default), "density", or "both"
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot object (if ggplot2 available) or base R plot (invisible NULL)
#'
#' @method plot mc_uncertainty
#' @export
plot.mc_uncertainty <- function(x, type = "histogram", ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    # Base R fallback
    if (type == "density") {
      plot(stats::density(x$samples), main = "Uncertainty Distribution",
           xlab = "Value", col = "steelblue", lwd = 2)
    } else {
      graphics::hist(x$samples, breaks = 50, col = "steelblue",
                     main = "Uncertainty Distribution", xlab = "Value",
                     border = "white", freq = FALSE)
    }
    graphics::abline(v = x$median, col = "red", lwd = 2, lty = 2)
    graphics::abline(v = c(x$ci_low, x$ci_high), col = "darkred", lwd = 1, lty = 3)
    graphics::legend("topright",
                     legend = c("Median", sprintf("%.0f%% CI", 100 * x$conf)),
                     col = c("red", "darkred"), lty = c(2, 3), lwd = c(2, 1))
    return(invisible(NULL))
  }

  # ggplot2 version
  df <- data.frame(value = x$samples)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = value))

  if (type == "histogram" || type == "both") {
    p <- p + ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                     bins = 50, fill = "steelblue", alpha = 0.7)
  }
  if (type == "density" || type == "both") {
    p <- p + ggplot2::geom_density(color = "darkblue", linewidth = 1)
  }

  p <- p +
    ggplot2::geom_vline(xintercept = x$median, color = "red",
                        linetype = "dashed", linewidth = 1) +
    ggplot2::geom_vline(xintercept = c(x$ci_low, x$ci_high),
                        color = "darkred", linetype = "dotted", linewidth = 0.8) +
    ggplot2::annotate("text", x = x$median, y = Inf, label = "Median",
                      vjust = 2, color = "red", size = 3) +
    ggplot2::labs(
      title = "Monte Carlo Uncertainty Distribution",
      subtitle = sprintf("N = %d | CV = %.1f%% | Skewness = %.2f",
                         x$n_samples, x$cv, x$skewness),
      x = "Estimated Value",
      y = "Density"
    ) +
    ggplot2::theme_minimal()

  print(p)
  invisible(p)
}


########### Create Measurement Uncertainty Specification ###########
#' Create Measurement Uncertainty Specification
#'
#' Helper function to create properly formatted input
#' specifications for measurement uncertainty.
#' @param value Measured value
#' @param absolute_error Absolute measurement error (e.g., +/- 2 mm for DBH)
#' @param relative_error Relative measurement error (e.g. 0.025 for 2.5 percent)
#'   Ignored if absolute_error is provided.
#' @param dist Distribution: "normal" (default), "lognormal", "uniform"
#' @return A list suitable for use in mc_uncertainty inputs
#' @examples
#' # DBH measurement with +/-2mm absolute error
#' dbh_input <- measurement_uncertainty(30, absolute_error = 0.2)
#'
#' # Height measurement with 5 percent relative error
#' height_input <- measurement_uncertainty(15, relative_error = 0.05)
#'
#' @export
measurement_uncertainty <- function(value, absolute_error = NULL,
                                    relative_error = 0.025, dist = "normal") {
  if (!is.null(absolute_error)) {
    sd_val <- absolute_error
  } else {
    sd_val <- abs(value) * relative_error
  }

  list(
    mean = value,
    sd = sd_val,
    dist = dist,
    source = "measurement"
  )
}


########### Create Parameter Uncertainty Specification ###########
#' Create Parameter Uncertainty Specification
#'
#' @description Helper function for allometric equation parameter uncertainty.
#'
#' @param value Parameter value (coefficient)
#' @param se Standard error of the parameter
#' @param relative_se Relative standard error. Used if se is NULL.
#'
#' @return A list suitable for use in mc_uncertainty inputs
#'
#' @examples
#' # Bunce coefficient 'a' with SE from regression
#' a_input <- parameter_uncertainty(1.868, se = 0.047)
#'
#' # Or with relative SE (2.5 percent of value)
#' b_input <- parameter_uncertainty(2.268, relative_se = 0.025)
#'
#' @export
parameter_uncertainty <- function(value, se = NULL, relative_se = 0.025) {
  if (!is.null(se)) {
    sd_val <- se
  } else {
    sd_val <- abs(value) * relative_se
  }

  list(
    mean = value,
    sd = sd_val,
    dist = "normal",
    source = "parameter"
  )
}


########### Create Residual Uncertainty Specification ###########
#' Create Residual Uncertainty Specification
#'
#' @description Helper function for model residual error. This represents the
#' inherent prediction uncertainty of the allometric equation.
#'
#' @param rse Residual standard error of the model (on original scale)
#' @param cv_residual Coefficient of variation of residuals. Used if rse is NULL.
#' @param estimate The point estimate (used with cv_residual)
#'
#' @details
#' Residual error is often the largest component of total uncertainty in
#' allometric estimates. It represents the natural variation in tree allometry
#' that the model cannot capture.
#'
#' For log-transformed models, RSE should be back-transformed appropriately.
#'
#' @return A list suitable for use in mc_uncertainty inputs
#'
#' @examples
#' # Model with RSE of 0.15 on log scale (approx 15 percent CV)
#' residual_input <- residual_uncertainty(rse = 50, estimate = 500)
#'
#' @export
residual_uncertainty <- function(rse = NULL, cv_residual = 0.20, estimate = 1) {
  if (!is.null(rse)) {
    sd_val <- rse
  } else {
    sd_val <- abs(estimate) * cv_residual
  }

  list(
    mean = 0,  # Residual is added to estimate
    sd = sd_val,
    dist = "normal",
    source = "residual"
  )
}


########### Method Choice Uncertainty ###########
#' Quantify Method Choice Uncertainty
#'
#' @description
#' Computes uncertainty arising from the choice of allometric method by
#' comparing estimates across multiple methods.
#'
#' @param estimates Named numeric vector of estimates from different methods
#' @param weights Optional named numeric vector of weights for each method
#'   (e.g., based on sample size or regional relevance)
#' @param conf Confidence level for interval
#'
#' @return A list with:
#'   \describe{
#'     \item{consensus}{Weighted mean across methods}
#'     \item{range}{Min-max range}
#'     \item{spread}{Max - min}
#'     \item{cv}{Coefficient of variation across methods}
#'     \item{ci}{Confidence interval (assuming normal)}
#'     \item{individual}{Data frame of individual estimates}
#'   }
#'
#' @examples
#' estimates <- c(WCC = 1.25, BIOMASS = 1.18, allodb = 1.32, Bunce = 1.15)
#' method_choice_uncertainty(estimates)
#'
#' # With weights (e.g., inverse of method CV or sample size)
#' weights <- c(WCC = 1, BIOMASS = 0.8, allodb = 0.6, Bunce = 0.9)
#' method_choice_uncertainty(estimates, weights)
#'
#' @export
method_choice_uncertainty <- function(estimates, weights = NULL, conf = 0.90) {

  if (!is.numeric(estimates) || length(estimates) < 2) {
    stop("Need at least 2 numeric estimates")
  }

  n_methods <- length(estimates)
  method_names <- names(estimates)
  if (is.null(method_names)) {
    method_names <- paste0("Method_", seq_len(n_methods))
  }

  # Default equal weights
  if (is.null(weights)) {
    weights <- rep(1, n_methods)
  }
  weights <- weights / sum(weights)  # Normalize

  # Statistics
  consensus <- sum(estimates * weights)
  variance <- sum(weights * (estimates - consensus)^2)
  sd_val <- sqrt(variance)
  cv_val <- 100 * sd_val / abs(consensus)

  # Range
  range_val <- range(estimates)
  spread <- diff(range_val)

  # CI (assuming approximately normal)
  alpha <- (1 - conf) / 2
  ci <- consensus + stats::qnorm(c(alpha, 1 - alpha)) * sd_val

  list(
    consensus = consensus,
    sd = sd_val,
    cv = cv_val,
    range = range_val,
    spread = spread,
    spread_pct = 100 * spread / abs(consensus),
    ci = ci,
    conf = conf,
    individual = data.frame(
      method = method_names,
      estimate = estimates,
      weight = weights,
      diff_from_consensus = estimates - consensus,
      pct_diff = 100 * (estimates - consensus) / abs(consensus),
      stringsAsFactors = FALSE
    )
  )
}


################ Combined Uncertainty (All Sources) ################
#' Comprehensive Uncertainty Analysis
#'
#' @description
#' Combines measurement, parameter, and residual uncertainty through Monte Carlo
#' simulation, then optionally incorporates method choice uncertainty.
#'
#' @param fn Allometric function
#' @param inputs Named list of input specifications (from helper functions)
#' @param method_estimates Optional named vector of estimates from other methods
#'   for method choice uncertainty
#' @param N Monte Carlo sample size
#' @param conf Confidence level
#' @param include_residual Logical. Include residual error? Default TRUE.
#' @param residual_cv CV for residual error if include_residual is TRUE
#'
#' @return A list with:
#'   \describe{
#'     \item{mc_result}{Monte Carlo result}
#'     \item{method_uncertainty}{Method choice uncertainty (if provided)}
#'     \item{combined_cv}{Combined CV from all sources}
#'     \item{summary_table}{Data frame summarizing all uncertainty components}
#'   }
#'
#' @examples
#' # Comprehensive analysis of Bunce estimate
#' bunce_fn <- function(dbh, a, b) exp(a + b * log(pi * dbh))
#'
#' inputs <- list(
#'   dbh = measurement_uncertainty(30, absolute_error = 0.5),
#'   a = parameter_uncertainty(1.868, se = 0.047),
#'   b = parameter_uncertainty(2.268, se = 0.057)
#' )
#'
#' other_methods <- c(WCC = 1.25, BIOMASS = 1.18, allodb = 1.32)
#'
#' result <- comprehensive_uncertainty(bunce_fn, inputs,
#'                                     method_estimates = other_methods,
#'                                     include_residual = TRUE,
#'                                     residual_cv = 0.25)
#'
#' @export
comprehensive_uncertainty <- function(fn, inputs, method_estimates = NULL,
                                      N = 1000, conf = 0.90,
                                      include_residual = TRUE,
                                      residual_cv = 0.20) {

  # Run Monte Carlo
  mc_result <- mc_uncertainty(fn, inputs, N = N, conf = conf, decompose = TRUE)

  # Get point estimate for residual calculation
  point_estimate <- mc_result$estimate

  # Add residual uncertainty if requested
  if (include_residual) {
    residual_sd <- abs(point_estimate) * residual_cv
    # Combine MC variance with residual variance (independent)
    combined_var <- mc_result$sd^2 + residual_sd^2
    combined_sd <- sqrt(combined_var)
    combined_cv <- 100 * combined_sd / abs(point_estimate)
  } else {
    residual_sd <- 0
    combined_sd <- mc_result$sd
    combined_cv <- mc_result$cv
  }

  # Method choice uncertainty if provided
  method_unc <- NULL
  if (!is.null(method_estimates)) {
    # Include current method estimate
    all_estimates <- c(current = point_estimate, method_estimates)
    method_unc <- method_choice_uncertainty(all_estimates, conf = conf)
  }

  # Build summary table
  summary_components <- data.frame(
    source = character(),
    sd = numeric(),
    cv_pct = numeric(),
    stringsAsFactors = FALSE
  )

  # From decomposition
  if (!is.null(mc_result$decomposition)) {
    by_source <- mc_result$decomposition$by_source
    for (i in seq_len(nrow(by_source))) {
      s <- by_source$source[i]
      # Approximate SD contribution from percentage
      sd_contrib <- mc_result$sd * sqrt(by_source$contribution_pct[i] / 100)
      cv_contrib <- 100 * sd_contrib / abs(point_estimate)
      summary_components <- rbind(summary_components,
                                  data.frame(source = s, sd = sd_contrib, cv_pct = cv_contrib))
    }
  }

  if (include_residual) {
    summary_components <- rbind(summary_components,
                                data.frame(source = "residual (model)",
                                           sd = residual_sd,
                                           cv_pct = residual_cv * 100))
  }

  if (!is.null(method_unc)) {
    summary_components <- rbind(summary_components,
                                data.frame(source = "method choice",
                                           sd = method_unc$sd,
                                           cv_pct = method_unc$cv))
  }

  # Total
  total_var <- sum(summary_components$sd^2)
  total_sd <- sqrt(total_var)
  total_cv <- 100 * total_sd / abs(point_estimate)
  summary_components <- rbind(summary_components,
                              data.frame(source = "TOTAL",
                                         sd = total_sd,
                                         cv_pct = total_cv))

  result <- list(
    estimate = point_estimate,
    mc_result = mc_result,
    method_uncertainty = method_unc,
    total_sd = total_sd,
    total_cv = total_cv,
    ci = point_estimate + stats::qnorm(c((1-conf)/2, 1-(1-conf)/2)) * total_sd,
    conf = conf,
    summary_table = summary_components
  )

  class(result) <- c("comprehensive_uncertainty", "list")
  return(result)
}


########### Print Comprehensive Uncertainty Results ###########
#' @title Print Comprehensive Uncertainty Results
#' @description Print method for comprehensive_uncertainty objects showing
#'   the breakdown of uncertainty sources.
#'
#' @param x A comprehensive_uncertainty object from \code{\link{comprehensive_uncertainty}}
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#'
#' @method print comprehensive_uncertainty
#' @export
print.comprehensive_uncertainty <- function(x, ...) {
  cat("--------- COMPREHENSIVE UNCERTAINTY ANALYSIS ---------\n")

  cat(sprintf("Point Estimate: %.4f\n", x$estimate))
  cat(sprintf("Total SD:       %.4f\n", x$total_sd))
  cat(sprintf("Total CV:       %.1f%%\n", x$total_cv))
  cat(sprintf("%.0f%% CI:        [%.4f, %.4f]\n\n",
              100 * x$conf, x$ci[1], x$ci[2]))

  cat("UNCERTAINTY BREAKDOWN:\n")
  for (i in seq_len(nrow(x$summary_table))) {
    row <- x$summary_table[i, ]
    if (row$source == "TOTAL")
    bar <- paste(rep("=", round(row$cv_pct / 2)), collapse = "")
    cat(sprintf("%-18s %6.1f%%  %s\n", row$source, row$cv_pct, bar))
  }

  invisible(x)
}


########### Quick Uncertainty Analysis ###########
#' Quick Uncertainty Analysis for Common Allometric Equations
#'
#' @description
#' A convenience wrapper that sets up Monte Carlo uncertainty analysis
#' with sensible defaults for common allometric equations.
#'
#' @param method Allometric method: "Bunce", "WCC", "Chave", "generic_power"
#' @param dbh DBH in cm
#' @param height Height in m (required for WCC, Chave)
#' @param dbh_error DBH measurement error (absolute, in cm). Default 0.5 cm
#' @param height_error Height measurement error (absolute, in m). Default 1 m
#' @param params Optional list of custom parameters with SEs
#' @param N Monte Carlo sample size
#'
#' @return An mc_uncertainty object
#'
#' @examples
#' # Quick uncertainty for Bunce equation
#' quick_uncertainty("Bunce", dbh = 30)
#'
#' # For WCC (requires height)
#' quick_uncertainty("WCC", dbh = 30, height = 15)
#'
#' @export
quick_uncertainty <- function(method = "Bunce", dbh, height = NULL,
                              dbh_error = 0.5, height_error = 1.0,
                              params = NULL, N = 1000) {

  method <- match.arg(method, c("Bunce", "WCC", "Chave", "generic_power"))

  # Default parameters with typical SEs
  default_params <- list(
    Bunce = list(
      a = list(mean = 1.868, sd = 0.047),
      b = list(mean = 2.268, sd = 0.057)
    ),
    Chave = list(
      a = list(mean = 0.0673, sd = 0.002),
      b = list(mean = 0.976, sd = 0.01),
      wd = list(mean = 0.6, sd = 0.05)  # Wood density placeholder
    ),
    generic_power = list(
      a = list(mean = 0.1, sd = 0.01),
      b = list(mean = 2.4, sd = 0.05)
    )
  )

  # Build inputs
  inputs <- list(
    dbh = measurement_uncertainty(dbh, absolute_error = dbh_error)
  )

  if (!is.null(height)) {
    inputs$height <- measurement_uncertainty(height, absolute_error = height_error)
  }

  # Set up function and parameters based on method
  if (method == "Bunce") {
    fn <- function(dbh, a, b) exp(a + b * log(pi * dbh))
    p <- if (!is.null(params)) params else default_params$Bunce
    inputs$a <- list(mean = p$a$mean, sd = p$a$sd, source = "parameter")
    inputs$b <- list(mean = p$b$mean, sd = p$b$sd, source = "parameter")

  } else if (method == "Chave" || method == "WCC") {
    if (is.null(height)) stop("Height required for ", method)
    fn <- function(dbh, height, a, b, wd) {
      a * (wd * height * dbh^2)^b
    }
    p <- if (!is.null(params)) params else default_params$Chave
    inputs$a <- list(mean = p$a$mean, sd = p$a$sd, source = "parameter")
    inputs$b <- list(mean = p$b$mean, sd = p$b$sd, source = "parameter")
    inputs$wd <- list(mean = p$wd$mean, sd = p$wd$sd, source = "parameter")

  } else {  # generic_power
    fn <- function(dbh, a, b) a * dbh^b
    p <- if (!is.null(params)) params else default_params$generic_power
    inputs$a <- list(mean = p$a$mean, sd = p$a$sd, source = "parameter")
    inputs$b <- list(mean = p$b$mean, sd = p$b$sd, source = "parameter")
  }

  mc_uncertainty(fn, inputs, N = N, decompose = TRUE)
}


#############  Monte Carlo Uncertainty Propagation #############
#'
#' Propagates uncertainty in tree biomass or carbon estimates using Monte Carlo simulation.
#' This function can be applied to any allometric function, including Bunce, WCC, Chave, or user-defined functions.
#' Users provide the function, input means and standard deviations (measurement or parameter uncertainty),
#' and optionally a correlation matrix between inputs.
#'
#' @param fn A function that returns biomass (or carbon) given input variables.
#' @param inputs A named list of inputs. Each element must be a list with components:
#'   - mean: numeric value or vector
#'   - sd: standard deviation representing uncertainty
#'   Example: list(dbh = list(mean=30, sd=0.01*30), a = list(mean=1.2, sd=0.025*1.2))
#' @param N Integer. Number of Monte Carlo draws. Default 5000.
#' @param corr_matrix Optional correlation matrix between input variables for multivariate sampling.
#' Must be consistent with the order of variables in `inputs`.
#' @param extra_args Optional list of additional fixed arguments to pass to `f`.
#'
#' @return A list containing:
#'   - mean: mean of simulated biomass/carbon
#'   - sd: standard deviation
#'   - ci: 2.5 and 97.5 percent quantiles
#'   - sim_values: vector of all simulated values
#'
#' @references
#' Rejou-Mechain M., et al. (2017). BIOMASS: an R package for estimating above-ground biomass and its uncertainty.
#' Chave J., et al. (2014). Improved allometric models to estimate the above-ground biomass of tropical trees.
#' Efron B. & Tibshirani R. (1993). An Introduction to the Bootstrap.
#'
#' @examples
#' Bunce_fun <- function(dbh, a, b){ exp(a + b * log(pi * dbh)) }
#' inputs <- list(dbh = list(mean=30, sd=0.01*30), a=list(mean=1.2, sd=0.025*1.2),
#'                b=list(mean=0.8, sd=0.025*0.8))
#' MC_result <- MC_propagate(f = Bunce_fun, inputs = inputs, N = 5000)
#' MC_result$mean
#' MC_result$sd
#' MC_result$ci
#' @export
#'
MC_propagate <- function(fn, inputs, N = 5000, corr_matrix = NULL, extra_args = list()) {

  # ****todo Work in progress ****
  # fn: function returning biomass, must accept vector inputs
  # inputs: named list, each element is a list(mean = , sd = )
  # N: number of MC draws
  # corr_matrix: optional correlation matrix for multivariate normal sampling
  # extra_args: additional fixed arguments to pass to f

  input_names <- names(inputs)
  n_inputs <- length(inputs)

  # If correlation matrix provided, sample jointly
  if(!is.null(corr_matrix)){
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("MASS package required for correlated sampling. Install with: install.packages('MASS')")
    }
    means <- sapply(inputs, function(x) x$mean)
    sds <- sapply(inputs, function(x) x$sd)
    cov_matrix <- diag(sds) %*% corr_matrix %*% diag(sds)
    samples <- MASS::mvrnorm(N, mu = means, Sigma = cov_matrix)
    colnames(samples) <- input_names
  } else {
    # Independent sampling
    samples <- sapply(inputs, function(x) rnorm(N, mean = x$mean, sd = x$sd))
  }

  # Evaluate function for each draw
  biomass_sim <- numeric(N)
  for(i in 1:N){
    arg_list <- lapply(1:n_inputs, function(j) samples[i,j])
    names(arg_list) <- input_names
    arg_list <- c(arg_list, extra_args)
    biomass_sim[i] <- do.call(fn, arg_list)
  }

  # Summarize
  result <- list(
    mean = mean(biomass_sim),
    sd = sd(biomass_sim),
    ci = quantile(biomass_sim, c(0.025, 0.975)),
    sim_values = biomass_sim
  )
  return(result)
}

############# Progression of errors ###########
#'
#' @title Carbon progression of errors
#' @description Progression of errors through monte carlo simulation
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param vol volume
#' @param den density
#' @param biom biomass
#' @param sig_vol sigma (standard deviation) for volume
#' @param sig_den sigma (standard deviation) for wood density
#' @param sig_biom sigma (standard deviation) for biomass
#' @param nruns number of iteration, suggest 10,000 as min and 100,000 is a good number
#' @param returnv if null then mean and sd is returned else vector of
#' quantiles ie c(5,50,95)/100 will return 0.05, mean and 0.95 quantiles.
#' @return  either vector of mean and sd or vector of quantiles
#' @references todo** and to write at export
#' @importFrom stats quantile rnorm sd
#' @import remotes
#' @export
#' @aliases pro_error_carbon
#'
#vol <- 100
#volsd <- 10
#den <- 0.5
#densd <- 0.005
#biom <- 0.5
#biomsd <- 0.0025
#nruns <-100000
#returnsv <- c(5,50,95)/100
# pro_error_carbon(vol,volsd,den,densd,biom,biomsd,nruns=100000,returnsv=c(5,50,95)/100)
# pro_error_carbon(vol,volsd,den,densd,biom,biomsd,nruns=100000)
pro_error_carbon <- function(vol, sig_vol, den, sig_den, biom, sig_biom,
                             nruns = 10000, returnv = NULL) {
  # Monte Carlo propagation for: carbon = vol * den * biom
  # Fixed bug: was using wrong variable names (volsd, densd, biomsd)

  vol_sim <- stats::rnorm(nruns, mean = vol, sd = sig_vol)
  den_sim <- stats::rnorm(nruns, mean = den, sd = sig_den)
  biom_sim <- stats::rnorm(nruns, mean = biom, sd = sig_biom)

  carbt <- vol_sim * den_sim * biom_sim

  if (!is.null(returnv)) {
    stats::quantile(carbt, probs = returnv)
  } else {
    c(mean = mean(carbt), sd = stats::sd(carbt))
  }
}
#AGB = 0.0673 * (WD * H * D^2)^0.976

