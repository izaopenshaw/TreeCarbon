# ==============================================================================
# WCC Error Propagation Rules - Documentation
# ==============================================================================

#' @title WCC error propagation rules
#' @name wcc_error_propagation
#' @description Rules and formulas for propagating uncertainty in WCC carbon
#'   calculations. Tree-level errors are treated as **independent**.
#' @details
#'   ## When to use each rule
#'
#'   **Sum of independent values** (e.g. total carbon across trees):
#'   \deqn{\sigma_{total} = \sqrt{\sum \sigma_i^2}}
#'   Use when summing carbon from independent trees or strata.
#'
#'   **Product** (e.g. carbon = biomass \eqn{\times} CF):
#'   Use \code{\link{error_product}} or Taylor-style relative errors:
#'   \deqn{(\sigma_f/f)^2 = (\sigma_a/a)^2 + (\sigma_b/b)^2 + \ldots}
#'
#'   **Linear scaling** (e.g. per hectare = per tree \eqn{\times} trees/ha):
#'   \deqn{\sigma_{new} = |k| \times \sigma_{old}}
#'   For \code{Y = k * X}, the uncertainty scales linearly.
#'
#'   **Mean of independent values**:
#'   \deqn{\sigma_{mean} = \sigma_{total} / n = \sqrt{\sum \sigma_i^2} / n}
#'
#'   ## Helper functions
#'
#'   \code{\link{wcc_per_hectare}} and \code{\link{wcc_stratify}} propagate
#'   uncertainty when sigma arguments/columns are provided:
#'   \itemize{
#'     \item \code{wcc_per_hectare}: Provide \code{sig_carbon_per_tree} or
#'       \code{sig_carbon_per_ha}; returns \code{sig_carbon_per_ha},
#'       \code{sig_total_carbon}.
#'     \item \code{wcc_stratify}: Include \code{total_sig_carbon_t} or
#'       \code{carbon_per_ha_sig_t} in strata_data; returns
#'       \code{total_sig_carbon_t}, \code{weighted_mean_sig}.
#'   }
#'
#'   ## Assumptions
#'
#'   \itemize{
#'     \item **Independence**: Tree-level errors are assumed independent. This
#'       may not hold for spatially clustered data.
#'     \item **Linear propagation**: Taylor (delta) method; suitable for small
#'       relative errors. For complex cases, use \code{\link{mc_uncertainty}} or
#'       \code{\link{MC_propagate}} (Monte Carlo).
#'   }
#'
#'   ## Worked examples
#'
#'   **Carbon ± sigma per tree, scale to per hectare:**
#'   \preformatted{
#'   carbon_per_tree <- c(0.5, 0.6, 0.45)
#'   sig_per_tree <- 0.05  # SD of mean
#'   wcc_per_hectare(carbon_per_tree, trees_per_ha = 200, area_ha = 0.5,
#'                   sig_carbon_per_tree = sig_per_tree)
#'   # Returns carbon_per_ha, total_carbon, sig_carbon_per_ha, sig_total_carbon
#'   }
#'
#'   **Carbon ± sigma per stratum, aggregate:**
#'   \preformatted{
#'   strata <- data.frame(area_ha = c(0.5, 0.3), carbon_per_ha_t = c(120, 95),
#'                       carbon_per_ha_sig_t = c(15, 12))
#'   wcc_stratify(strata, carbon_col = "carbon_per_ha_t")
#'   # Returns total_sig_carbon_t, weighted_mean_sig
#'   }
#'
#'   **Sum of trees (fc_agc_error returns per-tree sig_AGC):**
#'   \preformatted{
#'   total_sigma <- sqrt(sum(sig_AGC^2))
#'   }
#' @seealso \code{\link{fc_agc_error}}, \code{\link{wcc_per_hectare}},
#'   \code{\link{wcc_stratify}}, \code{\link{error_product}}, \code{\link{mc_uncertainty}}
#' @references
#'   Taylor, J.R. (1997). An Introduction to Error Analysis (2nd ed.).
#'   University Science Books.
#'
#'   Jenkins, T.A.R. et al. (2018). FC Woodland Carbon Code: Carbon Assessment
#'   Protocol (v2.0). Forestry Commission, Edinburgh.
#' @examples
#' # Sum of independent tree carbon uncertainties
#' sig_tree <- c(0.05, 0.06, 0.04)
#' total_sig <- sqrt(sum(sig_tree^2))
#'
#' # wcc_per_hectare with uncertainty
#' wcc_per_hectare(0.5, trees_per_ha = 200, area_ha = 1,
#'                sig_carbon_per_tree = 0.05)
#'
#' # wcc_stratify with uncertainty
#' strata <- data.frame(area_ha = c(0.5, 0.3),
#'                     carbon_per_ha_t = c(120, 95),
#'                     carbon_per_ha_sig_t = c(15, 12))
#' wcc_stratify(strata, carbon_col = "carbon_per_ha_t")
#' @export
wcc_error_propagation <- function() {
  list(
    sum_independent = "sigma_total = sqrt(sum(sigma_i^2))",
    product = "Use error_product(); (sigma_f/f)^2 = (sigma_a/a)^2 + (sigma_b/b)^2 + ...",
    linear_scaling = "sigma_new = |k| * sigma_old  (for Y = k * X)",
    mean_independent = "sigma_mean = sigma_total / n = sqrt(sum(sigma_i^2)) / n"
  )
}
