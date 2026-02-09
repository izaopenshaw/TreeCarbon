# ==============================================================================
# Example 1: Single Tree Calculation
# ==============================================================================
# This example shows:
# - Basic WCC calculation
# - Bunce method (no height needed)
# - Rich output for metadata
# ==============================================================================

library(TreeCarbon)

# ==== WCC Calculation ==================================================

# Calculate above-ground carbon for a single oak tree
result <- fc_agc(name = "Oak", dbh = 45, height = 18, type = "broadleaf")

cat(sprintf("Above-ground carbon: %.3f tonnes\n", result$AGC_WCC_t))

# Calculate carbon with measurement error propagation
result_wcc_error <- fc_agc_error(name = "Oak",            dbh = 45,
                                 height = 18,             type = "broadleaf",
                                 output.all = FALSE,
                                 re_dbh = 0.025,   # 2.5% DBH measurement error
                                 re_h = 0.05,      # 5% height measurement error
                                 re = 0.025)       # 2.5% coefficient error

cat(sprintf("Carbon: %.3f ± %.3f t C\n",
            result_wcc_error$AGC_WCC_t, result_wcc_error$sig_AGC))
cat(sprintf("CV: %.1f%%\n", 100 * result_wcc_error$sig_AGC / result_wcc_error$AGC_WCC_t))
cat(sprintf("95%% CI: [%.3f, %.3f] t C\n",
            result_wcc_error$AGC_WCC_t - 1.96 * result_wcc_error$sig_AGC,
            result_wcc_error$AGC_WCC_t + 1.96 * result_wcc_error$sig_AGC))

# ==== Bunce Method =======================================

# Bunce method only requires DBH and species name
result_bunce <- Bunce(name = "Oak",   dbh = 45)

cat(sprintf("Biomass: %.1f kg (%.3f tonnes)\n",
            result_bunce$biomass, result_bunce$biomass / 1000))

# Bunce with error
result_bunce_error <- Bunce(name = "Oak", dbh = 45, re_dbh = 0.025)

cat(sprintf("Biomass: %.1f ± %.1f kg\n",
            result_bunce_error$biomass, result_bunce_error$sigma))
cat(sprintf("CV: %.1f%%\n",
            100 * result_bunce_error$sigma / result_bunce_error$biomass))
cat(sprintf("95%% CI: [%.1f, %.1f] kg\n",
            result_bunce_error$biomass - 1.96 * result_bunce_error$sigma,
            result_bunce_error$biomass + 1.96 * result_bunce_error$sigma))

# Convert biomass to carbon using biomass2c function
biomass_t <- result_bunce_error$biomass / 1000  # Convert kg to tonnes
biomass_sigma_t <- result_bunce_error$sigma / 1000  # Convert uncertainty to tonnes

carbon_bunce <- biomass2c(biomass = biomass_t,    method = "Thomas",
                          type = "broadleaf",     sig_biomass = biomass_sigma_t)

cat(sprintf("Carbon: %.3f ± %.3f t C\n",
            carbon_bunce$AGC, carbon_bunce$sig_AGC))
cat(sprintf("95%% CI: [%.3f, %.3f] t C\n",
            carbon_bunce$AGC - 1.96 * carbon_bunce$sig_AGC,
            carbon_bunce$AGC + 1.96 * carbon_bunce$sig_AGC))

# ==== Carbon to CO2 Equivalent ================================================

# Convert carbon to CO2 equivalent (1 t C = 3.67 t CO2e)
carbon_t <- result_wcc_error$AGC_WCC_t
co2e_t <- ctoco2e(carbon_t)

cat(sprintf("Carbon: %.3f t C\n", carbon_t))
cat(sprintf("CO2 equivalent: %.3f t CO2e\n", co2e_t))

# With uncertainty: propagate uncertainty through conversion
co2e_uncertainty <- result_wcc_error$sig_AGC * (44 / 12)
cat(sprintf("CO2 equivalent: %.3f ± %.3f t CO2e\n",
            co2e_t, co2e_uncertainty))
cat(sprintf("95%% CI: [%.3f, %.3f] t CO2e\n",
            co2e_t - 1.96 * co2e_uncertainty,
            co2e_t + 1.96 * co2e_uncertainty))

# ==== Rich Output ==================================================
# Get comprehensive metadata including assumptions and citations

# WCC
result_rich <- fc_agc(name = "Oak",                 dbh = 45,
                      height = 18,                  type = "broadleaf",
                      rich_output = TRUE)
print(result_rich)

# Bunce
result_bunce_rich <- Bunce(name = "Oak", dbh = 45, rich_output = TRUE)

print(result_bunce_rich)
