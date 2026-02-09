# ==============================================================================
# Example 1: Single Tree Calculation
# ==============================================================================
# This example demonstrates the simplest possible usage of TreeCarbon:
# calculating carbon for a single tree using different methods.
#
# This example shows:
# - Basic WCC calculation
# - Bunce method (no height needed)
# - Rich output for metadata
# ==============================================================================

library(TreeCarbon)

# ==============================================================================
# Example 1.1: Simple WCC Calculation
# ==============================================================================

# Calculate above-ground carbon for a single oak tree
result <- fc_agc(
  name = "Oak",
  dbh = 45,        # cm
  height = 18,     # metres
  type = "broadleaf"
)

cat("=== Simple WCC Calculation ===\n")
cat(sprintf("Above-ground carbon: %.3f tonnes\n", result$AGC_WCC_t))
cat("\n")

# ==============================================================================
# Example 1.2: Bunce Method (No Height Required)
# ==============================================================================

# Bunce method only needs DBH and species name
result_bunce <- Bunce(
  name = "Oak",
  dbh = 45
)

cat("=== Bunce Method ===\n")
cat(sprintf("Biomass: %.1f kg (%.3f tonnes)\n",
            result_bunce$biomass, result_bunce$biomass / 1000))
cat("\n")

# ==============================================================================
# Example 1.3: Rich Output (Standard)
# ==============================================================================

# Get comprehensive metadata including assumptions and citations
result_rich <- fc_agc(
  name = "Oak",
  dbh = 45,
  height = 18,
  type = "broadleaf",
  rich_output = TRUE
)

cat("=== Rich Output (Standard) ===\n")
print(result_rich)
cat("\n")

# ==============================================================================
# Example 1.4: Rich Output (Bunce)
# ==============================================================================

result_bunce_rich <- Bunce(
  name = "Oak",
  dbh = 45,
  rich_output = TRUE
)

cat("=== Bunce Rich Output ===\n")
print(result_bunce_rich)
