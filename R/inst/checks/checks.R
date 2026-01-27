results <- allometries(genus = c("Quercus", "Fagus", "Pinus"),
                       species = c("robur", "sylvatica", "sylvestris"),
                       dbh = c(45, 38, 52),
                       height = c(18, 15, 22)
)

# Compare all methods with WCC as reference
comparison <- compare_allometries(results)
print(comparison)


saveRDS(comparison, "comparison_output.rds")

#'
#' # Compare only WCC and BIOMASS methods
comparison_subset <- compare_allometries(results, methods = c("WCC", "BIOMASS"))
#'
#' # Per-tree comparison
per_tree <- compare_allometries(results, aggregate = FALSE)

#
# Test 1: Direct data access
data("method_metadata")
print(method_metadata)

data("method_assumptions")
print(method_assumptions)

# Test 2: Function using data tables
get_method_metadata("WCC")
get_method_metadata("all")

# Test 3: Convenience functions
print_method_info("WCC")
method_comparison_table()

# Test 4: Validation function
validate_allometry_inputs("WCC", dbh = c(5, 50, 250), height = c(10, 20, 30))



# Bunce
Bunce("Oak", 24)
Bunce(c("Oak", "Beech"), c(23,23))

# Rich output with full metadata
result <- Bunce("Oak", 24, re_dbh = 0.05, rich_output = TRUE)
print(result)  # Shows assumptions prominently
summary(result)

# Check for extrapolation warnings
result <- Bunce(c("Oak", "Beech"), c(25, 200), rich_output = TRUE)  # Large DBH
result$validity_warnings

bunce <- result$summary_table

saveRDS(comparison, "comparison_output.rds")



# Sensitivity Analysis

tree_data <- data.frame(dbh = 45, height = 18, genus = "Quercus", species = "robur" )

result <- sensitivity_analysis( tree_data,    coords = c(-0.29, 51.48), type = "broadleaf")
print(result)

# Multiple trees
trees <- data.frame(dbh = c(30, 45, 60), height = c(15, 20, 25), genus = c("Quercus", "Fagus", "Fraxinus"), species = c("robur", "sylvatica", "excelsior"))
result <- sensitivity_analysis(trees, coords = c(-0.29, 51.48))
summary(result)
plot(result)


data <- data.frame(dbh = c(30, 45, 60), height = c(15, 20, 25), genus = c("Quercus", "Fagus", "Fraxinus"), species = c("robur", "sylvatica", "excelsior"))
coords = c(-0.29, 51.48)
methods = c("WCC", "Bunce", "BIOMASS", "allodb")
type = "broadleaf"
carbon_fraction = 0.5
aggregate = TRUE
reference = "WCC"


###

#' # Simple example: Bunce equation with measurement + parameter uncertainty
bunce_fn <- function(dbh, a, b) {
  exp(a + b * log(pi * dbh))
 }

inputs <- list(
dbh = list(mean = 30, sd = 0.75, source = "measurement"),
a = list(mean = 1.868, sd = 0.047, source = "parameter"),
b = list(mean = 2.268, sd = 0.057, source = "parameter"))

result <- mc_uncertainty(bunce_fn, inputs, N = 1000, decompose = TRUE)
print(result)
summary(result)

# With lognormal distribution for DBH (accounts for positive constraint)
inputs_ln <- list(
dbh = list(mean = 30, sd = 2, dist = "lognormal", source = "measurement"),
a = list(mean = 1.868, sd = 0.047, source = "parameter"),
b = list(mean = 2.268, sd = 0.057, source = "parameter")
)
result_ln <- mc_uncertainty(bunce_fn, inputs_ln, N = 1000)



result <- BIOMASS(30, 15, 'Quercus', 'robur', coords, rich_output = TRUE, uncertainty = TRUE)
result <- BIOMASS(30, 15, 'Quercus', 'robur', coords, rich_output = FALSE, uncertainty = TRUE)
