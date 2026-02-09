# Suggestions for TreeCarbon Package Improvements
## Based on WCC Carbon Assessment Protocol v2.0 Review

This document provides suggestions for improving the TreeCarbon package based on a review of the WCC Carbon Assessment Protocol v2.0 (Jenkins et al., 2018) and comparison with current package functionality.

---

## 1. Method A: Fell Sample Trees

### Current Status
- **Implemented**: `tariff_vol_area()` function exists and implements Equation 1
- **Functionality**: Calculates tariff from measured volume and DBH

### Suggestions
- ✅ **Keep as is**: The function correctly implements the WCC methodology
- **Documentation**: Consider adding a note that this is Method A from the protocol
- **Example**: Add example showing how to use this for felled sample trees

### Recommendation
**No changes needed** - Function is correctly implemented.

---

## 2. Method D: Top Height (Stand Tariff)

### Current Status
- **Implemented**: `stand_tariff()` function exists and implements Equation 2
- **Functionality**: Calculates tariff from top height (mean of 100 largest trees per hectare)

### Suggestions
- ✅ **Keep as is**: Function correctly implements stand-level tariff calculation
- **Documentation**: Clarify that top height is the mean height of the 100 largest trees per hectare
- **Example**: Add example showing stand-level calculations

### Recommendation
**No changes needed** - Function is correctly implemented.

---

## 3. Averaging Trees vs Individual Tree Calculations

### Current Issue
The WCC protocol often requires averaging trees (e.g., mean tariff number, mean volume per hectare), but the package functions work on individual trees or vectors of trees.

### Analysis
**Can the functions be used in both instances?**

**YES** - The functions can be used for both:
1. **Individual trees**: Functions already handle this
2. **Averaged calculations**: Users can:
   - Calculate values for all trees
   - Sum/average the results manually
   - For uncertainty: propagate errors appropriately

### Suggestions

#### Option 1: Add Helper Functions (Recommended)
Create helper functions for common averaging operations:

```r
# Example helper function
wcc_mean_tariff <- function(spcode, heights, dbhs, type, ...) {
  tariffs_result <- tariffs(spcode, heights, dbhs, type, ...)
  mean_tariff <- mean(tariffs_result$tariff)
  # Calculate uncertainty for mean
  mean_sigma <- sqrt(sum(tariffs_result$sigma^2)) / length(tariffs_result$tariff)
  return(list(mean_tariff = mean_tariff, mean_sigma = mean_sigma))
}
```

#### Option 2: Add Documentation Examples
Add clear examples showing:
- How to calculate mean tariff from sample trees
- How to sum carbon across trees
- How to calculate per-hectare values
- How to propagate uncertainty for averages

#### Option 3: Add Summary Functions
Create functions that handle common WCC summary operations:
- `wcc_summarize_plot()` - Summarize carbon for a plot
- `wcc_per_hectare()` - Calculate per-hectare values with uncertainty
- `wcc_mean_tariff()` - Calculate mean tariff with uncertainty

### Recommendation
**Add helper functions and documentation** - This would make the package more user-friendly for WCC protocol users while maintaining flexibility for individual tree calculations.

---

## 4. Seedlings and Saplings Definition

### Current Status
- **Implemented**: `sap_seedling2C()` function exists
- **Documentation**: Could be clearer about definitions

### Suggestions
- **Clarify definitions** in function documentation:
  - **Seedlings**: Height < 1m (100cm) OR DBH < 7cm
  - **Saplings**: Height 1-10m (100-1000cm) OR DBH 7-10cm
  - **Trees**: DBH >= 7cm AND height >= 10m
- **Add examples** showing the transition between categories
- **Note**: The function handles both seedlings and saplings using the same lookup tables

### Recommendation
**Improve documentation** - Add clear definitions and examples.

---

## 5. Timber Height vs Total Height for Broadleaves

### Current Status
- **Implemented**: `broadleaf_tariff()` uses timber height
- **Issue**: `fc_agc()` and `fc_agc_error()` use total height as input, but internally should use timber height for broadleaf tariff calculation

### Analysis
According to WCC protocol:
- **Broadleaves**: Tariff uses **timber height** (height to first branch or merchantable top)
- **Conifers**: Tariff uses **total height**

### Current Implementation
The package appears to handle this correctly internally, but it's not clear to users.

### Suggestions
- **Documentation**: Clarify that for broadleaves, users should provide total height to `fc_agc()`, but the function internally uses appropriate height for tariff calculation
- **Alternative**: Add optional `height_timber` parameter for broadleaves
- **Examples**: Show examples with both timber and total height measurements

### Recommendation
**Clarify in documentation** - The implementation seems correct, but users need clearer guidance.

---

## 6. Method E: Full Tree Count

### Current Status
- **Can be implemented**: Using `fc_agc()` or `fc_agc_error()` for each tree
- **Issue**: No helper functions for processing full tree tallies

### Suggestions
- **Add example**: Show how to process full tree count data (Method E)
- **Add helper function**: `wcc_process_tally()` to handle DBH class tallies
- **Add function**: `wcc_dbh_class_carbon()` to calculate carbon from DBH distribution

### Recommendation
**Add helper functions and examples** - Method E is commonly used but requires manual processing currently.

---

## 7. Stand-Level Calculations

### Current Status
- **Partially implemented**: `stand_tariff()` exists for stand-level tariff
- **Missing**: Helper functions for stand-level carbon calculations

### Suggestions
- **Add function**: `wcc_stand_carbon()` for stand-level carbon estimation
- **Add function**: `wcc_basal_area_carbon()` for carbon from basal area measurements
- **Documentation**: Add examples of stand-level vs tree-level calculations

### Recommendation
**Add stand-level helper functions** - Would make the package more useful for large-scale projects.

---

## 8. Carbon Conversion Methods

### Current Status
- **Well implemented**: `biomass2c()` supports multiple methods
- **WCC recommendation**: Protocol recommends Matthews methods for UK applications

### Suggestions
- **Documentation**: Emphasize that Matthews2 is recommended for WCC applications
- **Default**: Consider making Matthews2 the default method for WCC functions
- **Examples**: Show comparison of different methods

### Recommendation
**Improve documentation** - Current implementation is good, just needs clearer guidance.

---

## 9. Error Propagation

### Current Status
- **Well implemented**: Error propagation throughout the pipeline
- **Issue**: Uncertainty calculations for averages/sums could be clearer

### Suggestions
- **Documentation**: Add clear examples of error propagation for:
  - Mean tariff from sample trees
  - Total carbon for multiple trees
  - Per-hectare calculations
- **Helper functions**: Add functions that handle error propagation for common operations

### Recommendation
**Add documentation and helper functions** - Error propagation is complex and users need guidance.

---

## 10. Data Collection Forms

### Current Status
- **Not implemented**: No functions for WCC data collection forms

### Suggestions
- **Optional addition**: Consider adding functions to:
  - Generate WCC data collection forms
  - Import data from WCC forms
  - Validate data against WCC requirements

### Recommendation
**Low priority** - Nice to have but not essential for core functionality.

---

## 11. Summary Recommendations

### High Priority
1. ✅ **Add helper functions** for averaging and summarizing (mean tariff, per-hectare, etc.)
2. ✅ **Improve documentation** for seedlings/saplings definitions
3. ✅ **Add examples** for all WCC methods (A, B, C, D, E)
4. ✅ **Clarify timber vs total height** for broadleaves

### Medium Priority
5. ✅ **Add stand-level helper functions** for large-scale projects
6. ✅ **Add Method E helper functions** for full tree count processing
7. ✅ **Improve error propagation examples** and documentation

### Low Priority
8. ✅ **Consider data collection form functions** (optional)
9. ✅ **Add validation functions** for WCC protocol compliance

---

## 12. Function Compatibility Assessment

### Can Current Functions Be Used for WCC Protocol?

**YES** - All WCC methods can be implemented using current functions:

- **Method A (Fell sample trees)**: ✅ `tariff_vol_area()`
- **Method B (Broadleaf timber height)**: ✅ `broadleaf_tariff()` or `tariffs()`
- **Method C (Conifer total height)**: ✅ `conifer_tariff()` or `tariffs()`
- **Method D (Top height)**: ✅ `stand_tariff()`
- **Method E (Full tree count)**: ✅ `fc_agc()` or `fc_agc_error()` for each tree

### What's Missing?

**Helper functions and documentation** - The core functionality is there, but users need:
- Clear examples for each method
- Helper functions for common operations (averaging, summarizing)
- Better documentation of WCC-specific requirements

---

## 13. Conclusion

The TreeCarbon package **successfully implements** all core WCC methodology. The main improvements needed are:

1. **User-friendliness**: Helper functions for common WCC operations
2. **Documentation**: Clearer guidance on WCC protocol requirements
3. **Examples**: Comprehensive examples for all WCC methods

The package is **ready for WCC use** but would benefit from these enhancements to make it more accessible to WCC protocol users.

---

## References

Jenkins, T.A.R., Mackie, E.D., Matthews, R.W., Miller, G., Randle, T.J., & White, M.E. (2018). FC Woodland Carbon Code: Carbon Assessment Protocol (v2.0). Forestry Commission, Edinburgh. https://www.woodlandcarboncode.org.uk/
