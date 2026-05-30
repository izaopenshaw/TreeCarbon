# Reviewer Suggestions: TreeCarbon Paper
## As if reviewing for Methods in Ecology and Evolution

---

## Major revisions

### 1. **Reproducibility and data availability**

**Issue:** Methods in Ecology and Evolution requires reproducible, documented workflows. The paper lacks:

- A clear statement that the Wakehurst data and analysis scripts are available (or will be archived)
- Add a vignette or example that reproduces the key Results (e.g. method comparison, sensitivity analysis)
- If Wakehurst data cannot be shared, state clearly and provide a synthetic example dataset with the same structure

---

### 2. **Validation of uncertainty propagation**

**Issue:** The package propagates uncertainty analytically and via Monte Carlo, but the paper does not validate these estimates against:
- Known true values (e.g. simulated data)
- Empirical coverage (e.g. do 95% intervals contain the true value when known?)

**Suggestions:**
- Add a validation section: compare analytical vs Monte Carlo uncertainty for a subset of trees
- Compare TreeCarbon uncertainty estimates with those from BIOMASS/allodb when run directly
- Consider a simulation: generate "true" biomass, add measurement error, and check that propagated intervals achieve nominal coverage

---

### 3. **TLS data and validation**

**Issue:** The Discussion emphasises systematic underestimation vs TLS (Calders et al., Demol et al.), but:
- TLS validation is not presented in the Results
- The TLS total in the results (1,200,208 t C) appears to be a unit error and is not used
- The paper lacks a direct comparison of allometric vs TLS carbon for the same trees

**Suggestions:**
- If TLS data exist: add a Results subsection comparing WCC/allodb/Bunce vs TLS per tree (scatter, RMSE, bias, correlation)
- If TLS data are unavailable: soften the Discussion or move to "Future work"; avoid implying validation when none is shown
- Clarify whether Wakehurst TLS data are stem-only and how crown is handled

---

### 4. **Large‑tree vs small‑tree narrative**

**Issue:** The Results and Discussion are mixed:
- Results: small trees (DBH 0–20 cm) have highest method CV (76%); large trees have lower CV (21–34%)
- Discussion: "high method disagreement for large trees (DBH > 60 cm)" and Calders et al. (2022), who focus on large trees

**Suggestions:**
- Separate two concepts: (a) method *disagreement* (CV across methods) vs (b) calibration *bias* (underestimation vs TLS for large trees)
- Clarify: small trees show high method disagreement; large trees may be systematically underestimated by all methods (bias)
- Add the Calders-style cumulative carbon plot (large-tree dominance) to show that a small fraction of trees holds most carbon, and discuss implications for calibration

---

### 6. **Missing references and placeholders**

**Issue:** Several placeholders remain:
- `[JM1.1]`, `[JM3.1]`, `[JM4.1]`, `[JM6.1]`, `[JM7.1]`, `[JM8.2]`, etc.
- "to ref" for "young or managed stands"
- Table 1 has "Inputs variables" (should be "Input variables")

**Suggestions:**
- Add the key papers listed (Demol et al., Burt et al.)

---

### 7. **Statistical reporting**

**Issue:** Some statistics are reported without context:
- "Median CV 32.5%" — is this per-tree CV across methods?
- "Range ratio 2.98×" — is this max/min total carbon?
- No confidence intervals or bootstrap intervals for key metrics

**Suggestions:**
- Define CV clearly: "per-tree CV = SD(WCC, BIOMASS, allodb, Bunce) / mean of the four estimates"
- Add bootstrap 95% intervals for median CV, range ratio, etc.

---

### 8. **Package and CRAN readiness**

**Issue:** For a methods paper, the package should be:
- Installable from CRAN
- Passing `R CMD check` with no errors or warnings
- With vignettes that build successfully

**Suggestions:**
- Run `devtools::check()` and fix any issues
- Ensure all dependencies are listed correctly (BIOMASS, allodb are Imports; are they required or optional?)
- Add a `README.md` with installation and quick-start
- Consider `remotes` as a Suggests rather than Depends if it is only for development

---

## Minor revisions

### 9. **Typos and grammar**

- "assed" → "assessed" (line 61)
- "trucked cone" → "truncated cone" (line 47)
- "speices" → "species" (line 49)
- "ABG" → "AGB" (line 54)
- "it's" → "its" (line 26)
- "in for allow" → "in place to allow" (line 219)
- Duplicate "but) but" (line 45)

---

### 10. **Bunce equation inconsistency**

**Issue:** The Bunce equation is given as both:
- `AGB = a + b × ln(π × dbh)` (line 54)
- `AGB = a + dbh^b` (line 56)

These are different forms. Clarify which is implemented.

---

### 11. **WCC tariff rounding**

**Issue:** The Methods text says "Initially the tariff number is calculated and rounded down" but the equations show `floor(a_1 + ...)`. The protocol specifies:
- Individual tariff: round to nearest whole
- Mean tariff: floor

Ensure the Methods text matches the implementation.

---

### 12. **N trees discrepancy**

**Issue:** WCC uses 2,996 trees; BIOMASS, allodb, Bunce use 2,173. The reason (height requirement) is not stated in the Results.

**Suggestion:** Add one sentence: "WCC requires height for all trees; BIOMASS, allodb, and Bunce require height for the non‑sapling equations, so 823 trees with missing height were excluded from the four‑method comparison."

---

### 13. **Figure and table numbering**

**Issue:** Tables and figures are referenced but not numbered in the main text (e.g. "Table 1" exists; figures are listed in "Suggested figures" but not integrated).

**Suggestion:** Integrate the suggested figures into the Results and number them (Fig. 1, Fig. 2, etc.).

---

### 14. **Software availability**

**Issue:** The package version is 0.0.0.9002; the abstract says "v1.0". For a Methods paper, a stable release (e.g. v1.0.0 on CRAN) is expected at or before acceptance.

**Suggestion:** Plan a CRAN release; align version and DOI in the paper.

---

### 15. **Conclusion**

**Issue:** The paper lacks a short Conclusion section that summarises the main contribution and recommendations.

**Suggestion:** Add one paragraph: e.g. "TreeCarbon provides a unified, open-source framework for comparing tree carbon estimation methods in the UK. Our analysis of 2,173 trees at Wakehurst showed that method choice often introduces greater uncertainty than measurement error, and that protocol compliance (e.g. QMD vs arithmetic mean DBH) can substantially affect stand-level estimates. We recommend that practitioners report method choice and uncertainty, and use TreeCarbon to compare methods when data are available."

---

## Summary checklist

| Item | Status |
|------|--------|
| Data availability statement | To add |
| Reproducible workflow script | To add |
| Uncertainty validation | To add |
| TLS validation or clarify | To resolve |
| Large vs small tree narrative | To clarify |
| Study site description | To add |
| Placeholders resolved | To do |
| Typos and grammar | To fix |
| Bunce equation | To clarify |
| N trees explanation | To add |
| Figure/table integration | To do |
| CRAN release | To plan |
| Conclusion |
