# Wakehurst Coppice: Monte Carlo Carbon Methodology Summary

## Methodology

### Design
Spatial plot-based carbon assessment for the coppice stratum at Wakehurst Place, following the FC Woodland Carbon Code (FC_WCC) and WCC Survey Protocol 24 (SP). The method simulates protocol-compliant circular plots by:

1. **Plot radius**: Derived from stratum stocking density to target 7–20 trees per plot (midpoint ~12). Radius clamped to stay within 7–20 trees on average.

2. **Plot placement**: Eight circular centres placed randomly within the coppice extent. Non-overlapping placement attempted (minimum separation 2r between centres); when placement fails to converge within 2000 attempts, overlapping placement is used as fallback.

3. **Tree assignment**: Trees assigned to the nearest plot centre if within radius r.

4. **Filtering**: Plots with <7 or >20 trees (protocol FC_WCC 4.1.2) are rejected. Only plots with 7–20 trees are used for carbon.

5. **Carbon calculation**: `fc_plot_stratum_carbon` applies tariff, volume, biomass and carbon equations; QMD from all trees; species grouping for minor species; scale to stratum.

6. **Monte Carlo**: 500 runs with different random seeds. Each run places centres, assigns trees, filters valid plots, and computes carbon. Fixed radius r from reference run.

---

## Protocol Alignment

| Requirement | Implementation | Status |
|-------------|----------------|--------|
| Plot count (SP Table 3, 2–10 ha) | 8 plots required | 8 centres placed |
| Trees per plot (FC_WCC 4.1.2) | 7–20 trees | Plots outside range rejected |
| Plot size | Circular, known area | πr² per plot |
| Min separation | Non-overlapping preferred | Attempted; fallback when extent/placement fails |

**Protocol compliance** is defined as runs with ≥8 valid plots (7–20 trees each). For the 2–10 ha stratum, 8 plots are required per SP Table 3.

---

## Issues and Limitations

1. **Limited protocol-compliant runs**: Only 11 of 500 runs (2.2%) achieved ≥8 valid plots. Most runs had 4–6 valid plots.

2. **Overlapping fallback**: 404 of 500 valid runs (80.8%) used overlapping placement due to spatial extent constraints. Non-overlapping placement succeeded in 96 runs (19.2%).

3. **Spatial extent**: The coppice extent is relatively small for eight non-overlapping circles. Placement often fails to converge, triggering the overlapping fallback.

4. **Selection bias**: Runs with fewer valid plots tend to sample denser areas; their mean carbon per ha is higher than runs with ≥7 or ≥8 valid plots. Protocol-compliant runs give a more representative stratum estimate.

---

## Results

### Summary statistics

| Metric | Value |
|--------|-------|
| **Protocol-compliant carbon per ha** (n_valid_plots ≥ 8) | 42.53 t C/ha |
| **95% CI** (11 runs) | [38.14, 48.57] t C/ha |
| **SD** | 3.69 t C/ha |
| **Alternative (n_valid_plots ≥ 7)** | 40.84 t C/ha |

### Carbon totals (all valid runs)
- Mean: 214.11 t C, SD: 33.44 t C
- Placement: 96 min-sep, 404 overlapping

### Recommended reporting

**Protocol-compliant (recommended for paper):**
> Coppice carbon: **42.53 [38.14–48.57] t C/ha** (95% CI from 11 protocol-compliant Monte Carlo runs, n_valid_plots ≥ 8).

**Sensitivity (n_valid_plots ≥ 7):**
> Using a broader threshold (≥7 valid plots): **40.84 t C/ha** (more runs).

---

## References

- FC Woodland Carbon Code (FC_WCC) 4.1.2, 4.1.5, Appendix 1
- WCC Survey Protocol 24 (SP) Table 3, 8.3, 9.2
