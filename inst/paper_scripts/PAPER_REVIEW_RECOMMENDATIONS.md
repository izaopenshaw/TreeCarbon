# Reviewer Recommendations: TreeCarbon Paper
## As if reviewing for Methods in Ecology and Evolution or similar ecological journal

---

## Executive summary

The paper presents a valuable open-source tool for UK forest carbon accounting. The core message—that method choice introduces substantial uncertainty—is important and well-supported. However, the manuscript under-exploits the package's unique capabilities, particularly the WCC stand-level vs individual-tree comparison, and could be strengthened to maximise impact and better represent what TreeCarbon actually does.

---

## 1. Better representing the package

### 1.1 The package does more than the paper shows

**Issue:** The paper emphasises comparison across *allometric methods* (WCC, BIOMASS, allodb, Bunce) but gives limited attention to the package's other distinctive features:

- **WCC inventory methods (Methods B, C, D, E):** TreeCarbon implements multiple WCC approaches—individual-tree, stand-level (Method D), sample-level tariff (Methods B/C), and tally-based (Method E). For the same Wakehurst data, these yield **288 to 1,672 t C**—a ~6× range *within* the WCC protocol. This is a major finding that is currently buried in a short paragraph.

- **Shiny apps:** Mentioned briefly; their role in making the package accessible to non-coders could be emphasised more.

- **Workflow integration:** The package supports a full workflow (single tree → batch → stand → per-hectare). This is not clearly articulated.

**Recommendation:** Add a short "Package overview" or "Workflow" subsection that outlines: (1) single-tree functions, (2) batch processing, (3) stand-level WCC methods (B/C/D/E), (4) allometric comparison, (5) Shiny apps. A simple schematic or bullet list would help.

### 1.2 WCC stand vs individual: a critical missing comparison

**Issue:** You have the data but do not synthesise it. For Wakehurst:

| Approach | Total carbon (t C) | Notes |
|----------|-------------------|-------|
| Individual-tree WCC (sum over trees) | 1,672 | Tree-by-tree tariff |
| Stand-level Method D (fc_stand_carbon) | 540 | QMD, top height |
| Method B/C (sample tariff by habitat) | 288 | Floor(mean tariff) |
| Method E style (DBH-class tally) | 609 | Tally-based |

The same woodland, same protocol family (WCC), yields **288–1,672 t C** depending on inventory and aggregation method. This is arguably as important as the 2.98× spread across allometric methods (562–1,672 t C).

**Recommendation:** Add a **new Results subsection** (e.g. "WCC inventory and aggregation methods") with a table comparing these four approaches. Include a sentence such as: *"For the same Wakehurst inventory, WCC-compliant methods yielded totals ranging from 288 t C (Method B/C) to 1,672 t C (individual-tree sum), illustrating that choice of inventory design and aggregation method can affect reported carbon stocks as much as choice of allometric equation."*

### 1.3 Table 1 could better reflect package structure

**Issue:** Table 1 lists allometric methods but does not distinguish WCC *inventory* methods (B, C, D, E). Users need to understand that WCC can be applied in several ways.

**Recommendation:** Consider a second table or an expanded Table 1 that includes:
- Individual-tree WCC (Methods A/B/C style)
- Stand-level WCC (Method D)
- Method B/C (sample tariff)
- Method E (tally)

---

## 2. Larger impact

### 2.1 Lead with the policy and practice angle

**Issue:** The Introduction is sound but could more clearly state why this matters for UK policy and practice.

**Recommendation:** In the opening paragraph or abstract, add an explicit link to:
- UK net-zero and woodland creation targets
- Woodland Carbon Code as the standard for UK carbon projects
- The risk that inconsistent methods undermine confidence in carbon credits

Example: *"With the UK targeting significant woodland expansion and the Woodland Carbon Code as the primary verification standard, transparent and comparable carbon estimation is essential for credible carbon markets and policy accountability."*

### 2.2 Quantify the "danger zone" impact

**Issue:** You report 795 trees (37%) in the "danger zone" (CV > 40%) but do not quantify the carbon at stake.

**Recommendation:** Add: *"These 795 trees accounted for X% of total carbon (using mean across methods), highlighting that method choice is most consequential where carbon density is highest."* If the data support it, note whether danger-zone trees are concentrated in certain habitats or size classes.

### 2.3 Large-tree dominance

**Issue:** You cite Calders et al. but do not present your own large-tree dominance result. The wakehurst output shows: *"Top 10% of trees (217 trees) contain 27.6% of total carbon"* and *"0–10 cm DBH class holds 57% of carbon"* (likely driven by high tree counts in small classes—clarify).

**Recommendation:** Add a short paragraph or figure on cumulative carbon vs cumulative % of trees (ranked by DBH). This would:
- Support the Discussion point on large trees
- Provide a UK temperate example analogous to Calders et al.
- Show that a small fraction of trees holds a disproportionate share of carbon

### 2.4 Clearer take-home for practitioners

**Issue:** The Conclusion is good but could be more actionable.

**Recommendation:** Add a short "Recommendations for practitioners" sentence, e.g.: *"We recommend that carbon project developers (1) report which allometric method and WCC inventory method were used, (2) quantify uncertainty where possible, and (3) use TreeCarbon to compare methods when tree-level data are available."*

---

## 3. Writing quality

### 3.1 Abstract

**Issue:** The abstract is dense. The key result (32.5% median CV, 37% in danger zone) could be more prominent.

**Recommendation:** Move the quantitative result earlier: *"Analysis of 2,173 trees at a UK woodland site showed that method choice introduces a median coefficient of variation of 32.5% in carbon estimates, with 37% of trees in a 'danger zone' (CV > 40%) where method justification is critical."*

### 3.2 Redundancy

**Issue:** Some points are repeated (e.g. method choice vs measurement uncertainty in Results and Discussion).

**Recommendation:** In the Discussion, avoid repeating the exact numbers; instead, interpret them (e.g. implications for carbon markets, calibration needs).

### 3.3 Flow

**Issue:** The transition from Methods to Results is abrupt. The Study site appears after Shiny app, which breaks the narrative.

**Recommendation:** Move "Study site" to immediately before Results, or integrate it into a short "Data and study site" subsection at the end of Methods.

### 3.4 Terminology

**Issue:** "Method B/C" and "Method D" are used without a brief reminder of what they mean (sample-level tariff vs stand-level).

**Recommendation:** On first use in Results, add: *"Method B/C (sample-level tariff by habitat)"* and *"Method D (stand-level, using top height and quadratic mean DBH)"*.

---

## 4. WCC examples: do you need more?

**Yes.** The current paper under-uses the WCC-specific analyses. Recommended additions:

### 4.1 Stand vs individual vs other methods: add a synthesis table

Add **Table 2** (or integrate into an expanded table) comparing:

| Method type | Approach | Total carbon (t C) | Carbon per ha (range) |
|-------------|----------|--------------------|------------------------|
| Individual-tree allometric | WCC | 1,672 | 182–448 |
| Individual-tree allometric | BIOMASS | 562 | 141–179 |
| Individual-tree allometric | allodb | 778 | — |
| Individual-tree allometric | Bunce | 677 | — |
| WCC stand-level | Method D | 540 | 69–165 |
| WCC sample-level | Method B/C | 288 | — |
| WCC tally-based | Method E style | 609 | — |

This would make the package's scope and the range of possible estimates immediately clear.

### 4.2 Why do WCC methods differ so much?

**Issue:** The 288–1,672 t C range within WCC is striking but not explained.

**Recommendation:** Add 1–2 sentences: *"The large spread reflects differences in inventory design (full census vs sample), aggregation (individual-tree sum vs stand mean), and DBH treatment (quadratic mean for Method D). Individual-tree WCC sums tree-level estimates; Method D uses stand-level top height and quadratic mean DBH, which typically yields lower per-tree volume in skewed distributions."* (Adjust if your interpretation differs.)

### 4.3 Worked example

**Issue:** A single worked example (e.g. one tree or one stand) would help readers see the workflow.

**Recommendation:** Consider a short "Worked example" in Methods or as supplementary material: one tree through `fc_agc()`, and one stand through `fc_stand_carbon()`, with inputs and outputs. This would improve reproducibility and clarity.

---

## 5. Other suggestions

### 5.1 Figures

**Issue:** The paper references "suggested figures" in the write-up but the LaTeX has no figures. For a methods paper, 1–2 key figures would strengthen the narrative.

**Recommendation:** Include at least:
1. **Method comparison:** Bar chart of total carbon by method (WCC, BIOMASS, allodb, Bunce, and optionally stand D, B/C, tally).
2. **CV distribution or by DBH class:** Histogram or boxplot showing that small trees have higher method disagreement.

### 5.2 TLS clarification

**Issue:** The Discussion discusses TLS validation from the literature but Wakehurst TLS data were not validated/usable. A reviewer may infer that you validated against TLS.

**Recommendation:** Add: *"We did not validate TreeCarbon estimates against TLS at our study site; the TLS discussion draws on published comparisons (Calders et al., Demol et al.)."*

### 5.3 Software availability

**Issue:** "TreeCarbon v1.0 is available on GitHub [to add url]"—placeholder.

**Recommendation:** Add the actual URL and, if applicable, a Zenodo DOI for the version used in the paper.

### 5.4 Author and date

**Issue:** Author is "X", date is blank.

**Recommendation:** Complete before submission.

---

## 6. Summary checklist

| Recommendation | Priority |
|----------------|----------|
| Add WCC inventory methods comparison table (stand D vs B/C vs tally vs individual) | **High** |
| Add "WCC inventory and aggregation methods" Results subsection | **High** |
| Add policy/practice framing in Introduction | **Medium** |
| Add large-tree dominance result (cumulative % carbon) | **Medium** |
| Move Study site before Results | **Medium** |
| Add 1–2 figures (method comparison, CV) | **Medium** |
| Clarify TLS is from literature, not Wakehurst validation | **Medium** |
| Add practitioner recommendations in Conclusion | **Low** |
| Add worked example (optional) | **Low** |
| Complete author, URL, date | **Essential** |

---

## 7. Bottom line

The paper is publishable but would be stronger if it:

1. **Synthesises the WCC methods comparison**—stand vs individual vs B/C vs tally—into a clear table and narrative. This is a unique contribution that the package enables and the data support.

2. **Frames impact** more explicitly for UK carbon policy and the Woodland Carbon Code.

3. **Adds 1–2 figures** to illustrate method differences and the danger zone.

4. **Tightens structure** (Study site placement, redundancy) and **clarifies TLS** (literature, not site validation).

These changes would better represent TreeCarbon's capabilities and increase the paper's impact for both researchers and practitioners.
