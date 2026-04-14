# VAP-1

Reproducible R analysis pipeline for the study:

**Higher Formants (F3 and F4) and Anthropometric Characteristics in Healthy Young Women**

---

## Project overview

This repository contains a fully reproducible workflow for statistical analyses examining the relationship between higher formants (F3, F4) and anthropometric variables.

The pipeline includes:
- data processing
- variable construction
- statistical modeling (correlations, regression)
- equivalence testing (TOST)
- Bayes factors
- figure and table generation

---

## Repository structure

- `scripts/` – full analysis pipeline
- `outputs/` – generated results (figures, tables, diagnostics)
- `renv/` and `renv.lock` – reproducible R environment
- `00_run_all.R` – main pipeline script

---

## Reproducibility

To reproduce all analyses:

```r
source("00_run_all.R")