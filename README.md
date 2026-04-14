# VAP-1

Reproducible R analysis pipeline for the study on higher formants and anthropometric characteristics in healthy young women.

## Contents

- `scripts/` – analysis scripts
- `outputs/` – generated figures, tables, and checks
- `renv/` and `renv.lock` – reproducible R environment
- `00_run_all.R` – main project runner
- `01_pipeline_check.R` – pipeline check script

## Reproducibility

Open the project in RStudio and run:

```r
source("00_run_all.R")