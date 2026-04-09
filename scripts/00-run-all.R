# scripts/00-run-all.R

message("Starting full pipeline...")

source("scripts/01-setup.R.")

message("Setup done.")
message("Pipeline ready.")
source("scripts/02-load-data.R")
source("scripts/03-derive-variables.R")
source("scripts/04-table-1.R")
source("scripts/05-validation.R")
source("scripts/06-correlations-forest-vowels.R")
source("scripts/07-correlations-forest-sentence.R")
source("scripts/08-regressions-vowels.R")
source("scripts/10-manifest.R")
source("scripts/09-tost-bayes-power.R")
source("scripts/11-diagnostics.R")
source("scripts/12-sensitivity.R")


    