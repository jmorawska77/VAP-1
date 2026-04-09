############################################
# 01_pipeline_check.R — Reproducibility checks
############################################

# 1) Run the full pipeline from a clean-ish state
rm(list = ls())
gc()

# Run
source("00_run_all.R")

# 2) Basic smoke checks: do outputs exist?
stopifnot(dir.exists("outputs/tables"))
stopifnot(dir.exists("outputs/figures"))
stopifnot(file.exists("outputs/tables/Table4_F3_coefficients.html"))
stopifnot(file.exists("outputs/tables/Table5_F4_coefficients.html"))
stopifnot(file.exists("outputs/tables/TableS1_F3_sentence_coefficients.html"))
stopifnot(file.exists("outputs/tables/TableS2_F4_sentence_coefficients.html"))
stopifnot(file.exists("outputs/figures/Figure_1_forest_vowels.tiff"))
stopifnot(file.exists("outputs/figures/Figure_S_forest_sentence.tiff"))
stopifnot(file.exists("outputs/checks/TOST_key_pairs.csv"))
stopifnot(file.exists("outputs/checks/BayesFactor_key_pairs.csv"))

# 3) Save key numeric results as a "golden" snapshot (or compare if already exists)
# We compare numeric tables that are deterministic.
gold_path <- "outputs/checks/golden_snapshot.rds"

snapshot <- list(
  N = nrow(df),
  stats_f3 = stats_f3,
  stats_f4 = stats_f4,
  tost = read.csv("outputs/checks/TOST_key_pairs.csv"),
  bf = read.csv("outputs/checks/BayesFactor_key_pairs.csv")
)

if (!file.exists(gold_path)) {
  saveRDS(snapshot, gold_path)
  message("Golden snapshot created: ", gold_path)
} else {
  gold <- readRDS(gold_path)
  
  # Tolerances for floating point
  stopifnot(isTRUE(all.equal(snapshot$N, gold$N)))
  stopifnot(isTRUE(all.equal(snapshot$stats_f3, gold$stats_f3, tolerance = 1e-10)))
  stopifnot(isTRUE(all.equal(snapshot$stats_f4, gold$stats_f4, tolerance = 1e-10)))
  stopifnot(isTRUE(all.equal(snapshot$tost, gold$tost, tolerance = 1e-10)))
  stopifnot(isTRUE(all.equal(snapshot$bf, gold$bf, tolerance = 1e-10)))
  
  message("Reproducibility check PASSED ✅ — results match golden snapshot.")
}

# 4) Log summary
writeLines(
  c(
    paste("CHECK DATE:", Sys.time()),
    paste("N:", snapshot$N),
    "Key files exist: YES",
    "Golden snapshot: OK"
  ),
  "outputs/checks/pipeline_check_log.txt"
)

message("All checks completed.")

save.image("BACKUP_2026_02_22.RData")
savehistory("history.Rhistory")

savehistory("history.Rhistory")
