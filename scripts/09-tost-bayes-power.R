# scripts/09-tost-bayes-power.R
message("TOST + Bayes + Power...")

df <- readRDS("outputs/analysis_df.rds")

# --- TOST for correlations via Fisher z (equivalence bounds ±0.36) ----
tost_cor_fisher <- function(r, n, low_eq = -0.36, high_eq = 0.36, alpha = 0.05) {
  # Fisher z transform
  z_r <- atanh(r)
  se <- 1 / sqrt(n - 3)
  
  z_low <- atanh(low_eq)
  z_high <- atanh(high_eq)
  
  # Two one-sided tests:
  # H1: r > low_eq
  z1 <- (z_r - z_low) / se
  p1 <- 1 - stats::pnorm(z1)
  
  # H1: r < high_eq
  z2 <- (z_high - z_r) / se
  p2 <- 1 - stats::pnorm(z2)
  
  tost_p <- max(p1, p2)
  
  tibble::tibble(
    r = r,
    n = n,
    low_eq = low_eq,
    high_eq = high_eq,
    p1 = p1,
    p2 = p2,
    tost_p = tost_p,
    equivalent = tost_p < alpha
  )
}

# correlations we care about (anthropometry vs F3/F4; vowels + sentence)
pairs <- tibble::tibble(
  outcome = c("mean_F3_6", "mean_F3_6", "mean_F3_6", "mean_F3_6",
              "mean_F4_6", "mean_F4_6", "mean_F4_6", "mean_F4_6",
              "F3_sentence", "F3_sentence", "F3_sentence", "F3_sentence",
              "F4_sentence", "F4_sentence", "F4_sentence", "F4_sentence"),
  predictor = rep(c("height", "BMI", "WHR", "Grip_MAX_both"), times = 4),
  context = rep(c("F3 vowels", "F4 vowels", "F3 sentence", "F4 sentence"), each = 4)
)

tost_tbl <- pairs |>
  dplyr::rowwise() |>
  dplyr::mutate(
    r_obs = stats::cor(df[[outcome]], df[[predictor]], use = "pairwise.complete.obs"),
    n_obs = sum(stats::complete.cases(df[[outcome]], df[[predictor]])),
    res = list(tost_cor_fisher(r = r_obs, n = n_obs))
  ) |>

  tidyr::unnest(res) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    r = round(r_obs, 3),
    tost_p = ifelse(tost_p < .001, "< .001", sprintf("%.3f", tost_p)),
    equivalent = ifelse(equivalent, "Yes", "No")
  ) |>
  dplyr::select(context, predictor, r, tost_p, equivalent)

gt_tbl <- gt::gt(tost_tbl) |>
  gt::tab_header(title = "TOST (equivalence bounds ±0.36) for correlations") |>
  gt::cols_label(
    context = "Outcome",
    predictor = "Predictor",
    r = "r",
    tost_p = "TOST p",
    equivalent = "Equivalent?"
  )

gt::gtsave(gt_tbl, "outputs/tables/TOST_correlations.html")

message("TOST table saved.")

# --- Bayes Factors (BF01) for correlations --------------------------------
message("Computing Bayes Factors (BF01) for correlations...")

bf_one <- function(x, y) {
  ok <- stats::complete.cases(x, y)
  x <- x[ok]
  y <- y[ok]
  
  # BF10 as BFBayesFactor object (S4)
  bf_obj <- BayesFactor::correlationBF(x = x, y = y)
  
  # Convert to data.frame and pull bf (this is robust across versions)
  bf_df <- as.data.frame(bf_obj)
  
  # Column name is usually "bf"; fallback to first numeric column
  if ("bf" %in% names(bf_df)) {
    bf10 <- bf_df$bf[1]
  } else {
    num_cols <- names(bf_df)[vapply(bf_df, is.numeric, logical(1))]
    bf10 <- bf_df[[num_cols[1]]][1]
  }
  
  bf01 <- 1 / bf10
  
  tibble::tibble(
    r = stats::cor(x, y),
    n = length(x),
    bf10 = bf10,
    bf01 = bf01
  )
}

bf_list <- vector("list", nrow(pairs))

for (i in seq_len(nrow(pairs))) {
  out <- pairs$outcome[i]
  pred <- pairs$predictor[i]
  
  bf_list[[i]] <- bf_one(df[[out]], df[[pred]]) |>
    dplyr::mutate(
      context = pairs$context[i],
      predictor = pred,
      .before = 1
    )
}

bf_tbl <- dplyr::bind_rows(bf_list) |>
  dplyr::mutate(
    r = round(r, 3),
    BF01 = round(bf01, 2)
  ) |>
  dplyr::select(context, predictor, r, BF01)

gt_bf <- gt::gt(bf_tbl) |>
  gt::tab_header(title = "Bayes Factors for correlations (BF01)") |>
  gt::cols_label(
    context = "Outcome",
    predictor = "Predictor",
    r = "r",
    BF01 = "BF01"
  )

gt::gtsave(gt_bf, "outputs/tables/BF01_correlations.html")

message("BF01 table saved.")

# --- Power analysis -------------------------------------------------------

message("Computing power analysis...")

# N = 79
n_total <- nrow(df)

# minimalny efekt dla 80% mocy przy alpha = .05
power_target <- 0.80

# szukamy minimalnego |r|
power_fun <- function(r) {
  pwr::pwr.r.test(
    n = n_total,
    r = r,
    sig.level = 0.05
  )$power - power_target
}

# rozwiązanie numeryczne
r_min <- uniroot(power_fun, interval = c(0.01, 0.99))$root

power_tbl <- tibble::tibble(
  N = n_total,
  alpha = 0.05,
  target_power = power_target,
  min_detectable_r = round(r_min, 3)
)

gt_power <- gt::gt(power_tbl) |>
  gt::tab_header(title = "Power analysis (two-sided correlation test)") |>
  gt::cols_label(
    N = "Sample size",
    alpha = "Alpha",
    target_power = "Target power",
    min_detectable_r = "Minimal detectable |r|"
  )

gt::gtsave(gt_power, "outputs/tables/Power_analysis.html")

message("Power table saved.")