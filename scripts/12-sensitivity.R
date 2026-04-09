# scripts/12-sensitivity.R

message("Running sensitivity analyses...")

df <- readRDS("outputs/analysis_df.rds")
dir.create("outputs/checks", showWarnings = FALSE)

# Final model formulas (vowels)
f_f3_full <- mean_F3_6 ~ mean_F0_6 + age + height + BMI + WHR + Grip_MAX_both
f_f4_full <- mean_F4_6 ~ mean_F0_6 + age + height + BMI + WHR + Grip_MAX_both

# Alternative: BMI replaced by mass
f_f3_mass <- mean_F3_6 ~ mean_F0_6 + age + height + mass + WHR + Grip_MAX_both
f_f4_mass <- mean_F4_6 ~ mean_F0_6 + age + height + mass + WHR + Grip_MAX_both

# Quadratic terms for BMI (as in your sensitivity description)
f_f3_quad <- mean_F3_6 ~ mean_F0_6 + age + height + BMI + I(BMI^2) + WHR + Grip_MAX_both
f_f4_quad <- mean_F4_6 ~ mean_F0_6 + age + height + BMI + I(BMI^2) + WHR + Grip_MAX_both

fit_and_extract <- function(formula, data, label) {
  m <- estimatr::lm_robust(formula = formula, data = data, se_type = "HC3")
  
  # Extract just the "somatic" terms we care about
  keep <- c("height", "BMI", "mass", "WHR", "Grip_MAX_both", "I(BMI^2)")
  tbl <- broom::tidy(m) |>
    dplyr::filter(term %in% keep) |>
    dplyr::transmute(
      analysis = label,
      term,
      b = estimate,
      se = std.error,
      p = p.value
    )
  
  tbl
}

# 1) Influential points: exclude Cook's D > 4/n from OLS model
exclude_influential <- function(formula, data) {
  m_lm <- stats::lm(formula = formula, data = data)
  cd <- stats::cooks.distance(m_lm)
  keep <- cd <= (4 / stats::nobs(m_lm))
  data[keep, , drop = FALSE]
}

# 2) Outliers ±3 SD on outcome (simple, transparent rule)
exclude_outcome_outliers <- function(outcome, data) {
  x <- data[[outcome]]
  z <- (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
  data[abs(z) <= 3 | is.na(z), , drop = FALSE]
}

# Run for F3
df_f3_infl <- exclude_influential(f_f3_full, df)
df_f3_out <- exclude_outcome_outliers("mean_F3_6", df)

sens_f3 <- dplyr::bind_rows(
  fit_and_extract(f_f3_full, df, "F3 full (original)"),
  fit_and_extract(f_f3_full, df_f3_infl, "F3 full (exclude influential)"),
  fit_and_extract(f_f3_full, df_f3_out, "F3 full (exclude ±3 SD outcome)"),
  fit_and_extract(f_f3_mass, df, "F3 full (mass instead of BMI)"),
  fit_and_extract(f_f3_quad, df, "F3 full (+ BMI^2)")
)

# Run for F4
df_f4_infl <- exclude_influential(f_f4_full, df)
df_f4_out <- exclude_outcome_outliers("mean_F4_6", df)

sens_f4 <- dplyr::bind_rows(
  fit_and_extract(f_f4_full, df, "F4 full (original)"),
  fit_and_extract(f_f4_full, df_f4_infl, "F4 full (exclude influential)"),
  fit_and_extract(f_f4_full, df_f4_out, "F4 full (exclude ±3 SD outcome)"),
  fit_and_extract(f_f4_mass, df, "F4 full (mass instead of BMI)"),
  fit_and_extract(f_f4_quad, df, "F4 full (+ BMI^2)")
)

sens_all <- dplyr::bind_rows(
  sens_f3 |> dplyr::mutate(outcome = "F3 vowels", .before = 1),
  sens_f4 |> dplyr::mutate(outcome = "F4 vowels", .before = 1)
) |>
  dplyr::mutate(
    b = round(b, 2),
    se = round(se, 2),
    p = dplyr::case_when(
      p < .001 ~ "< .001",
      TRUE ~ sprintf("%.3f", p)
    )
  )

readr::write_csv(sens_all, "outputs/checks/sensitivity_vowels_models.csv")

message("Sensitivity results saved.")