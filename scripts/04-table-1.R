# scripts/04-table-1.R

message("Creating Table 1...")

df <- readRDS("outputs/analysis_df.rds")

vars <- c(
  "age", "height", "mass", "BMI", "WHR", "Grip_MAX_both",
  "mean_F0_6", "mean_F3_6", "mean_F4_6",
  "F0_sentence", "F3_sentence", "F4_sentence"
)

# helper: mean, sd, 95% CI
summ_one <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  m <- mean(x)
  s <- stats::sd(x)
  se <- s / sqrt(n)
  tcrit <- stats::qt(0.975, df = n - 1)
  ci_low <- m - tcrit * se
  ci_high <- m + tcrit * se
  
  tibble::tibble(
    n = n,
    mean = m,
    sd = s,
    ci95_low = ci_low,
    ci95_high = ci_high
  )
}

tab <- purrr::map_dfr(vars, \(v) {
  summ_one(df[[v]]) |>
    dplyr::mutate(variable = v, .before = 1)
})

tab_fmt <- tab |>
  dplyr::mutate(
    dplyr::across(c(mean, sd, ci95_low, ci95_high), \(x) round(x, 2))
  )

gt_tbl <- gt::gt(tab_fmt) |>
  gt::tab_header(title = "Table 1. Descriptive statistics (mean, SD, 95% CI)") |>
  gt::cols_label(
    variable = "Variable",
    n = "N",
    mean = "Mean",
    sd = "SD",
    ci95_low = "95% CI low",
    ci95_high = "95% CI high"
  )

gt::gtsave(gt_tbl, "outputs/tables/Table1.html")

message("Table 1 saved.")