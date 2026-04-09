# scripts/08-regressions-vowels.R

message("Running hierarchical regressions (vowels) with HC3...")

df <- readRDS("outputs/analysis_df.rds")

fit_hc3 <- function(formula, data) {
  estimatr::lm_robust(formula = formula, data = data, se_type = "HC3")
}

model_stats <- function(mod) {
  s <- summary(mod)
  
  tibble::tibble(
    n = stats::nobs(mod),
    r2 = unname(s$r.squared),
    adj_r2 = unname(s$adj.r.squared),
    f_stat = unname(s$fstatistic[1]),
    df1 = unname(s$fstatistic[2]),
    df2 = unname(s$fstatistic[3]),
    p_f = stats::pf(f_stat, df1, df2, lower.tail = FALSE)
  )
}

aicc <- function(formula, data) {
  m_lm <- stats::lm(formula = formula, data = data)
  
  out <- try(AICcmodavg::AICc(m_lm), silent = TRUE)
  if (!inherits(out, "try-error")) return(unname(out))
  
  aic <- stats::AIC(m_lm)
  k <- length(stats::coef(m_lm))
  n <- stats::nobs(m_lm)
  aic + (2 * k * (k + 1)) / (n - k - 1)
}

tidy_coef <- function(mod) {
  broom::tidy(mod) |>
    dplyr::transmute(
      term,
      b = estimate,
      se = std.error,
      p = p.value
    )
}

make_tables_for_outcome <- function(outcome, file_stub) {
  # Hierarchical models
  f0 <- "mean_F0_6"
  
  f_m0 <- stats::as.formula(paste0(outcome, " ~ ", f0, " + age"))
  f_m1 <- stats::as.formula(paste0(outcome, " ~ ", f0, " + age + height + BMI"))
  f_m2 <- stats::as.formula(paste0(outcome, " ~ ", f0, " + age + height + BMI + WHR"))
  f_m3 <- stats::as.formula(paste0(outcome, " ~ ", f0, " + age + height + BMI + WHR + Grip_MAX_both"))
  
  m0 <- fit_hc3(f_m0, df)
  m1 <- fit_hc3(f_m1, df)
  m2 <- fit_hc3(f_m2, df)
  m3 <- fit_hc3(f_m3, df)
  
  forms <- list(M0 = f_m0, M1 = f_m1, M2 = f_m2, M3 = f_m3)
  mods  <- list(M0 = m0,  M1 = m1,  M2 = m2,  M3 = m3)
  # Coefficients table: b (SE) + p per model
  coef_tbl <- purrr::imap_dfr(mods, \(m, name) {
    tidy_coef(m) |>
      dplyr::mutate(model = name, .before = 1)
  }) |>
    dplyr::mutate(
      b_se = paste0(round(b, 2), " (", round(se, 2), ")"),
      p_fmt = dplyr::case_when(
        p < .001 ~ "< .001",
        TRUE ~ sprintf("%.3f", p)
      )
    ) |>
    dplyr::select(model, term, b_se, p_fmt) |>
    tidyr::pivot_wider(names_from = model, values_from = c(b_se, p_fmt), names_sep = " ")
  
  gt_coef <- gt::gt(coef_tbl) |>
    gt::tab_header(title = paste0(file_stub, ": HC3 robust coefficients")) |>
    gt::cols_label(term = "Predictor")
  
  gt::gtsave(gt_coef, paste0("outputs/tables/", file_stub, "_coefficients.html"))
  
  # Model stats table
  stats_tbl <- purrr::imap_dfr(mods, \(m, name) {
    ms <- model_stats(m)
    tibble::tibble(
      model = name,
      n = ms$n,
      r2 = ms$r2,
      adj_r2 = ms$adj_r2,
      f_stat = ms$f_stat,
      p_f = ms$p_f,
      aicc = aicc(forms[[name]], df)
    )
  }) |>
    dplyr::arrange(model) |>
    dplyr::mutate(
      dplyr::across(c(r2, adj_r2), \(x) round(x, 3)),
      f_stat = round(f_stat, 2),
      p_f = ifelse(p_f < .001, "< .001", sprintf("%.3f", p_f)),
      aicc = round(aicc, 2),
      delta_r2 = c(NA, diff(r2))
    ) |>
    dplyr::mutate(
      delta_r2 = ifelse(is.na(delta_r2), "", sprintf("%.3f", delta_r2))
    )
  
  gt_stats <- gt::gt(stats_tbl) |>
    gt::tab_header(title = paste0(file_stub, ": Model fit statistics")) |>
    gt::cols_label(
      model = "Model",
      n = "N",
      r2 = "R²",
      adj_r2 = "Adj R²",
      delta_r2 = "ΔR²",
      f_stat = "F",
      p_f = "p",
      aicc = "AICc"
    )
  
  gt::gtsave(gt_stats, paste0("outputs/tables/", file_stub, "_modelstats.html"))
  
  invisible(TRUE)
}

make_tables_for_outcome("mean_F3_6", "Table4_F3")
make_tables_for_outcome("mean_F4_6", "Table5_F4")

message("Tables 4–5 saved.")