# scripts/11-diagnostics.R

message("Running diagnostics (VIF, Cook's D, Shapiro)...")

df <- readRDS("outputs/analysis_df.rds")

dir.create("outputs/checks", showWarnings = FALSE)

# Formuły finalnych modeli (tak jak w tabelach)
f_f3 <- mean_F3_6 ~ mean_F0_6 + age + height + BMI + WHR + Grip_MAX_both
f_f4 <- mean_F4_6 ~ mean_F0_6 + age + height + BMI + WHR + Grip_MAX_both

diag_one <- function(formula, data, label) {
  m <- stats::lm(formula = formula, data = data)
  
  # VIF (car)
  vif <- car::vif(m)
  vif_df <- tibble::tibble(
    model = label,
    term = names(vif),
    vif = as.numeric(vif)
  )
  
  # Cook's distance
  cooks <- stats::cooks.distance(m)
  cooks_df <- tibble::tibble(
    model = label,
    cook_max = max(cooks, na.rm = TRUE),
    cook_n_over_4 = sum(cooks > (4 / stats::nobs(m)), na.rm = TRUE)
  )
  
  # Shapiro-Wilk on residuals
  sh <- stats::shapiro.test(stats::residuals(m))
  sh_df <- tibble::tibble(
    model = label,
    shapiro_w = unname(sh$statistic),
    shapiro_p = unname(sh$p.value)
  )
  
  list(vif = vif_df, cooks = cooks_df, shapiro = sh_df)
}

d1 <- diag_one(f_f3, df, "F3 vowels (final)")
d2 <- diag_one(f_f4, df, "F4 vowels (final)")

vif_all <- dplyr::bind_rows(d1$vif, d2$vif)
cooks_all <- dplyr::bind_rows(d1$cooks, d2$cooks)
shapiro_all <- dplyr::bind_rows(d1$shapiro, d2$shapiro)

# Zapis do CSV (najprościej do checks)
readr::write_csv(vif_all, "outputs/checks/diagnostics_vif.csv")
readr::write_csv(cooks_all, "outputs/checks/diagnostics_cooks.csv")
readr::write_csv(shapiro_all, "outputs/checks/diagnostics_shapiro.csv")

message("Diagnostics saved to outputs/checks/.")