############################################
# 00_run_all.R — Full reproducible pipeline
# Project: VAP Paper 1
# Outputs: tables -> HTML, figures -> TIFF
############################################

# =========================
# 0) Setup
# =========================
options(stringsAsFactors = FALSE)
set.seed(123)

# Packages
pkgs <- c(
  "readxl", "dplyr", "tidyr", "purrr", "tibble", "stringr",
  "ggplot2", "scales", "sandwich", "lmtest", "car",
  "gt", "BayesFactor", "TOSTER"
)

to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(ggplot2)
library(scales)
library(sandwich)
library(lmtest)
library(car)
library(gt)
library(BayesFactor)
library(TOSTER)

# Folders
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/checks", recursive = TRUE, showWarnings = FALSE)

writeLines(capture.output(sessionInfo()), "outputs/checks/sessionInfo.txt")

# =========================
# 1) Load data
# =========================
# EDIT THIS PATH:
data_path <- "Praca 1 całość.xlsx"

df_raw <- readxl::read_excel(data_path)

# ---- ROBUST COLUMN NAME STANDARDIZATION (auto-detect) ----
normalize_nm <- function(x) {
  x <- tolower(x)
  x <- gsub('[^a-z0-9]+', '', x)
  x
}

nms_raw <- names(df_raw)
nms_norm <- normalize_nm(nms_raw)

pick_col <- function(keys) {
  hit <- which(nms_norm %in% keys)
  if (length(hit) == 0) return(NA_integer_)
  hit[1]
}

# klucze (dopasowania) — celowo szerokie
keys <- list(
  mean_F0_6      = c('meanf06','meanf0_6','mean_f0_6','meanf0vowels','meanf0vowel','meanf0'),
  mean_F3_6      = c('meanf36','meanf3_6','mean_f3_6','meanf3vowels','meanf3'),
  mean_F4_6      = c('meanf46','meanf4_6','mean_f4_6','meanf4vowels','meanf4'),
  height         = c('height','wzrost'),
  mass           = c('mass','weight','waga'),
  BMI            = c('bmi'),
  WHR            = c('whr'),
  Grip_MAX_both  = c('gripmaxboth','gripmax_both','gripmax','gripstrength','grip'),
  age            = c('age','wiek'),
  F0_sentence    = c('f0sentence','f0_sentence','f0sent','f0zdanie'),
  F3_sentence    = c('f3sentence','f3_sentence','f3sent','f3zdanie'),
  F4_sentence    = c('f4sentence','f4_sentence','f4sent','f4zdanie')
)

# dodatkowo: jeśli kolumny mają dokładnie nazwy ze spacjami, bierzemy je wprost
if (all(c('F0 sentence','F3 sentence','F4 sentence') %in% nms_raw)) {
  keys$F0_sentence <- unique(c(keys$F0_sentence, normalize_nm('F0 sentence')))
  keys$F3_sentence <- unique(c(keys$F3_sentence, normalize_nm('F3 sentence')))
  keys$F4_sentence <- unique(c(keys$F4_sentence, normalize_nm('F4 sentence')))
}

idx <- lapply(keys, pick_col)

# zbuduj mapę rename: tylko te, które znalezione
rename_map <- purrr::imap(idx, \(i, nm) {
  if (is.na(i)) return(NULL)
  old <- nms_raw[[i]]
  setNames(old, nm)
}) |> purrr::compact() |> unlist()

df <- df_raw
if (length(rename_map) > 0) {
  df <- dplyr::rename(df, !!!rename_map)
}

# raport mapowania (żeby było transparentnie)
map_df <- tibble::tibble(new_name = names(rename_map), old_name = unname(rename_map))
dir.create('outputs/checks', recursive = TRUE, showWarnings = FALSE)
write.csv(map_df, 'outputs/checks/column_mapping.csv', row.names = FALSE)

# upewnij się, że numeric
vars_numeric <- intersect(c(
  'mean_F0_6','mean_F3_6','mean_F4_6','height','mass','BMI','WHR','Grip_MAX_both','age',
  'F0_sentence','F3_sentence','F4_sentence'
), names(df))
df <- dplyr::mutate(df, dplyr::across(dplyr::all_of(vars_numeric), as.numeric))
# ---- END ROBUST STANDARDIZATION ----


# ---- Minimal cleaning: keep names as-is, but standardize the sentence columns if present

# If sentence columns exist with spaces, rename them to syntactic names
if (all(c("F0 sentence", "F3 sentence", "F4 sentence") %in% names(df))) {
  df <- df |>
    rename(
      F0_sentence = `F0 sentence`,
      F3_sentence = `F3 sentence`,
      F4_sentence = `F4 sentence`
    )
}

# Sanity checks
required <- c(
  "mean_F0_6", "mean_F3_6", "mean_F4_6",
  "height", "mass", "BMI", "WHR", "Grip_MAX_both", "age"
)
req_sentence <- c("F0_sentence", "F3_sentence", "F4_sentence")
missing_main <- setdiff(required, names(df))
missing_sent <- setdiff(req_sentence, names(df))

if (length(missing_main) > 0) stop("Missing required columns: ", paste(missing_main, collapse = ", "))
if (length(missing_sent) > 0) warning("Missing sentence columns (will skip sentence analyses): ", paste(missing_sent, collapse = ", "))

# Force numeric for analysis variables (safe coercion)
vars_numeric <- intersect(c(required, req_sentence), names(df))
df <- df |>
  mutate(across(all_of(vars_numeric), as.numeric))

# N check
writeLines(paste("N rows:", nrow(df)), "outputs/checks/N_rows.txt")

# =========================
# 2) Helpers
# =========================

# 2.1 Correlation with CI
cor_ci <- function(x, y) {
  out <- cor.test(x, y, method = "pearson")
  tibble(
    r = unname(out$estimate),
    ci_low = unname(out$conf.int[1]),
    ci_high = unname(out$conf.int[2]),
    p = out$p.value
  )
}

# 2.2 r/p label
rp_label <- function(x, y, digits_r = 2) {
  ct <- cor.test(x, y)
  r <- unname(ct$estimate)
  p <- ct$p.value
  p_txt <- if (p < .001) "p < .001" else paste0("p = ", format.pval(p, digits = 2, eps = .001))
  paste0("r = ", sprintf(paste0("%.", digits_r, "f"), r), "\n", p_txt)
}

# 2.3 AICc
AICc <- function(fit) {
  k <- length(coef(fit))
  n <- nobs(fit)
  AIC(fit) + (2 * k * (k + 1)) / (n - k - 1)
}

# 2.4 Model stats
get_stats <- function(fit) {
  s <- summary(fit)
  f_stat <- unname(s$fstatistic[1])
  df1 <- unname(s$fstatistic[2])
  df2 <- unname(s$fstatistic[3])
  
  tibble(
    F = f_stat,
    p = pf(f_stat, df1, df2, lower.tail = FALSE),
    R2 = s$r.squared,
    Adj_R2 = s$adj.r.squared,
    AICc = AICc(fit)
  )
}

# 2.5 HC3 coefficient table (long, for joining)
hc3_coef_tbl <- function(fit, model_tag) {
  ct <- lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type = "HC3"))
  mat <- as.matrix(ct)
  tibble(
    Predictor = rownames(mat),
    !!paste0("b_", model_tag) := mat[, 1],
    !!paste0("SE_", model_tag) := mat[, 2],
    !!paste0("p_", model_tag) := mat[, 4]
  )
}

# 2.6 Pretty format b(SE) + p in wide layout
pretty_coeff_table <- function(tbl_wide, pred_order, pred_labels) {
  out <- tbl_wide |>
    pivot_longer(cols = -Predictor,
                 names_to = c(".value", "Model"),
                 names_sep = "_") |>
    mutate(
      b_se = if_else(is.na(b) | is.na(SE), NA_character_, sprintf("%.2f (%.2f)", b, SE)),
      p_fmt = if_else(is.na(p), NA_character_, sprintf("%.3f", p))
    ) |>
    select(Predictor, Model, b_se, p_fmt) |>
    pivot_wider(
      names_from = Model,
      values_from = c(b_se, p_fmt),
      names_glue = "{Model}_{.value}"
    ) |>
    mutate(Predictor = factor(Predictor, levels = pred_order)) |>
    arrange(Predictor) |>
    mutate(Predictor = recode(as.character(Predictor), !!!pred_labels))
  
  out[is.na(out)] <- ""
  out
}

# 2.7 Save gt to HTML
gt_save_html <- function(x, path, title = NULL) {
  g <- gt(x)
  if (!is.null(title)) g <- g |> tab_header(title = title)
  gtsave(g, path)
}

# =========================
# 3) Descriptives + 95% CI  (Table 1)
# =========================
desc_vars <- c(
  "mean_F0_6", "mean_F3_6", "mean_F4_6",
  "F0_sentence", "F3_sentence", "F4_sentence",
  "height", "mass", "BMI", "WHR", "Grip_MAX_both", "age"
)
desc_vars <- intersect(desc_vars, names(df))

desc_tbl <- df |>
  summarise(across(all_of(desc_vars),
                   list(
                     mean = ~mean(.x, na.rm = TRUE),
                     sd   = ~sd(.x, na.rm = TRUE),
                     n    = ~sum(!is.na(.x)),
                     se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
                     ci_low  = ~mean(.x, na.rm = TRUE) - qt(.975, df = sum(!is.na(.x)) - 1) * (sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))),
                     ci_high = ~mean(.x, na.rm = TRUE) + qt(.975, df = sum(!is.na(.x)) - 1) * (sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))
                   ),
                   .names = "{.col}__{.fn}")) |>
  pivot_longer(everything(),
               names_to = c("Variable", "Stat"),
               names_sep = "__") |>
  pivot_wider(names_from = Stat, values_from = value) |>
  select(Variable, n, mean, sd, ci_low, ci_high) |>
  mutate(across(c(mean, sd, ci_low, ci_high), ~round(.x, 3)))

gt_save_html(desc_tbl, "outputs/tables/Table1_descriptives.html",
             title = "Table 1. Descriptive statistics with 95% CI")

# =========================
# 4) Validation vowels vs sentence (Table 2 + Figure)
# =========================
if (all(c("F3_sentence", "F4_sentence", "F0_sentence") %in% names(df))) {
  
  val_tbl <- tibble(
    Pair = c("F3 vowels vs sentence", "F4 vowels vs sentence", "F0 vowels vs sentence"),
    res = list(
      cor_ci(df$mean_F3_6, df$F3_sentence),
      cor_ci(df$mean_F4_6, df$F4_sentence),
      cor_ci(df$mean_F0_6, df$F0_sentence)
    )
  ) |>
    unnest(res) |>
    mutate(
      r = round(r, 3),
      ci_low = round(ci_low, 3),
      ci_high = round(ci_high, 3),
      p = ifelse(p < .001, "< .001", sprintf("%.3f", p))
    )
  
  gt_save_html(val_tbl, "outputs/tables/Table2_validation_vowels_sentence.html",
               title = "Table 2. Validation correlations between vowels and sentence measures")
  
  # Validation figure (2 panels: F3 and F4)
  p_f3 <- ggplot(df, aes(x = mean_F3_6, y = F3_sentence)) +
    geom_point(alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(x = "Mean F3 (vowels, Hz)", y = "F3 (sentence, Hz)", title = "A. F3") +
    annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2,
             label = rp_label(df$mean_F3_6, df$F3_sentence)) +
    theme_classic(base_size = 14)
  
  p_f4 <- ggplot(df, aes(x = mean_F4_6, y = F4_sentence)) +
    geom_point(alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(x = "Mean F4 (vowels, Hz)", y = "F4 (sentence, Hz)", title = "B. F4") +
    annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2,
             label = rp_label(df$mean_F4_6, df$F4_sentence)) +
    theme_classic(base_size = 14)
  
  # Use patchwork for combining (install if missing)
  if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
  library(patchwork)
  p_validation <- p_f3 + p_f4
  
  ggsave(
    "outputs/figures/Figure_validation_vowels_sentence.tiff",
    p_validation,
    device = "tiff",
    width = 180, height = 90, units = "mm",
    dpi = 600, compression = "lzw"
  )
}

# =========================
# 5) Correlations with anthropometry + forest plots
# =========================

preds <- c("height", "Grip_MAX_both", "BMI", "WHR")
pred_labels <- c(height = "Height", Grip_MAX_both = "Grip", BMI = "BMI", WHR = "WHR")

# 5.1 Vowels forest: F3/F4 vs predictors  (Figure 1)
dvs_vowels <- c("mean_F3_6", "mean_F4_6")
plot_df_vowels <- expand_grid(dv = dvs_vowels, predictor = preds) |>
  mutate(res = map2(dv, predictor, \(dv, pr) cor_ci(df[[dv]], df[[pr]]))) |>
  unnest(res) |>
  mutate(
    dv = recode(dv, mean_F3_6 = "F3", mean_F4_6 = "F4"),
    predictor = recode(predictor, !!!pred_labels),
    predictor = factor(predictor, levels = c("WHR", "BMI", "Grip", "Height"))
  )

p_vowels <- ggplot(plot_df_vowels, aes(x = r, y = predictor)) +
  geom_vline(xintercept = 0, linewidth = 1) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high),
                orientation = "y", width = 0.25, linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~ dv, ncol = 1) +
  scale_x_continuous(
    limits = c(-0.6, 0.6),
    breaks = seq(-0.6, 0.6, 0.2),
    labels = number_format(accuracy = 0.1)
  ) +
  labs(x = "r", y = "Predictor") +
  theme_classic(base_size = 14) +
  theme(strip.background = element_rect(fill = "white", colour = "black"),
        strip.text = element_text(face = "bold"))

ggsave(
  "outputs/figures/Figure_1_forest_vowels.tiff",
  p_vowels,
  device = "tiff",
  width = 120, height = 120, units = "mm",
  dpi = 600, compression = "lzw"
)

# 5.2 Sentence forest: F3/F4 sentence vs predictors (Figure S)
if (all(c("F3_sentence", "F4_sentence") %in% names(df))) {
  dvs_sent <- c("F3_sentence", "F4_sentence")
  plot_df_sent <- expand_grid(dv = dvs_sent, predictor = preds) |>
    mutate(res = map2(dv, predictor, \(dv, pr) cor_ci(df[[dv]], df[[pr]]))) |>
    unnest(res) |>
    mutate(
      dv = recode(dv, F3_sentence = "F3 (sentence)", F4_sentence = "F4 (sentence)"),
      predictor = recode(predictor, !!!pred_labels),
      predictor = factor(predictor, levels = c("WHR", "BMI", "Grip", "Height"))
    )
  
  p_sent <- ggplot(plot_df_sent, aes(x = r, y = predictor)) +
    geom_vline(xintercept = 0, linewidth = 1) +
    geom_errorbar(aes(xmin = ci_low, xmax = ci_high),
                  orientation = "y", width = 0.25, linewidth = 1) +
    geom_point(size = 3) +
    facet_wrap(~ dv, ncol = 1) +
    scale_x_continuous(
      limits = c(-0.6, 0.6),
      breaks = seq(-0.6, 0.6, 0.2),
      labels = number_format(accuracy = 0.1)
    ) +
    labs(x = "r", y = "Predictor") +
    theme_classic(base_size = 14) +
    theme(strip.background = element_rect(fill = "white", colour = "black"),
          strip.text = element_text(face = "bold"))
  
  ggsave(
    "outputs/figures/Figure_S_forest_sentence.tiff",
    p_sent,
    device = "tiff",
    width = 120, height = 120, units = "mm",
    dpi = 600, compression = "lzw"
  )
}

# =========================
# 6) Hierarchical regressions (VOWELS): Tables 4 & 5 (HC3)
# =========================

# ---- Table 4: mean_F3_6
m0_f3 <- lm(mean_F3_6 ~ mean_F0_6 + age, data = df)
m1_f3 <- lm(mean_F3_6 ~ mean_F0_6 + age + height + BMI, data = df)
m2_f3 <- lm(mean_F3_6 ~ mean_F0_6 + age + height + BMI + WHR, data = df)
m3_f3 <- lm(mean_F3_6 ~ mean_F0_6 + age + height + BMI + WHR + Grip_MAX_both, data = df)

stats_f3 <- bind_rows(
  get_stats(m0_f3) |> mutate(Model = "Model 0", .before = 1),
  get_stats(m1_f3) |> mutate(Model = "Model 1", .before = 1),
  get_stats(m2_f3) |> mutate(Model = "Model 2", .before = 1),
  get_stats(m3_f3) |> mutate(Model = "Model 3", .before = 1)
) |>
  mutate(dR2 = c(NA, diff(R2)))

# coefficients wide join
table4_raw <- purrr::reduce(
  list(
    hc3_coef_tbl(m0_f3, "M0"),
    hc3_coef_tbl(m1_f3, "M1"),
    hc3_coef_tbl(m2_f3, "M2"),
    hc3_coef_tbl(m3_f3, "M3")
  ),
  full_join,
  by = "Predictor"
)

pred_order_vowels <- c("(Intercept)", "mean_F0_6", "age", "height", "BMI", "WHR", "Grip_MAX_both")
pred_labels_vowels <- c(
  "(Intercept)" = "Intercept",
  "mean_F0_6" = "Mean F0 (vowels)",
  "age" = "Age",
  "height" = "Height",
  "BMI" = "BMI",
  "WHR" = "WHR",
  "Grip_MAX_both" = "Grip strength"
)

table4_pretty <- pretty_coeff_table(table4_raw, pred_order_vowels, pred_labels_vowels)

gt_save_html(table4_pretty, "outputs/tables/Table4_F3_coefficients.html",
             title = "Table 4. Hierarchical regression predicting mean F3 (vowels), HC3 robust SE, N = 79")
gt_save_html(stats_f3, "outputs/tables/Table4_F3_modelstats.html",
             title = "Table 4 (continued). Model statistics")

# ---- Table 5: mean_F4_6
m0_f4 <- lm(mean_F4_6 ~ mean_F0_6 + age, data = df)
m1_f4 <- lm(mean_F4_6 ~ mean_F0_6 + age + height + BMI, data = df)
m2_f4 <- lm(mean_F4_6 ~ mean_F0_6 + age + height + BMI + WHR, data = df)
m3_f4 <- lm(mean_F4_6 ~ mean_F0_6 + age + height + BMI + WHR + Grip_MAX_both, data = df)

stats_f4 <- bind_rows(
  get_stats(m0_f4) |> mutate(Model = "Model 0", .before = 1),
  get_stats(m1_f4) |> mutate(Model = "Model 1", .before = 1),
  get_stats(m2_f4) |> mutate(Model = "Model 2", .before = 1),
  get_stats(m3_f4) |> mutate(Model = "Model 3", .before = 1)
) |>
  mutate(dR2 = c(NA, diff(R2)))

table5_raw <- purrr::reduce(
  list(
    hc3_coef_tbl(m0_f4, "M0"),
    hc3_coef_tbl(m1_f4, "M1"),
    hc3_coef_tbl(m2_f4, "M2"),
    hc3_coef_tbl(m3_f4, "M3")
  ),
  full_join,
  by = "Predictor"
)

table5_pretty <- pretty_coeff_table(table5_raw, pred_order_vowels, pred_labels_vowels)

gt_save_html(table5_pretty, "outputs/tables/Table5_F4_coefficients.html",
             title = "Table 5. Hierarchical regression predicting mean F4 (vowels), HC3 robust SE, N = 79")
gt_save_html(stats_f4, "outputs/tables/Table5_F4_modelstats.html",
             title = "Table 5 (continued). Model statistics")

# =========================
# 7) Supplement regressions (SENTENCE): Tables S1 & S2 (Base vs Full, HC3)
# =========================
if (all(c("F0_sentence", "F3_sentence", "F4_sentence") %in% names(df))) {
  
  # S1: F3 sentence
  m0_s1 <- lm(F3_sentence ~ F0_sentence + age, data = df)
  mF_s1 <- lm(F3_sentence ~ F0_sentence + age + height + BMI + WHR + Grip_MAX_both, data = df)
  
  stats_s1 <- bind_rows(
    get_stats(m0_s1) |> mutate(Model = "Base", .before = 1),
    get_stats(mF_s1) |> mutate(Model = "Full", .before = 1)
  ) |>
    mutate(dR2 = c(NA, R2[2] - R2[1]))
  
  s1_raw <- purrr::reduce(
    list(
      hc3_coef_tbl(m0_s1, "Base"),
      hc3_coef_tbl(mF_s1, "Full")
    ),
    full_join,
    by = "Predictor"
  )
  
  pred_order_sent <- c("(Intercept)", "F0_sentence", "age", "height", "BMI", "WHR", "Grip_MAX_both")
  pred_labels_sent <- c(
    "(Intercept)" = "Intercept",
    "F0_sentence" = "Mean F0 (sentence)",
    "age" = "Age",
    "height" = "Height",
    "BMI" = "BMI",
    "WHR" = "WHR",
    "Grip_MAX_both" = "Grip strength"
  )
  
  s1_pretty <- pretty_coeff_table(s1_raw, pred_order_sent, pred_labels_sent)
  
  gt_save_html(s1_pretty, "outputs/tables/TableS1_F3_sentence_coefficients.html",
               title = "Table S1. Hierarchical regression predicting F3 (sentence), HC3 robust SE, N = 79")
  gt_save_html(stats_s1, "outputs/tables/TableS1_F3_sentence_modelstats.html",
               title = "Table S1 (continued). Model statistics")
  
  # S2: F4 sentence
  m0_s2 <- lm(F4_sentence ~ F0_sentence + age, data = df)
  mF_s2 <- lm(F4_sentence ~ F0_sentence + age + height + BMI + WHR + Grip_MAX_both, data = df)
  
  stats_s2 <- bind_rows(
    get_stats(m0_s2) |> mutate(Model = "Base", .before = 1),
    get_stats(mF_s2) |> mutate(Model = "Full", .before = 1)
  ) |>
    mutate(dR2 = c(NA, R2[2] - R2[1]))
  
  s2_raw <- purrr::reduce(
    list(
      hc3_coef_tbl(m0_s2, "Base"),
      hc3_coef_tbl(mF_s2, "Full")
    ),
    full_join,
    by = "Predictor"
  )
  
  s2_pretty <- pretty_coeff_table(s2_raw, pred_order_sent, pred_labels_sent)
  
  gt_save_html(s2_pretty, "outputs/tables/TableS2_F4_sentence_coefficients.html",
               title = "Table S2. Hierarchical regression predicting F4 (sentence), HC3 robust SE, N = 79")
  gt_save_html(stats_s2, "outputs/tables/TableS2_F4_sentence_modelstats.html",
               title = "Table S2 (continued). Model statistics")
}

# =========================
# 8) Diagnostics (report to checks)
# =========================
diag_lines <- c()

# VIF on full vowels models
diag_lines <- c(diag_lines, "VIF (F3 full):")
diag_lines <- c(diag_lines, capture.output(car::vif(m3_f3)))

diag_lines <- c(diag_lines, "\nVIF (F4 full):")
diag_lines <- c(diag_lines, capture.output(car::vif(m3_f4)))

# Shapiro on residuals
diag_lines <- c(diag_lines, "\nShapiro residuals (F3 full):")
diag_lines <- c(diag_lines, capture.output(shapiro.test(residuals(m3_f3))))

diag_lines <- c(diag_lines, "\nShapiro residuals (F4 full):")
diag_lines <- c(diag_lines, capture.output(shapiro.test(residuals(m3_f4))))

# Cook's distance summary
cook_f3 <- cooks.distance(m3_f3)
cook_f4 <- cooks.distance(m3_f4)
diag_lines <- c(diag_lines, "\nCook's D summary (F3 full):", capture.output(summary(cook_f3)))
diag_lines <- c(diag_lines, "\nCook's D summary (F4 full):", capture.output(summary(cook_f4)))

writeLines(diag_lines, "outputs/checks/diagnostics.txt")

# =========================
# 9) Sensitivity (basic automated summaries)
# =========================
sens_lines <- c()

# BMI vs mass in full model (vowels)
m3_f3_mass <- lm(mean_F3_6 ~ mean_F0_6 + age + height + mass + WHR + Grip_MAX_both, data = df)
m3_f4_mass <- lm(mean_F4_6 ~ mean_F0_6 + age + height + mass + WHR + Grip_MAX_both, data = df)

sens_lines <- c(sens_lines, "R2 comparison (F3 full): BMI vs mass")
sens_lines <- c(sens_lines, paste("BMI model R2:", round(summary(m3_f3)$r.squared, 6)))
sens_lines <- c(sens_lines, paste("mass model R2:", round(summary(m3_f3_mass)$r.squared, 6)))

sens_lines <- c(sens_lines, "\nR2 comparison (F4 full): BMI vs mass")
sens_lines <- c(sens_lines, paste("BMI model R2:", round(summary(m3_f4)$r.squared, 6)))
sens_lines <- c(sens_lines, paste("mass model R2:", round(summary(m3_f4_mass)$r.squared, 6)))

# Quadratic terms quick check (height^2, BMI^2, WHR^2, Grip^2)
m3_f3_quad <- lm(mean_F3_6 ~ mean_F0_6 + age +
                   height + I(height^2) +
                   BMI + I(BMI^2) +
                   WHR + I(WHR^2) +
                   Grip_MAX_both + I(Grip_MAX_both^2),
                 data = df)

m3_f4_quad <- lm(mean_F4_6 ~ mean_F0_6 + age +
                   height + I(height^2) +
                   BMI + I(BMI^2) +
                   WHR + I(WHR^2) +
                   Grip_MAX_both + I(Grip_MAX_both^2),
                 data = df)

sens_lines <- c(sens_lines, "\nQuadratic models: R2")
sens_lines <- c(sens_lines, paste("F3 quad R2:", round(summary(m3_f3_quad)$r.squared, 6)))
sens_lines <- c(sens_lines, paste("F4 quad R2:", round(summary(m3_f4_quad)$r.squared, 6)))

# Outliers ±3 SD on DV (vowels)
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
df_no_out <- df |>
  mutate(zF3 = z(mean_F3_6), zF4 = z(mean_F4_6)) |>
  filter(abs(zF3) <= 3, abs(zF4) <= 3)

m3_f3_noout <- lm(mean_F3_6 ~ mean_F0_6 + age + height + BMI + WHR + Grip_MAX_both, data = df_no_out)
m3_f4_noout <- lm(mean_F4_6 ~ mean_F0_6 + age + height + BMI + WHR + Grip_MAX_both, data = df_no_out)

sens_lines <- c(sens_lines, "\nOutliers removed (|z|<=3): N and R2")
sens_lines <- c(sens_lines, paste("N:", nrow(df_no_out)))
sens_lines <- c(sens_lines, paste("F3 R2:", round(summary(m3_f3_noout)$r.squared, 6)))
sens_lines <- c(sens_lines, paste("F4 R2:", round(summary(m3_f4_noout)$r.squared, 6)))

writeLines(sens_lines, "outputs/checks/sensitivity.txt")

# =========================
# 10) Equivalence tests (TOST) for key correlations
# =========================
# Bound ±0.36 on r
eq_lines <- c()
eq_bound <- 0.36

# Example: correlations of vowels F3/F4 with anthropometry (can extend as needed)
key_pairs <- expand_grid(
  dv = c("mean_F3_6", "mean_F4_6"),
  predictor = preds
)

eq_res <- key_pairs |>
  mutate(
    r = map2_dbl(dv, predictor, \(dv, pr) unname(cor.test(df[[dv]], df[[pr]])$estimate)),
    n = nrow(df),
    tost = pmap(list(r = r, n = n, low_eqbound_r = -eq_bound, high_eqbound_r = eq_bound),
                \(r, n, low_eqbound_r, high_eqbound_r)
                TOSTER::TOSTcorrelations(r = r, n = n,
                                         low_eqbound_r = low_eqbound_r,
                                         high_eqbound_r = high_eqbound_r))
  ) |>
  mutate(
    p_tost = map_dbl(tost, \(x) x$TOST$p.value),
    eq = p_tost < 0.05
  ) |>
  select(dv, predictor, r, p_tost, eq)

write.csv(eq_res, "outputs/checks/TOST_key_pairs.csv", row.names = FALSE)

# =========================
# 11) Bayesian correlations (BF01) for key pairs
# =========================
# BayesFactor::correlationBF returns BF10; we report BF01 = 1 / BF10
bf_res <- key_pairs |>
  mutate(
    bf10 = map2_dbl(dv, predictor, \(dv, pr) {
      bf <- BayesFactor::correlationBF(df[[dv]], df[[pr]], rscale = 1) # default scale
      as.numeric(bf)
    }),
    bf01 = 1 / bf10
  ) |>
  select(dv, predictor, bf10, bf01)

write.csv(bf_res, "outputs/checks/BayesFactor_key_pairs.csv", row.names = FALSE)

# =========================
# 12) Power note (r >= .31 at N=79 for 80%)
# =========================
# Save a small note
writeLines(
  c("Power note:",
    "For N = 79, 80% power corresponds approximately to |r| >= 0.31 (two-tailed alpha = .05)."),
  "outputs/checks/power_note.txt"
)

# =========================
# 13) Manifest of outputs
# =========================
files_out <- list.files("outputs", recursive = TRUE, full.names = TRUE)
manifest <- tibble(
  file = basename(files_out),
  path = files_out
)
write.csv(manifest, "outputs/manifest.csv", row.names = FALSE)

message("DONE. See outputs/ for HTML tables, TIFF figures, and checks.")
############################################
# 00_run_all.R — Full reproducible pipeline
# Project: VAP Paper 1
# Outputs: tables -> HTML, figures -> TIFF
############################################

options(stringsAsFactors = FALSE)
set.seed(123)

# =========================
# 0) Packages
# =========================
pkgs <- c(
  "readxl", "dplyr", "tidyr", "purrr", "tibble", "stringr",
  "ggplot2", "scales", "sandwich", "lmtest", "car",
  "gt", "BayesFactor", "TOSTER", "patchwork"
)

to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(ggplot2)
library(scales)
library(sandwich)
library(lmtest)
library(car)
library(gt)
library(BayesFactor)
library(TOSTER)
library(patchwork)

# =========================
# 1) Folders + session info
# =========================
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/checks", recursive = TRUE, showWarnings = FALSE)

writeLines(capture.output(sessionInfo()), "outputs/checks/sessionInfo.txt")

# =========================
# 2) Load data (GOOD filename)
# =========================
data_path <- "Praca 1 całość.xlsx"

if (!file.exists(data_path)) {
  stop("Nie widzę pliku: ", data_path,
       "\nSprawdź getwd() i czy plik leży w folderze projektu.")
}

df_raw <- readxl::read_excel(data_path)
df <- df_raw

# Sentence columns with spaces -> rename to syntactic names
if (all(c("F0 sentence", "F3 sentence", "F4 sentence") %in% names(df))) {
  df <- df |>
    rename(
      F0_sentence = `F0 sentence`,
      F3_sentence = `F3 sentence`,
      F4_sentence = `F4 sentence`
    )
}

# Required columns
required <- c(
  "mean_F0_6", "mean_F3_6", "mean_F4_6",
  "height", "mass", "BMI", "WHR", "Grip_MAX_both", "age"
)
req_sentence <- c("F0_sentence", "F3_sentence", "F4_sentence")

missing_main <- setdiff(required, names(df))
if (length(missing_main) > 0) stop("Brakuje kolumn: ", paste(missing_main, collapse = ", "))

missing_sent <- setdiff(req_sentence, names(df))
if (length(missing_sent) > 0) warning("Brak kolumn sentence (pominę S1/S2 i wykres sentence): ", paste(missing_sent, collapse = ", "))

# Make numeric
vars_numeric <- intersect(c(required, req_sentence), names(df))
df <- df |>
  mutate(across(all_of(vars_numeric), as.numeric))

writeLines(paste("N rows:", nrow(df)), "outputs/checks/N_rows.txt")

# =========================
# 3) Helpers
# =========================
cor_ci <- function(x, y) {
  out <- cor.test(x, y, method = "pearson")
  tibble(
    r = unname(out$estimate),
    ci_low = unname(out$conf.int[1]),
    ci_high = unname(out$conf.int[2]),
    p = out$p.value
  )
}

rp_label <- function(x, y, digits_r = 2) {
  ct <- cor.test(x, y)
  r <- unname(ct$estimate)
  p <- ct$p.value
  p_txt <- if (p < .001) "p < .001" else paste0("p = ", format.pval(p, digits = 2, eps = .001))
  paste0("r = ", sprintf(paste0("%.", digits_r, "f"), r), "\n", p_txt)
}

AICc <- function(fit) {
  k <- length(coef(fit))
  n <- nobs(fit)
  AIC(fit) + (2 * k * (k + 1)) / (n - k - 1)
}

get_stats <- function(fit) {
  s <- summary(fit)
  f_stat <- unname(s$fstatistic[1])
  df1 <- unname(s$fstatistic[2])
  df2 <- unname(s$fstatistic[3])
  tibble(
    F = f_stat,
    p = pf(f_stat, df1, df2, lower.tail = FALSE),
    R2 = s$r.squared,
    Adj_R2 = s$adj.r.squared,
    AICc = AICc(fit)
  )
}

hc3_coef_tbl <- function(fit, model_tag) {
  ct <- lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type = "HC3"))
  mat <- as.matrix(ct)
  tibble(
    Predictor = rownames(mat),
    !!paste0("b_", model_tag) := mat[, 1],
    !!paste0("SE_", model_tag) := mat[, 2],
    !!paste0("p_", model_tag) := mat[, 4]
  )
}

pretty_coeff_table <- function(tbl_wide, pred_order, pred_labels) {
  out <- tbl_wide |>
    pivot_longer(cols = -Predictor, names_to = c(".value", "Model"), names_sep = "_") |>
    mutate(
      b_se = if_else(is.na(b) | is.na(SE), NA_character_, sprintf("%.2f (%.2f)", b, SE)),
      p_fmt = if_else(is.na(p), NA_character_, sprintf("%.3f", p))
    ) |>
    select(Predictor, Model, b_se, p_fmt) |>
    pivot_wider(names_from = Model, values_from = c(b_se, p_fmt), names_glue = "{Model}_{.value}") |>
    mutate(Predictor = factor(Predictor, levels = pred_order)) |>
    arrange(Predictor) |>
    mutate(Predictor = recode(as.character(Predictor), !!!pred_labels))
  
  out[is.na(out)] <- ""
  out
}

gt_save_html <- function(x, path, title = NULL) {
  g <- gt(x)
  if (!is.null(title)) g <- g |> tab_header(title = title)
  gtsave(g, path)
}

# =========================
# 4) Table 1: descriptives + 95% CI
# =========================
desc_vars <- intersect(
  c("mean_F0_6", "mean_F3_6", "mean_F4_6", "F0_sentence", "F3_sentence", "F4_sentence",
    "height", "mass", "BMI", "WHR", "Grip_MAX_both", "age"),
  names(df)
)

desc_tbl <- df |>
  summarise(across(all_of(desc_vars),
                   list(
                     mean = ~mean(.x, na.rm = TRUE),
                     sd   = ~sd(.x, na.rm = TRUE),
                     n    = ~sum(!is.na(.x)),
                     ci_low  = ~mean(.x, na.rm = TRUE) - qt(.975, df = sum(!is.na(.x)) - 1) * (sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))),
                     ci_high = ~mean(.x, na.rm = TRUE) + qt(.975, df = sum(!is.na(.x)) - 1) * (sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))
                   ),
                   .names = "{.col}__{.fn}")) |>
  pivot_longer(everything(), names_to = c("Variable", "Stat"), names_sep = "__") |>
  pivot_wider(names_from = Stat, values_from = value) |>
  select(Variable, n, mean, sd, ci_low, ci_high) |>
  mutate(across(c(mean, sd, ci_low, ci_high), ~round(.x, 3)))

gt_save_html(desc_tbl, "outputs/tables/Table1_descriptives.html",
             title = "Table 1. Descriptive statistics with 95% CI")

# =========================
# 5) Table 2 + validation figure
# =========================
if (all(c("F0_sentence", "F3_sentence", "F4_sentence") %in% names(df))) {
  
  val_tbl <- tibble(
    Pair = c("F3 vowels vs sentence", "F4 vowels vs sentence", "F0 vowels vs sentence"),
    res = list(
      cor_ci(df$mean_F3_6, df$F3_sentence),
      cor_ci(df$mean_F4_6, df$F4_sentence),
      cor_ci(df$mean_F0_6, df$F0_sentence)
    )
  ) |>
    unnest(res) |>
    mutate(
      r = round(r, 3),
      ci_low = round(ci_low, 3),
      ci_high = round(ci_high, 3),
      p = ifelse(p < .001, "< .001", sprintf("%.3f", p))
    )
  
  gt_save_html(val_tbl, "outputs/tables/Table2_validation_vowels_sentence.html",
               title = "Table 2. Validation correlations between vowels and sentence measures")
  
  p_f3 <- ggplot(df, aes(x = mean_F3_6, y = F3_sentence)) +
    geom_point(alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(x = "Mean F3 (vowels, Hz)", y = "F3 (sentence, Hz)", title = "A. F3") +
    annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2,
             label = rp_label(df$mean_F3_6, df$F3_sentence)) +
    theme_classic(base_size = 14)
  
  p_f4 <- ggplot(df, aes(x = mean_F4_6, y = F4_sentence)) +
    geom_point(alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(x = "Mean F4 (vowels, Hz)", y = "F4 (sentence, Hz)", title = "B. F4") +
    annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2,
             label = rp_label(df$mean_F4_6, df$F4_sentence)) +
    theme_classic(base_size = 14)
  
  p_validation <- p_f3 + p_f4
  
  ggsave(
    "outputs/figures/Figure_validation_vowels_sentence.tiff",
    p_validation,
    device = "tiff",
    width = 180, height = 90, units = "mm",
    dpi = 600, compression = "lzw"
  )
}

# =========================
# 6) Forest plots: vowels + sentence
# =========================
preds <- c("height", "Grip_MAX_both", "BMI", "WHR")
pred_labels <- c(height = "Height", Grip_MAX_both = "Grip", BMI = "BMI", WHR = "WHR")

# Figure 1: vowels
dvs_vowels <- c("mean_F3_6", "mean_F4_6")
plot_df_vowels <- expand_grid(dv = dvs_vowels, predictor = preds) |>
  mutate(res = map2(dv, predictor, \(dv, pr) cor_ci(df[[dv]], df[[pr]]))) |>
  unnest(res) |>
  mutate(
    dv = recode(dv, mean_F3_6 = "F3", mean_F4_6 = "F4"),
    predictor = recode(predictor, !!!pred_labels),
    predictor = factor(predictor, levels = c("WHR", "BMI", "Grip", "Height"))
  )

p_vowels <- ggplot(plot_df_vowels, aes(x = r, y = predictor)) +
  geom_vline(xintercept = 0, linewidth = 1) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high),
                orientation = "y", width = 0.25, linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~ dv, ncol = 1) +
  scale_x_continuous(
    limits = c(-0.6, 0.6),
    breaks = seq(-0.6, 0.6, 0.2),
    labels = number_format(accuracy = 0.1)
  ) +
  labs(x = "r", y = "Predictor") +
  theme_classic(base_size = 14) +
  theme(strip.background = element_rect(fill = "white", colour = "black"),
        strip.text = element_text(face = "bold"))

ggsave(
  "outputs/figures/Figure_1_forest_vowels.tiff",
  p_vowels,
  device = "tiff",
  width = 120, height = 120, units = "mm",
  dpi = 600, compression = "lzw"
)

# Figure S: sentence
if (all(c("F3_sentence", "F4_sentence") %in% names(df))) {
  dvs_sent <- c("F3_sentence", "F4_sentence")
  
  plot_df_sent <- expand_grid(dv = dvs_sent, predictor = preds) |>
    mutate(res = map2(dv, predictor, \(dv, pr) cor_ci(df[[dv]], df[[pr]]))) |>
    unnest(res) |>
    mutate(
      dv = recode(dv, F3_sentence = "F3 (sentence)", F4_sentence = "F4 (sentence)"),
      predictor = recode(predictor, !!!pred_labels),
      predictor = factor(predictor, levels = c("WHR", "BMI", "Grip", "Height"))
    )
  
  p_sent <- ggplot(plot_df_sent, aes(x = r, y = predictor)) +
    geom_vline(xintercept = 0, linewidth = 1) +
    geom_errorbar(aes(xmin = ci_low, xmax = ci_high),
                  orientation = "y", width = 0.25, linewidth = 1) +
    geom_point(size = 3) +
    facet_wrap(~ dv, ncol = 1) +
    scale_x_continuous(
      limits = c(-0.6, 0.6),
      breaks = seq(-0.6, 0.6, 0.2),
      labels = number_format(accuracy = 0.1)
    ) +
    labs(x = "r", y = "Predictor") +
    theme_classic(base_size = 14) +
    theme(strip.background = element_rect(fill = "white", colour = "black"),
          strip.text = element_text(face = "bold"))
  
  ggsave(
    "outputs/figures/Figure_S_forest_sentence.tiff",
    p_sent,
    device = "tiff",
    width = 120, height = 120, units = "mm",
    dpi = 600, compression = "lzw"
  )
}

# =========================
# 7) Hierarchical regressions (vowels): Table 4/5 HTML
# =========================
pred_order_vowels <- c("(Intercept)", "mean_F0_6", "age", "height", "BMI", "WHR", "Grip_MAX_both")
pred_labels_vowels <- c(
  "(Intercept)" = "Intercept",
  "mean_F0_6" = "Mean F0 (vowels)",
  "age" = "Age",
  "height" = "Height",
  "BMI" = "BMI",
  "WHR" = "WHR",
  "Grip_MAX_both" = "Grip strength"
)

# Table 4 (F3 vowels)
m0_f3 <- lm(mean_F3_6 ~ mean_F0_6 + age, data = df)
m1_f3 <- lm(mean_F3_6 ~ mean_F0_6 + age + height + BMI, data = df)
m2_f3 <- lm(mean_F3_6 ~ mean_F0_6 + age + height + BMI + WHR, data = df)
m3_f3 <- lm(mean_F3_6 ~ mean_F0_6 + age + height + BMI + WHR + Grip_MAX_both, data = df)

stats_f3 <- bind_rows(
  get_stats(m0_f3) |> mutate(Model = "Model 0", .before = 1),
  get_stats(m1_f3) |> mutate(Model = "Model 1", .before = 1),
  get_stats(m2_f3) |> mutate(Model = "Model 2", .before = 1),
  get_stats(m3_f3) |> mutate(Model = "Model 3", .before = 1)
) |>
  mutate(dR2 = c(NA, diff(R2)))

table4_raw <- reduce(
  list(
    hc3_coef_tbl(m0_f3, "M0"),
    hc3_coef_tbl(m1_f3, "M1"),
    hc3_coef_tbl(m2_f3, "M2"),
    hc3_coef_tbl(m3_f3, "M3")
  ),
  full_join,
  by = "Predictor"
)

table4_pretty <- pretty_coeff_table(table4_raw, pred_order_vowels, pred_labels_vowels)

gt_save_html(table4_pretty, "outputs/tables/Table4_F3_coefficients.html",
             title = "Table 4. Hierarchical regression predicting mean F3 (vowels), HC3 robust SE, N = 79")
gt_save_html(stats_f3, "outputs/tables/Table4_F3_modelstats.html",
             title = "Table 4 (continued). Model statistics")

# Table 5 (F4 vowels)
m0_f4 <- lm(mean_F4_6 ~ mean_F0_6 + age, data = df)
m1_f4 <- lm(mean_F4_6 ~ mean_F0_6 + age + height + BMI, data = df)
m2_f4 <- lm(mean_F4_6 ~ mean_F0_6 + age + height + BMI + WHR, data = df)
m3_f4 <- lm(mean_F4_6 ~ mean_F0_6 + age + height + BMI + WHR + Grip_MAX_both, data = df)

stats_f4 <- bind_rows(
  get_stats(m0_f4) |> mutate(Model = "Model 0", .before = 1),
  get_stats(m1_f4) |> mutate(Model = "Model 1", .before = 1),
  get_stats(m2_f4) |> mutate(Model = "Model 2", .before = 1),
  get_stats(m3_f4) |> mutate(Model = "Model 3", .before = 1)
) |>
  mutate(dR2 = c(NA, diff(R2)))

table5_raw <- reduce(
  list(
    hc3_coef_tbl(m0_f4, "M0"),
    hc3_coef_tbl(m1_f4, "M1"),
    hc3_coef_tbl(m2_f4, "M2"),
    hc3_coef_tbl(m3_f4, "M3")
  ),
  full_join,
  by = "Predictor"
)

table5_pretty <- pretty_coeff_table(table5_raw, pred_order_vowels, pred_labels_vowels)

gt_save_html(table5_pretty, "outputs/tables/Table5_F4_coefficients.html",
             title = "Table 5. Hierarchical regression predicting mean F4 (vowels), HC3 robust SE, N = 79")
gt_save_html(stats_f4, "outputs/tables/Table5_F4_modelstats.html",
             title = "Table 5 (continued). Model statistics")

# =========================
# 8) Supplement: S1/S2 (sentence) HTML
# =========================
if (all(c("F0_sentence", "F3_sentence", "F4_sentence") %in% names(df))) {
  
  pred_order_sent <- c("(Intercept)", "F0_sentence", "age", "height", "BMI", "WHR", "Grip_MAX_both")
  pred_labels_sent <- c(
    "(Intercept)" = "Intercept",
    "F0_sentence" = "Mean F0 (sentence)",
    "age" = "Age",
    "height" = "Height",
    "BMI" = "BMI",
    "WHR" = "WHR",
    "Grip_MAX_both" = "Grip strength"
  )
  
  # S1: F3 sentence
  m0_s1 <- lm(F3_sentence ~ F0_sentence + age, data = df)
  mF_s1 <- lm(F3_sentence ~ F0_sentence + age + height + BMI + WHR + Grip_MAX_both, data = df)
  
  stats_s1 <- bind_rows(
    get_stats(m0_s1) |> mutate(Model = "Base", .before = 1),
    get_stats(mF_s1) |> mutate(Model = "Full", .before = 1)
  ) |>
    mutate(dR2 = c(NA, R2[2] - R2[1]))
  
  s1_raw <- reduce(list(hc3_coef_tbl(m0_s1, "Base"), hc3_coef_tbl(mF_s1, "Full")),
                   full_join, by = "Predictor")
  s1_pretty <- pretty_coeff_table(s1_raw, pred_order_sent, pred_labels_sent)
  
  gt_save_html(s1_pretty, "outputs/tables/TableS1_F3_sentence_coefficients.html",
               title = "Table S1. Hierarchical regression predicting F3 (sentence), HC3 robust SE, N = 79")
  gt_save_html(stats_s1, "outputs/tables/TableS1_F3_sentence_modelstats.html",
               title = "Table S1 (continued). Model statistics")
  
  # S2: F4 sentence
  m0_s2 <- lm(F4_sentence ~ F0_sentence + age, data = df)
  mF_s2 <- lm(F4_sentence ~ F0_sentence + age + height + BMI + WHR + Grip_MAX_both, data = df)
  
  stats_s2 <- bind_rows(
    get_stats(m0_s2) |> mutate(Model = "Base", .before = 1),
    get_stats(mF_s2) |> mutate(Model = "Full", .before = 1)
  ) |>
    mutate(dR2 = c(NA, R2[2] - R2[1]))
  
  s2_raw <- reduce(list(hc3_coef_tbl(m0_s2, "Base"), hc3_coef_tbl(mF_s2, "Full")),
                   full_join, by = "Predictor")
  s2_pretty <- pretty_coeff_table(s2_raw, pred_order_sent, pred_labels_sent)
  
  gt_save_html(s2_pretty, "outputs/tables/TableS2_F4_sentence_coefficients.html",
               title = "Table S2. Hierarchical regression predicting F4 (sentence), HC3 robust SE, N = 79")
  gt_save_html(stats_s2, "outputs/tables/TableS2_F4_sentence_modelstats.html",
               title = "Table S2 (continued). Model statistics")
}

# =========================
# 9) Diagnostics + notes (files)
# =========================
diag_lines <- c(
  "VIF (F3 full):",
  capture.output(car::vif(m3_f3)),
  "",
  "VIF (F4 full):",
  capture.output(car::vif(m3_f4)),
  "",
  "Shapiro residuals (F3 full):",
  capture.output(shapiro.test(residuals(m3_f3))),
  "",
  "Shapiro residuals (F4 full):",
  capture.output(shapiro.test(residuals(m3_f4)))
)

writeLines(diag_lines, "outputs/checks/diagnostics.txt")

# =========================
# 10) TOST + BayesFactor (key correlations)
# =========================
eq_bound <- 0.36
key_pairs <- expand_grid(dv = c("mean_F3_6", "mean_F4_6"), predictor = preds)

tost_out <- key_pairs |>
  mutate(
    r = map2_dbl(dv, predictor, \(dv, pr) unname(cor.test(df[[dv]], df[[pr]])$estimate)),
    n = nrow(df),
    tost = pmap(
      list(r = r, n = n),
      \(r, n) TOSTER::TOSTcorrelations(r = r, n = n,
                                       low_eqbound_r = -eq_bound,
                                       high_eqbound_r = eq_bound)
    ),
    p_tost = map_dbl(tost, \(x) x$TOST$p.value),
    eq = p_tost < 0.05
  ) |>
  select(dv, predictor, r, p_tost, eq)

write.csv(tost_out, "outputs/checks/TOST_key_pairs.csv", row.names = FALSE)

bf_out <- key_pairs |>
  mutate(
    bf10 = map2_dbl(dv, predictor, \(dv, pr) as.numeric(BayesFactor::correlationBF(df[[dv]], df[[pr]]))),
    bf01 = 1 / bf10
  ) |>
  select(dv, predictor, bf10, bf01)

write.csv(bf_out, "outputs/checks/BayesFactor_key_pairs.csv", row.names = FALSE)

writeLines(
  c("Power note:",
    "For N = 79, 80% power corresponds approximately to |r| >= 0.31 (two-tailed alpha = .05)."),
  "outputs/checks/power_note.txt"
)

# =========================
# 11) Manifest
# =========================
manifest <- tibble(path = list.files("outputs", recursive = TRUE, full.names = TRUE)) |>
  mutate(file = basename(path))
write.csv(manifest, "outputs/manifest.csv", row.names = FALSE)

message("DONE ✅  Sprawdź outputs/tables (HTML) i outputs/figures (TIFF).")
