# scripts/07-correlations-forest-sentence.R

message("Creating Table S (sentence correlations) + Figure S...")

df <- readRDS("outputs/analysis_df.rds")

outcomes <- c("F3_sentence", "F4_sentence")
predictors <- c("height", "mass", "BMI", "WHR", "Grip_MAX_both", "age")

cor_one <- function(x, y) {
  ct <- stats::cor.test(x, y, method = "pearson")
  tibble::tibble(
    r = unname(ct$estimate),
    p = ct$p.value,
    ci_low = unname(ct$conf.int[1]),
    ci_high = unname(ct$conf.int[2])
  )
}

tab_s <- tidyr::expand_grid(
  outcome = outcomes,
  predictor = predictors
) |>
  dplyr::rowwise() |>
  dplyr::mutate(res = list(cor_one(df[[outcome]], df[[predictor]]))) |>
  tidyr::unnest(res) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    r_round = round(r, 3),
    p_fmt = ifelse(p < .001, "< .001", sprintf("%.3f", p)),
    ci = paste0("[", round(ci_low, 3), ", ", round(ci_high, 3), "]")
  )

# Tabela (Supplement) jako HTML
tab_s_wide <- tab_s |>
  dplyr::mutate(outcome = dplyr::recode(
    outcome,
    F3_sentence = "F3 (sentence)",
    F4_sentence = "F4 (sentence)"
  )) |>
  dplyr::select(outcome, predictor, r_round, ci, p_fmt) |>
  tidyr::pivot_wider(
    names_from = outcome,
    values_from = c(r_round, ci, p_fmt),
    names_sep = " "
  )

gt_tbl <- gt::gt(tab_s_wide) |>
  gt::tab_header(title = "Table S. Correlations of F3/F4 (sentence) with anthropometry") |>
  gt::cols_label(predictor = "Predictor")

gt::gtsave(gt_tbl, "outputs/tables/TableS_correlations_sentence.html")

# Forest plot (Figure S)
plot_df <- tab_s |>
  dplyr::mutate(
    outcome = dplyr::recode(
      outcome,
      F3_sentence = "F3 (sentence)",
      F4_sentence = "F4 (sentence)"
    ),
    predictor = factor(predictor, levels = rev(predictors))
  )

p <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = r, y = predictor)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = ci_low, xmax = ci_high),
    height = 0.2
  ) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ outcome, ncol = 2) +
  ggplot2::scale_x_continuous(
    limits = c(-0.6, 0.6),
    breaks = seq(-0.6, 0.6, by = 0.2),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  ggplot2::labs(x = "Pearson r (95% CI)", y = NULL) +
  ggplot2::theme_classic(base_size = 12)

ggplot2::ggsave(
  filename = "outputs/figures/Figure_S_forest_sentence.tiff",
  plot = p,
device = ragg::agg_tiff,
  dpi = 600,
  compression = "lzw",
  width = 7.5,
  height = 4.6,
  units = "in"
)

message("Table S + Figure S saved.")