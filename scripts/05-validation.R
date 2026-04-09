# scripts/05-validation.R

message("Creating Table 2 + validation figure...")

df <- readRDS("outputs/analysis_df.rds")

# Pearson correlations vowels vs sentence
cor_one <- function(x, y) {
  ct <- stats::cor.test(x, y, method = "pearson")
  tibble::tibble(
    r = unname(ct$estimate),
    p = ct$p.value,
    ci_low = unname(ct$conf.int[1]),
    ci_high = unname(ct$conf.int[2])
  )
}

tab2 <- tibble::tibble(
  Measure = c("F0", "F3", "F4"),
  x = c("mean_F0_6", "mean_F3_6", "mean_F4_6"),
  y = c("F0_sentence", "F3_sentence", "F4_sentence")
) |>
  dplyr::rowwise() |>
  dplyr::mutate(res = list(cor_one(df[[x]], df[[y]]))) |>
  tidyr::unnest(res) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    r = round(r, 3),
    p = ifelse(p < .001, "< .001", sprintf("%.3f", p)),
    ci = paste0("[", round(ci_low, 3), ", ", round(ci_high, 3), "]")
  ) |>
  dplyr::select(Measure, r, ci, p)

gt_tbl <- gt::gt(tab2) |>
  gt::tab_header(title = "Table 2. Validation: vowels vs sentence correlations") |>
  gt::cols_label(
    Measure = "Measure",
    r = "r",
    ci = "95% CI",
    p = "p"
  )

gt::gtsave(gt_tbl, "outputs/tables/Table2_validation.html")

# 2-panel figure (F3, F4) - scatter with lm line
p_f3 <- ggplot2::ggplot(df, ggplot2::aes(x = mean_F3_6, y = F3_sentence)) +
  ggplot2::geom_point(alpha = 0.75) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, color = "black") +
  ggplot2::labs(x = "Mean F3 (vowels)", y = "F3 (sentence)") +
  ggplot2::theme_classic(base_size = 12)

p_f4 <- ggplot2::ggplot(df, ggplot2::aes(x = mean_F4_6, y = F4_sentence)) +
  ggplot2::geom_point(alpha = 0.75) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, color = "black") +
  ggplot2::labs(x = "Mean F4 (vowels)", y = "F4 (sentence)") +
  ggplot2::theme_classic(base_size = 12)

p <- patchwork::wrap_plots(p_f3, p_f4, ncol = 2)

ggplot2::ggsave(
  filename = "outputs/figures/Figure_validation_F3_F4.tiff",
  plot = p,
  device = ragg::agg_tiff,
  dpi = 600,
  compression = "lzw",
  width = 7.5,
  height = 3.6,
  units = "in"
)

message("Table 2 + validation figure saved.")