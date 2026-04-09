# scripts/03-derive-variables.R

message("Deriving analysis variables...")

df_raw <- readRDS("outputs/raw_df.rds")

vowels <- c("A", "O", "E", "U", "I", "Y")

f0_cols <- paste0("F0_", vowels)
f3_cols <- paste0("F3_", vowels)
f4_cols <- paste0("F4_", vowels)

needed <- c(
  f0_cols, f3_cols, f4_cols,
  "F0 sentence", "F3 sentence", "F4 sentence",
  "Grip Right MAX", "Grip Left Max"
)

missing <- setdiff(needed, names(df_raw))
if (length(missing) > 0) {
  stop("Missing columns in Excel: ", paste(missing, collapse = ", "))
}

df <- df_raw |>
  dplyr::mutate(
    age = as.numeric(df_raw[[2]]),
    
    mean_F0_6 = rowMeans(dplyr::pick(dplyr::all_of(f0_cols)), na.rm = TRUE),
    mean_F3_6 = rowMeans(dplyr::pick(dplyr::all_of(f3_cols)), na.rm = TRUE),
    mean_F4_6 = rowMeans(dplyr::pick(dplyr::all_of(f4_cols)), na.rm = TRUE),
    
    F0_sentence = .data[["F0 sentence"]],
    F3_sentence = .data[["F3 sentence"]],
    F4_sentence = .data[["F4 sentence"]],
    
    Grip_MAX_both = rowMeans(
      cbind(.data[["Grip Right MAX"]], .data[["Grip Left Max"]]),
      na.rm = TRUE
    )
  )

saveRDS(df, "outputs/analysis_df.rds")

message("Analysis dataset saved.")