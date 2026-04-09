# scripts/02-load-data.R

message("Loading raw data...")

df_raw <- readxl::read_excel("Praca 1 całość.xlsx")

saveRDS(df_raw, "outputs/raw_df.rds")

message("Raw data saved.")