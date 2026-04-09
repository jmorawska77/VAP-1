# scripts/10-manifest.R

message("Writing manifest...")

files <- list.files("outputs", recursive = TRUE, full.names = TRUE)

# nie hashujemy samego manifestu (żeby nie było pętli)
files <- setdiff(
  files,
  c(
    "outputs/manifest.json",
    "outputs/manifest_golden.json"
  )
)

dir.create("outputs", showWarnings = FALSE)

manifest <- tibble::tibble(
  path = files,
  size = file.info(files)$size,
  sha256 = vapply(files, digest::digest, character(1), algo = "sha256", file = TRUE)
)

jsonlite::write_json(manifest, "outputs/manifest.json", pretty = TRUE, auto_unbox = TRUE)

# jeśli nie ma golden, tworzymy go raz
if (!file.exists("outputs/manifest_golden.json")) {
  file.copy("outputs/manifest.json", "outputs/manifest_golden.json")
}

message("Manifest written.")