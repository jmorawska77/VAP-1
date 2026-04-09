# scripts/13-export-tables-word.R

message("Exporting tables to Word (.docx)...")

dir.create("outputs/tables_word", recursive = TRUE, showWarnings = FALSE)

html_files <- list.files("outputs/tables", pattern = "\\.html$", full.names = TRUE)

if (length(html_files) == 0) {
  stop("No HTML tables found in outputs/tables/. Run the pipeline first.")
}

# helper: read first <table> from HTML and convert to data.frame
read_html_table <- function(path) {
  doc <- xml2::read_html(path)
  tabs <- rvest::html_table(doc, fill = TRUE)
  
  if (length(tabs) == 0) {
    stop("No <table> found in: ", path)
  }
  
  tabs[[1]]
}

for (f in html_files) {
  tab <- read_html_table(f)
  
  # make a Word doc with a title + table
  title <- tools::file_path_sans_ext(basename(f))
  
  ft <- flextable::flextable(tab)
  ft <- flextable::autofit(ft)
  
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  doc <- flextable::body_add_flextable(doc, ft)
 
  
  out <- file.path("outputs/tables_word", paste0(title, ".docx"))
  print(doc, target = out)
}

message("Word exports saved to outputs/tables_word/.")