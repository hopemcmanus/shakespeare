# R script to generate per-play HTML pages from template
library(readr)
library(stringr)

# Paths
template_file <- "templates/play_template.html"  # template above
plays_dir <- "plays"                             # output folder
meta_csv <- "data/metadata/meta_shakespeare.csv"

# Create output folder if it doesn't exist
dir.create(plays_dir, recursive = TRUE, showWarnings = FALSE)

# Read metadata
meta <- read_csv(meta_csv, show_col_types = FALSE)

for (short_title in meta$short_title) {
  # Generate filename
  file_name <- str_to_lower(short_title) %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
  html_file <- file.path(plays_dir, paste0(file_name, ".html"))
  
  # Read template
  template <- readLines(template_file)
  
  # Replace placeholder title
  template <- gsub("PLAY TITLE", short_title, template)
  
  # Write HTML page
  writeLines(template, html_file)
  message("Created: ", html_file)
}
