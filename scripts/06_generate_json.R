library(tidyverse)
library(jsonlite)
library(here)

# Configuration
INPUT_DIR_CLEANED <- here("data", "cleaned")
INPUT_DIR_METADATA <- here("data", "metadata")
OUTPUT_DIR_JSON_PLAYS <- here("data", "json", "full_text")
OUTPUT_DIR_JSON_META <- INPUT_DIR_METADATA  # Keep metadata JSON with CSV metadata

# Create output directories
dir.create(OUTPUT_DIR_JSON_PLAYS, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_DIR_JSON_META, recursive = TRUE, showWarnings = FALSE)

message("\n", strrep("=", 70))
message("SHAKESPEARE JSON EXPORT")
message(strrep("=", 70))
message("Input CSV: ", INPUT_DIR_CLEANED)
message("Output JSON: ", dirname(OUTPUT_DIR_JSON_PLAYS))
message(strrep("=", 70), "\n")

# Export individual play JSON files
message("Exporting individual play JSON files...")

# List all play CSV files (exclude combined file)
csv_files <- list.files(
  INPUT_DIR_CLEANED, 
  pattern = "\\.csv$", 
  full.names = TRUE
) %>%
  setdiff(file.path(INPUT_DIR_CLEANED, "all_shakespeare.csv"))  # Exclude combined file

message("Found ", length(csv_files), " play files to convert\n")

export_count <- 0

for (csv_file in csv_files) {
  # Read play CSV
  df <- read_csv(csv_file, show_col_types = FALSE)
  
  # Generate JSON filename from short_title
  if ("short_title" %in% colnames(df) && !is.na(unique(df$short_title)[1])) {
    short <- unique(df$short_title)[1]
    json_name <- short %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_remove("^_|_$") %>%
      paste0(".json")
  } else {
    # Fallback to CSV filename
    json_name <- tools::file_path_sans_ext(basename(csv_file)) %>%
      paste0(".json")
  }
  
  # Write JSON
  output_path <- file.path(OUTPUT_DIR_JSON_PLAYS, json_name)
  write_json(df, output_path, pretty = TRUE, auto_unbox = TRUE)
  
  export_count <- export_count + 1
  if (export_count %% 10 == 0) {
    message("  Exported ", export_count, "/", length(csv_files), " plays...")
  }
}

message("✓ Exported ", export_count, " play JSON files\n")

# Export combined JSON (all plays)
message("Exporting combined JSON (all plays)...")

all_shakespeare <- read_csv(
  file.path(INPUT_DIR_CLEANED, "all_shakespeare.csv"),
  show_col_types = FALSE
)

combined_json <- file.path(dirname(OUTPUT_DIR_JSON_PLAYS), "all_shakespeare.json")
write_json(all_shakespeare, combined_json, pretty = TRUE, auto_unbox = TRUE)

message("✓ Exported combined JSON: ", basename(combined_json))
message("  Size: ", format(file.size(combined_json) / 1024^2, digits = 2), " MB\n")

# Export metadata JSON
message("Exporting metadata JSON...")

# Main metadata
meta_shakespeare <- read_csv(
  file.path(INPUT_DIR_METADATA, "meta_shakespeare.csv"),
  show_col_types = FALSE
)

meta_json <- file.path(OUTPUT_DIR_JSON_META, "meta_shakespeare.json")
write_json(meta_shakespeare, meta_json, pretty = TRUE, auto_unbox = TRUE)

message("✓ Exported metadata JSON: ", basename(meta_json))

# Export logs as JSON
message("Exporting log files as JSON...")

log_files <- c(
  "download_log_latest.csv",
  "cleaning_log_latest.csv",
  "tokenisation_log.csv",
  "filter_log.csv"
)

for (log_file in log_files) {
  csv_path <- file.path(INPUT_DIR_METADATA, log_file)
  
  if (file.exists(csv_path)) {
    log_data <- read_csv(csv_path, show_col_types = FALSE)
    
    json_name <- str_replace(log_file, "\\.csv$", ".json")
    json_path <- file.path(OUTPUT_DIR_JSON_META, json_name)
    
    write_json(log_data, json_path, pretty = TRUE, auto_unbox = TRUE)
    message("  ✓ ", json_name)
  }
}

message("")

# Create JSON index file
message("Creating JSON index file...")

# Create an index of all available JSON files
json_index <- tibble(
  play_title = meta_shakespeare$short_title,
  gutenberg_id = meta_shakespeare$gutenberg_id,
  genre = meta_shakespeare$genre,
  year = meta_shakespeare$year,
  period = if("period" %in% names(meta_shakespeare)) meta_shakespeare$period else NA_character_,
  json_file = paste0(
    str_to_lower(play_title) %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_remove("^_|_$"),
    ".json"
  ),
  json_path = file.path("full_text", json_file)
)

index_json <- file.path(OUTPUT_DIR_JSON_META, "plays_index.json")
write_json(json_index, index_json, pretty = TRUE, auto_unbox = TRUE)

message("✓ Exported plays index: ", basename(index_json), "\n")

# Summary report
message(strrep("=", 70))
message("JSON EXPORT SUMMARY")
message(strrep("=", 70))
message("Individual plays exported: ", export_count)
message("Combined file exported:     all_shakespeare.json")
message("Metadata exported:          meta_shakespeare.json")
message("Index file created:         plays_index.json")
message(strrep("=", 70))
message("JSON files location:")
message("  Plays:    ", OUTPUT_DIR_JSON_PLAYS)
message("  Metadata: ", OUTPUT_DIR_JSON_META)
message(strrep("=", 70), "\n")

# Display directory structure
message("Directory structure:")
message("data/json/")
message("├── full_text/")
message("│   ├── hamlet.json")
message("│   ├── othello.json")
message("│   └── ... (", export_count, " play files)")
message("├── metadata/")
message("│   ├── meta_shakespeare.json")
message("│   ├── plays_index.json")
message("│   └── ... (log files)")
message("└── all_shakespeare.json")
message("")

message("✓ JSON export complete!\n")

# Display sample from index
message("Sample from plays_index.json:")
print(json_index %>% head(10))