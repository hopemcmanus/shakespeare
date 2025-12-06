library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Directory with token CSVs
dir_path <- "data/processed/tokens/"

# Output directory for metadata
meta_dir <- "data/metadata"
if(!dir.exists(meta_dir)) dir.create(meta_dir)

# List CSVs, excluding "all_shakespeare_tokens.csv"
files <- list.files(dir_path, pattern = "_tokens.csv$", full.names = TRUE)
files <- files[!grepl("all_shakespeare_tokens.csv", files)]

all_chars <- list()

for(f in files){
  play_name <- tools::file_path_sans_ext(basename(f))
  cat("Processing characters for:", play_name, "\n")
  
  # Read the play CSV
  play <- read_csv(f, show_col_types = FALSE)
  
  # Remove collective entries starting with "all" or "both"
  play <- play %>% filter(!str_detect(tolower(character), "^(all|both)"))
  
  # First split on commas
  play <- play %>%
    mutate(character = str_split(character, ",")) %>%
    unnest(character)
  
  # Then split on " and " (case-insensitive)
  play <- play %>%
    mutate(character = str_split(character, regex("\\s+and\\s+", ignore_case = TRUE))) %>%
    unnest(character) %>%
    mutate(character = str_trim(character))
  
  # Remove empty or NA characters
  play <- play %>% filter(!is.na(character) & character != "")
  
  # Collect unique characters with play name
  chars <- play %>%
    distinct(character) %>%
    arrange(character) %>%
    mutate(play = play_name)
  
  all_chars[[play_name]] <- chars
}

# Combine all into one data frame
all_chars_df <- bind_rows(all_chars) %>%
  select(play, character)

# Save safely to CSV
write_csv(all_chars_df, file.path(meta_dir, "all_characters.csv"))
