library(tidyverse)
library(tidytext)
library(here)

# Configuration
INPUT_DIR_CLEANED <- here("data", "cleaned")
OUTPUT_DIR_TOKENS <- here("data", "processed", "tokens")
INPUT_DIR_METADATA <- here("data", "metadata")

# Create output directory
dir.create(OUTPUT_DIR_TOKENS, recursive = TRUE, showWarnings = FALSE)

message("\n", strrep("=", 70))
message("SHAKESPEARE TEXT TOKENISATION")
message(strrep("=", 70))
message("Input: ", INPUT_DIR_CLEANED)
message("Output: ", OUTPUT_DIR_TOKENS)
message(strrep("=", 70), "\n")

# Load cleaned data
message("Loading cleaned Shakespeare data...")
all_shakespeare <- read_csv(
  file.path(INPUT_DIR_CLEANED, "all_shakespeare.csv"),
  show_col_types = FALSE
)

message("✓ Loaded ", nrow(all_shakespeare), " rows from ", 
        length(unique(all_shakespeare$short_title)), " plays\n")

# Tokenise all text
message("Tokenising all text (dialogue + directions + references)...")

shakespeare_tokens <- all_shakespeare %>%
  # Tokenise: split text into individual words
  unnest_tokens(word, text) %>%
  # Select relevant columns (tokens only, no original text)
  select(
    gutenberg_id,
    short_title,
    gutenberg_title,
    genre,
    year,
    author,
    section,
    class,
    act,
    scene,
    character,
    line_number,
    word
  )

message("✓ Created ", format(nrow(shakespeare_tokens), big.mark = ","), " tokens\n")

# Summary statistics
token_summary <- shakespeare_tokens %>%
  group_by(short_title, class) %>%
  summarise(
    tokens = n(),
    unique_words = n_distinct(word),
    .groups = "drop"
  )

message("Token summary by play and class:")
print(token_summary %>% arrange(short_title, class), n = 20)
message("")

# Save combined tokens
combined_file <- file.path(OUTPUT_DIR_TOKENS, "all_shakespeare_tokens.csv")
write_csv(shakespeare_tokens, combined_file)
message("✓ Saved combined tokens: ", basename(combined_file))
message("  Total tokens: ", format(nrow(shakespeare_tokens), big.mark = ","))
message("  Unique words: ", format(n_distinct(shakespeare_tokens$word), big.mark = ","))
message("")

# Save individual play tokens
message("Saving individual play token files...")

play_list <- unique(shakespeare_tokens$short_title)
saved_count <- 0

for (play in play_list) {
  play_tokens <- shakespeare_tokens %>%
    filter(short_title == play)
  
  # Generate filename from play title
  filename <- play %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_remove("^_|_$") %>%
    paste0("_tokens.csv")
  
  output_path <- file.path(OUTPUT_DIR_TOKENS, filename)
  write_csv(play_tokens, output_path)
  
  saved_count <- saved_count + 1
  if (saved_count %% 10 == 0) {
    message("  Saved ", saved_count, "/", length(play_list), " plays...")
  }
}

message("✓ Saved ", saved_count, " individual play token files\n")

# Create tokenisation log
log_data <- shakespeare_tokens %>%
  group_by(short_title, genre, year) %>%
  summarise(
    total_tokens = n(),
    dialogue_tokens = sum(class == "dialogue"),
    direction_tokens = sum(class == "directions"),
    reference_tokens = sum(class == "reference"),
    unique_words = n_distinct(word),
    unique_characters = n_distinct(character[!is.na(character)]),
    .groups = "drop"
  ) %>%
  arrange(desc(total_tokens))

log_file <- file.path(INPUT_DIR_METADATA, "tokenisation_log.csv")
write_csv(log_data, log_file)

message("✓ Saved tokenisation log: ", basename(log_file), "\n")

# Summary report
message(strrep("=", 70))
message("TOKENISATION SUMMARY")
message(strrep("=", 70))
message("Total plays processed:    ", length(play_list))
message("Total tokens created:     ", format(nrow(shakespeare_tokens), big.mark = ","))
message("Unique words:             ", format(n_distinct(shakespeare_tokens$word), big.mark = ","))
message("Dialogue tokens:          ", format(sum(shakespeare_tokens$class == "dialogue"), big.mark = ","))
message("Direction tokens:         ", format(sum(shakespeare_tokens$class == "directions"), big.mark = ","))
message("Reference tokens:         ", format(sum(shakespeare_tokens$class == "reference"), big.mark = ","))
message(strrep("=", 70))
message("Files saved to: ", OUTPUT_DIR_TOKENS)
message(strrep("=", 70), "\n")

message("✓ Tokenisation complete!\n")

# Display sample
message("Sample tokens (first 20 rows):")
print(shakespeare_tokens %>% head(20))