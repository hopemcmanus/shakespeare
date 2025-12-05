library(tidyverse)
library(text2map)
library(here)

# Configuration
OUTPUT_DIR_METADATA <- here("data", "metadata")

# Create directory
dir.create(OUTPUT_DIR_METADATA, recursive = TRUE, showWarnings = FALSE)

message("\n", strrep("=", 70))
message("SHAKESPEARE METADATA SETUP")
message(strrep("=", 70), "\n")

# Load original metadata from text2map package
message("Loading metadata from text2map package...")
data(meta_shakespeare)
message("✓ Loaded ", nrow(meta_shakespeare), " plays\n")

# Add custom columns
message("Adding custom classification columns...")

meta_shakespeare <- meta_shakespeare %>%
  mutate(
    # Period: Elizabethan (pre-1603) vs Jacobean (1603+)
    # Elizabeth I died March 1603, James I became king
    period = case_when(
      year < 1603 ~ "Elizabethan",
      year >= 1603 ~ "Jacobean",
      TRUE ~ NA_character_
    ),
    
    # Romance (Late Romances / Tragicomedies)
    # Written late in career, mix tragic/comic, feature reconciliation
    romance = short_title %in% c("Tempest", "Winter's Tale", "Cymbeline", "Pericles"),
    
    # Problem Play
    # Morally ambiguous, don't fit neatly into comedy/tragedy
    problem_play = short_title %in% c("Measure for Measure", 
                                      "All's Well That Ends Well", 
                                      "Troilus and Cressida"),
    
    # Roman Play
    # Set in Ancient Rome, based on Plutarch and classical sources
    roman = short_title %in% c("Julius Caesar", 
                               "Antony and Cleopatra", 
                               "Coriolanus", 
                               "Titus Andronicus")
  )

message("✓ Added columns: period, romance, problem_play, roman\n")

# Display summary
message("Classification summary:")
message(strrep("-", 70))

period_summary <- meta_shakespeare %>%
  count(period, name = "plays") %>%
  arrange(period)
message("\nBy Period:")
print(period_summary)

romance_summary <- meta_shakespeare %>%
  filter(romance) %>%
  select(short_title, year, genre)
message("\nRomances (", nrow(romance_summary), " plays):")
print(romance_summary)

problem_summary <- meta_shakespeare %>%
  filter(problem_play) %>%
  select(short_title, year, genre)
message("\nProblem Plays (", nrow(problem_summary), " plays):")
print(problem_summary)

roman_summary <- meta_shakespeare %>%
  filter(roman) %>%
  select(short_title, year, genre)
message("\nRoman Plays (", nrow(roman_summary), " plays):")
print(roman_summary)

# Save updated metadata
output_file <- file.path(OUTPUT_DIR_METADATA, "meta_shakespeare.csv")
write_csv(meta_shakespeare, output_file)

message("\n", strrep("=", 70))
message("✓ Saved updated metadata: ", output_file)
message(strrep("=", 70), "\n")

# Display sample with new columns
message("Sample with new columns:")
meta_shakespeare %>%
  select(short_title, year, genre, period, romance, problem_play, roman) %>%
  arrange(year) %>%
  head(20) %>%
  print()

message("\n✓ Metadata setup complete!\n")
message("Run this script first, then proceed with 01_download_gutenberg.R\n")