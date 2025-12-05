library(tidyverse)
library(here)

# Configuration
INPUT_DIR_CLEANED <- here("data", "cleaned")
OUTPUT_DIR_VALIDATION <- here("data", "validation")

# Create output directory
dir.create(OUTPUT_DIR_VALIDATION, recursive = TRUE, showWarnings = FALSE)

# Load cleaned data
message("Loading cleaned Shakespeare data...")
all_plays <- read_csv(
  file.path(INPUT_DIR_CLEANED, "all_shakespeare.csv"),
  show_col_types = FALSE
)

message("✓ Loaded ", nrow(all_plays), " rows from ", 
        length(unique(all_plays$short_title)), " plays\n")

# Initialise validation report
validation_issues <- list()
issue_count <- 0

add_issue <- function(issue_type, description, sample_data, severity = "WARNING") {
  issue_count <<- issue_count + 1
  
  validation_issues[[issue_count]] <<- list(
    issue_id = issue_count,
    issue_type = issue_type,
    severity = severity,
    description = description,
    count = nrow(sample_data),
    sample_data = sample_data
  )
  
  message(sprintf("[%s] %s: %s (%d cases)", 
                  severity, issue_type, description, nrow(sample_data)))
  
  if (nrow(sample_data) > 0 && nrow(sample_data) <= 5) {
    print(sample_data %>% select(1:min(8, ncol(sample_data))))
  } else if (nrow(sample_data) > 5) {
    message("  Showing first 5 examples:")
    print(sample_data %>% head(5) %>% select(1:min(8, ncol(sample_data))))
  }
  message("")
}

message(strrep("=", 70))
message("VALIDATION CHECKS")
message(strrep("=", 70), "\n")

# Check 1: Dialogue without character assignment
message("Check 1: Dialogue lines without character...")
orphan_dialogue <- all_plays %>%
  filter(class == "dialogue", is.na(character) | character == "") %>%
  select(short_title, act, scene, line_number, text)

add_issue(
  "Orphan Dialogue",
  "Dialogue lines with no character assigned",
  orphan_dialogue,
  severity = "ERROR"
)

# Check 2: Character names with unusual patterns
message("Check 2: Unusual character names...")
unusual_characters <- all_plays %>%
  filter(class == "dialogue", !is.na(character)) %>%
  count(character, sort = TRUE) %>%
  filter(
    # Check for lowercase in character names (except 'and', 'of', 'the')
    str_detect(character, "[a-z]") & 
      !str_detect(character, "^[A-Z\\s']+ (and|of|the|de|von|van) [A-Z\\s']+$")
  ) %>%
  head(20)

add_issue(
  "Unusual Character Names",
  "Character names containing unexpected lowercase letters",
  unusual_characters,
  severity = "WARNING"
)

# Check 3: Stage directions within dialogue text
message("Check 3: Stage directions within dialogue...")
directions_in_dialogue <- all_plays %>%
  filter(class == "dialogue") %>%
  filter(str_detect(text, "\\[.*\\]")) %>%
  select(short_title, character, text) %>%
  head(20)

add_issue(
  "Inline Stage Directions",
  "Dialogue lines containing [bracketed] stage directions",
  directions_in_dialogue,
  severity = "INFO"
)

# Check 4: Multi-line Enter/Exit misclassified as dialogue
message("Check 4: Enter/Exit continuation lines...")
enter_exit_issues <- all_plays %>%
  filter(str_detect(text, "^(Enter|Exit|Exeunt|Re-enter)")) %>%
  mutate(
    next1_class = lead(class, 1),
    next2_class = lead(class, 2),
    next3_class = lead(class, 3),
    next1_text = lead(text, 1),
    next2_text = lead(text, 2),
    next3_text = lead(text, 3),
    next1_char = lead(character, 1),
    next2_char = lead(character, 2)
  ) %>%
  filter(
    (next1_class == "dialogue" & !is.na(next1_char)) | 
      (next2_class == "dialogue" & !is.na(next2_char))
  ) %>%
  select(short_title, text, next1_class, next1_char, next1_text, 
         next2_class, next2_char, next2_text) %>%
  head(20)

add_issue(
  "Enter/Exit Continuation",
  "Lines after Enter/Exit incorrectly classified as dialogue",
  enter_exit_issues,
  severity = "ERROR"
)

# Check 5: Missing act or scene labels in dialogue
message("Check 5: Dialogue with missing act/scene...")
missing_structure <- all_plays %>%
  filter(class == "dialogue", is.na(act) | is.na(scene)) %>%
  select(short_title, act, scene, character, text) %>%
  head(20)

add_issue(
  "Missing Structure",
  "Dialogue lines missing act or scene labels",
  missing_structure,
  severity = "WARNING"
)

# Check 6: Empty or NA text fields
message("Check 6: Empty text fields...")
empty_text <- all_plays %>%
  filter(is.na(text) | text == "") %>%
  select(short_title, section, class, act, scene)

add_issue(
  "Empty Text",
  "Rows with empty or NA text",
  empty_text,
  severity = "ERROR"
)

# Check 7: Line number gaps or duplicates
message("Check 7: Line number continuity...")
line_number_issues <- all_plays %>%
  filter(class == "dialogue", !is.na(line_number)) %>%
  arrange(short_title, line_number) %>%
  group_by(short_title) %>%
  mutate(
    gap = line_number - lag(line_number),
    is_duplicate = gap == 0,
    is_gap = gap > 1 & !is.na(gap)
  ) %>%
  filter(is_duplicate | is_gap) %>%
  ungroup() %>%
  select(short_title, line_number, gap, character, text) %>%
  head(20)

add_issue(
  "Line Number Issues",
  "Line numbers with gaps or duplicates",
  line_number_issues,
  severity = "WARNING"
)

# Check 8: Character names with numbers
message("Check 8: Numbered character names...")
numbered_characters <- all_plays %>%
  filter(class == "dialogue", !is.na(character)) %>%
  filter(str_detect(character, "FIRST|SECOND|THIRD|FOURTH|FIFTH|\\d")) %>%
  count(short_title, character, sort = TRUE) %>%
  head(20)

add_issue(
  "Numbered Characters",
  "Characters with ordinal numbers (FIRST, SECOND, etc.)",
  numbered_characters,
  severity = "INFO"
)

# Check 9: 'ALL' as character
message("Check 9: 'ALL' speaking in unison...")
all_speaking <- all_plays %>%
  filter(character == "ALL") %>%
  select(short_title, act, scene, text) %>%
  head(20)

add_issue(
  "Group Speaking",
  "Lines where 'ALL' speak together",
  all_speaking,
  severity = "INFO"
)

# Check 10: Front matter classification
message("Check 10: Front matter structure...")
front_matter_summary <- all_plays %>%
  filter(section == "front_matter") %>%
  count(short_title, class) %>%
  pivot_wider(names_from = class, values_from = n, values_fill = 0)

message("Front matter structure by play:")
print(front_matter_summary)
message("")

add_issue(
  "Front Matter",
  "Front matter classification summary",
  front_matter_summary %>% head(10),
  severity = "INFO"
)

# Check 11: All-caps lines without periods (potential character names)
message("Check 11: All-caps lines without periods...")
potential_characters <- all_plays %>%
  filter(
    section == "contents",
    str_detect(text, "^[A-Z][A-Z\\s',\\-]+$"),  # All caps with spaces
    !str_detect(text, "\\.$"),  # No period
    !str_detect(text, "^(ACT|SCENE|PROLOGUE|EPILOGUE)")  # Not structural
  ) %>%
  select(short_title, class, text) %>%
  head(20)

add_issue(
  "Potential Missed Characters",
  "All-caps lines without periods that might be character names",
  potential_characters,
  severity = "WARNING"
)

# Check 12: Very short or very long dialogue lines
message("Check 12: Unusual dialogue length...")
unusual_length <- all_plays %>%
  filter(class == "dialogue") %>%
  mutate(length = nchar(text)) %>%
  filter(length < 3 | length > 500) %>%
  arrange(desc(length)) %>%
  select(short_title, character, length, text) %>%
  head(20)

add_issue(
  "Unusual Dialogue Length",
  "Dialogue lines that are very short (<3 chars) or very long (>500 chars)",
  unusual_length,
  severity = "WARNING"
)

# Check 13: Acts and scenes per play
message("Check 13: Play structure completeness...")
play_structure <- all_plays %>%
  filter(class == "reference") %>%
  group_by(short_title) %>%
  summarise(
    acts = n_distinct(act[!is.na(act)]),
    scenes = n_distinct(scene[!is.na(scene)]),
    .groups = "drop"
  ) %>%
  arrange(acts)

message("Acts and scenes per play:")
print(play_structure, n = Inf)
message("")

add_issue(
  "Play Structure",
  "Number of acts and scenes per play",
  play_structure,
  severity = "INFO"
)

# Check 14: Character carryforward issues (character appearing in directions)
message("Check 14: Characters in non-dialogue sections...")
character_in_directions <- all_plays %>%
  filter(class %in% c("directions", "reference"), !is.na(character), character != "") %>%
  select(short_title, class, act, scene, character, text) %>%
  head(20)

add_issue(
  "Character in Directions",
  "Character names appearing in direction/reference lines",
  character_in_directions,
  severity = "ERROR"
)

# Check 15: Dialogue line count per play
message("Check 15: Dialogue statistics per play...")
dialogue_stats <- all_plays %>%
  group_by(short_title, genre) %>%
  summarise(
    total_lines = n(),
    dialogue_lines = sum(class == "dialogue"),
    direction_lines = sum(class == "directions"),
    reference_lines = sum(class == "reference"),
    unique_characters = n_distinct(character[!is.na(character) & character != ""]),
    .groups = "drop"
  ) %>%
  arrange(desc(dialogue_lines))

message("Dialogue statistics:")
print(dialogue_stats, n = Inf)
message("")

add_issue(
  "Dialogue Statistics",
  "Line counts and character counts per play",
  dialogue_stats,
  severity = "INFO"
)

# Generate summary report
message("\n", strrep("=", 70))
message("VALIDATION SUMMARY")
message(strrep("=", 70))

summary_by_severity <- validation_issues %>%
  map_df(~ tibble(
    issue_type = .x$issue_type,
    severity = .x$severity,
    count = .x$count
  )) %>%
  arrange(desc(severity), desc(count))

message("\nIssues by severity:")
print(summary_by_severity, n = Inf)

error_count <- sum(summary_by_severity$severity == "ERROR" & summary_by_severity$count > 0)
warning_count <- sum(summary_by_severity$severity == "WARNING" & summary_by_severity$count > 0)
info_count <- sum(summary_by_severity$severity == "INFO")

message("\n", strrep("-", 70))
message("Total ERRORS: ", error_count)
message("Total WARNINGS: ", warning_count)
message("Total INFO: ", info_count)
message(strrep("-", 70))

# Save detailed report
report_file <- file.path(OUTPUT_DIR_VALIDATION, 
                         paste0("validation_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))

sink(report_file)
cat(strrep("=", 70), "\n")
cat("SHAKESPEARE PARSING VALIDATION REPORT\n")
cat("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n\n")

for (issue in validation_issues) {
  cat("\n", strrep("-", 70), "\n")
  cat(sprintf("[%s] %s\n", issue$severity, issue$issue_type))
  cat(strrep("-", 70), "\n")
  cat("Description: ", issue$description, "\n")
  cat("Count: ", issue$count, "\n\n")
  
  if (issue$count > 0) {
    cat("Sample data:\n")
    print(issue$sample_data %>% head(10))
    cat("\n")
  }
}

cat("\n", strrep("=", 70), "\n")
cat("END OF REPORT\n")
cat(strrep("=", 70), "\n")
sink()

# Save summary CSV
summary_file <- file.path(OUTPUT_DIR_VALIDATION, "validation_summary.csv")
write_csv(summary_by_severity, summary_file)

# Save individual issue CSVs for ERROR and WARNING severities
for (issue in validation_issues) {
  if (issue$severity %in% c("ERROR", "WARNING") && issue$count > 0) {
    issue_file <- file.path(
      OUTPUT_DIR_VALIDATION,
      paste0(tolower(gsub(" ", "_", issue$issue_type)), "_details.csv")
    )
    write_csv(issue$sample_data, issue_file)
    message("Saved details: ", basename(issue_file))
  }
}

message("\n", strrep("=", 70))
message("VALIDATION COMPLETE")
message(strrep("=", 70))
message("Report saved to: ", report_file)
message("Summary saved to: ", summary_file)
message(strrep("=", 70), "\n")

# Return summary
if (error_count > 0) {
  message("⚠️  ERRORS FOUND - Review the report for critical parsing issues")
} else if (warning_count > 0) {
  message("⚠️  WARNINGS FOUND - Review the report for potential issues")
} else {
  message("✓ NO CRITICAL ISSUES FOUND")
}

message("\nRecommendation: Review ", report_file)
message("for detailed findings and sample data.\n")