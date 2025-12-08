library(tidyverse)
library(tidytext)
library(here)
library(dplyr)
library(readr)


# Configuration
INPUT_DIR_TOKENS <- here("data", "processed", "tokens")
OUTPUT_DIR_FILTERED <- here("data", "processed", "filtered")
INPUT_DIR_METADATA <- here("data", "metadata")

# Create output directories
dir.create(file.path(OUTPUT_DIR_FILTERED, "by_content"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR_FILTERED, "by_play"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR_FILTERED, "by_genre"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR_FILTERED, "by_stopwords"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR_FILTERED, "by_character"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR_FILTERED, "by_subset"), recursive = TRUE, showWarnings = FALSE)

message("\n", strrep("=", 70))
message("SHAKESPEARE TOKEN FILTERING")
message(strrep("=", 70))
message("Input: ", INPUT_DIR_TOKENS)
message("Output: ", OUTPUT_DIR_FILTERED)
message(strrep("=", 70), "\n")

# Load tokens
message("Loading tokenised Shakespeare data...")
all_tokens <- read_csv(
  file.path(INPUT_DIR_TOKENS, "all_shakespeare_tokens.csv"),
  show_col_types = FALSE
) # %>% # if not yet 
  #mutate(
   # word = str_replace_all(word, "[‘’]", "'") #,
   # word = str_replace_all(word, "[“”]", "\"")
  # )

message("✓ Loaded ", format(nrow(all_tokens), big.mark = ","), " tokens\n")

# Define custom stop words
message("Creating custom stop word list...")
stop_words_custom <- bind_rows(
  stop_words,
  tibble(word = c(
    #
    # Shakespearean interjections, pronouns and verbs
    "thou", "thee", "thy", "thine", "thyself",
    "hath", "doth", "dost", "art", "wilt", "shalt",
    "tis", "'tis", "twas", "'twas", "mine", "ay", "till", "hast", "nay", "ere",
    # Stage directions
    "enter", "exit", "exeunt", "re-enter",
    # Structural
    "act", "scene"
  ))
)

stop_words_custom %>%
mutate(word = str_replace_all(word, "['']", "'"))   # Normalize apostrophes

message("✓ Stop word list: ", nrow(stop_words_custom), " words\n")

#Extended custom stop word list to filter
stop_words_custom_extended <- # bind_rows(
  # stop_words_custom,
    tibble(word = c(
      # Formal & respectful titles
      "sir", "madam", "lord", "lady", "grace", "majesty", "highness",
      "excellency", "honor", "honour", "liege", "sovereign",
      "worship", "worshipper", "princess", "prince", "duke", "duchess",
      "earl", "countess", "thane", "king", "queen",
      
      # Social & occupational forms of address
      "master", "mistress", "goodman", "goodwife", "goody",
      "host", "hostess", "friar", "father", "abbot",
      
      # Familiar / relational forms
      "cousin", "coz", "kinsman", "kinswoman", "brother",
      "sister", "uncle", "aunt", "nephew", "niece",
      "neighbor", "neighbour", "groom", "page", "servant",
      "attendant", "messenger", "herald",
      
      # Affectionate forms
      "sweetheart", "sweeting", "love", "dear", "beloved",
      "fair", "youth", "maid", "maiden", "virgin",
      
      # Neutral / role-based classical forms
      "soldier", "captain", "general", "doctor", "nurse",
      "apothecary", "clown", "fool",
      
      # Derogatory / insulting direct-address forms
      "knave", "rogue", "villain", "rascal", "varlet", "coxcomb",
      "cur", "slave", "minion", "bawd", "strumpet", "harlot",
      "wench", "churl", "dotard", "cutpurse", "caitiff",
      "scullion", "malefactor", "miscreant", "fustilarian",
      "moldwarp", "whoreson", "dog", "devil",
      
      # Rare or colourful Shakespearean address tokens
      "sirrah", "prow", "swain", "swainling",
      "gallant", "rival", "reveller", "usurer",
      "mountebank", "cox", "breech", "wight"
    ))
#  )
  
#Check top words before filtration to change stop_words_custom
#top_300_words <- all_tokens %>%
# count(word, sort = TRUE) %>%   # count frequencies
#  slice_head(n = 300)             # keep top 300

# Optional: save to CSV
#write_csv(top_200_words, "top_200_shakespeare_words.csv")

#top_300_nonstop <- top_300_words %>%
#  +     filter(!word %in% c(stop_words_custom$word, stop_words_custom_extended$word))

# Initialise filter log
filter_log <- tibble()

# Helper function to save and log
save_filtered <- function(data, category, name, description) {
  filepath <- file.path(OUTPUT_DIR_FILTERED, category, paste0(name, ".csv"))
  write_csv(data, filepath)
  
  # Add to log
  log_entry <- tibble(
    category = category,
    name = name,
    description = description,
    tokens = nrow(data),
    unique_words = n_distinct(data$word),
    plays = n_distinct(data$short_title),
    filepath = filepath
  )
  
  filter_log <<- bind_rows(filter_log, log_entry)
  
  message(sprintf("  ✓ %s: %s tokens", name, format(nrow(data), big.mark = ",")))
  
  return(invisible(data))
}

# Filter 1: By content type
message("\n", strrep("-", 70))
message("FILTER 1: By Content Type")
message(strrep("-", 70))

# Dialogue only
all_tokens %>%
  filter(class == "dialogue") %>%
  save_filtered("by_content", "dialogue_only", "All dialogue lines")

# Directions only
all_tokens %>%
  filter(class == "directions") %>%
  save_filtered("by_content", "directions_only", "All stage directions")

# References only (act/scene markers)
all_tokens %>%
  filter(class == "reference") %>%
  save_filtered("by_content", "references_only", "All structural references")

# Filter 2: By play (all plays)
message("\n", strrep("-", 70))
message("FILTER 2: By Individual Play (All Plays)")
message(strrep("-", 70))

# Get list of all unique plays
play_list <- unique(all_tokens$short_title)
play_count <- 0

for (play in play_list) {
  # Generate filename
  filename <- play %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_remove("^_|_$")
  
  # Save with dialogue and directions only (no references)
  all_tokens %>%
    filter(short_title == play, class %in% c("dialogue", "directions")) %>%
    save_filtered("by_play", filename, paste0(play, " - dialogue and directions"))
  
  # Save filtered version (no stop words)
  all_tokens %>%
    filter(short_title == play, class %in% c("dialogue", "directions")) %>%
    anti_join(stop_words_custom, by = "word") %>%
    save_filtered("by_play", paste0(filename, "_filtered"), paste0(play, " - dialogue and directions, no stop words"))
  
  play_count <- play_count + 1
}

message("✓ Created ", play_count, " play filters (with and without stop words)\n")

# Filter 3: By genre
message("\n", strrep("-", 70))
message("FILTER 3: By Genre")
message(strrep("-", 70))

# Tragedies
all_tokens %>%
  filter(genre == "Tragedy") %>%
  save_filtered("by_genre", "tragedies", "All tragedies")

# Comedies
all_tokens %>%
  filter(genre == "Comedy") %>%
  save_filtered("by_genre", "comedies", "All comedies")

# Histories
all_tokens %>%
  filter(genre == "History") %>%
  save_filtered("by_genre", "histories", "All histories")

# Filter 4: By genre refinements
message("\n", strrep("-", 70))
message("FILTER 4: By Genre Refinements")
message(strrep("-", 70))

# Late Romances
all_tokens %>%
  filter(short_title %in% c("Tempest", "Winter's Tale", "Cymbeline", "Pericles")) %>%
  save_filtered("by_genre", "late_romances", "Late Romances (Tempest, Winter's Tale, Cymbeline, Pericles)")

# Problem Plays
all_tokens %>%
  filter(short_title %in% c("Measure for Measure", "All's Well That Ends Well", 
                            "Troilus and Cressida")) %>%
  save_filtered("by_genre", "problem_plays", "Problem Plays (Measure for Measure, All's Well, Troilus and Cressida)")

# Roman Plays
all_tokens %>%
  filter(short_title %in% c("Julius Caesar", "Antony and Cleopatra", 
                            "Coriolanus", "Titus Andronicus")) %>%
  save_filtered("by_genre", "roman_plays", "Roman Plays (Julius Caesar, Antony and Cleopatra, Coriolanus, Titus Andronicus)")

# Early plays (before 1600)
all_tokens %>%
  filter(!is.na(year), year < 1600) %>%
  save_filtered("by_genre", "early_plays", "Early plays (before 1600)")

# Late plays (1600 and after)
all_tokens %>%
  filter(!is.na(year), year >= 1600) %>%
  save_filtered("by_genre", "late_plays", "Late plays (1600 and after)")

# Filter 5: By stop word removal
message("\n", strrep("-", 70))
message("FILTER 5: By Stop Word Removal")
message(strrep("-", 70))

# All tokens without stop words
all_tokens %>%
  anti_join(stop_words_custom, by = "word") %>%
  save_filtered("by_stopwords", "all_no_stopwords", "All tokens without stop words")

# Dialogue only without stop words
all_tokens %>%
  filter(class == "dialogue") %>%
  anti_join(stop_words_custom, by = "word") %>%
  save_filtered("by_stopwords", "dialogue_no_stopwords", "Dialogue tokens without stop words")

# Tragedies without stop words
all_tokens %>%
  filter(genre == "Tragedy") %>%
  anti_join(stop_words_custom, by = "word") %>%
  save_filtered("by_stopwords", "tragedies_no_stopwords", "Tragedy tokens without stop words")

# Histories without stop words
all_tokens %>%
  filter(genre == "History") %>%
  anti_join(stop_words_custom, by = "word") %>%
  save_filtered("by_stopwords", "histories_no_stopwords", "History tokens without stop words")

# Comedies without stop words
all_tokens %>%
  filter(genre == "Comedy") %>%
  anti_join(stop_words_custom, by = "word") %>%
  save_filtered("by_stopwords", "comedies_no_stopwords", "Comedies tokens without stop words")

# Filter 6: By character (examples)
message("\n", strrep("-", 70))
message("FILTER 6: By Character (Examples)")
message(strrep("-", 70))

# Hamlet's lines
all_tokens %>%
  filter(character == "HAMLET", !is.na(character)) %>%
  save_filtered("by_character", "hamlet_character", "All lines spoken by Hamlet")

# Othello's lines
all_tokens %>%
  filter(character == "OTHELLO", !is.na(character)) %>%
  save_filtered("by_character", "othello_character", "All lines spoken by Othello")

# Lady Macbeth's lines
all_tokens %>%
  filter(character == "LADY MACBETH", !is.na(character)) %>%
  save_filtered("by_character", "lady_macbeth_character", "All lines spoken by Lady Macbeth")

# Filter 7: By custom subset (HSC plays)
message("\n", strrep("-", 70))
message("FILTER 7: By Custom Subset (HSC Plays)")
message(strrep("-", 70))

# HSC plays (2019/2027 prescriptions)
hsc_ids <- c("1516", "1519", "1522", "1524", "1523", "1503", "1540", "1531", "1515")

all_tokens %>%
  filter(as.character(gutenberg_id) %in% hsc_ids) %>%
  save_filtered("by_subset", "hsc_plays", "HSC English prescribed plays (2019/2027)")

# HSC plays directions and dialogue only, no stop words
all_tokens %>%
  filter(as.character(gutenberg_id) %in% hsc_ids, class %in% c("dialogue", "directions")) %>%
  anti_join(stop_words_custom, by = "word") %>%
  save_filtered("by_subset", "hsc_contents_filtered", "HSC plays - directions and dialogue lines, no stop words")

# HSC plays dialogue only, no stop words
all_tokens %>%
  filter(as.character(gutenberg_id) %in% hsc_ids, class == "dialogue") %>%
  anti_join(stop_words_custom, by = "word") %>%
  save_filtered("by_subset", "hsc_dialogue_filtered", "HSC plays - dialogue lines, no stop words")

# Filter 8: By act (examples)
message("\n", strrep("-", 70))
message("FILTER 8: By Act (Examples)")
message(strrep("-", 70))

# All Act V (endings)
all_tokens %>%
  filter(act == "ACT V") %>%
  save_filtered("by_content", "act_v_all", "All Act V scenes (endings)")

# Hamlet Act III Scene I (To be or not to be)
all_tokens %>%
  filter(short_title == "Hamlet", act == "ACT III", scene == "SCENE I") %>%
  save_filtered("by_play", "hamlet_act3_scene1", "Hamlet Act III Scene I")

# Save filter log
message("\n", strrep("-", 70))
message("Saving filter log...")
log_file <- file.path(INPUT_DIR_METADATA, "filter_log.csv")
write_csv(filter_log, log_file)
message("✓ Saved filter log: ", basename(log_file), "\n")

# Summary report
message(strrep("=", 70))
message("FILTERING SUMMARY")
message(strrep("=", 70))

summary_by_category <- filter_log %>%
  group_by(category) %>%
  summarise(
    filters_created = n(),
    total_tokens = sum(tokens),
    .groups = "drop"
  )

print(summary_by_category)

message("\nTotal filters created: ", nrow(filter_log))
message("Categories: ", n_distinct(filter_log$category))
message(strrep("=", 70))
message("Files saved to: ", OUTPUT_DIR_FILTERED)
message(strrep("=", 70), "\n")

message("✓ Filtering complete!\n")

# Display sample filtered data
message("Sample filtered data (dialogue only, no stop words):")
sample_data <- all_tokens %>%
  filter(class == "dialogue") %>%
  anti_join(stop_words_custom, by = "word") %>%
  head(20)
print(sample_data)