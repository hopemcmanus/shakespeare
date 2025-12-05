library(tidyverse)
library(here)

# Configuration
INPUT_DIR_RAW <- here("data", "raw")
INPUT_DIR_METADATA <- here("data", "metadata")
OUTPUT_DIR_CLEANED <- here("data", "cleaned")
SAVE_INDIVIDUAL_PLAYS <- TRUE
SAVE_COMBINED <- TRUE
COMBINED_FILENAME <- "all_shakespeare.csv"

# Create output directory
dir.create(OUTPUT_DIR_CLEANED, recursive = TRUE, showWarnings = FALSE)

# Clean Shakespeare function - parses raw text into structured format
clean_shakespeare <- function(text_lines, gutenberg_id = NA) {
  
  # Extract metadata and clean text
  
  # Look for Gutenberg metadata (if downloading)
  title_meta_line <- str_which(text_lines, "^Title: ")
  author_meta_line <- str_which(text_lines, "^Author: ")
  start_line <- str_which(text_lines, "^\\*\\*\\* START OF")
  end_line <- str_which(text_lines, "^\\*\\*\\* END OF")
  
  # Extract from Gutenberg header if present
  title_from_meta <- if (length(title_meta_line) > 0) {
    str_remove(text_lines[title_meta_line[1]], "^Title: ")
  } else {
    NA_character_
  }
  
  author_from_meta <- if (length(author_meta_line) > 0) {
    str_remove(text_lines[author_meta_line[1]], "^Author: ")
  } else {
    NA_character_
  }
  
  # Remove everything before "*** START OF" and after "*** END OF"
  if (length(start_line) > 0) {
    start_idx <- start_line[1] + 1
  } else {
    start_idx <- 1
  }
  
  if (length(end_line) > 0) {
    end_idx <- end_line[1] - 1
  } else {
    end_idx <- length(text_lines)
  }
  
  text_lines <- text_lines[start_idx:end_idx]
  
  # If no Gutenberg metadata, extract from play text
  # Title is all lines before "by William Shakespeare"
  # Author is usually "William Shakespeare"
  if (is.na(title_from_meta)) {
    by_line_idx <- str_which(text_lines, "^by William Shakespeare$")
    if (length(by_line_idx) > 0) {
      # Get all non-blank lines before "by William Shakespeare"
      title_lines <- text_lines[1:(by_line_idx[1] - 1)]
      title_lines <- title_lines[str_trim(title_lines) != ""]
      title <- paste(title_lines, collapse = " ")
    } else {
      # No "by" line found, use first non-blank line
      title <- str_trim(text_lines[str_trim(text_lines) != ""][1])
    }
  } else {
    title <- title_from_meta
  }
  
  if (is.na(author_from_meta)) {
    author_line_idx <- str_which(text_lines, "^by William Shakespeare$")
    author <- if (length(author_line_idx) > 0) "William Shakespeare" else NA_character_
  } else {
    author <- author_from_meta
  }
  
  # Create dataframe
  df <- tibble(
    line_num = seq_along(text_lines),
    text = str_trim(text_lines)
  ) %>%
    filter(text != "") %>%
    mutate(
      # Remove underscores
      text = str_remove_all(text, "_"),
      
      # Detect ACT I specifically
      act_i_line = str_detect(text, regex("^\\s*ACT\\s+I\\.?$", ignore_case = TRUE)),
      
      # Cumulative count of ACT I occurrences
      act_i_count = cumsum(act_i_line),
      
      # Mark where the second ACT I appears
      second_act_i = act_i_count == 2 & act_i_line,
      
      # Get the row number of second ACT I
      first_play_line = if_else(any(second_act_i), 
                                min(which(second_act_i)), 
                                if_else(any(act_i_line), min(which(act_i_line)), as.integer(n() + 1))),
      
      # Assign section
      section = if_else(row_number() < first_play_line, "front_matter", "contents")
    )
  
  # Process front matter
  front_matter <- df %>%
    filter(section == "front_matter") %>%
    mutate(
      # Find key sections
      is_contents_header = str_detect(text, "^Contents$"),
      is_dramatis_header = str_detect(text, regex("^Dramatis Person", ignore_case = TRUE)),
      is_author_line = str_detect(text, "^by William Shakespeare$"),
      is_act_line = str_detect(text, regex("^\\s*ACT\\s+[IVXLC]+", ignore_case = TRUE)),
      is_scene_line = str_detect(text, regex("^\\s*Scene\\s+[IVXLC]+\\.", ignore_case = FALSE)),
      is_scene_desc = str_detect(text, "^(SCENE[.:]|The scene)"),
      
      # Find section boundaries
      contents_start = if_else(any(is_contents_header), min(which(is_contents_header)), as.integer(NA)),
      dramatis_start = if_else(any(is_dramatis_header), min(which(is_dramatis_header)), as.integer(NA))
    )
  
  # Calculate scene_desc_start outside mutate to avoid warning
  scene_desc_idx <- which(front_matter$is_scene_desc)
  scene_desc_start_val <- if (length(scene_desc_idx) > 0) scene_desc_idx[1] else NA_integer_
  
  front_matter <- front_matter %>%
    mutate(
      scene_desc_start = scene_desc_start_val,
      
      # Classify subsection
      subsection = case_when(
        is_author_line ~ "author",
        is_contents_header ~ "contents_header",
        is_dramatis_header ~ "dramatis_header",
        !is.na(contents_start) & !is.na(dramatis_start) & 
          row_number() > contents_start & row_number() < dramatis_start ~ "contents",
        !is.na(dramatis_start) & !is.na(scene_desc_start) &
          row_number() > dramatis_start & row_number() < scene_desc_start ~ "dramatis_personae",
        !is.na(scene_desc_start) & row_number() >= scene_desc_start ~ "scene_description",
        TRUE ~ "title"
      ),
      
      # Extract act and scene references for contents ONLY (not dramatis/scene_desc)
      act = if_else(subsection == "contents" & is_act_line,
                    str_extract(text, regex("ACT\\s+[IVXLC]+", ignore_case = TRUE)),
                    NA_character_),
      # Only fill act within contents section
      act = if_else(subsection == "contents", zoo::na.locf(act, na.rm = FALSE), NA_character_),
      
      scene = if_else(subsection == "contents" & is_scene_line,
                      str_extract(text, "Scene [IVX]+"),
                      NA_character_),
      
      character = NA_character_,
      line_number = NA_integer_,
      author = author,
      gutenberg_title = title,
      short_title = NA_character_,
      genre = NA_character_,
      year = NA_integer_,
      gutenberg_id = gutenberg_id
    ) %>%
    rename(class = subsection) %>%
    select(gutenberg_id, short_title, gutenberg_title, genre, year, author, section, class, act, scene, character, line_number, text)
  
  # Process play contents
  play_contents <- df %>%
    filter(section == "contents") %>%
    mutate(
      # Detect structural elements
      is_act = str_detect(text, regex("^\\s*ACT\\s+[IVXLC]+\\.?$", ignore_case = TRUE)),
      is_scene = str_detect(text, regex("^\\s*SCENE\\s+[IVXLC]+\\.", ignore_case = TRUE)),
      is_prologue = str_detect(text, regex("^(Prologue|Epilogue)$", ignore_case = TRUE)),
      
      # Detect bracket boundaries for multi-line directions
      bracket_opens = str_count(text, "\\["),
      bracket_closes = str_count(text, "\\]"),
      in_multiline_bracket = cumsum(bracket_opens) > cumsum(bracket_closes),
      
      # Stage directions: 
      # 1. Lines entirely within brackets: ^[...]$ 
      # 2. OR lines that are part of multi-line bracket AND have no text before [ or after ]
      is_entirely_bracketed = str_detect(text, "^\\s*\\[.*\\]\\s*$"),
      is_bracket_continuation = in_multiline_bracket | 
        (str_detect(text, "^\\s*\\[") & !str_detect(text, "\\]")) |  # Starts [ but doesn't end
        (str_detect(text, "\\]\\s*$") & !str_detect(text, "^\\s*\\[")),  # Ends ] but doesn't start
      
      is_stage_direction = is_entirely_bracketed | is_bracket_continuation,
      
      is_enter_exit = str_detect(text, "^(Enter|Exit|Exeunt|Re-enter)"),
      is_character_name = str_detect(text, "^[A-Z][A-Z\\s',\\-]{2,}(,\\s+[A-Z][A-Z\\s',\\-]+)*(\\s+and\\s+[A-Z][A-Z\\s',\\-]+)?\\.?$") & 
        !is_act & !is_scene & !is_prologue,
      
      # Toggle tracking for multi-line stage directions
      # Only Enter/Exit statements start multi-line direction blocks
      direction_starts = is_enter_exit,
      # ONLY character names end multi-line direction blocks (not act/scene markers)
      direction_ends = is_character_name,
      # We're in a multi-line direction if we've started more than we've ended
      in_multiline_direction = cumsum(direction_starts) > cumsum(direction_ends),
      
      # Extract and carry forward ACT
      act = if_else(is_act, text, NA_character_),
      act = zoo::na.locf(act, na.rm = FALSE),
      
      # Extract and carry forward SCENE (including Prologue/Epilogue)
      scene = case_when(
        is_scene ~ str_extract(text, regex("SCENE\\s+[IVXLC]+", ignore_case = TRUE)),
        is_prologue ~ text,
        TRUE ~ NA_character_
      ),
      scene = zoo::na.locf(scene, na.rm = FALSE),
      
      # Extract and carry forward CHARACTER
      character_temp = if_else(is_character_name, str_remove(text, "\\.$"), NA_character_),
      
      # CRITICAL: Only clear character at act/scene boundaries, not at stage directions
      # Stage directions can happen mid-speech
      character_cleared = is_act | is_scene,
      character_temp = if_else(character_cleared, "", character_temp),
      
      # Carry forward character (will carry forward "" for cleared sections)
      character_temp = zoo::na.locf(character_temp, na.rm = FALSE),
      
      # Classify subsections with improved logic
      subsection = case_when(
        is_act | is_scene | is_prologue ~ "reference",
        is_stage_direction ~ "directions",  # Explicit [ ] always directions
        is_enter_exit ~ "directions",       # Enter/Exit lines always directions
        in_multiline_direction ~ "directions",  # Continuation of Enter/Exit
        !is.na(character_temp) & character_temp != "" & !is_character_name & 
          !is_stage_direction & !is_enter_exit & !in_multiline_direction ~ "dialogue",
        (is.na(character_temp) | character_temp == "") & !is_act & !is_scene & !is_prologue ~ "directions",
        TRUE ~ NA_character_
      ),
      
      # Convert empty string to NA for final character assignment
      character_temp = if_else(character_temp == "", NA_character_, character_temp),
      
      # Character only for dialogue (not directions)
      character = if_else(subsection == "dialogue", character_temp, NA_character_),
      
      author = author,
      gutenberg_title = title,
      short_title = NA_character_,
      genre = NA_character_,
      year = NA_integer_,
      gutenberg_id = gutenberg_id
    ) %>%
    filter(!is.na(subsection)) %>%
    # Add continuous line numbers for dialogue only
    mutate(
      line_number = if_else(subsection == "dialogue", 
                            cumsum(subsection == "dialogue"), 
                            NA_integer_)
    ) %>%
    rename(class = subsection) %>%
    select(gutenberg_id, short_title, gutenberg_title, genre, year, author, 
           section, class, act, scene, character, line_number, text)
  
  # Combine and return
  result <- bind_rows(front_matter, play_contents)
  
  return(result)
}

# Generate filename from title
generate_filename <- function(title, gutenberg_id) {
  if (!is.na(title) && title != "") {
    filename <- title %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_remove("^_|_$")
    return(paste0(filename, ".csv"))
  } else {
    return(paste0("play_", gutenberg_id, ".csv"))
  }
}

# Main cleaning function
clean_all_shakespeare <- function(meta_shakespeare = NULL,
                                  save_individual = SAVE_INDIVIDUAL_PLAYS,
                                  save_combined = SAVE_COMBINED) {
  
  message("\n", strrep("=", 70))
  message("SHAKESPEARE TEXT CLEANING")
  message(strrep("=", 70))
  message("Input directory: ", INPUT_DIR_RAW)
  message("Output directory: ", OUTPUT_DIR_CLEANED)
  message(strrep("=", 70), "\n")
  
  # Get list of raw text files
  raw_files <- list.files(INPUT_DIR_RAW, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(raw_files) == 0) {
    stop("No raw text files found in ", INPUT_DIR_RAW, 
         "\nPlease run 01_download_gutenberg.R first.")
  }
  
  message("Found ", length(raw_files), " raw text files to process\n")
  
  # Load metadata if available
  if (is.null(meta_shakespeare)) {
    meta_file <- file.path(INPUT_DIR_METADATA, "meta_shakespeare.csv")
    if (file.exists(meta_file)) {
      meta_shakespeare <- read_csv(meta_file, show_col_types = FALSE)
      message("✓ Loaded metadata from ", meta_file, "\n")
    } else {
      message("⚠ No metadata file found, proceeding without metadata\n")
    }
  }
  
  # Initialise results and log
  all_plays <- list()
  cleaning_log <- tibble()
  
  # Progress tracking
  total <- length(raw_files)
  success_count <- 0
  failed_count <- 0
  
  for (i in seq_along(raw_files)) {
    file_path <- raw_files[i]
    file_name <- basename(file_path)
    gutenberg_id <- str_extract(file_name, "^\\d+") %>% as.integer()
    
    message(sprintf("[%d/%d] Processing: %s (ID: %s)", i, total, file_name, gutenberg_id))
    
    # Read raw text
    raw_text <- tryCatch({
      read_lines(file_path, progress = FALSE)
    }, error = function(e) {
      message("  ✗ Failed to read file: ", e$message)
      return(NULL)
    })
    
    if (is.null(raw_text)) {
      failed_count <- failed_count + 1
      next
    }
    
    # Clean and structure
    cleaned_text <- tryCatch({
      clean_shakespeare(raw_text, gutenberg_id = gutenberg_id)
    }, error = function(e) {
      message("  ✗ Cleaning failed: ", e$message)
      return(NULL)
    })
    
    if (is.null(cleaned_text) || nrow(cleaned_text) == 0) {
      message("  ✗ No valid content after cleaning")
      failed_count <- failed_count + 1
      
      # Log failed cleaning
      log_entry <- tibble(
        gutenberg_id = gutenberg_id,
        short_title = NA_character_,
        cleaning_timestamp = Sys.time(),
        cleaning_success = FALSE,
        total_rows = 0,
        dialogue_lines = 0,
        front_matter_rows = 0,
        contents_rows = 0,
        output_file = NA_character_
      )
      cleaning_log <- bind_rows(cleaning_log, log_entry)
      next
    }
    
    # Get play title from metadata FIRST (for filename generation)
    play_title <- NA_character_
    
    if (!is.null(meta_shakespeare)) {
      metadata_row <- meta_shakespeare %>%
        filter(gutenberg_id == !!gutenberg_id)
      
      if (nrow(metadata_row) > 0) {
        play_title <- metadata_row$short_title[1]
      }
    }
    
    # If no metadata title, try from cleaned text
    if (is.na(play_title)) {
      play_title <- unique(cleaned_text$short_title)[1]
      if (is.na(play_title)) {
        play_title <- unique(cleaned_text$gutenberg_title)[1]
      }
    }
    
    # Now merge with metadata (after getting title for filename)
    if (!is.null(meta_shakespeare)) {
      metadata_row <- meta_shakespeare %>%
        filter(gutenberg_id == !!gutenberg_id)
      
      if (nrow(metadata_row) > 0) {
        cleaned_text <- cleaned_text %>%
          mutate(
            short_title = metadata_row$short_title[1],
            genre = metadata_row$genre[1],
            year = metadata_row$year[1]
          )
      }
    }
    
    message("  ✓ Cleaned: ", play_title)
    message("    Total rows: ", nrow(cleaned_text))
    message("    Dialogue lines: ", sum(cleaned_text$class == "dialogue", na.rm = TRUE))
    
    # Save individual play if requested
    output_filename <- generate_filename(play_title, gutenberg_id)
    
    if (save_individual) {
      output_path <- file.path(OUTPUT_DIR_CLEANED, output_filename)
      write_csv(cleaned_text, output_path)
      message("    Saved to: ", output_filename)
    }
    
    # Store for combined dataset
    all_plays[[as.character(gutenberg_id)]] <- cleaned_text
    success_count <- success_count + 1
    
    # Log successful cleaning
    log_entry <- tibble(
      gutenberg_id = gutenberg_id,
      short_title = play_title,
      cleaning_timestamp = Sys.time(),
      cleaning_success = TRUE,
      total_rows = nrow(cleaned_text),
      dialogue_lines = sum(cleaned_text$class == "dialogue", na.rm = TRUE),
      front_matter_rows = sum(cleaned_text$section == "front_matter", na.rm = TRUE),
      contents_rows = sum(cleaned_text$section == "contents", na.rm = TRUE),
      output_file = output_filename
    )
    cleaning_log <- bind_rows(cleaning_log, log_entry)
    
    message("")
  }
  
  # Save combined dataset if requested
  if (save_combined && length(all_plays) > 0) {
    message("Combining all plays into single dataset...")
    combined_data <- bind_rows(all_plays)
    combined_path <- file.path(OUTPUT_DIR_CLEANED, COMBINED_FILENAME)
    write_csv(combined_data, combined_path)
    message("✓ Saved combined dataset: ", COMBINED_FILENAME)
    message("  Total rows: ", nrow(combined_data))
    message("  Total plays: ", length(all_plays))
  }
  
  # Save cleaning log
  log_file <- file.path(INPUT_DIR_METADATA, 
                        paste0("cleaning_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
  write_csv(cleaning_log, log_file)
  write_csv(cleaning_log, file.path(INPUT_DIR_METADATA, "cleaning_log_latest.csv"))
  
  # Summary report
  message("\n", strrep("=", 70))
  message("CLEANING SUMMARY")
  message(strrep("=", 70))
  message("Total processed:    ", total)
  message("Successfully cleaned: ", success_count)
  message("Failed:             ", failed_count)
  message("Success rate:       ", round(success_count / total * 100, 1), "%")
  message(strrep("=", 70))
  message("Log saved to: ", log_file)
  message(strrep("=", 70), "\n")
  
  # List failed cleans if any
  if (failed_count > 0) {
    failed_ids <- cleaning_log %>% 
      filter(!cleaning_success) %>% 
      pull(gutenberg_id)
    
    if (length(failed_ids) > 0) {
      message("\nFailed Gutenberg IDs: ", paste(failed_ids, collapse = ", "))
      message("Check log file for details.\n")
    }
  }
  
  return(cleaning_log)
}

# Usage example

# Load metadata
meta_shakespeare <- read_csv(
  file.path(INPUT_DIR_METADATA, "meta_shakespeare.csv"),
  show_col_types = FALSE
)

# Clean all plays
cleaning_log <- clean_all_shakespeare(
  meta_shakespeare = meta_shakespeare,
  save_individual = TRUE,
  save_combined = TRUE
)

# Example: Clean but only save combined file (not individual plays)
# cleaning_log <- clean_all_shakespeare(
#   meta_shakespeare = meta_shakespeare,
#   save_individual = FALSE,
#   save_combined = TRUE
# )

# Example: Access the combined data in memory for analysis
# all_shakespeare <- read_csv(here("data", "cleaned", "all_shakespeare.csv"))
# shakespeare_contents_only <- all_shakespeare %>% filter(section == "contents")