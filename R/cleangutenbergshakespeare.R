library(tidyverse)

# ============================================================================
# CLEAN SHAKESPEARE WITH FRONT MATTER
# ============================================================================

clean_shakespeare <- function(text_lines, gutenberg_id = NA) {
  
  # =========================================================================
  # EXTRACT METADATA AND CLEAN TEXT
  # =========================================================================
  
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
  
  # =========================================================================
  # PROCESS FRONT MATTER
  # =========================================================================
  
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
  
  # =========================================================================
  # PROCESS PLAY CONTENTS
  # =========================================================================
  
  play_contents <- df %>%
    filter(section == "contents") %>%
    mutate(
      # Detect structural elements
      is_act = str_detect(text, regex("^\\s*ACT\\s+[IVXLC]+\\.?$", ignore_case = TRUE)),
      is_scene = str_detect(text, regex("^\\s*SCENE\\s+[IVXLC]+\\.", ignore_case = TRUE)),
      is_prologue = str_detect(text, regex("^(Prologue|Epilogue)$", ignore_case = TRUE)),
      is_stage_direction = str_detect(text, "^\\[") | str_detect(text, "\\]$"),  # Starts with [ OR ends with ]
      is_enter_exit = str_detect(text, "^(Enter|Exit|Exeunt|Re-enter)"),
      is_character_name = str_detect(text, "^[A-Z][A-Z\\s'-]+\\.$") & 
        !is_act & !is_scene & !is_prologue,
      
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
      character_temp = zoo::na.locf(character_temp, na.rm = FALSE),
      
      # Classify subsections
      subsection = case_when(
        is_act | is_scene | is_prologue ~ "reference",
        is_stage_direction | is_enter_exit ~ "directions",
        !is.na(character_temp) & !is_character_name & 
          !is_stage_direction & !is_enter_exit ~ "dialogue",
        is.na(character_temp) & !is_act & !is_scene & !is_prologue ~ "directions",
        TRUE ~ NA_character_
      ),
      
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
  
  # =========================================================================
  # COMBINE AND RETURN
  # =========================================================================
  
  result <- bind_rows(front_matter, play_contents)
  
  return(result)
}

# ============================================================================
# DOWNLOAD AND CLEAN
# ============================================================================

download_and_clean_plays <- function(gutenberg_ids = NULL, 
                                     meta_shakespeare = NULL,
                                     output_dir = ".",
                                     save_combined = TRUE,
                                     combined_filename = "all_shakespeare.csv") {
  
  # If metadata provided and no IDs specified, use all IDs from metadata
  if (!is.null(meta_shakespeare) && is.null(gutenberg_ids)) {
    gutenberg_ids <- meta_shakespeare$gutenberg_id
    message("Using all ", length(gutenberg_ids), " plays from metadata")
  }
  
  results <- list()
  
  for (id in gutenberg_ids) {
    message("\nProcessing play ", id, "...")
    
    # Download
    url <- paste0("https://www.gutenberg.org/files/", id, "/", id, "-0.txt")
    raw_text <- tryCatch({
      read_lines(url)
    }, error = function(e) {
      message("✗ Failed to download play ", id, ": ", e$message)
      return(NULL)
    })
    
    if (is.null(raw_text)) {
      next
    }
    
    # Clean
    clean_text <- clean_shakespeare(raw_text, gutenberg_id = id)
    
    # Merge with metadata if provided
    if (!is.null(meta_shakespeare)) {
      metadata_row <- meta_shakespeare %>%
        filter(gutenberg_id == id)
      
      if (nrow(metadata_row) > 0) {
        # Update the placeholder columns with metadata values
        clean_text <- clean_text %>%
          mutate(
            short_title = metadata_row$short_title[1],
            genre = metadata_row$genre[1],
            year = metadata_row$year[1]
          )
      }
    }
    
    # Get title for filename
    if (!is.null(meta_shakespeare) && "short_title" %in% names(clean_text)) {
      play_title <- unique(clean_text$short_title)[1]
    } else {
      play_title <- unique(clean_text$title)[1]
    }
    
    if (!is.na(play_title)) {
      filename <- play_title %>%
        str_to_lower() %>%
        str_replace_all("[^a-z0-9]+", "_") %>%
        str_remove("^_|_$")
      output_file <- file.path(output_dir, paste0(filename, ".csv"))
    } else {
      output_file <- file.path(output_dir, paste0("play_", id, ".csv"))
    }
    
    # Save individual play
    write_csv(clean_text, output_file)
    message("✓ Saved: ", output_file)
    message("  Title: ", play_title)
    message("  Rows: ", nrow(clean_text))
    message("  Dialogue lines: ", sum(clean_text$class == "dialogue", na.rm = TRUE))
    
    results[[as.character(id)]] <- clean_text
  }
  
  # Save combined file if requested
  if (save_combined && length(results) > 0) {
    message("\n", strrep("=", 60))
    message("Combining all plays...")
    all_plays <- bind_rows(results)
    combined_path <- file.path(output_dir, combined_filename)
    write_csv(all_plays, combined_path)
    message("✓ Saved combined file: ", combined_path)
    message("  Total plays: ", length(results))
    message("  Total rows: ", nrow(all_plays))
    message("  Total dialogue lines: ", sum(all_plays$class == "dialogue", na.rm = TRUE))
  }
  
  message("\n✓ Complete! Downloaded ", length(results), " plays")
  return(results)
}

# ============================================================================
# USAGE
# ============================================================================

# Load metadata from text2map package
library(text2map)
data(meta_shakespeare)

# Or load from CSV if saved locally
# meta_shakespeare <- read_csv("meta_shakespeare.csv")

# Download ALL Shakespeare plays to 'data' folder
dir.create("data", showWarnings = FALSE)
plays <- download_and_clean_plays(
meta_shakespeare = meta_shakespeare,
output_dir = "data"
)

# Or download specific plays
# plays <- download_and_clean_plays(
#   gutenberg_ids = c(1524, 1519),
#   meta_shakespeare = meta_shakespeare,
#   output_dir = "data"
# )

# Convert to single tidy dataframe (remove final hash for contents only)
shakespeare <- plays %>%
bind_rows() %>%
filter(section == "contents")

# Save combined dataset
# write_csv(shakespeare, "data/shakespeare.csv")

# Or tokenize for text analysis
# library(tidytext)
# tidy_shakespeare <- shakespeare %>%
#   unnest_tokens(word, text) %>%
#   anti_join(stop_words)