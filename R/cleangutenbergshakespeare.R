library(tidyverse)

# ============================================================================
# CLEAN SHAKESPEARE WITH FRONT MATTER
# ============================================================================

clean_shakespeare <- function(text_lines, gutenberg_id = NA) {
  
  # =========================================================================
  # EXTRACT METADATA AND CLEAN TEXT
  # =========================================================================
  
  # Find title and author from Gutenberg header
  title_line <- str_which(text_lines, "^Title: ")
  author_line <- str_which(text_lines, "^Author: ")
  start_line <- str_which(text_lines, "^\\*\\*\\* START OF")
  end_line <- str_which(text_lines, "^\\*\\*\\* END OF")
  
  title <- if (length(title_line) > 0) {
    str_remove(text_lines[title_line[1]], "^Title: ")
  } else {
    NA_character_
  }
  
  author <- if (length(author_line) > 0) {
    str_remove(text_lines[author_line[1]], "^Author: ")
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
      dramatis_start = if_else(any(is_dramatis_header), min(which(is_dramatis_header)), as.integer(NA)),
      scene_desc_idx = which(is_scene_desc),
      scene_desc_start = if_else(length(scene_desc_idx) > 0, min(scene_desc_idx), as.integer(NA)),
      
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
      title = title,
      author = author,
      gutenberg_id = gutenberg_id
    ) %>%
    select(gutenberg_id, title, author, section, subsection, act, scene, character, line_number, text)
  
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
      is_stage_direction = str_detect(text, "^\\[.*\\]$"),
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
      
      title = title,
      author = author,
      gutenberg_id = gutenberg_id
    ) %>%
    filter(!is.na(subsection)) %>%
    # Add continuous line numbers for dialogue only
    mutate(
      line_number = if_else(subsection == "dialogue", 
                            cumsum(subsection == "dialogue"), 
                            NA_integer_)
    ) %>%
    select(gutenberg_id, title, author, section, subsection, act, scene, character, line_number, text)
  
  # =========================================================================
  # COMBINE AND RETURN
  # =========================================================================
  
  result <- bind_rows(front_matter, play_contents)
  
  return(result)
}

# ============================================================================
# DOWNLOAD AND CLEAN
# ============================================================================

download_and_clean_plays <- function(gutenberg_ids, output_dir = ".") {
  
  results <- list()
  
  for (id in gutenberg_ids) {
    message("Processing play ", id, "...")
    
    # Download
    url <- paste0("https://www.gutenberg.org/files/", id, "/", id, "-0.txt")
    raw_text <- read_lines(url)
    
    # Clean
    clean_text <- clean_shakespeare(raw_text, gutenberg_id = id)
    
    # Get title for filename (clean it up)
    play_title <- unique(clean_text$title)[1]
    if (!is.na(play_title)) {
      filename <- play_title %>%
        str_to_lower() %>%
        str_replace_all("[^a-z0-9]+", "_") %>%
        str_remove("^_|_$")
      output_file <- file.path(output_dir, paste0(filename, "_clean.csv"))
    } else {
      output_file <- file.path(output_dir, paste0("play_", id, "_clean.csv"))
    }
    
    # Save
    write_csv(clean_text, output_file)
    message("✓ Saved: ", output_file)
    message("  Title: ", play_title)
    message("  Rows: ", nrow(clean_text))
    message("  Front matter: ", sum(clean_text$section == "front_matter"))
    message("  Contents: ", sum(clean_text$section == "contents"), "\n")
    
    results[[as.character(id)]] <- clean_text
  }
  
  return(results)
}

# ============================================================================
# USAGE
# ============================================================================

# Download and clean one play
# hamlet <- download_and_clean_plays(1524)

# Download and clean multiple plays  
plays <- download_and_clean_plays(c(1524, 1523, 1522))

# Or manually:
# raw <- read_lines("hamlet.txt")
# clean <- clean_shakespeare(raw, gutenberg_id = 1524)
# write_csv(clean, "hamlet_clean.csv")