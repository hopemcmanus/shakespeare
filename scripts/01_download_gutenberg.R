library(tidyverse)
library(text2map)
library(here)

# Configuration
OUTPUT_DIR_RAW <- here("data", "raw")
OUTPUT_DIR_METADATA <- here("data", "metadata")
MAX_RETRIES <- 3
RETRY_DELAY_SECONDS <- 2

# Create directory structure
dir.create(OUTPUT_DIR_RAW, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_DIR_METADATA, recursive = TRUE, showWarnings = FALSE)

# Validation function - checks if downloaded text is valid Shakespeare play
validate_download <- function(text, gutenberg_id) {
  
  if (is.null(text) || length(text) == 0) {
    return(list(
      valid = FALSE,
      checks = list(has_content = FALSE),
      warnings = "No text content"
    ))
  }
  
  # Run quality checks
  checks <- list(
    has_content = length(text) > 100,
    has_acts = any(str_detect(text, regex("ACT\\s+[IVX]+", ignore_case = TRUE))),
    has_character_names = sum(str_detect(text, "^[A-Z][A-Z\\s]+\\.$")) > 20,
    reasonable_length = length(text) > 500 && length(text) < 100000,
    has_shakespeare_marker = any(str_detect(text, "Shakespeare"))
  )
  
  all_valid <- all(unlist(checks))
  
  # Generate warnings for failed checks
  failed_checks <- names(checks)[!unlist(checks)]
  warning_msg <- if (length(failed_checks) > 0) {
    paste("Failed checks:", paste(failed_checks, collapse = ", "))
  } else {
    NA_character_
  }
  
  return(list(
    valid = all_valid,
    checks = checks,
    warnings = warning_msg
  ))
}

# Download with retry logic
download_with_retry <- function(gutenberg_id, max_retries = MAX_RETRIES) {
  
  for (attempt in 1:max_retries) {
    
    result <- tryCatch({
      url <- paste0("https://www.gutenberg.org/files/", gutenberg_id, "/", 
                    gutenberg_id, "-0.txt")
      text <- read_lines(url, progress = FALSE)
      list(success = TRUE, text = text, error = NA_character_)
    }, error = function(e) {
      list(success = FALSE, text = NULL, error = as.character(e$message))
    })
    
    if (result$success) {
      return(result)
    }
    
    # Wait before retry (except on last attempt)
    if (attempt < max_retries) {
      message("  Retry ", attempt, "/", max_retries - 1, " after ", 
              RETRY_DELAY_SECONDS, " seconds...")
      Sys.sleep(RETRY_DELAY_SECONDS)
    }
  }
  
  return(result)
}

# Main download and cache function
download_and_cache_gutenberg <- function(gutenberg_ids, 
                                         meta_shakespeare = NULL,
                                         force_redownload = FALSE) {
  
  message("\n", strrep("=", 70))
  message("SHAKESPEARE TEXT ACQUISITION")
  message(strrep("=", 70))
  message("Total plays to process: ", length(gutenberg_ids))
  message("Output directory: ", OUTPUT_DIR_RAW)
  message("Force redownload: ", force_redownload)
  message(strrep("=", 70), "\n")
  
  # Initialise log
  download_log <- tibble()
  
  # Progress tracking
  total <- length(gutenberg_ids)
  success_count <- 0
  cached_count <- 0
  failed_count <- 0
  
  for (i in seq_along(gutenberg_ids)) {
    id <- gutenberg_ids[i]
    
    message(sprintf("[%d/%d] Processing Gutenberg ID: %s", i, total, id))
    
    # Get metadata if available
    play_metadata <- if (!is.null(meta_shakespeare)) {
      meta_shakespeare %>% filter(gutenberg_id == id)
    } else {
      NULL
    }
    
    play_title <- if (!is.null(play_metadata) && nrow(play_metadata) > 0) {
      play_metadata$short_title[1]
    } else {
      NA_character_
    }
    
    # Check cache
    cache_file <- file.path(OUTPUT_DIR_RAW, paste0(id, ".txt"))
    
    if (file.exists(cache_file) && !force_redownload) {
      message("  ✓ Using cached version")
      raw_text <- read_lines(cache_file, progress = FALSE)
      download_method <- "cached"
      download_success <- TRUE
      download_error <- NA_character_
      cached_count <- cached_count + 1
    } else {
      # Download
      message("  Downloading from Project Gutenberg...")
      download_result <- download_with_retry(id)
      
      raw_text <- download_result$text
      download_success <- download_result$success
      download_error <- download_result$error
      download_method <- "direct_url"
      
      if (!download_success) {
        message("  ✗ Download failed: ", download_error)
        failed_count <- failed_count + 1
      }
    }
    
    # Validate if download succeeded
    if (download_success) {
      validation <- validate_download(raw_text, id)
      
      if (validation$valid) {
        message("  ✓ Validation passed")
        
        # Save to cache if newly downloaded
        if (download_method != "cached") {
          write_lines(raw_text, cache_file)
          message("  ✓ Saved to cache")
        }
        
        success_count <- success_count + 1
      } else {
        message("  ✗ Validation failed: ", validation$warnings)
        download_success <- FALSE
        download_error <- validation$warnings
        failed_count <- failed_count + 1
      }
    } else {
      validation <- list(
        valid = FALSE,
        checks = list(),
        warnings = "Download failed"
      )
    }
    
    # Log entry
    log_entry <- tibble(
      gutenberg_id = id,
      short_title = play_title,
      download_timestamp = Sys.time(),
      download_method = download_method,
      download_success = download_success,
      download_error = download_error,
      validation_passed = validation$valid,
      validation_warnings = validation$warnings,
      raw_line_count = if (download_success) length(raw_text) else NA_integer_,
      has_content = if (download_success) validation$checks$has_content else NA,
      has_acts = if (download_success) validation$checks$has_acts else NA,
      has_character_names = if (download_success) validation$checks$has_character_names else NA,
      reasonable_length = if (download_success) validation$checks$reasonable_length else NA,
      has_shakespeare_marker = if (download_success) validation$checks$has_shakespeare_marker else NA,
      cache_file_path = cache_file,
      script_version = "1.0"
    )
    
    download_log <- bind_rows(download_log, log_entry)
    message("")
  }
  
  # Save comprehensive log
  log_file <- file.path(OUTPUT_DIR_METADATA, 
                        paste0("download_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
  write_csv(download_log, log_file)
  
  # Also save as latest
  write_csv(download_log, file.path(OUTPUT_DIR_METADATA, "download_log_latest.csv"))
  
  # Summary report
  message("\n", strrep("=", 70))
  message("DOWNLOAD SUMMARY")
  message(strrep("=", 70))
  message("Total processed:    ", total)
  message("Successfully cached: ", cached_count)
  message("Successfully downloaded: ", success_count - cached_count)
  message("Failed:             ", failed_count)
  message("Success rate:       ", round(success_count / total * 100, 1), "%")
  message(strrep("=", 70))
  message("Log saved to: ", log_file)
  message(strrep("=", 70), "\n")
  
  # List failed downloads if any
  if (failed_count > 0) {
    failed_ids <- download_log %>% 
      filter(!download_success) %>% 
      pull(gutenberg_id)
    
    message("\nFailed Gutenberg IDs: ", paste(failed_ids, collapse = ", "))
    message("Check log file for details.\n")
  }
  
  return(download_log)
}

# Usage example
# Load metadata from text2map package
data(meta_shakespeare)

# Save metadata for reference in data folder
write_csv(meta_shakespeare, file.path(OUTPUT_DIR_METADATA, "meta_shakespeare.csv"))
message("✓ Saved metadata: ", file.path(OUTPUT_DIR_METADATA, "meta_shakespeare.csv"), "\n")

# Download all Shakespeare plays
download_log <- download_and_cache_gutenberg(
  gutenberg_ids = meta_shakespeare$gutenberg_id,
  meta_shakespeare = meta_shakespeare,
  force_redownload = FALSE  # Set to TRUE to re-download everything
)

# Example: Download specific plays only
# download_log <- download_and_cache_gutenberg(
#   gutenberg_ids = c(1524, 1519, 1522),  # Hamlet, Much Ado, Julius Caesar
#   meta_shakespeare = meta_shakespeare
# )

# Example: Re-download everything (ignore cache)
# download_log <- download_and_cache_gutenberg(
#   gutenberg_ids = meta_shakespeare$gutenberg_id,
#   meta_shakespeare = meta_shakespeare,
#   force_redownload = TRUE
# )