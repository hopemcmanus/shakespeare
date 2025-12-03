# Load the data
#dickinson <- read.csv("/mnt/user-data/uploads/dickinson.csv", 
#                      stringsAsFactors = FALSE)

# Step 1: Remove blank lines
dickinson_clean <- dickinson[dickinson$text != "", ]

# Step 2: Add metadata fields
dickinson_structured <- dickinson_clean

# Add basic metadata
dickinson_structured$title <- "Poems by Emily Dickinson"
dickinson_structured$year <- 1890
dickinson_structured$author <- "Emily Dickinson"

# Identify the start of contents (first occurrence of "I. LIFE.")
contents_start_row <- min(which(dickinson_structured$text == "I. LIFE."))

# Classify section: front_matter or contents
dickinson_structured$section <- ifelse(
  1:nrow(dickinson_structured) >= contents_start_row,
  "contents",
  "front_matter"
)

# Create series number and section_id based on section headers (I. LIFE., II. LOVE., etc.)
dickinson_structured$series <- NA_integer_
dickinson_structured$section_id <- NA_character_

# Track series: each time we see "I. LIFE." we increment the series
section_counter <- 0
series_num <- 0
current_section_name <- NA_character_

for (i in 1:nrow(dickinson_structured)) {
  txt <- dickinson_structured$text[i]
  
  # Check if this is "I. LIFE." which starts a new series
  if (txt == "I. LIFE.") {
    series_num <- series_num + 1
  }
  
  # Check if this is a section header (Roman numeral + period + all caps)
  if (grepl("^[IVX]+\\. [A-Z][A-Z ]+\\.$", txt)) {
    section_counter <- section_counter + 1
    current_section_name <- txt
  }
  
  # Fill down series and section_id for all rows
  if (dickinson_structured$section[i] == "contents") {
    dickinson_structured$series[i] <- series_num
    dickinson_structured$section_id[i] <- current_section_name
  }
}

# Initialize class column
dickinson_structured$class <- NA_character_

# Classify content types
for (i in 1:nrow(dickinson_structured)) {
  txt <- dickinson_structured$text[i]
  sect <- dickinson_structured$section[i]
  
  if (sect == "front_matter") {
    # Front matter has NA for class
    dickinson_structured$class[i] <- NA_character_
    
  } else {
    # Section headers: Roman numeral + period + all caps text
    # Matches patterns like "I. LIFE.", "II. LOVE.", "III. NATURE.", etc.
    if (grepl("^[IVX]+\\. [A-Z][A-Z ]+\\.$", txt)) {
      dickinson_structured$class[i] <- "section"
      
      # Poem numbers: Roman numeral + period only (e.g., "I.", "II.", "III.", "XL.", "XLVI.")
    } else if (grepl("^[IVXLCDM]+\\.$", txt)) {
      dickinson_structured$class[i] <- "poem_number"
      
      # Poem titles: All caps text ending with period or exclamation (after poem_number)
    } else if (grepl("^[A-Z][A-Z ',.-]+[.!]$", txt)) {
      dickinson_structured$class[i] <- "poem_title"
      
      # Notes: Lines within square brackets
    } else if (grepl("^\\[.*\\]$", txt) || grepl("^\\[", txt) || grepl("\\]$", txt)) {
      dickinson_structured$class[i] <- "notes"
      
      # Everything else in contents is poem text
    } else {
      dickinson_structured$class[i] <- "poem"
    }
  }
}

# Step 3: Handle multi-line notes (lines between [ and ])
in_note <- FALSE
for (i in 1:nrow(dickinson_structured)) {
  if (dickinson_structured$section[i] == "contents") {
    txt <- dickinson_structured$text[i]
    
    # Check if this line starts a note
    if (grepl("^\\[", txt) && !grepl("\\]$", txt)) {
      in_note <- TRUE
      dickinson_structured$class[i] <- "notes"
    } 
    # Check if we're in the middle of a multi-line note
    else if (in_note) {
      dickinson_structured$class[i] <- "notes"
      # Check if this line ends the note
      if (grepl("\\]$", txt)) {
        in_note <- FALSE
      }
    }
  }
}

# Step 4: Create poem_number column and fill down
dickinson_structured$poem_number <- NA_character_

current_poem <- NA_character_
for (i in 1:nrow(dickinson_structured)) {
  # Extract the Roman numeral from poem_number lines
  if (!is.na(dickinson_structured$class[i]) && 
      dickinson_structured$class[i] == "poem_number") {
    current_poem <- sub("\\.$", "", dickinson_structured$text[i])
  }
  
  # Fill down for poem content (poem_number, poem_title, notes, poem)
  if (!is.na(dickinson_structured$class[i]) && 
      dickinson_structured$class[i] %in% c("poem_number", "poem_title", "notes", "poem")) {
    dickinson_structured$poem_number[i] <- current_poem
  }
}

# Step 5: Add line_number for poem lines and poem_id (first line of poem)
# Create unique poem identifier based on series + section_id + poem_number
dickinson_structured$poem_id_temp <- NA_character_
dickinson_structured$line_number <- NA_integer_

current_series <- NA_integer_
current_section <- NA_character_
current_poem_num <- NA_character_

for (i in 1:nrow(dickinson_structured)) {
  # Track current series and section
  if (!is.na(dickinson_structured$series[i])) {
    current_series <- dickinson_structured$series[i]
  }
  if (!is.na(dickinson_structured$section_id[i])) {
    current_section <- dickinson_structured$section_id[i]
  }
  
  # Track current poem number and create unique ID
  if (!is.na(dickinson_structured$class[i]) && 
      dickinson_structured$class[i] == "poem_number") {
    current_poem_num <- sub("\\.$", "", dickinson_structured$text[i])
    # Create unique poem identifier: series + section_id + poem_number
    unique_id <- paste0("Series", current_series, "|", current_section, "|", current_poem_num)
    dickinson_structured$poem_id_temp[i] <- unique_id
  }
  
  # Fill down poem_id_temp for poem content
  if (!is.na(dickinson_structured$class[i]) && 
      dickinson_structured$class[i] %in% c("poem_number", "poem_title", "notes", "poem")) {
    if (!is.na(current_series) && !is.na(current_section) && !is.na(current_poem_num)) {
      dickinson_structured$poem_id_temp[i] <- paste0("Series", current_series, "|", current_section, "|", current_poem_num)
    }
  }
}

# Now number lines within each unique poem AND capture first line as poem_id
dickinson_structured$poem_id <- NA_character_
unique_poem_ids <- unique(dickinson_structured$poem_id_temp[!is.na(dickinson_structured$poem_id_temp)])

for (poem_id_temp in unique_poem_ids) {
  # Find rows for this poem that are actual poem lines
  poem_rows <- which(dickinson_structured$poem_id_temp == poem_id_temp & 
                       !is.na(dickinson_structured$class) &
                       dickinson_structured$class == "poem")
  
  if (length(poem_rows) > 0) {
    # Number the poem lines starting from 1
    dickinson_structured$line_number[poem_rows] <- 1:length(poem_rows)
    
    # Get the first line of the poem as poem_id
    first_line <- dickinson_structured$text[poem_rows[1]]
    
    # Fill poem_id for all rows of this poem (poem_number, poem_title, notes, poem)
    all_poem_rows <- which(dickinson_structured$poem_id_temp == poem_id_temp)
    dickinson_structured$poem_id[all_poem_rows] <- first_line
  }
}

# Remove the temporary poem_id column
dickinson_structured$poem_id_temp <- NULL

# Step 6: Reorder columns to match specification
dickinson_final <- dickinson_structured[, c(
  "gutenberg_id",
  "title",
  "year",
  "author",
  "section",
  "series",
  "section_id",
  "class",
  "poem_id",
  "poem_number",
  "line_number",
  "text"
)]

# Display summary statistics
cat("\n=== Dickinson Dataset Summary ===\n")
cat("Total rows:", nrow(dickinson_final), "\n")
cat("\nSection breakdown:\n")
print(table(dickinson_final$section, useNA = "ifany"))
cat("\nClass breakdown:\n")
print(table(dickinson_final$class, useNA = "ifany"))
cat("\nNumber of poems identified:", 
    length(unique(na.omit(dickinson_final$poem_number))), "\n")

# View first few rows of each section
cat("\n=== First few rows of front_matter ===\n")
print(head(dickinson_final[dickinson_final$section == "front_matter", ], 10))

cat("\n=== First poem in contents ===\n")
print(head(dickinson_final[dickinson_final$section == "contents", ], 30))

# Save the cleaned data
write.csv(dickinson_final, "dickinson_cleaned.csv", 
          row.names = FALSE)
cat("\n=== Data saved to dickinson_cleaned.csv ===\n")

library(stringr)
library(tidytext)
library(dplyr)

tidy_dickinson <- dickinson_final%>%
  unnest_tokens(word, text)

tidy_dickinson_hsc <- tidy_dickinson %>%
filter(poem_id %in% c("To learn the transport by the pain,", "I felt a funeral in my brain,", "No rack can torture me,", "Much madness is divinest sense", "I died for beauty, but was scarce", "One need not be a chamber to be haunted,", "Because I could not stop for Death,", "The show is not the show,"))
