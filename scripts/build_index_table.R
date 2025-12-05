library(dplyr)
library(readr)
library(stringr)
library(purrr)

# -------- CONFIG --------
csv_folder <- "data/plays_csv"
meta_shakespeare_path <- "data/meta_shakespeare.csv"

# -------- READ METADATA --------
meta_shakespeare <- read_csv(meta_shakespeare_path)

# -------- LIST AND READ CSV FILES --------
csv_files <- list.files(csv_folder, pattern = "\\.csv$", full.names = TRUE)

# -------- READ ALL CSVs AND ADD PLAY TITLE --------
plays_data <- csv_files %>%
  set_names() %>% 
  map_dfr(read_csv, .id = "file_name") %>%
  mutate(
    play_title = str_remove(basename(file_name), "\\.csv$")
  )

# -------- MERGE WITH METADATA --------
index_table <- plays_data %>%
  left_join(meta_shakespeare, by = c("play_title" = "short_title"))

# -------- FLAG MISSING METADATA --------
missing_meta <- index_table %>% filter(is.na(author)) %>% distinct(play_title)
if(nrow(missing_meta) > 0) {
  warning("These plays have no metadata match: ", paste(missing_meta$play_title, collapse = ", "))
}

# -------- CLEAN TEXT --------
index_table <- index_table %>%
  filter(!is.na(text)) %>%
  mutate(speaker = str_trim(speaker))

# -------- SAVE INDEX TABLE --------
write_csv(index_table, "data/index_table.csv")_
