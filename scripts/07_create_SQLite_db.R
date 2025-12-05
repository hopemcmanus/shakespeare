library(tidyverse)
library(DBI)
library(RSQLite)
library(here)

# Configuration
INPUT_DIR_CLEANED <- here("data", "cleaned")
INPUT_DIR_METADATA <- here("data", "metadata")
OUTPUT_DB_PATH <- here("data", "shakespeare.sqlite")

message("\n", strrep("=", 70))
message("SHAKESPEARE SQLITE DATABASE EXPORT")
message(strrep("=", 70))
message("Input CSV: ", INPUT_DIR_CLEANED)
message("Output DB:  ", OUTPUT_DB_PATH)
message(strrep("=", 70), "\n")

# Remove existing database to start fresh
if (file.exists(OUTPUT_DB_PATH)) {
  message("Removing existing database...")
  file.remove(OUTPUT_DB_PATH)
}

# Connect to SQLite database
message("Connecting to SQLite database...")
con <- dbConnect(SQLite(), OUTPUT_DB_PATH)

message("✓ Database connection established\n")

# Load and insert metadata table first
message("Creating metadata table...")
meta_shakespeare <- read_csv(
  file.path(INPUT_DIR_METADATA, "meta_shakespeare.csv"),
  show_col_types = FALSE
)

dbWriteTable(con, "metadata", meta_shakespeare, overwrite = TRUE)

# Create indexes on metadata table
dbExecute(con, "CREATE INDEX idx_metadata_gutenberg_id ON metadata(gutenberg_id)")
dbExecute(con, "CREATE INDEX idx_metadata_short_title ON metadata(short_title)")
dbExecute(con, "CREATE INDEX idx_metadata_genre ON metadata(genre)")
if ("period" %in% names(meta_shakespeare)) {
  dbExecute(con, "CREATE INDEX idx_metadata_period ON metadata(period)")
}

message("✓ Metadata table created with indexes\n")

# Create main plays table (combined data)
message("Creating main plays table...")
all_shakespeare <- read_csv(
  file.path(INPUT_DIR_CLEANED, "all_shakespeare.csv"),
  show_col_types = FALSE
)

dbWriteTable(con, "plays", all_shakespeare, overwrite = TRUE)

# Create indexes on plays table for faster queries
message("Creating indexes on plays table...")
dbExecute(con, "CREATE INDEX idx_plays_gutenberg_id ON plays(gutenberg_id)")
dbExecute(con, "CREATE INDEX idx_plays_short_title ON plays(short_title)")
dbExecute(con, "CREATE INDEX idx_plays_genre ON plays(genre)")
dbExecute(con, "CREATE INDEX idx_plays_section ON plays(section)")
dbExecute(con, "CREATE INDEX idx_plays_class ON plays(class)")
dbExecute(con, "CREATE INDEX idx_plays_character ON plays(character)")
dbExecute(con, "CREATE INDEX idx_plays_act ON plays(act)")
dbExecute(con, "CREATE INDEX idx_plays_scene ON plays(scene)")

message("✓ Main plays table created with indexes\n")

# Create individual play tables
message("Creating individual play tables...")

csv_files <- list.files(
  INPUT_DIR_CLEANED, 
  pattern = "\\.csv$", 
  full.names = TRUE
) %>%
  setdiff(file.path(INPUT_DIR_CLEANED, "all_shakespeare.csv"))

play_metadata <- tibble()
table_count <- 0

for (csv_file in csv_files) {
  df <- read_csv(csv_file, show_col_types = FALSE)
  
  # Generate table name from short_title
  if ("short_title" %in% colnames(df) && !is.na(unique(df$short_title)[1])) {
    table_name <- unique(df$short_title)[1] %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_remove("^_|_$")
  } else {
    table_name <- tools::file_path_sans_ext(basename(csv_file))
  }
  
  # Write table
  dbWriteTable(con, table_name, df, overwrite = TRUE)
  
  # Collect play statistics
  play_stats <- tibble(
    table_name = table_name,
    short_title = if("short_title" %in% names(df)) unique(df$short_title)[1] else NA_character_,
    gutenberg_id = if("gutenberg_id" %in% names(df)) unique(df$gutenberg_id)[1] else NA_integer_,
    genre = if("genre" %in% names(df)) unique(df$genre)[1] else NA_character_,
    year = if("year" %in% names(df)) unique(df$year)[1] else NA_integer_,
    total_rows = nrow(df),
    dialogue_rows = sum(df$class == "dialogue", na.rm = TRUE),
    direction_rows = sum(df$class == "directions", na.rm = TRUE),
    unique_characters = n_distinct(df$character[!is.na(df$character)]),
    unique_acts = n_distinct(df$act[!is.na(df$act)]),
    unique_scenes = n_distinct(df$scene[!is.na(df$scene)])
  )
  
  play_metadata <- bind_rows(play_metadata, play_stats)
  
  table_count <- table_count + 1
  if (table_count %% 10 == 0) {
    message("  Created ", table_count, "/", length(csv_files), " play tables...")
  }
}

message("✓ Created ", table_count, " individual play tables\n")

# Create play_metadata table in database
message("Creating play statistics table...")
dbWriteTable(con, "play_statistics", play_metadata, overwrite = TRUE)
dbExecute(con, "CREATE INDEX idx_play_statistics_short_title ON play_statistics(short_title)")
message("✓ Play statistics table created\n")

# Create views for common queries
message("Creating database views...")

# View 1: Dialogue only
dbExecute(con, "
  CREATE VIEW IF NOT EXISTS dialogue_only AS
  SELECT * FROM plays WHERE class = 'dialogue'
")

# View 2: Tragedies
dbExecute(con, "
  CREATE VIEW IF NOT EXISTS tragedies AS
  SELECT * FROM plays WHERE genre = 'Tragedy'
")

# View 3: Comedies
dbExecute(con, "
  CREATE VIEW IF NOT EXISTS comedies AS
  SELECT * FROM plays WHERE genre = 'Comedy'
")

# View 4: Histories
dbExecute(con, "
  CREATE VIEW IF NOT EXISTS histories AS
  SELECT * FROM plays WHERE genre = 'History'
")

# View 5: Character line counts
dbExecute(con, "
  CREATE VIEW IF NOT EXISTS character_line_counts AS
  SELECT 
    short_title,
    character,
    COUNT(*) as line_count
  FROM plays
  WHERE class = 'dialogue' AND character IS NOT NULL
  GROUP BY short_title, character
  ORDER BY short_title, line_count DESC
")

message("✓ Created 5 database views\n")

# Get database statistics
message("Gathering database statistics...")

tables <- dbListTables(con)
db_stats <- tibble(
  table_name = tables,
  row_count = map_int(tables, ~dbGetQuery(con, paste0("SELECT COUNT(*) FROM ", .x))[[1]])
) %>%
  arrange(desc(row_count))

# Summary report
message(strrep("=", 70))
message("DATABASE EXPORT SUMMARY")
message(strrep("=", 70))
message("Database path:       ", OUTPUT_DB_PATH)
message("Database size:       ", format(file.size(OUTPUT_DB_PATH) / 1024^2, digits = 2), " MB")
message("Total tables:        ", length(tables))
message("  - metadata:        1 table")
message("  - plays (main):    1 table")
message("  - play_statistics: 1 table")
message("  - individual plays:", table_count, " tables")
message("Total views:         5")
message("Total rows in main:  ", format(nrow(all_shakespeare), big.mark = ","))
message(strrep("=", 70), "\n")

message("Tables in database:")
print(db_stats %>% head(15))
message("")

# Example queries
message("Example SQL queries:")
message(strrep("-", 70))
message("
# Get all Hamlet dialogue:
SELECT * FROM hamlet WHERE class = 'dialogue'

# Get Hamlet's lines across all plays:
SELECT * FROM plays WHERE character = 'HAMLET'

# Count lines by character in Hamlet:
SELECT character, COUNT(*) as lines 
FROM hamlet 
WHERE class = 'dialogue' 
GROUP BY character 
ORDER BY lines DESC

# Get all tragedy dialogue:
SELECT * FROM tragedies WHERE class = 'dialogue'

# Character statistics across all plays:
SELECT * FROM character_line_counts WHERE short_title = 'Hamlet'
")
message(strrep("-", 70), "\n")

# Save database info to metadata
db_info <- tibble(
  database_path = OUTPUT_DB_PATH,
  database_size_mb = file.size(OUTPUT_DB_PATH) / 1024^2,
  created_timestamp = Sys.time(),
  total_tables = length(tables),
  total_rows = nrow(all_shakespeare),
  total_plays = table_count
)

db_info_file <- file.path(INPUT_DIR_METADATA, "database_info.csv")
write_csv(db_info, db_info_file)
message("✓ Saved database info: ", basename(db_info_file))

# Disconnect
dbDisconnect(con)
message("✓ Database connection closed\n")

message("✓ SQLite export complete!\n")

# Display connection instructions
message("To connect to the database in R:")
message('  con <- dbConnect(RSQLite::SQLite(), "', OUTPUT_DB_PATH, '")')
message('  dbListTables(con)')
message('  dbGetQuery(con, "SELECT * FROM metadata LIMIT 5")')
message('  dbDisconnect(con)')
message("")