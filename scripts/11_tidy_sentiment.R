library(tidyverse)
library(tidytext)
library(here)

# Configuration
INPUT_DIR_CLEANED <- here("data", "cleaned")
INPUT_DIR_METADATA <- here("data", "metadata")
OUTPUT_DIR_PLOTS <- here("plots", "sentiment")

# Analysis settings
SENTIMENT_LEXICON <- "bing"  # Options: "bing", "afinn", "nrc"

# Single play analysis
SINGLE_PLAY <- "Hamlet"  # Change to analyze different play

# Filter settings for aggregate analysis
FILTER_BY <- "all"  # Options: "gutenberg_id", "genre", "short_title", "period", "romance", "problem_play", "roman", "all"
FILTER_VALUES <- NULL

# Example plays for comparison (one per genre)
EXAMPLE_PLAYS <- list(
  Comedy = "Twelfth Night",      # gutenberg_id 1526
  History = "Henry IV, Part 1",  # gutenberg_id 1516
  Tragedy = "Hamlet"             # gutenberg_id 1524
)

# Create output directory
dir.create(OUTPUT_DIR_PLOTS, recursive = TRUE, showWarnings = FALSE)

message("\n", strrep("=", 70))
message("SHAKESPEARE SENTIMENT ANALYSIS")
message(strrep("=", 70))
message("Input: ", INPUT_DIR_CLEANED)
message("Output: ", OUTPUT_DIR_PLOTS)
message("Lexicon: ", SENTIMENT_LEXICON)
message(strrep("=", 70), "\n")

# Load data
message("Loading plays data...")
all_plays <- read_csv(
  file.path(INPUT_DIR_CLEANED, "all_shakespeare.csv"),
  show_col_types = FALSE
) %>%
  filter(class == "dialogue")

message("✓ Loaded ", format(nrow(all_plays), big.mark = ","), " dialogue lines\n")

# Part 1: Single Play Sentiment Analysis
message("PART 1: Single Play Sentiment Analysis (", SINGLE_PLAY, ")")
message(strrep("-", 70))

# Filter for single play
single_play_data <- all_plays %>%
  filter(short_title == SINGLE_PLAY) %>%
  mutate(
    # Clean act and scene
    act_clean = str_trim(str_remove(act, "\\.$")),
    scene_clean = str_trim(str_remove(scene, "\\.$")),
    # Extract Roman numerals
    act_num = str_extract(act_clean, "[IVXLCM]+$"),
    scene_num = str_extract(scene_clean, "[IVXLCM]+$"),
    # Combine for act-scene label
    act_scene = paste(act_num, scene_num, sep = ".")
  ) %>%
  # Tokenize
  unnest_tokens(word, text) %>%
  # Join sentiment
  inner_join(get_sentiments(SENTIMENT_LEXICON), by = "word")

message("✓ Matched ", format(nrow(single_play_data), big.mark = ","), " sentiment words")

# Calculate sentiment by act-scene
if (SENTIMENT_LEXICON == "bing") {
  single_play_sentiment <- single_play_data %>%
    count(act_scene, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(sentiment_score = positive - negative)
} else if (SENTIMENT_LEXICON == "afinn") {
  single_play_sentiment <- single_play_data %>%
    group_by(act_scene) %>%
    summarise(sentiment_score = sum(value), .groups = "drop")
}

message("✓ Calculated sentiment for ", nrow(single_play_sentiment), " act-scenes\n")

# Plot: Single play sentiment by act-scene
message("Creating single play sentiment plot...")

p_single <- ggplot(single_play_sentiment, aes(x = act_scene, y = sentiment_score)) +
  geom_col(aes(fill = sentiment_score)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "gray30") +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#ffffbf",
    high = "#4575b4",
    midpoint = 0,
    name = "Sentiment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    x = "Act - Scene",
    y = "Sentiment Score",
    title = paste("Sentiment Analysis:", SINGLE_PLAY),
    subtitle = paste("Using", SENTIMENT_LEXICON, "lexicon | Positive (blue) vs Negative (red)")
  )

ggsave(
  filename = paste0("sentiment_", str_to_lower(str_replace_all(SINGLE_PLAY, "[^a-z0-9]+", "_")), ".png"),
  plot = p_single,
  path = OUTPUT_DIR_PLOTS,
  width = 12,
  height = 6,
  dpi = 600,
  bg = "white"
)

message("✓ Saved single play sentiment plot\n")

# Part 2: Genre Aggregate vs Example Plays
message("PART 2: Genre Aggregate Sentiment vs Example Plays")
message(strrep("-", 70))

# Tokenize all plays
message("Tokenizing all plays for aggregate analysis...")

all_plays_tokenized <- all_plays %>%
  mutate(
    act_num = str_extract(str_trim(str_remove(act, "\\.$")), "[IVXLCM]+$")
  ) %>%
  filter(!is.na(act_num)) %>%
  unnest_tokens(word, text)

message("✓ Tokenized ", format(nrow(all_plays_tokenized), big.mark = ","), " words")

# Calculate genre aggregate sentiment (normalized per 100 words)
message("Calculating genre aggregate sentiment...")

genre_sentiment <- all_plays_tokenized %>%
  group_by(gutenberg_id, genre, act_num) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  inner_join(get_sentiments(SENTIMENT_LEXICON), by = "word")

if (SENTIMENT_LEXICON == "bing") {
  genre_sentiment <- genre_sentiment %>%
    count(gutenberg_id, genre, act_num, sentiment, total_words) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(sentiment_score = (positive - negative) / total_words * 100)
} else if (SENTIMENT_LEXICON == "afinn") {
  genre_sentiment <- genre_sentiment %>%
    group_by(gutenberg_id, genre, act_num, total_words) %>%
    summarise(sentiment_sum = sum(value), .groups = "drop") %>%
    mutate(sentiment_score = sentiment_sum / total_words * 100)
}

# Aggregate by genre
genre_aggregate <- genre_sentiment %>%
  group_by(genre, act_num) %>%
  summarise(sentiment_score = mean(sentiment_score), .groups = "drop") %>%
  mutate(
    type = "Aggregate",
    play = NA_character_
  )

message("✓ Calculated aggregate sentiment for ", n_distinct(genre_aggregate$genre), " genres")

# Calculate example play sentiment (normalized per 100 words)
message("Calculating example play sentiment...")

example_sentiment <- all_plays_tokenized %>%
  filter(short_title %in% unlist(EXAMPLE_PLAYS)) %>%
  mutate(
    genre = case_when(
      short_title == EXAMPLE_PLAYS$Comedy ~ "Comedy",
      short_title == EXAMPLE_PLAYS$History ~ "History",
      short_title == EXAMPLE_PLAYS$Tragedy ~ "Tragedy",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(gutenberg_id, short_title, genre, act_num) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  inner_join(get_sentiments(SENTIMENT_LEXICON), by = "word")

if (SENTIMENT_LEXICON == "bing") {
  example_sentiment <- example_sentiment %>%
    count(genre, short_title, act_num, sentiment, total_words) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(sentiment_score = (positive - negative) / total_words * 100)
} else if (SENTIMENT_LEXICON == "afinn") {
  example_sentiment <- example_sentiment %>%
    group_by(genre, short_title, act_num, total_words) %>%
    summarise(sentiment_sum = sum(value), .groups = "drop") %>%
    mutate(sentiment_score = sentiment_sum / total_words * 100)
}

example_plays_data <- example_sentiment %>%
  mutate(
    type = "Example",
    play = short_title
  ) %>%
  select(genre, act_num, sentiment_score, type, play)

message("✓ Calculated sentiment for ", n_distinct(example_plays_data$play), " example plays\n")

# Combine aggregate and examples
combined_sentiment <- bind_rows(
  genre_aggregate,
  example_plays_data
) %>%
  mutate(
    facet_label = if_else(
      type == "Example",
      paste0(genre, " - ", play),
      paste0(genre, " - Aggregate")
    ),
    act_num = factor(act_num, levels = c("I", "II", "III", "IV", "V"))
  )

# Plot: Genre aggregate vs example plays
message("Creating genre comparison plot...")

p_combined <- ggplot(combined_sentiment, aes(x = act_num, y = sentiment_score, fill = sentiment_score)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "gray30") +
  facet_wrap(~facet_label, ncol = 2, scales = "free_x") +
  scale_fill_gradient2(
    low = "#d73027",      # Red for negative
    mid = "#ffffbf",      # Light yellow for neutral
    high = "#4575b4",     # Blue for positive
    midpoint = 0,
    name = "Sentiment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 12,
      margin = margin(b = 10)
    ),
    strip.background = element_rect(
      fill = "gray95",
      color = NA
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.spacing = unit(1.5, "lines"),
    axis.text.x = element_text(size = 11, color = "gray20"),
    axis.text.y = element_text(size = 10, color = "gray20"),
    axis.title = element_text(size = 12, face = "bold", color = "gray10"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "gray30",
      hjust = 0,
      margin = margin(b = 15)
    ),
    plot.caption = element_text(
      size = 9,
      color = "gray50",
      hjust = 0,
      margin = margin(t = 15)
    ),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    x = "Act",
    y = "Sentiment Density (per 100 words)",
    title = "Sentiment Patterns Across Shakespeare's Dramatic Structure",
    subtitle = paste("Blue indicates positive sentiment, red indicates negative sentiment | Using", SENTIMENT_LEXICON, "lexicon"),
    caption = "Data: Project Gutenberg | Net sentiment words per 100 total words"
  )

ggsave(
  filename = "genre_aggregate_vs_examples.png",
  plot = p_combined,
  path = OUTPUT_DIR_PLOTS,
  width = 13,
  height = 10,
  dpi = 600,
  bg = "white"
)

message("✓ Saved genre comparison plot\n")

# Save analysis results
message("Saving analysis results...")

write_csv(
  single_play_sentiment,
  file.path(OUTPUT_DIR_PLOTS, paste0("sentiment_", str_to_lower(str_replace_all(SINGLE_PLAY, "[^a-z0-9]+", "_")), ".csv"))
)

write_csv(
  combined_sentiment,
  file.path(OUTPUT_DIR_PLOTS, "genre_aggregate_sentiment.csv")
)

message("✓ Saved CSV files\n")

# Summary
message(strrep("=", 70))
message("SENTIMENT ANALYSIS SUMMARY")
message(strrep("=", 70))
message("\nAnalyses completed:")
message("  1. Single play: ", SINGLE_PLAY)
message("  2. Genre aggregates with examples")
message("\nPlots created:")
message("  - sentiment_", str_to_lower(str_replace_all(SINGLE_PLAY, "[^a-z0-9]+", "_")), ".png")
message("  - genre_aggregate_vs_examples.png")
message("\nSentiment lexicon: ", SENTIMENT_LEXICON)
message("\nExample plays:")
message("  Comedy:  ", EXAMPLE_PLAYS$Comedy)
message("  History: ", EXAMPLE_PLAYS$History)
message("  Tragedy: ", EXAMPLE_PLAYS$Tragedy)
message(strrep("=", 70))
message("Files saved to: ", OUTPUT_DIR_PLOTS)
message(strrep("=", 70), "\n")

message("✓ Sentiment analysis complete!\n")