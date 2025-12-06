library(tidyverse)
library(tidytext)
library(here)

# Configuration
INPUT_DIR_FILTERED <- here("data", "processed", "filtered")
OUTPUT_DIR_PLOTS <- here("plots")

# Create output directory
dir.create(OUTPUT_DIR_PLOTS, recursive = TRUE, showWarnings = FALSE)

message("\n", strrep("=", 70))
message("SHAKESPEARE FREQUENCY ANALYSIS")
message(strrep("=", 70))
message("Input: ", INPUT_DIR_FILTERED)
message("Output: ", OUTPUT_DIR_PLOTS)
message(strrep("=", 70), "\n")

# Load filtered HSC plays data (dialogue and directions only, no stop words)
message("Loading HSC plays (filtered tokens)...")
hsc_tokens <- read_csv(
  file.path(INPUT_DIR_FILTERED, "by_subset", "hsc_contents_filtered.csv"),
  show_col_types = FALSE
)

message("✓ Loaded ", format(nrow(hsc_tokens), big.mark = ","), " tokens from ", 
        n_distinct(hsc_tokens$short_title), " plays\n")

# Term frequency analysis
message("Calculating term frequencies...")

play_words <- hsc_tokens %>%
  group_by(gutenberg_id, short_title, word) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))

# Calculate total words per play
total_words <- play_words %>%
  group_by(gutenberg_id, short_title) %>%
  summarise(total = sum(n), .groups = "drop")

# Join totals
play_words <- play_words %>%
  left_join(total_words, by = c("gutenberg_id", "short_title"))

message("✓ Calculated frequencies for ", nrow(play_words), " unique word-play combinations\n")

# Plot 1: Term frequency distribution
message("Creating term frequency distribution plot...")

p1 <- ggplot(play_words, aes(n/total, fill = short_title)) +
  geom_histogram(show.legend = FALSE, bins = 30) +
  xlim(NA, 0.0009) +
  facet_wrap(~short_title, ncol = 2, scales = "free_y") +
  labs(
    x = "Term Frequency (n/total)",
    y = "Count",
    title = "Term Frequency Distribution by Play",
    subtitle = "HSC prescribed Shakespeare plays"
  ) +
  theme_minimal()

ggsave(
  filename = "term_frequency_distribution.png",
  plot = p1,
  path = OUTPUT_DIR_PLOTS,
  width = 10,
  height = 12,
  dpi = 600
)

message("✓ Saved: term_frequency_distribution.png\n")

# Zipf's law analysis
message("Calculating Zipf's law rankings...")

freq_by_rank <- play_words %>%
  group_by(short_title) %>%
  mutate(
    rank = row_number(),
    term_frequency = n / total
  ) %>%
  ungroup()

message("✓ Ranked ", nrow(freq_by_rank), " terms\n")

# Plot 2: Zipf's law
message("Creating Zipf's law plot...")

p2 <- freq_by_rank %>%
  ggplot(aes(rank, term_frequency, colour = short_title)) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Rank (log scale)",
    y = "Term Frequency (log scale)",
    title = "Zipf's Law in Shakespeare",
    subtitle = "Word frequency vs rank follows power law distribution"
  ) +
  theme_minimal()

ggsave(
  filename = "zipfs_law.png",
  plot = p2,
  path = OUTPUT_DIR_PLOTS,
  width = 10,
  height = 12,
  dpi = 600
)

message("✓ Saved: zipfs_law.png\n")

# Calculate Zipf's law coefficient
message("Fitting Zipf's law model...")

rank_subset <- freq_by_rank %>%
  filter(rank < 500, rank > 10)

zipf_model <- lm(log10(term_frequency) ~ log10(rank), data = rank_subset)
zipf_coef <- coef(zipf_model)

message("Zipf's law coefficients:")
message("  Intercept: ", round(zipf_coef[1], 4))
message("  Slope:     ", round(zipf_coef[2], 4))
message("")

# Plot 3: Zipf's law with fitted line
message("Creating Zipf's law plot with fitted model...")

p3 <- freq_by_rank %>%
  ggplot(aes(rank, term_frequency, colour = short_title)) +
  geom_abline(
    intercept = zipf_coef[1],
    slope = zipf_coef[2],
    colour = "gray50",
    linetype = 2,
    linewidth = 1
  ) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Rank (log scale)",
    y = "Term Frequency (log scale)",
    title = "Zipf's Law with Fitted Exponent",
    subtitle = sprintf("log(frequency) = %.2f + %.2f × log(rank)", zipf_coef[1], zipf_coef[2])
  ) +
  theme_minimal()

ggsave(
  filename = "zipfs_law_fitted.png",
  plot = p3,
  path = OUTPUT_DIR_PLOTS,
  width = 10,
  height = 12,
  dpi = 600
)

message("✓ Saved: zipfs_law_fitted.png\n")

# TF-IDF analysis
message("Calculating TF-IDF scores...")

play_tf_idf <- play_words %>%
  bind_tf_idf(word, short_title, n) %>%
  arrange(desc(tf_idf))

message("✓ Calculated TF-IDF for ", nrow(play_tf_idf), " terms\n")

# Display top TF-IDF terms
message("Top 20 TF-IDF terms:")
print(
  play_tf_idf %>%
    select(short_title, word, n, tf, idf, tf_idf) %>%
    head(20)
)
message("")

# Plot 4: Top TF-IDF terms by play
message("Creating TF-IDF plot...")

p4 <- play_tf_idf %>%
  group_by(short_title) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = short_title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~short_title, ncol = 2, scales = "free") +
  labs(
    x = "TF-IDF",
    y = NULL,
    title = "Highest TF-IDF Words by Play",
    subtitle = "Words that are distinctive to each play"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 10)
  )

ggsave(
  filename = "highest_tf_idf_words.png",
  plot = p4,
  path = OUTPUT_DIR_PLOTS,
  width = 10,
  height = 12,
  dpi = 600
)

message("✓ Saved: highest_tf_idf_words.png\n")

# Save analysis results
message("Saving analysis results...")

# Save frequency data
write_csv(
  play_words,
  file.path(OUTPUT_DIR_PLOTS, "word_frequencies.csv")
)

# Save TF-IDF data
write_csv(
  play_tf_idf,
  file.path(OUTPUT_DIR_PLOTS, "tf_idf_scores.csv")
)

# Save Zipf model summary
zipf_summary <- tibble(
  model = "Zipf's Law",
  intercept = zipf_coef[1],
  slope = zipf_coef[2],
  r_squared = summary(zipf_model)$r.squared,
  plays_analysed = n_distinct(freq_by_rank$short_title),
  terms_analysed = nrow(rank_subset)
)

write_csv(
  zipf_summary,
  file.path(OUTPUT_DIR_PLOTS, "zipf_model_summary.csv")
)

message("✓ Saved analysis results to plots/\n")

# Summary statistics
message(strrep("=", 70))
message("FREQUENCY ANALYSIS SUMMARY")
message(strrep("=", 70))
message("Plays analysed:        ", n_distinct(play_words$short_title))
message("Total tokens:          ", format(nrow(hsc_tokens), big.mark = ","))
message("Unique words:          ", format(n_distinct(play_words$word), big.mark = ","))
message("Plots created:         4")
message("  - term_frequency_distribution.png")
message("  - zipfs_law.png")
message("  - zipfs_law_fitted.png")
message("  - highest_tf_idf_words.png")
message(strrep("=", 70))
message("Files saved to: ", OUTPUT_DIR_PLOTS)
message(strrep("=", 70), "\n")

message("✓ Frequency analysis complete!\n")

# Display vocabulary statistics by play
message("Vocabulary statistics by play:")
vocab_stats <- play_words %>%
  group_by(short_title) %>%
  summarise(
    total_words = sum(n),
    unique_words = n(),
    avg_frequency = mean(n),
    .groups = "drop"
  ) %>%
  arrange(desc(total_words))

print(vocab_stats)