library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2) 

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


# Create a combined act-scene index
shakespeare_sentiment <- tidy_hsc %>%
  filter(short_title != "Tempest") %>% 
  mutate(
    # Clean act and scene
    act_clean   = str_trim(str_remove(act, "\\.$")),
    scene_clean = str_trim(str_remove(scene, "\\.$")),
    
    # Extract full Roman numerals at the end
    act_num     = str_extract(act_clean, "[IVXLCM]+$"),
    scene_num   = str_extract(scene_clean, "[IVXLCM]+$"),
    
    # Combine
    act_scene   = paste(act_num, scene_num, sep = ".")
  ) %>%
  inner_join(get_sentiments("bing"), relationship = "many-to-many") %>%
  count(short_title, act_scene, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Plot
p <- ggplot(shakespeare_sentiment, aes(act_scene, sentiment, fill = short_title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~short_title, ncol = 2, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)) +
  labs(
    x = "Act - Scene",
    y = "Sentiment Score",
    title = "Sentiment Analysis by Act and Scene in Shakespeare Plays"
  )


# 2. Save the plot inside the "plots" folder
ggsave(
  filename = "bing_sentiments_hsc.png",
  plot = p,
  path = "plots",   # save inside the "plots" folder
  width = 10,
  height = 12,
  dpi = 600
)

# Aggregate plays by genre
genre_sentiment <- tidy_shakespeare %>%
  mutate(
    # Remove trailing dots and spaces
    act_clean = str_trim(str_remove(act, "\\.$")),
    # Extract full Roman numeral at end
    act_num   = str_extract(act_clean, "[IVXLCM]+$")
  ) %>%
  inner_join(get_sentiments("bing"), relationship = "many-to-many") %>%
  count(genre, act_num, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Plot
p <- ggplot(genre_sentiment, aes(act_num, sentiment, fill = genre)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~genre, ncol = 1, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)) +
  labs(
    x = "Act - Scene",
    y = "Sentiment Score",
    title = "Sentiment Analysis by Act and Scene in Shakespeare Plays Aggregated by Genre"
  )


# 2. Save the plot inside the "plots" folder
ggsave(
  filename = "bing_sentiments_genre.png",
  plot = p,
  path = "plots",   # save inside the "plots" folder
  width = 10,
  height = 12,
  dpi = 600
)

# sentiment patterns across acts in Shakespeare's plays, measuring the density of positive versus negative vocabulary (per 100 words) to compare aggregate genre trends against individual exemplar works (Twelfth Night, Henry IV Part 1, and Hamlet)
# 1. Genre aggregate sentiment by act (normalized)
genre_sentiment <- tidy_shakespeare %>%
  mutate(
    act_num = str_extract(str_trim(str_remove(act, "\\.$")), "[IVXLCM]+$")
  ) %>%
  group_by(gutenberg_id, genre, act_num) %>%
  mutate(total_words = n()) %>%  # Count total words per act per play
  ungroup() %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(gutenberg_id, genre, act_num, sentiment, total_words) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(
    sentiment = (positive - negative) / total_words * 100  # Sentiment per 100 words
  ) %>%
  group_by(genre, act_num) %>%
  summarise(sentiment = mean(sentiment), .groups = "drop") %>%  # Mean of normalized values
  mutate(
    type = "Aggregate",
    play = NA_character_
  )

# 2. Example play sentiment by act (normalized)
example_plays <- tidy_shakespeare %>%
  filter(gutenberg_id %in% c(1526, 1516, 1524)) %>%
  mutate(
    act_num = str_extract(str_trim(str_remove(act, "\\.$")), "[IVXLCM]+$"),
    genre = case_when(
      short_title == "Twelfth Night" ~ "Comedy",
      short_title == "Henry IV, Part 1" ~ "History",
      short_title == "Hamlet" ~ "Tragedy",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(gutenberg_id, short_title, genre, act_num) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(genre, short_title, act_num, sentiment, total_words) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(
    sentiment = (positive - negative) / total_words * 100,
    type = "Example",
    play = short_title
  ) %>%
  select(genre, act_num, sentiment, type, play)

# 3. Combine aggregate and example
combined_sentiment <- bind_rows(
  genre_sentiment,
  example_plays
)

# 4. Create facet label with play name for examples
combined_sentiment <- combined_sentiment %>%
  mutate(
    facet_label = if_else(
      type == "Example",
      paste0(genre, " - ", play),
      paste(genre, " - Aggregate")
    ),
    act_num = factor(act_num, levels = c("I", "II", "III", "IV", "V"))
  )

# 5. Plot with same scales
p_combined <- ggplot(combined_sentiment, aes(x = act_num, y = sentiment, fill = genre)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~facet_label, ncol = 2, scales = "free_x") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 11)
  ) +
  labs(
    x = "Act",
    y = "Sentiment (per 100 words)",
    title = "Shakespeare Sentiment Analysis: Genre Averages vs Example Plays"
  )

# 6. Save plot
ggsave(
  filename = "plots/combined_genre_example_by_act.png",
  plot = p_combined,
  width = 12,
  height = 10,
  dpi = 600
)

#Sentiment patterns across acts in Shakespeare's plays, comparing aggregate genre trends against exemplar works (Twelfth Night, Henry IV Part 1, Hamlet). Blue bars indicate positive sentiment, red bars indicate negative sentiment, with color intensity reflecting magnitude.

# 1. Genre aggregate sentiment by act (normalized)
genre_sentiment <- tidy_shakespeare %>%
  mutate(
    act_num = str_extract(str_trim(str_remove(act, "\\.$")), "[IVXLCM]+$")
  ) %>%
  group_by(gutenberg_id, genre, act_num) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(gutenberg_id, genre, act_num, sentiment, total_words) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(
    sentiment = (positive - negative) / total_words * 100
  ) %>%
  group_by(genre, act_num) %>%
  summarise(sentiment = mean(sentiment), .groups = "drop") %>%
  mutate(
    type = "Aggregate",
    play = NA_character_
  )

# 2. Example play sentiment by act (normalized)
example_plays <- tidy_shakespeare %>%
  filter(gutenberg_id %in% c(1526, 1516, 1524)) %>%
  mutate(
    act_num = str_extract(str_trim(str_remove(act, "\\.$")), "[IVXLCM]+$"),
    genre = case_when(
      short_title == "Twelfth Night" ~ "Comedy",
      short_title == "Henry IV, Part 1" ~ "History",
      short_title == "Hamlet" ~ "Tragedy",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(gutenberg_id, short_title, genre, act_num) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(genre, short_title, act_num, sentiment, total_words) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(
    sentiment = (positive - negative) / total_words * 100,
    type = "Example",
    play = short_title
  ) %>%
  select(genre, act_num, sentiment, type, play)

# 3. Combine
combined_sentiment <- bind_rows(
  genre_sentiment,
  example_plays
)

# 4. Create facet labels
combined_sentiment <- combined_sentiment %>%
  mutate(
    facet_label = if_else(
      type == "Example",
      paste0(genre, " - ", play),
      paste(genre, " - Aggregate")
    ),
    act_num = factor(act_num, levels = c("I", "II", "III", "IV", "V"))
  )

# 5. Plot with gradient coloring based on sentiment value
p_combined <- ggplot(combined_sentiment, aes(x = act_num, y = sentiment, fill = sentiment)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "gray30") +
  facet_wrap(~facet_label, ncol = 2, scales = "free_x") +
  
  # Gradient from red (negative) through white (neutral) to blue (positive)
  scale_fill_gradient2(
    low = "#d73027",      # Red for negative
    mid = "#ffffbf",      # Light yellow for neutral
    high = "#4575b4",     # Blue for positive
    midpoint = 0,
    name = "Sentiment"
  ) +
  
  # Clean theme
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
    subtitle = "Blue indicates positive sentiment, red indicates negative sentiment, with color intensity showing magnitude",
    caption = "Data: Project Gutenberg | Analysis: Bing Sentiment Lexicon | Net sentiment words per 100 total words"
  )

# 6. Save
ggsave(
  filename = "plots/combined_genre_example_by_act.png",
  plot = p_combined,
  width = 13,
  height = 10,
  dpi = 600,
  bg = "white"
)
