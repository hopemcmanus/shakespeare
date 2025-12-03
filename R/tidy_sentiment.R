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