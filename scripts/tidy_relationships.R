library(dplyr)
library(tidytext)

shakespeare_bigrams <- all_shakespeare %>%
  filter(gutenberg_id %in% c("1516", "1519", "1522", "1524", "1520", "1523", "1503", "1540", "1531", "1515"), section == "contents")%>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

shakespeare_bigrams

library(tidyr)

bigrams_separated <- shakespeare_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words_custom$word) %>%
  filter(!word2 %in% stop_words_custom$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(short_title, word1, word2, sort = TRUE)

bigram_counts

#Bigrams without stop-words
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

#bigram tf-idf
bigram_tf_idf <- bigrams_united %>%
  count(short_title, bigram) %>%
  bind_tf_idf(bigram, short_title, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

library(forcats)

p <- bigram_tf_idf %>%
  group_by(short_title) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = short_title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~short_title, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

ggsave(
  filename = "highest_tf-idf_bigram.png",
  plot = p,
  path = "plots",   # save inside the "plots" folder
  width = 10,
  height = 12,
  dpi = 600
)

library(igraph)
library(ggraph)

# Get unique titles
titles <- unique(bigram_counts$short_title)

# Create a graph for each play
for (play in titles) {
  bigram_graph <- bigram_counts %>%
    filter(short_title == play, n > 2) %>%
    select(word1, word2, n) %>%  # Remove title column before creating graph
    graph_from_data_frame()
  
  # Plot the graph
  set.seed(2020)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  p <- ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    labs(title = play) +
    theme_void()
  
  print(p)
  
  # Save each plot
  ggsave(paste0("bigram_network_", gsub(" ", "_", play), ".png"), 
         plot = p,  path = "plots", width = 10, height = 8, dpi = 300)
  
  cat("Saved graph for:", play, "\n")
}

#widyr
library(dplyr)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)

# Step 1: Prepare data from tidy_hsc
shakespeare_words <- tidy_hsc %>%
  filter(!word %in% stop_words_custom$word) %>%
  select(short_title, act, scene, word)

# Step 2: Unique titles
titles <- unique(shakespeare_words$short_title)

# -----------------------------
# SCENE-LEVEL NETWORKS
# -----------------------------
for (play in titles) {
  
  play_data <- shakespeare_words %>%
    filter(short_title == play, !is.na(scene))
  
  # Count word frequency and filter infrequent words
  word_cors_scene <- play_data %>%
    add_count(word) %>% 
    filter(n >= 10) %>% 
    pairwise_cor(word, scene, sort = TRUE, method = "pearson")
  
  if (nrow(word_cors_scene) == 0) {
    message("No scene correlations for: ", play)
    next
  }
  
  word_cors_filtered <- word_cors_scene %>%
    filter(correlation > 0.5)
  
  if (nrow(word_cors_filtered) == 0) {
    message("No scene edges above threshold for: ", play)
    next
  }
  
  set.seed(2016)
  
  p <- word_cors_filtered %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), edge_width = 0.3, show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    labs(title = paste("Word Correlations by Scene:", play)) +
    theme_void()
  
  print(p)
  
  ggsave(paste0("scene_correlations_", gsub(" ", "_", play), ".png"),
         plot = p, path = "plots", width = 12, height = 10, dpi = 300)
  
  cat("✓ Saved scene correlation graph for:", play, "\n")
}

# -----------------------------
# ACT-LEVEL NETWORKS
# -----------------------------
for (play in titles) {
  
  play_data <- shakespeare_words %>%
    filter(short_title == play, !is.na(act))
  
  word_cors_act <- play_data %>%
    add_count(word) %>%
    filter(n >= 10) %>%
    pairwise_cor(word, act, sort = TRUE, method = "pearson")
  
  if (nrow(word_cors_act) == 0) {
    message("No act correlations for: ", play)
    next
  }
  
  word_cors_filtered <- word_cors_act %>%
    filter(correlation > 0.5)
  
  if (nrow(word_cors_filtered) == 0) {
    message("No act edges above threshold for: ", play)
    next
  }
  
  set.seed(2016)
  
  p <- word_cors_filtered %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), edge_width = 0.3, show.legend = FALSE) +
    geom_node_point(color = "salmon", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    labs(title = paste("Word Correlations by Act:", play)) +
    theme_void()
  
  print(p)
  
  ggsave(paste0("act_correlations_", gsub(" ", "_", play), ".png"),
         plot = p, path = "plots", width = 12, height = 10, dpi = 300)
  
  cat("✓ Saved act correlation graph for:", play, "\n")
}
library(dplyr)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(ggplot2)

# Choose one play
play <- "Hamlet"

# Filter for the play and relevant columns
play_data <- all_shakespeare %>%
  filter(short_title == play, section == "contents") %>%
  select(short_title, text) %>%
  mutate(line = row_number(),
         section = line %/% 10) %>%  # 10 lines per section
  filter(section > 0) %>%        # skip first incomplete section
  unnest_tokens(word, text) %>%  # split lines into words
  filter(!word %in% stop_words_custom$word)  # remove stop words

# Keep words that appear at least 10 times
play_data <- play_data %>%
  add_count(word) %>%
  filter(n >= 10)

# Compute pairwise correlations across sections
word_cors <- play_data %>%
  pairwise_cor(word, section, sort = TRUE)

if(nrow(word_cors) == 0){
  message("No correlations found for play: ", play)
} else {
  
  # Filter strong correlations
  word_cors_filtered <- word_cors %>%
    filter(correlation > 0.15)
  
  if(nrow(word_cors_filtered) == 0){
    message("No edges above threshold for play: ", play)
  } else {
    # Build graph
    g <- word_cors_filtered %>%
      graph_from_data_frame(directed = FALSE)
    
    # Plot graph
    p <- ggraph(g, layout = "fr") +
      geom_edge_link(aes(edge_alpha = correlation),
                     edge_width = 0.3,
                     show.legend = FALSE) +
      geom_node_point(color = "lightblue", size = 4) +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void()
    
    
    print(p)
    
    # Save plot
    if(!dir.exists("plots")) dir.create("plots")
    ggsave(paste0("plots/section_correlations_", play, ".png"),
           plot = p, width = 12, height = 10, dpi = 300)
    
    message("✓ Saved section correlation graph for play: ", play)
  }
}


library(dplyr)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(ggplot2)

# Select Plays 
selected_plays <- c("1516", "1519", "1522", "1524", "1520", "1523", "1503", "1540", "1531", "1515")

# Filter the data first
all_shakespeare_filtered <- all_shakespeare %>%
  filter(gutenberg_id %in% selected_plays, section == "contents") %>%
  select(short_title, text)

# Unique plays
titles <- unique(all_shakespeare_filtered$short_title)

# Loop through each play
for (play in titles) {
  
  # Step 1: Filter for this play and add line numbers
  play_data <- all_shakespeare_filtered %>%
    filter(short_title == play) %>%
    mutate(line = row_number()) %>%
    mutate(section = line %/% 10) %>%   # 10 lines per section
    filter(section > 0) %>%             # optional: skip first incomplete section
    unnest_tokens(word, text) %>%       # split lines into words
    filter(!word %in% stop_words_custom$word)
  
  # Step 2: Keep words that appear at least 10 times (optional)
  play_data <- play_data %>%
    add_count(word) %>%
    filter(n >= 10)
  
  # Step 3: Compute pairwise correlations across sections
  word_cors <- play_data %>%
    pairwise_cor(word, section, sort = TRUE)
  
  # Skip if no correlations found
  if (nrow(word_cors) == 0) {
    message("No correlations for play: ", play)
    next
  }
  
  # Step 4: Filter strong correlations
  word_cors_filtered <- word_cors %>%
    filter(correlation > 0.15)
  
  if (nrow(word_cors_filtered) == 0) {
    message("No edges above threshold for play: ", play)
    next
  }
  
  # Step 5: Build graph and plot
  g <- word_cors_filtered %>%
    graph_from_data_frame(directed = FALSE)
  
  p <- ggraph(g, layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation),
                   edge_width = 0.3,
                   show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 4) +
    geom_node_text(aes(label = name), repel = TRUE) +
    labs(title = paste("Word Correlations by Sections (10 lines) — Play:", play)) +
    theme_void()
  
  print(p)
  
  # Step 6: Save plot
  if (!dir.exists("plots")) dir.create("plots")
  ggsave(paste0("plots/section_correlations_", play, ".png"),
         plot = p, width = 12, height = 10, dpi = 300)
  
  message("✓ Saved section correlation graph for play: ", play)
}

selected_plays <- c("1516", "1519", "1522", "1524", "1520", 
                    "1523", "1503", "1540", "1531", "1515")

all_shakespeare_filtered <- all_shakespeare %>%
  filter(gutenberg_id %in% selected_plays, section == "contents") %>%
  select(gutenberg_id, short_title, text)

library(dplyr)
library(tidytext)

shakespeare_section_words <- all_shakespeare_filtered %>%
  mutate(line = row_number()) %>%           # assign line numbers
  mutate(section = line %/% 10) %>%        # chunk: 10 lines per section
  filter(section > 0) %>%                  # skip first incomplete section
  unnest_tokens(word, text) %>%            # split each line into words
  filter(!word %in% stop_words_custom$word) # remove stop words

shakespeare_section_words <- shakespeare_section_words %>%
  count(gutenberg_id, short_title, section, word, sort = TRUE, name = "n")


library(visNetwork)
library(widyr)
library(dplyr)

# Create folder for saving files
if (!dir.exists("interactive_networks")) dir.create("interactive_networks")

titles <- unique(shakespeare_section_words$short_title)

for (play in titles) {
  
  play_data <- shakespeare_section_words %>%
    filter(short_title == play)
  
  word_cors <- play_data %>%
    add_count(word, name = "word_total") %>%  # avoids 'nn' message
    filter(word_total >= 10) %>%
    pairwise_cor(word, section, value = n, sort = TRUE) %>%
    filter(correlation > 0.15)
  
  if (nrow(word_cors) == 0) {
    message("No edges above threshold for play: ", play)
    next
  }
  
  words <- unique(c(word_cors$item1, word_cors$item2))
  
  nodes <- data.frame(
    id = words,
    label = words,
    color = "lightblue",
    title = paste("Word:", words)
  )
  
  edges <- data.frame(
    from = word_cors$item1,
    to = word_cors$item2,
    value = word_cors$correlation * 10,
    title = paste("Correlation:", round(word_cors$correlation, 3))
  )
  
  network <- visNetwork(nodes, edges, main = paste("Word Correlations:", play)) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visPhysics(stabilization = TRUE)
  
  visSave(network, file = paste0("interactive_networks/", gsub(" ", "_", play), ".html"))
  
  cat("Saved interactive section graph for:", play, "\n")
}
