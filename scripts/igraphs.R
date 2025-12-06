library(readr)
library(dplyr)
library(tidyr)
library(igraph)

# Example for first play
play_file <- "data/processed/tokens/a_midsummer_night_s_dream_tokens.csv"
play_name <- "a_midsummer_night_s_dream"

play_df <- read_csv(play_file, show_col_types = FALSE)

# Assuming you have all_chars_df from before
play_chars <- all_chars_df %>% filter(play == play_name)
play_df <- play_df %>% filter(character %in% play_chars$character)

# Build scene × character matrix
scene_character_matrix <- play_df %>%
  group_by(scene, character) %>%
  summarise(present = 1, .groups = "drop") %>%
  pivot_wider(names_from = character, values_from = present, values_fill = 0)

scene_mat <- as.matrix(scene_character_matrix[,-1])
cooccur_mat <- t(scene_mat) %*% scene_mat
diag(cooccur_mat) <- 0

# Create igraph object
g <- graph_from_adjacency_matrix(cooccur_mat, mode = "undirected", weighted = TRUE)

# Optional: filter edges by minimum weight
g <- delete_edges(g, E(g)[weight < 2])

# Optional: remove isolated nodes
g <- delete_vertices(g, V(g)[degree(g) == 0])

play_networks <- list()

for(f in files){
  play_name <- tools::file_path_sans_ext(basename(f))
  play_df <- read_csv(f, show_col_types = FALSE)
  play_chars <- all_chars_df %>% filter(play == play_name)
  play_df <- play_df %>% filter(character %in% play_chars$character)
  
  scene_character_matrix <- play_df %>%
    group_by(scene, character) %>%
    summarise(present = 1, .groups = "drop") %>%
    pivot_wider(names_from = character, values_from = present, values_fill = 0)
  
  scene_mat <- as.matrix(scene_character_matrix[,-1])
  cooccur_mat <- t(scene_mat) %*% scene_mat
  diag(cooccur_mat) <- 0
  
  g <- graph_from_adjacency_matrix(cooccur_mat, mode = "undirected", weighted = TRUE)
  g <- delete_edges(g, E(g)[weight < 2])
  g <- delete_vertices(g, V(g)[degree(g) == 0])
  
  play_networks[[play_name]] <- g
}
