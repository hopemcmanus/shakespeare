library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(readr)

# Directories
dir_path <- "data/processed/tokens/"
plot_dir <- "plots/"
meta_dir <- "data/metadata"
all_chars_df <- read_csv(file.path(meta_dir, "all_characters.csv"), show_col_types = FALSE)

if(!dir.exists(plot_dir)) dir.create(plot_dir)

# List CSVs
files <- list.files(dir_path, pattern = "_tokens.csv$", full.names = TRUE)
files <- files[!grepl("all_shakespeare_tokens.csv", files)]

# Function to build co-occurrence network for one play
build_cooccurrence_network <- function(play_df, play_name, min_weight = 2, level = c("scene","act")) {
  level <- match.arg(level)
  
  # Filter characters to only unique cleaned ones
  play_chars <- all_chars_df %>% filter(play == play_name)
  play_df <- play_df %>% filter(character %in% play_chars$character)
  
  # Make level × character matrix
  level_character_matrix <- play_df %>%
    group_by(.data[[level]], character) %>%
    summarise(present = 1, .groups = "drop") %>%
    pivot_wider(names_from = character, values_from = present, values_fill = 0)
  
  level_mat <- as.matrix(level_character_matrix[,-1])
  cooccur_mat <- t(level_mat) %*% level_mat
  diag(cooccur_mat) <- 0
  
  # Create igraph
  g <- graph_from_adjacency_matrix(cooccur_mat, mode = "undirected", weighted = TRUE)
  
  # Filter weak edges
  if(min_weight > 1){
    g <- delete_edges(g, E(g)[weight < min_weight])
  }
  
  return(g)
}

# Loop through plays and build/save improved networks
for(f in files){
  play_name <- tools::file_path_sans_ext(basename(f))
  cat("Processing network for:", play_name, "\n")
  
  play_df <- read_csv(f, show_col_types = FALSE)
  
  # Build scene-level network
  g <- build_cooccurrence_network(play_df, play_name, min_weight = 2, level = "scene")
  
  # Optional: filter low-degree nodes to reduce clutter
  g <- delete_vertices(g, V(g)[degree(g) == 0])
  
  # Plot network
  p <- ggraph(g, layout = "kk") +  # Kamada-Kawai layout
    geom_edge_link(aes(width = weight), alpha = 0.5, color = "gray50") +
    scale_edge_width(range = c(0.3, 2)) +
    geom_node_point(aes(size = degree(g)), color = "steelblue") +
    geom_node_text(aes(label = ifelse(degree(g) > 2, name, "")), repel = TRUE, size = 3) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(play_name)
  
  ggsave(filename = paste0(plot_dir, "/", play_name, "_network.png"),
         plot = p, width = 8, height = 6, dpi = 300)
}
