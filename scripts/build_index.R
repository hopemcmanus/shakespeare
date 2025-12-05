# Folders
data_dir <- "data"
plots_dir <- "plots"
interactive_dir <- "interactive_networks"

# List CSV files
csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = FALSE)

# Extract play names (assuming filenames like "othello.csv")
plays <- tools::file_path_sans_ext(csv_files)

# Prepare output
lines <- c("# Shakespeare Files Index", "")

for (play in plays) {
  play_title <- tools::toTitleCase(play)
  
  # Find files related to this play
  plot_files <- list.files(plots_dir, pattern = play, full.names = FALSE, ignore.case = TRUE)
  interactive_files <- list.files(interactive_dir, pattern = play, full.names = FALSE, ignore.case = TRUE)
  
  # Write play section
  lines <- c(
    lines,
    paste0("## ", play_title),
    "",
    "**Data**",
    paste0("- [", play, ".csv](", data_dir, "/", play, ".csv)"),
    ""
  )
  
  # Add plots
  if (length(plot_files) > 0) {
    lines <- c(lines, "**Plots**")
    for (p in plot_files) {
      lines <- c(lines, paste0("- [", p, "](", plots_dir, "/", p, ")"))
    }
    lines <- c(lines, "")
  }
  
  # Add interactive
  if (length(interactive_files) > 0) {
    lines <- c(lines, "**Interactive**")
    for (i in interactive_files) {
      lines <- c(lines, paste0("- [", i, "](", interactive_dir, "/", i, ")"))
    }
    lines <- c(lines, "")
  }
}

# Write Markdown file
writeLines(lines, "INDEX.md")

cat("INDEX.md created successfully.\n")
