library(gutenbergr)

# Use the main Gutenberg site as mirror
options(gutenbergr.download_mirror = "http://www.gutenberg.org")

# Download The Prince
machiavelli <- gutenberg_download(1232)


url <- "https://www.gutenberg.org/files/1232/1232-0.txt"
machiavelli_text <- readLines(url, warn = FALSE)

# Optional: convert to tibble for analysis
library(tibble)
machiavelli <- tibble(text = machiavelli_text)
