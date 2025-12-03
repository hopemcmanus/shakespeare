# Clean and Structure Machiavelli's The Prince
# Output: structured dataframe with metadata

library(tidyverse)

# Read the text file
raw_text <- readLines("pg1232.txt", warn = FALSE)

# Define metadata
gutenberg_id <- 1232
short_title <- "The Prince"
gutenberg_title <- "The Prince"
author <- "Niccolò Machiavelli"
year <- 1532  # Original publication year

# Define all titles that should be marked as "reference"
titles <- c(
  "INTRODUCTION",
  "YOUTH — Æt. 1-25—1469-94",
  "OFFICE — Æt. 25-43—1494-1512",
  "LITERATURE AND DEATH — Æt. 43-58—1512-27",
  "THE MAN AND HIS WORKS",
  "DEDICATION",
  "THE PRINCE",
  "CHAPTER I. HOW MANY KINDS OF PRINCIPALITIES THERE ARE, AND BY WHAT MEANS THEY ARE ACQUIRED",
  "CHAPTER II. CONCERNING HEREDITARY PRINCIPALITIES",
  "CHAPTER III. CONCERNING MIXED PRINCIPALITIES",
  "CHAPTER IV. WHY THE KINGDOM OF DARIUS, CONQUERED BY ALEXANDER, DID NOT REBEL AGAINST THE SUCCESSORS OF ALEXANDER AT HIS DEATH",
  "CHAPTER V. CONCERNING THE WAY TO GOVERN CITIES OR PRINCIPALITIES WHICH LIVED UNDER THEIR OWN LAWS BEFORE THEY WERE ANNEXED",
  "CHAPTER VI. CONCERNING NEW PRINCIPALITIES WHICH ARE ACQUIRED BY ONE'S OWN ARMS AND ABILITY",
  "CHAPTER VII. CONCERNING NEW PRINCIPALITIES WHICH ARE ACQUIRED EITHER BY THE ARMS OF OTHERS OR BY GOOD FORTUNE",
  "CHAPTER VIII. CONCERNING THOSE WHO HAVE OBTAINED A PRINCIPALITY BY WICKEDNESS",
  "CHAPTER IX. CONCERNING A CIVIL PRINCIPALITY",
  "CHAPTER X. CONCERNING THE WAY IN WHICH THE STRENGTH OF ALL PRINCIPALITIES OUGHT TO BE MEASURED",
  "CHAPTER XI. CONCERNING ECCLESIASTICAL PRINCIPALITIES",
  "CHAPTER XII. HOW MANY KINDS OF SOLDIERY THERE ARE AND CONCERNING MERCENARIES",
  "CHAPTER XIII. CONCERNING AUXILIARIES, MIXED SOLDIERY, AND ONE'S OWN",
  "CHAPTER XIV. THAT WHICH CONCERNS A PRINCE ON THE SUBJECT OF WAR",
  "CHAPTER XV. CONCERNING THINGS FOR WHICH MEN, AND ESPECIALLY PRINCES, ARE PRAISED OR BLAMED",
  "CHAPTER XVI. CONCERNING LIBERALITY AND MEANNESS",
  "CHAPTER XVII. CONCERNING CRUELTY AND CLEMENCY, AND WHETHER IT IS BETTER TO BE LOVED THAN FEARED",
  "CHAPTER XVIII. CONCERNING THE WAY IN WHICH PRINCES SHOULD KEEP FAITH",
  "CHAPTER XIX. THAT ONE SHOULD AVOID BEING DESPISED AND HATED",
  "CHAPTER XX. ARE FORTRESSES, AND MANY OTHER THINGS TO WHICH PRINCES OFTEN RESORT, ADVANTAGEOUS OR HURTFUL?",
  "CHAPTER XXI. HOW A PRINCE SHOULD CONDUCT HIMSELF SO AS TO GAIN RENOWN",
  "CHAPTER XXII. CONCERNING THE SECRETARIES OF PRINCES",
  "CHAPTER XXIII. HOW FLATTERERS SHOULD BE AVOIDED",
  "CHAPTER XXIV. WHY THE PRINCES OF ITALY HAVE LOST THEIR STATES",
  "CHAPTER XXV. WHAT FORTUNE CAN EFFECT IN HUMAN AFFAIRS AND HOW TO WITHSTAND HER",
  "CHAPTER XXVI. AN EXHORTATION TO LIBERATE ITALY FROM THE BARBARIANS",
  "DESCRIPTION OF THE METHODS ADOPTED BY THE DUKE VALENTINO WHEN MURDERING VITELLOZZO VITELLI, OLIVEROTTO DA FERMO, THE SIGNOR PAGOLO, AND THE DUKE DI GRAVINA ORSINI",
  "THE LIFE OF CASTRUCCIO CASTRACANI OF LUCCA"
)

# Step 1: Clean text
# Remove blank lines
text_clean <- raw_text[raw_text != ""]

# Remove underscore characters
text_clean <- str_replace_all(text_clean, "_", "")

# Step 2: Find the split point between front matter and contents
# Contents start at the line about Machiavelli's birth
contents_start_pattern <- "^\\s*Nicolo Machiavelli, born at Florence on 3rd May 1469"
contents_start_idx <- which(str_detect(text_clean, contents_start_pattern))[1]

# Create dataframe with initial structure
df <- tibble(
  original_line = seq_along(text_clean),
  text = text_clean
)

# Step 3: Add section column (front_matter vs contents)
the_prince_idx <- which(str_trim(df$text) == "THE PRINCE")[2]

df <- df %>%
  mutate(
    section = case_when(
      original_line < contents_start_idx ~ "front_matter",
      original_line < the_prince_idx ~ "introduction",
      TRUE ~ "contents"
    )
  )

# Step 4: Add class column (reference vs content)
# Mark lines with no lowercase letters as reference
df <- df %>%
  mutate(
    text_trimmed = str_trim(text),
    class = case_when(
      # Special cases with en-dashes and special characters
      str_detect(text_trimmed, "^YOUTH") ~ "reference",
      str_detect(text_trimmed, "^OFFICE") ~ "reference",
      str_detect(text_trimmed, "^LITERATURE AND DEATH") ~ "reference",
      # Lines with no lowercase letters (and at least one uppercase letter)
      !str_detect(text_trimmed, "[a-z]") & str_detect(text_trimmed, "[A-Z]") ~ "reference",
      TRUE ~ "content"
    )
  ) %>%
  select(-text_trimmed)

# Step 5: Add metadata columns
df <- df %>%
  mutate(
    gutenberg_id = gutenberg_id,
    short_title = short_title,
    gutenberg_title = gutenberg_title,
    author = author,
    year = year
  )

# Step 6: Add line numbers (only for contents + content lines)
df <- df %>%
  mutate(
    line_number = if_else(
      section == "contents" & class == "content",
      row_number(),
      NA_integer_
    )
  ) %>%
  group_by(section, class) %>%
  mutate(
    line_number = if_else(
      section == "contents" & class == "content",
      row_number(),
      NA_integer_
    )
  ) %>%
  ungroup()

# Step 7: Reorder columns to match requested format
machiavelli <- df %>%
  select(
    gutenberg_id,
    short_title,
    gutenberg_title,
    year,
    author,
    section,
    class,
    line_number,
    text
  )

# Display summary
cat("Data cleaning complete!\n\n")
cat("Summary:\n")
cat("Total lines:", nrow(machiavelli), "\n")
cat("Front matter lines:", sum(machiavelli$section == "front_matter"), "\n")
cat("Contents lines:", sum(machiavelli$section == "contents"), "\n")
cat("Reference lines (titles):", sum(machiavelli$class == "reference"), "\n")
cat("Content lines:", sum(machiavelli$class == "content"), "\n")
cat("Lines with line_number (contents + content):", sum(!is.na(machiavelli$line_number)), "\n\n")

# Show first few rows
cat("First 10 rows:\n")
print(head(machiavelli, 10))

# Show examples of reference lines
cat("\nExample reference lines:\n")
print(machiavelli %>% filter(class == "reference") %>% head(5))

# Save to CSV
output_file <- "machiavelli_structured.csv"
write_csv(machiavelli, output_file)
cat("\nData saved to:", output_file, "\n")

# Optionally save as RDS for R users
saveRDS(machiavelli, "machiavelli_structured.rds")
cat("Data also saved as RDS:", "machiavelli_structured.rds", "\n")

tidy_machiavelli <- machiavelli %>%
  filter(section == "contents")%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words_custom)

tidy_jc <- shakespeare %>%
  filter(gutenberg_id == "1522")%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words_custom)  

library(tidyr)

frequency <- bind_rows(mutate(tidy_machiavelli, author = "Niccolò Machiavelli"),
                       mutate(tidy_jc, author = "William Shakespeare")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proportion) %>%
  pivot_longer(`Niccolò Machiavelli`,
               names_to = "author", values_to = "proportion")

frequency

library(scales)

# expect a warning about rows with missing values being removed
p <- ggplot(frequency, aes(x = proportion, y = `William Shakespeare`, 
                      color = abs(`William Shakespeare` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "William Shakespeare", x = NULL)

ggsave(
  filename = "word_frequency_comparison.png",
  plot = p,
  path = "plots",   # save inside the "plots" folder
  width = 10,
  height = 12,
  dpi = 600
)
cor.test(data = frequency[frequency$author == "Niccolò Machiavelli",],
         ~ proportion + `William Shakespeare`)
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  proportion and William Shakespeare
#> t = 119.65, df = 10404, p-value < 2.2e-16
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.7527854 0.7689628
#> sample estimates:
#>       cor 
#> 0.7609924
