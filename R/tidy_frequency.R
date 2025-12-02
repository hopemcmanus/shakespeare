library(dplyr)
library(stringr)
library(tidyverse)
library(tidytext)

stop_words_custom <- bind_rows(
  stop_words,
  data.frame(word = c("thou", "thee", "thy", "thine", "hath", "doth", 
                      "art", "tis", "enter", "exit", "exeunt", "act", "scene"))
)

#Tidy Shakespeare
tidy_shakespeare <- shakespeare %>%
unnest_tokens(word, text)

tidy_hsc <- shakespeare %>%
  filter(gutenberg_id %in% c("1516", "1519", "1522", "1524", "1520", "1523", "1503", "1540", "1531", "1515"), section == "contents") %>%
  unnest_tokens(word, text)          # split into words first


#Analysing word and document frequency
#Term Frequency 
play_words <- tidy_hsc %>%
count(gutenberg_id, word, sort= TRUE)

play_words <- tidy_hsc %>%
  group_by(gutenberg_id, short_title, word) %>%  # include metadata
  summarize(n = n(), .groups = "drop") %>%
  arrange(desc(n))

total_words <- play_words %>% 
  group_by(gutenberg_id, short_title) %>% 
  summarize(total = sum(n))

play_words <- left_join(play_words, total_words)

play_words

#plot
library(ggplot2)

p <- ggplot(play_words, aes(n/total, fill = short_title)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~short_title, ncol = 2, scales = "free_y")

# Create a new folder called "plots" in the current working directory
if (!dir.exists("plots")) {
  dir.create("plots")
}

# 2. Save the plot inside the "plots" folder
ggsave(
  filename = "term_frequency_distribution.png",
  plot = p,
  path = "plots",   # save inside the "plots" folder
  width = 10,
  height = 12,
  dpi = 600
)

freq_by_rank <- play_words %>% 
  group_by(short_title) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total) %>%
  ungroup()

freq_by_rank
## A tibble: 32,165 × 7
#gutenberg_id short_title        word      n total  rank
#<dbl> <chr>              <chr> <int> <int> <int>
#  1         1524 Hamlet             the    1098 30702     1
#2         1503 Richard III        the     987 29650     1
#3         1524 Hamlet             and     984 30702     2
#4         1503 Richard III        and     926 29650     2
#5         1516 Henry IV, Part 1   the     870 24928     1
#6         1516 Henry IV, Part 1   and     864 24928     2
#7         1531 Othello            i       840 26634     1
#8         1515 Merchant of Venice the     827 21553     1
#9         1531 Othello            and     796 26634     2
#10         1503 Richard III        to      754 29650     3
p <- freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = short_title)) + 
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

ggsave(
  filename = "zipfs_law.png",
  plot = p,
  path = "plots",   # save inside the "plots" folder
  width = 10,
  height = 12,
  dpi = 600
)

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(term_frequency) ~ log10(rank), data = rank_subset)
#> 
#> Call:
#> lm(formula = log10(term_frequency) ~ log10(rank), data = rank_subset)
#> 
#> Coefficients:
#> (Intercept)  log10(rank)  
#>     -0.6226      -1.1125

p <- freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = short_title)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

ggsave(
  filename = "fitting_exponent_zipfs_law.png",
  plot = p,
  path = "plots",   # save inside the "plots" folder
  width = 10,
  height = 12,
  dpi = 600
)

play_tf_idf <- play_words %>%
  bind_tf_idf(word, short_title, n)

play_tf_idf
# A tibble: 32,165 × 8
# gutenberg_id short_title      word      n total     tf   idf
# <dbl> <chr>            <chr> <int> <int>  <dbl> <dbl>
# 1         1524 Hamlet           the    1098 30702 0.0358     0
# 2         1503 Richard III      the     987 29650 0.0333     0
# 3         1524 Hamlet           and     984 30702 0.0321     0
# 4         1503 Richard III      and     926 29650 0.0312     0
# 5         1516 Henry IV, Part 1 the     870 24928 0.0349     0
# 6         1516 Henry IV, Part 1 and     864 24928 0.0347     0
# 7         1531 Othello          i       840 26634 0.0315     0
# 8         1515 Merchant of Ven… the     827 21553 0.0384     0
# 9         1531 Othello          and     796 26634 0.0299     0
# 10         1503 Richard III      to      754 29650 0.0254     0
# ℹ 32,155 more rows


play_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))
# A tibble: 32,165 × 7
# gutenberg_id short_title   word      n      tf   idf  tf_idf
# <dbl> <chr>         <chr> <int>   <dbl> <dbl>   <dbl>
#   1         1531 Othello       cass…   131 0.00492  2.20 0.0108 
# 2         1522 Julius Caesar caes…   193 0.00968  1.10 0.0106 
# 3         1522 Julius Caesar cass…    88 0.00441  2.20 0.00970
# 4         1522 Julius Caesar brut…   169 0.00848  1.10 0.00931
# 5         1523 As You Like … rosa…    78 0.00354  2.20 0.00777
# 6         1531 Othello       iago     87 0.00327  2.20 0.00718
# 7         1524 Hamlet        haml…    99 0.00322  2.20 0.00709
# 8         1519 Much Ado abo… bene…    64 0.00299  2.20 0.00658
# 9         1540 Tempest       ariel    45 0.00267  2.20 0.00586
# 10         1522 Julius Caesar anto…    76 0.00381  1.50 0.00573
# ℹ 32,155 more rows
library(forcats)

p <- play_tf_idf %>%
  group_by(short_title) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = short_title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~short_title, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

ggsave(
  filename = "highest_tf-idf.png",
  plot = p,
  path = "plots",   # save inside the "plots" folder
  width = 10,
  height = 12,
  dpi = 600
)