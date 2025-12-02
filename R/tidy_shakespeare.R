library(dplyr)
library(stringr)
library(tidyverse)
library(tidytext)
tidy_shakespeare <- shakespeare %>%
  bind_rows() %>%
  unnest_tokens(word, text) 