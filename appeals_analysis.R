require(tidyverse)
require(tidytext)
require(tm)

appeals.data <- read_tsv('recent_opinions.tsv')
appeals.data <- appeals.data %>%
  mutate(opinion_id = 1:n())

# Load stop words
data("stop_words")
custom_words <- read.delim("custom_words.txt", header = F, sep = " ")
