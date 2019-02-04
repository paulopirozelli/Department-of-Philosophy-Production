library(readr)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(tm)
library(topicmodels)

Teses_filosofia <- read_csv("Teses_filosofia.csv")

# The file is from early 2018, therefore some papers from 2017 were not available
# yet. I decided to restrict the analysis up to 2016
Teses_filosofia <- Teses_filosofia %>%
  filter(Year <= 2016)

# Number of publications - total and by type
production_year <- Teses_filosofia %>%
  count(Year) %>%
  ggplot() +
  geom_line(aes(Year, n))

production_year_type <- Teses_filosofia %>%
  count(Type, Year) %>%
  ggplot() +
  geom_line(aes(Year, n, color = Type))

# Most commmon subjects
most_common_subjects <- Teses_filosofia %>%
  unnest_tokens(word, `Subject(Library)`) %>%
  filter(!word %in% c("da", "do", "e", "de")) %>%
  filter(!is.na(word)) %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

## Topic model with titles and subtitles
# Merging title and subtitle columns
Teses_filosofia$Complete_title <- 
  apply(Teses_filosofia[, 4:5], 1, function(x) toString(na.omit(x)))

# Topic model
myCorpus <- dfm(Teses_filosofia$Complete_title)

topics <- LDA(myCorpus, k = 20, control = list(seed = 1234))

ap_topics <- tidy(topics, matrix = "beta")

topics_terms <- ap_topics %>%
  group_by(topic) %>%
  filter(!term %in% stopwords("pt")) %>%
  filter(!term %in% c(",", ")", "\"", ".")) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

topic_graph <- topics_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
  