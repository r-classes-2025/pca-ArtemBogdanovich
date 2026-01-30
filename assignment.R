library(friends)
library(tidyverse) 
library(tidytext)
library(factoextra)

top_speakers <- friends %>%
  count(speaker, sort = TRUE) %>%
  slice_head(n = 6) %>%
  pull(speaker)

friends_tokens <- friends %>%
  filter(speaker %in% top_speakers) %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_remove_all(word, "\\d+")) %>%
  filter(word != "") %>%
  select(speaker, word)

friends_tf <- friends_tokens %>%
  count(speaker, word) %>%
  group_by(speaker) %>%
  mutate(tf = n / sum(n)) %>%
  slice_max(n, n = 500) %>%  # топ-500 слов на персонажа
  ungroup() %>%
  select(speaker, word, tf)

friends_tf_wide <- friends_tf %>%
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) %>%
  column_to_rownames("speaker")

set.seed(123)
km.out <- kmeans(scale(friends_tf_wide), centers = 3, nstart = 20)

pca_fit <- prcomp(friends_tf_wide, center = TRUE, scale. = TRUE)




