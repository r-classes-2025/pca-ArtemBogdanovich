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
  mutate(word = str_remove_all(word, "\\d")) %>%
  filter(word != "") %>%
  select(speaker, word)

friends_tf <- friends_tokens %>%
  count(speaker, word, sort = TRUE) %>%
  group_by(speaker) %>%
  arrange(desc(n)) %>%
  slice_head(n = 500) %>%
  mutate(tf = n / sum(n)) %>%
  ungroup()

friends_tf_wide <- friends_tf %>%
  select(speaker, word, tf) %>%
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) %>%
  column_to_rownames("speaker")

set.seed(123)
km.out <- kmeans(scale(friends_tf_wide), centers = 3, nstart = 20)

pca_fit <- prcomp(scale(friends_tf_wide), center = TRUE, scale. = TRUE)

q <- fviz_pca_biplot(pca_fit,
                     label = "var",
                     habillage = km.out$cluster,
                     geom.ind = "text",
                     select.var = list(cos2 = 20))




