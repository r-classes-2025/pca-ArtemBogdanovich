library(friends)
library(tidyverse)
library(tidytext)
library(factoextra)

# 1. 6 главных персонажей
top_speakers <- friends %>%
  count(speaker, sort = TRUE) %>%
  slice_head(n = 6) %>%
  pull(speaker)

# 2. Токенизация и очистка
friends_tokens <- friends %>%
  filter(speaker %in% top_speakers) %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_remove_all(word, "\\d+")) %>%
  filter(word != "") %>%
  select(speaker, word)

# 3. Топ-500 слов на персонажа + относительная частота
friends_tf <- friends_tokens %>%
  count(speaker, word) %>%
  group_by(speaker) %>%
  arrange(desc(n), word) %>%      # стабильная сортировка
  slice_head(n = 500) %>%         # точно 500 слов на персонажа
  mutate(tf = n / sum(n)) %>%
  ungroup() %>%
  select(speaker, word, tf)

# 4. Преобразование в широкий формат
friends_tf_wide <- friends_tf %>%
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) %>%
  column_to_rownames("speaker")

# 5. K-means кластеризация
set.seed(123)
km.out <- kmeans(scale(friends_tf_wide), centers = 3, nstart = 20)

# 6. PCA на масштабированных данных
pca_fit <- prcomp(scale(friends_tf_wide), center = TRUE, scale. = TRUE)

 




