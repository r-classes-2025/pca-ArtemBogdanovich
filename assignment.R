# установите и загрузите пакеты
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra)

# 1. отберите 6 главных персонажей по количеству реплик
top_speakers <- friends %>%
  count(speaker, sort = TRUE) %>%
  slice_head(n = 6) %>%
  pull(speaker)

# 2. отфильтруйте топ-спикеров, токенизируйте их реплики, удалите цифры
friends_tokens <- friends %>%
  filter(speaker %in% top_speakers) %>%
  unnest_tokens(word, text) %>%        # создаем столбец word
  mutate(word = str_remove_all(word, "\\d")) %>%  # удаляем цифры
  select(speaker, word)

# 3. отберите по 500 самых частотных слов для каждого персонажа
# и посчитайте относительные частотности
friends_tf <- friends_tokens %>%
  count(speaker, word, sort = TRUE) %>%
  group_by(speaker) %>%
  slice_max(n, n = 500) %>%           # топ-500 слов на персонажа
  ungroup() %>%
  group_by(speaker) %>%
  mutate(rel_freq = n / sum(n)) %>%    # относительные частоты
  ungroup()

# 4. преобразуем в широкий формат
friends_tf_wide <- friends_tf %>%
  filter(word != "" & !is.na(word)) %>%  # убираем пустые слова
  select(speaker, word, rel_freq) %>%
  pivot_wider(names_from = word, values_from = rel_freq, values_fill = 0) %>%
  column_to_rownames("speaker")        # превращаем столбец speaker в имена рядов

# 5. кластеризация k-means (k=3)
set.seed(123)
km.out <- kmeans(scale(friends_tf_wide), centers = 3, nstart = 20)

# 6. PCA
pca_fit <- prcomp(scale(friends_tf_wide), center = TRUE, scale. = TRUE)



