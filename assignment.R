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
  mutate(word = str_remove_all(word, "\\d+")) %>% # удалить цифры
  filter(word != "") %>%
  select(speaker, word)

# 3. Топ-500 слов на персонажа + относительная частота
friends_tf <- friends_tokens %>%
  count(speaker, word) %>%
  group_by(speaker) %>%
  slice_max(n, n = 500) %>%      
  slice_head(n = 500) %>%       
  mutate(tf = n / sum(n)) %>%
  ungroup() %>%
  select(speaker, word, tf)

# 4. Широкий формат
friends_tf_wide <- friends_tf %>%
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) %>%
  column_to_rownames("speaker")

# 5. K-means кластеризация
set.seed(123)
km.out <- kmeans(scale(friends_tf_wide), centers = 3, nstart = 20)

# 6. PCA
pca_fit <- prcomp(friends_tf_wide, center = TRUE, scale. = TRUE)

# 7. Биплот PCA
q <- fviz_pca_biplot(
  pca_fit,
  label = "var",
  habillage = km.out$cluster,
  geom.ind = "text",
  select.var = list(cos2 = 20)
)





