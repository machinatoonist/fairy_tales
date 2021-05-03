# Goal: How do fairy tales perpetuate gender stereotypes about men and woman?

# Libraries ----
library(tidytext)
library(tidyverse)
library(openxlsx)
library(scales)
library(snakecase)
library(tidylo)
library(widyr)
library(broom)
library(umap)
library(ggrepel)
library(timetk)
library(tidyquant)
library(plotly)

fairy_tales_tbl <- openxlsx::readWorkbook("data/grimms_tale.xlsx", sheet = 1, colNames = TRUE)

fairy_tales_tbl %>% glimpse()

tidy_tales_tbl <- fairy_tales_tbl %>%
  unnest_tokens(output = word, 
                input = Story, 
                token = "words", 
                to_lower = TRUE, 
                drop = TRUE)

tidy_tales_tbl %>% glimpse()  

# Analyse the most frequent groupings of 3 words

fairy_tale_trigrams_tbl <- fairy_tales_tbl %>%
  unnest_tokens(output = trigram, 
                input = Story, 
                token = "ngrams", 
                to_lower = TRUE, n = 3)

fairy_tale_trigrams_tbl %>% glimpse()

fairy_tale_trigrams_tbl %>%
  count(trigram, sort = TRUE)

trigrams_separated <- fairy_tale_trigrams_tbl %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

trigram_counts <- trigrams_filtered %>%
  count(word1, word2, word3, sort = TRUE)

trigram_counts

# Analyse the most frequent groupings of 2 words

fairy_tale_bigrams_tbl <- fairy_tales_tbl %>%
  unnest_tokens(output = bigram, 
                input = Story, 
                token = "ngrams", 
                to_lower = TRUE, n = 2)

fairy_tale_bigrams_tbl %>% glimpse()

fairy_tale_bigrams_tbl %>%
  count(bigram, sort = TRUE)

bigrams_separated <- fairy_tale_bigrams_tbl %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts

# Most common words in Grimm's Fairy Tales ----
common_words <- tidy_tales_tbl %>%
  count(word, sort = TRUE)

common_words

tales_words_cleaned_tbl <- tidy_tales_tbl %>%
  anti_join(stop_words, by = "word") 

tales_words_cleaned_tbl %>% glimpse()

# Book clusters by word similarity ----

word_counts_by_title_tbl <- tales_words_cleaned_tbl %>%
  select(Title, word) %>%
  group_by(Title) %>%
  count(word, sort = TRUE)  %>%
  mutate(word_fraction = n/sum(n)) %>%
  ungroup()

title_word_tbl <- word_counts_by_title_tbl %>%
  select(Title, word, word_fraction) %>%
  pivot_wider(names_from = word, 
              values_from = word_fraction, 
              values_fill = 0, 
              names_sort = TRUE) %>% select(-c(`0`, `1`))

kmeans_obj <- title_word_tbl %>% 
  select(-Title) %>% 
  kmeans(centers = 4, nstart = 100)

kmeans_obj$cluster

# Use for tot.withinss for SKREE plot
broom::glance(kmeans_obj)

# Create a function that works on one element
kmeans_mapper <- function(centers = 4) {
  
  title_word_tbl %>%
    select(-Title) %>%
    kmeans(centers = centers, nstart = 100)
}

4 %>% kmeans_mapper() %>% glance()

# Mapping the function to many elements
# Implement purrr row-wise mutate() and map()
kmeans_mapped_tbl <- tibble(centers = 1:15) %>%
  mutate(k_means = centers %>% map(kmeans_mapper)) %>%
  mutate(glance = k_means %>% map(glance)) 

kmeans_mapped_tbl

kmeans_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss)
  
  # 2.4 Skree Plot ----
kmeans_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss) %>%
  #Visualisation
  ggplot(aes(centers, tot.withinss)) +
  geom_point(color = "#2c3e50", size = 4) +
  geom_line(color = "#2c3e50", size = 1) +
  ggrepel::geom_label_repel(aes(label = centers), color = "#2c3e50") +
  
  # Formatting
  # theme_tq() +
  labs(
    title = "Skree Plot",
    subtitle = "Measures the distance each of the fairy tales are from the closest K-Means center",
    caption = "Conclusion: Based on the Scree Plot, we select 8 clusters to segment the customer base"
  )

# 3.1 Use UMAP to get 2-D Projection ----
umap_obj <- title_word_tbl %>%
  select(-Title) %>%
  umap()

umap_results_tbl <- umap_obj$layout %>%
  as_tibble() %>%
  set_names(c("x", "y")) %>%
  bind_cols(
    title_word_tbl %>% select(Title)
  )


umap_results_tbl %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_label_repel(aes(label = Title), size = 3)

# 3.2 Use K-Means to Add Cluster Assignments ----
umap_results_tbl

k_means_5_obj <- kmeans_mapped_tbl %>%
  pull(k_means) %>%
  pluck(5)

kmeans_5_clusters_tbl <- k_means_5_obj %>%
  augment(title_word_tbl) %>%
  select(Title, .cluster)

umap_kmeans_5_results_tbl <- umap_results_tbl %>%
  left_join(kmeans_5_clusters_tbl, by = "Title")

# 3.3 Visualize UMAP'ed Projections with Cluster Assignments ----
g <- umap_kmeans_5_results_tbl %>%
  mutate(label_text = str_glue("{Title},
                                 Cluster:{.cluster}")) %>%
  ggplot(aes(x, y, color = .cluster)) +
  # geom_point(aes(size = 2, text = label_text)) +
  geom_point(size = 2) +
  geom_label_repel(aes(label = label_text, size = 3)) +
  
  # Formatting
  theme_tq() +
  scale_color_tq() +
  labs(
    title = "Grimm's Fairy Tale's: 2D Projection",
    subtitle = "IMAP 2D Projection with K-Means Cluster Assignment",
    caption = "Clusters Based on Word Similarity Using UMAP and K-Means") +
  theme(legend.position = "none")

g

ggplotly(g, tooltip = c("text"))

# Sentiment analysis ----

# How many positive and negative words are in each tale?
fairy_tale_sentiment <- tales_words_cleaned_tbl %>%
  inner_join(get_sentiments())

fairy_tale_sentiment %>%
  count(Title, sentiment)


tales_words_cleaned_tbl %>%
  count(Title)

# Comparing one book to another ----
# tales_words_cleaned_tbl %>%
#   count(Title, word, sort = TRUE) %>%
#   pivot_wider(names_from = Title, values_from = n, values_fill = 0) %>%
#   
#   mutate(total = movie + tv_show) %>%
#   arrange(desc(total)) %>%
#   head(50) %>%
#   ggplot(aes(movie, tv_show)) +
#   
#   geom_point() +
#   geom_text(aes(label = word), vjust = 1, hjust = 1) +
#   scale_x_log10() +
#   scale_y_log10()

# What words tend to appear together - This takes quite a long time to render!!!
# tales_words_cleaned_tbl %>%
#   distinct(Title, word) %>%
#   add_count(word, name = "word_total") %>%  
#   filter(word_total >= 30) %>%  # remove really rare words
#   pairwise_cor(word, Title, sort = TRUE) %>%
#   filter(correlation >= .1) %>%
#   igraph::graph_from_data_frame() %>%
#   ggraph(layout = "fr") +
#   geom_edge_link(aes(alpha = correlation)) +
#   geom_node_point() +
#   geom_node_text(aes(label = name),
#                  repel = TRUE) +
#   theme(legend.position = "none")

tales_words_cleaned_tbl %>% glimpse()
