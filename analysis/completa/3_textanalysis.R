clean_text <- full_data %>%
  filter(retweet == 'False') %>%
  mutate(tweet_clean = tweet_lower) %>%
  select(tweet_clean, interval_1day) %>%
  mutate_at(vars(tweet_clean), textprocessing)

tweet_bigrams <- clean_text %>%
  unnest_tokens(bigram, tweet_clean, token = "ngrams", n = 2)

ntweets_bigrams <- tweet_bigrams %>%
  count(bigram, sort = T) %>%
  drop_na()

ntweets_bigrams_separated <- tweet_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") 

tweets_bigrams_filtered <- ntweets_bigrams_separated %>%
  filter(!word1 %in% stopwords('pt')) %>%
  filter(!word2 %in% stopwords('pt'))

bigram_counts <-  tweets_bigrams_filtered %>%
  count(interval_1day, word1, word2, sort = TRUE)

tweets_bigrams_united <- tweets_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- tweets_bigrams_united %>%
  count(interval_1day, bigram) %>%
  bind_tf_idf(bigram, interval_1day, n) %>%
  arrange(interval_1day, desc(tf_idf))

subset_bigram_tf_idf <- bigram_tf_idf %>%
  group_by(interval_1day) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup()

# Plots bigram (tf-idf) per day  ------------------------------------------

weeks <- unique(subset_bigram_tf_idf$interval_7day)

for (i in seq(1, length(weeks))){
  print(paste0('plot tfidf', i))
  week_tfidf <- subset_bigram_tf_idf %>% filter(interval_7day == weeks[i])
  
  plot_td_idf <- ggplot(week_tfidf, 
                        aes(tf_idf, fct_reorder(bigram, tf_idf))) +
    geom_col(show.legend = FALSE, alpha = 0.8, fill = paleta_its[2]) + 
    theme_minimal() +
    labs(x = "", y = NULL) +
    theme(text = element_text(size = 16, family = 'Open Sans'),
          axis.text.y = element_text(size = 30,
                                     colour = paleta_its[8]),
          axis.text.x = element_text(size = 24,
                                     colour = paleta_its[8])); plot_td_idf
  ggsave(paste0('./Imagens/plot_tfidf_', i, '.png'), width = 14, height = 8, units = 'in', dpi = 100)
  
}

# Network -----------------------------------------------------------------

bigram_graph <- bigram_counts %>%
  filter(interval_7day == '2021-09-04') %>%
  select(word1, word2, n) %>%
  filter(n > 1000) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# 2. Wordcloed pavlavras por dia ####

all_words_period <- unnest_tokens(clean_text, w, 'tweet_clean') %>%
  group_by(interval_1day, w) %>%
  count(w, sort = T) %>%
  filter(!w %in% c(stopwords('pt'), 'q', 'é', 'tá', 'ser', 'vai', 'pra', 'para', 'que', 'ou', 'sobre'))

days <- unique(all_words_period$interval_1day)

for (i in 1:length(days)){
  
  top_words_day <- all_words_period %>%
    filter(interval_1day == days[i]) %>%
    filter(!w == 'ucrânia') %>%
    filter(!w == 'rússia') %>%
    filter(!w == 'ucrania') %>%
    filter(!w == 'russia') %>%
    filter(!w == 'guerra') %>%
    arrange(desc(n)) %>%
    head(200)
  
  wc <- wordcloud2(top_words_day[-1], color = paleta_its,
             shape = 'circle')
  saveWidget(wc, paste0("output/wc",i,".html"),selfcontained = F)
  webshot::webshot(paste0("output/wc",i,".html"),paste0("output/wc",i,".png"),
                   vwidth = 1300, vheight = 1200, delay = 10)
}




