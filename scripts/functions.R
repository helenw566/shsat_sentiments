# Helper Functions


# Preprocessing -----------------------------------------------------------

process_corpus <- function(corpus, ngrams) {
  
  #process corpus
  toks <- corpus %>%
    tokens(
      remove_numbers = TRUE,
      remove_punct = TRUE,
      remove_symbols = TRUE
    ) %>%
    tokens_tolower() %>%
    tokens_compound(pattern = phrase(ngrams)) 
  
  #return tokens
  return(toks)
}

# Identifying Shared Words ------------------------------------------------

shared_words <- function(toks_1, toks_2) {
  #finds shared words across two dfms
  words <- intersect(
    featnames(toks_1 %>% dfm), 
    featnames(toks_2 %>% dfm)
  )
  return(words)
}

# Bootstrap Resampling of Corpus ----------------------------------------------------

sample_news <- function(df, post = T) {
  #filter to time period
  if (post == T) {
    period <- df %>% filter(post2018 == FALSE)
  } else if (post == F) {
    period <- df %>% filter(post2018 == TRUE)
  }
  
  #resample for text ids
  text_ids <- unique(period$text_id)
  resample_ids <- sample(text_ids, size = length(text_ids), replace = T)
  
  #get new dataframe of texts
  resample_df <- period %>%
    merge(
      data.frame(text_id = resample_ids),
      by = "text_id",
      all.y = T
    )
  
  #convert to corpus
  news_corpus <- corpus(resample_df, text_field = "full_text")
  
  return(news_corpus)
}

# Glove ------------------------------------------------------------

#function to run glove
glove_matrix <- function(tidy = T, df, period_words) {
  
  #feature co-occurance
  feat_matrix <- fcm(
    df, 
    context = "window", 
    window = 6, 
    count = "frequency", 
    tri = FALSE
  )
  
  #glove
  glove <- GlobalVectors$new(rank = 50, 
                             x_max = 10,
                             learning_rate = 0.05)
  
  local_glove <- glove$fit_transform(feat_matrix, 
                                     n_iter = 15,
                                     convergence_tol = 1e-3, 
                                     n_threads = detectCores() - 1)
  
  #word vector
  word_vec <- local_glove + t(glove$components)
  word_vec <- word_vec %>%
    as.data.frame() %>%
    mutate(
      vocab = rownames(.)
    ) %>%
    #filter for only words that appear in both
    filter(vocab %in% period_words) %>% 
    select(-vocab) %>%
    as.matrix()
  
  #tidying it up
  word_vec_tidy <- word_vec %>%
    as.data.frame() %>%
    mutate(
      vocab = rownames(.)
    ) %>%
    pivot_longer(
      !vocab,
      names_to = "feature",
      values_to = "value"
    ) %>%
    #filter for only words that appear in both
    filter(vocab %in% period_words)
  
  if (tidy == T) {
    return(word_vec_tidy) 
  } else if (tidy == F) {
    return(word_vec)
  }
}


# Cosine Similarity -------------------------------------------------------

temporal_similarity <- function(word, pre_input, post_input, method) {
  #
  if (method == "aligned") {
    pre <- as.numeric(pre_input[word, ] %>% select(-vocab, -period))
    post <- as.numeric(post_input[word, ] %>% select(-vocab, -period))
  } else if (method == "chrono") {
    pre <- pre_input$values[word,]
    post <- post_input$values[word,]
  }
  
  #compute similarity
  score <- sim2(
    matrix(pre, nrow = 1),
    matrix(post, nrow = 1),
    method = "cosine"
  )
  
  return(score)
}

cos_similarity_plot <- function(data, word) {
  #plots change in
  #cosine similarity
  #for given word
  #combos
  return_plot <- data %>%
    mutate(shsat = ifelse(stringr::str_detect(word_combo, word), 1, 0)) %>%
    filter(shsat == 1) %>%
    ggplot() +
    geom_point(aes(y = word_combo, x = pre, col = "Pre 2018")) +
    geom_point(aes(y = word_combo, x = post, col = "Post 2018")) +
    geom_segment(aes(y = word_combo, yend = word_combo,
                     x = pre, xend = post),
                 color = "grey30",
                 arrow = arrow(length = unit(1, "mm"))) +
    theme_minimal() +
    labs(
      x = "Cosine Similarity",
      y = "",
      title = bquote("Change in Cosine Similarity to " * italic(.(word))),
      colour = "Time Period",
      caption = "Chronologically Trained Model"
    ) +
    scale_color_manual(values = palette) +
    scale_x_continuous(limits = c(0, 1))  +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, face = "italic")
    )
  
  return(return_plot)
}



