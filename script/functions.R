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
    period <- df %>% filter(post2018 == FALSE)
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

temporal_similarity <- function(word, pre_mat, post_mat) {
  #get word vectors for word
  pre <- as.numeric(pre_mat[word, ] %>% select(-vocab, -period))
  post <- as.numeric(post_mat[word, ] %>% select(-vocab, -period))
  
  #compute similarity
  score <- sim2(
    matrix(pre, nrow = 1),
    matrix(post, nrow = 1),
    method = "cosine"
  )
  
  return(score)
}


