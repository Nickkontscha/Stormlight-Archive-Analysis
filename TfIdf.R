#
# Plotting QUanteda
# https://docs.quanteda.io/articles/pkgdown/examples/plotting.html
# https://docs.quanteda.io/articles/pkgdown/comparison.html
# Good quanteda analysis example:
# https://github.com/datasciencedojo/IntroToTextAnalyticsWithR/blob/master/IntroToTextAnalytics_Part11.R
# text2vec analysis:
# https://cran.r-project.org/web/packages/text2vec/vignettes/text-vectorization.html
# 

# In fact quantedas dfm-class inherits from dgCMatrix-class. So if your code works with dfm-class, in most cases it will work with dgCMatrix as well


library(text2vec)

idf_smooth <- function(dtm_matrix){
  # Number of Documents:
  N <- nrow(dtm_matrix)
  # Number of Documents containing a term
  n_t <- colSums(abs(sign(dtm_matrix)))
  # Calculating IDF-Value
  idf <- log( 1 + N/n_t )
  return(Diagonal(x = idf))
}

# create a tfidf-model with one document for each class
create_tfidf_model <- function(data, text_variable = "text", class_variable = "class", FUN = NULL, term_count_min = 0){
  data$text <- data[, text_variable]
  data$class <- data[, class_variable]
  # Carefull: This reorders the data!
  data <- aggregate(text ~ class, data = data, FUN = paste, collapse = " ")
  data_tokens <- itoken(data$text, ids = data$class,
                        preprocessor = tolower, tokenizer = word_tokenizer, progressbar = FALSE)
  data_vocab <- create_vocabulary(data_tokens, stopwords = stopwords::stopwords("en"))
  data_vocab <- prune_vocabulary(data_vocab, term_count_min = term_count_min)
  vectorizer <- vocab_vectorizer(data_vocab)
  data_dtm <- create_dtm(data_tokens, vectorizer)
  
  if( !is.null(FUN) ){
    tfidf_model <- idf_smooth(data_dtm)
  } else {
    tfidf_model <- TfIdf$new()
    data_tfidf <- fit_transform(data_dtm, tfidf_model) # Fits model
  }
  return(list(tfidf_model, vectorizer))
}
# Take a vectorizer and (tf)idf-model and calculate a tfidf-Matrix for data with the idf-weights of the tfidf-model for the words in the vectorizer
create_tfidf_from_model <- function(data, text_variable = "text",  FUN = NULL,
                                    tfidf_model, vectorizer){
  data$text <- data[, text_variable]
  data_tokens <- itoken(data$text, 
                        preprocessor = tolower, tokenizer = word_tokenizer, progressbar = FALSE)
  data_dtm <- create_dtm(data_tokens, vectorizer)
  if( !is.null(FUN) ){
    data_tfidf <- data_dtm %*% tfidf_model
  } else {
    data_tfidf <- transform(data_dtm, tfidf_model)
  }
  return(data_tfidf)
}

## Prediction
tfidf_model <- create_tfidf_model(train, text_variable = "review", class_variable = "sentiment")
tfidf_model_smooth <- create_tfidf_model(train, text_variable = "review", class_variable = "sentiment", FUN = idf_smooth)

train_tfidf <- create_tfidf_from_model(train, tfidf_model = tfidf_model[[1]], vectorizer = tfidf_model[[2]])
test_tfidf <- create_tfidf_from_model(test, tfidf_model = tfidf_model[[1]], vectorizer = tfidf_model[[2]])



























