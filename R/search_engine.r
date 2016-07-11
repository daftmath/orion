library(magrittr)
load("opspro_data2.r")

# R Search Engine

idf_weight <- function(tf_vec, df) {
  # Computes tfidf weights from a term frequency vector and a document frequency scalar
  weight = rep(0, length(tf_vec))
  weight[tf_vec > 0] = (1 + log2(tf_vec[tf_vec > 0])) * log2(doc_count/df)
  weight
}

term_weight <- function(idf_row) {
  term_df <- sum(idf_row[1:doc_count] > 0)
  tfidv_vec <- idf_weight(idf_row, term_df)
  return(tfidv_vec)
}




orion_prep <- function(docs) {

  # Stemming, removing punctuation, white spaces to improve matching ability
  corp <- VectorSource(docs) %>%
    Corpus() %>%
    suppressWarnings() %>%
    tm_map(., removePunctuation) %>%
    tm_map(., stemDocument) %>%
    tm_map(., tolower) %>%
    tm_map(., stripWhitespace)

  corp <- tm_map(corp, PlainTextDocument)

  tdm <- TermDocumentMatrix(corp) %>% weightTfIdf()

  return(as.matrix(tdm))
}




orion <- function(tdm, search_q) {

  #search_q <- "bogan arm"
  search_q2 <- unlist(strsplit(search_q, split=" "))

  for (i in 1:length(search_q2)) {
    search_q3 <- append(search_q2, synonyms(search_q2[i], return.list = FALSE))
  }
  search_q4 <- unlist(strsplit(search_q3, split=" "))
  # Pasting original search term to give it double weight
  search_q5 <- paste(search_q, paste(search_q3, collapse = " "))

  query_clean <- VectorSource(search_q5) %>%
    Corpus() %>%
    suppressWarnings() %>%
    tm_map(., removePunctuation) %>%
    tm_map(., stemDocument) %>%
    tm_map(., tolower) %>%
    tm_map(., stripWhitespace)

  search_q6 <-  tm_map(query_clean, PlainTextDocument) %>% TermDocumentMatrix() %>% as.matrix()

  query_vec <- rep(0, NROW(tdm))
  query_vec[which(row.names(tdm) %in% search_q)] <- 1
  query_vec[which(row.names(tdm) %in% search_q2)] <- 2

  tdm <- scale(tdm, center = FALSE, scale = sqrt(colSums(tdm^2)))


  scores <- crossprod(tdm, query_vec)
  return(scores)

}


orion_time_test <- function(n) {
  lst = list()
  for (i in 1:n) {
    ptm <- proc.time()
    test <- suppressMessages(orion(sp_test, "nutrition stowage"))
    time_dur <- proc.time() - ptm
    time_dur <- time_dur %>% .[3] %>% as.numeric()
    lst <- c(lst, time_dur)
  }
  print(lst)
  print('Avg Run Time:')
  print(mean(as.numeric(lst)))
}






