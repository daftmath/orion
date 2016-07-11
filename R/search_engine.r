
# R Dataframe Search Engine

# Creates a tf-idf matrix to prepare for searching.
orion_prep <- function(docs) {

  # Stemming, removing punctuation, white spaces to improve matching ability
  corp <- tm::VectorSource(docs) %>%
    tm::Corpus() %>%
    suppressWarnings() %>%
    tm::tm_map(., removePunctuation) %>%
    tm::tm_map(., stemDocument) %>%
    tm::tm_map(., tolower) %>%
    tm::tm_map(., stripWhitespace)

  corp <- tm::tm_map(corp, PlainTextDocument)

  tdm <- tm::TermDocumentMatrix(corp) %>% tm::weightTfIdf()

  return(as.matrix(tdm))
}


# Search Engine
orion_search <- function(tdm, search_q, print_q=FALSE) {

  # Preparing the search query
  search_q2 <- unlist(strsplit(search_q, split=" "))

  # Locating synonyms
  for (i in 1:length(search_q2)) {
    search_q3 <- append(search_q2, qdap::synonyms(search_q2[i], return.list = FALSE))
  }
  search_q4 <- unlist(strsplit(search_q3, split=" "))
  search_q5 <- paste(search_q3, collapse = " ")

  if(print_q==TRUE) {
    print(search_q5)
  }

  query_clean <- tm::VectorSource(search_q5) %>%
    tm::Corpus() %>%
    suppressWarnings() %>%
    tm::tm_map(., removePunctuation) %>%
    tm::tm_map(., stemDocument) %>%
    tm::tm_map(., tolower) %>%
    tm::tm_map(., stripWhitespace)

  search_q6 <-  tm::tm_map(query_clean, PlainTextDocument) %>% tm::TermDocumentMatrix() %>% as.matrix()

  # Creating vector to compare against weighted tdm.
  query_vec <- rep(0, NROW(tdm))
  query_vec[which(row.names(tdm) %in% row.names(search_q6))] <- 1
  query_vec[which(row.names(tdm) %in% search_q2)] <- 2

  tdm <- scale(tdm, center = FALSE, scale = sqrt(colSums(tdm^2)))


  scores <- as.numeric(crossprod(tdm, query_vec))
  return(scores)

}


orion_time_test <- function(n, sp_test) {
  lst = list()
  for (i in 1:n) {
    ptm <- proc.time()
    test <- suppressMessages(orion_search(sp_test, "nutrition stowage"))
    time_dur <- proc.time() - ptm
    time_dur <- time_dur %>% .[3] %>% as.numeric()
    lst <- c(lst, time_dur)
  }
  print(lst)
  print('Avg Run Time:')
  print(mean(as.numeric(lst)))
}






