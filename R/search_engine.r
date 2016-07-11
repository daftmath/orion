
# R Dataframe Search Engine

# Creates a tf-idf matrix to prepare for searching.
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


# Search Engine
orion <- function(tdm, search_q, print_q=FALSE) {

  # Preparing the search query
  search_q2 <- unlist(strsplit(search_q, split=" "))

  # Locating synonyms
  for (i in 1:length(search_q2)) {
    search_q3 <- append(search_q2, synonyms(search_q2[i], return.list = FALSE))
  }
  search_q4 <- unlist(strsplit(search_q3, split=" "))
  search_q5 <- paste(search_q3, collapse = " ")

  if(print_q==TRUE) {
    print(search_q5)
  }

  query_clean <- VectorSource(search_q5) %>%
    Corpus() %>%
    suppressWarnings() %>%
    tm_map(., removePunctuation) %>%
    tm_map(., stemDocument) %>%
    tm_map(., tolower) %>%
    tm_map(., stripWhitespace)

  search_q6 <-  tm_map(query_clean, PlainTextDocument) %>% TermDocumentMatrix() %>% as.matrix()

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
    test <- suppressMessages(orion(sp_test, "nutrition stowage"))
    time_dur <- proc.time() - ptm
    time_dur <- time_dur %>% .[3] %>% as.numeric()
    lst <- c(lst, time_dur)
  }
  print(lst)
  print('Avg Run Time:')
  print(mean(as.numeric(lst)))
}






