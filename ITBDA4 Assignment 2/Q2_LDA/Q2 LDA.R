# Q2.1 - Build a data frame of the news files (file_name, content) and preprocess it

library(topicmodels)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(stopwords)
library(ggplot2)

# Preprocessing function
filetext_prep <- function(file_name){
  file <- file(file_name, "r")
  text <- readLines(file, warn = FALSE)
  close(file)
  text_rep <- c()
  n_line <- length(text)
  for (i in 1:n_line) {
    text_line <- tolower(text[i]) # Lower case
    text_line <- stringr::str_replace_all(text_line, "[^a-zA-Z\\s]", " ") # Remove non-characters
    text_line <- stringr::str_replace_all(text_line, "[\\s]+", " ") # Shrink down any excess white space
    text_line <- trimws(text_line, which = c("both")) # Remove leading and/or Trailing white space
    if (nchar(text_line) > 0) {
      text_rep <- paste(text_rep, text_line, sep = " ")
    }
  }

  text_rep <- trimws(text_rep, which = c("both"))
  word_list <- stringr::str_split(text_rep, " ")[[1]] # Split

  stw <- stopwords("en") # List of our stop words

  # Customized removal of words
  # Every article has its date on line 2, so the day and month names show up in
  # all ten docs. "said" was also the most common word in the whole corpus.
  custom_stopwords <- c(tolower(weekdays(as.Date("2024-07-08") + 0:6)),
                        tolower(month.name),
                        "said", "says", "will", "can", "may", "also",
                        "told", "added", "sanews", "gov", "www")
  stw <- c(stw, custom_stopwords)

  # Remove Stop words
  words <- unlist(strsplit(word_list, "\\s+")) # Tokenize words
  word_list <- words[!words %in% stw]
  word_list <- word_list[nchar(word_list) > 2] # Drop one and two letter fragments

  bag_of_words <- unique(word_list)
  res <- list("bag" = bag_of_words, "words" = word_list) # Returning bag and wordlist
}


# Load the news files from the unzipped news.zip folder
news_dir <- "C:/Users/Kailash/Documents/Eduvos/ITBDA4 - Big Data Analytics Techniques (T14)/Assignment 2/news/sa_news"
files <- list.files(news_dir, pattern = "\\.txt$", full.names = TRUE)

# Sort them numerically, otherwise news_10 lands after news_1
files <- files[order(as.numeric(gsub("\\D", "", basename(files))))]

NDoc <- length(files)
text_list <- c()
file_list <- c()

for (i in 1:NDoc){
  DocWords <- filetext_prep(files[i]) # bag of words per document
  # Using the word list and not the bag. The bag only keeps each word once per
  # doc which loses the frequencies, and the topics came out as filler words
  text_line <- paste(DocWords$words, collapse = " ")
  text_list <- append(text_list, text_line) # Generate list of content
  file_list <- append(file_list, basename(files[i]))
}

# Create a data frame
doc_frame <- data.frame(file_name = file_list, content = text_list, stringsAsFactors = FALSE)

# Check what came in
dim(doc_frame)
names(doc_frame)
data.frame(file_name = doc_frame$file_name,
           words = sapply(strsplit(doc_frame$content, "\\s+"), length))

corpus <- Corpus(VectorSource(doc_frame$content)) # Creating Corpus
dtm <- DocumentTermMatrix(corpus) # Document Term Matrix
dtm$dimnames$Docs <- doc_frame$file_name
dtm

# Check nothing came out empty and the top words are real words
sum(slam::row_sums(dtm) == 0)
sort(slam::col_sums(dtm), decreasing = TRUE)[1:10]


# Q2.2 - Perform LDA to find the top n topics and the top k words per topic

num_topics <- 4
num_words  <- 10

lda_model <- LDA(dtm, k = num_topics, control = list(seed = 2026))
terms(lda_model, num_words)

# Word distribution per topic
topic_probabilities <- as.data.frame(posterior(lda_model)$terms)
dim(topic_probabilities)
rowSums(topic_probabilities)

# Term and probability for each topic
topic_dfs <- list()
topic_terms <- terms(lda_model, num_words)
for (i in 1:num_topics) {
  topic_df <- data.frame(term = topic_terms[, i],
                         probability = as.numeric(topic_probabilities[i, topic_terms[, i]]))
  topic_df <- topic_df[order(topic_df$probability, decreasing = TRUE), ] # Sort descending
  topic_dfs[[paste("Topic", i)]] <- topic_df
}

# Word distribution per topic
for (i in 1:num_topics) {
  cat("\n--- Topic", i, "---\n")
  print(topic_dfs[[i]], row.names = FALSE)
}

# Topic distribution per document
doc_probabilities <- as.data.frame(posterior(lda_model)$topics)
rownames(doc_probabilities) <- doc_frame$file_name
round(doc_probabilities, 3)

# Which topic each document belongs to
data.frame(file_name      = doc_frame$file_name,
           dominant_topic = topics(lda_model),
           probability    = round(apply(doc_probabilities, 1, max), 3),
           row.names = NULL)


# Log-likelihood and perplexity for a few values of k.
# These are measured on the training data so they just keep improving as k goes
# up, they cannot really pick k here. I went with 4 because the topics read well
for (k in 2:6) {
  m <- LDA(dtm, k = k, control = list(seed = 2026))
  cat("k =", k, " logLik =", round(as.numeric(logLik(m)), 1),
      " perplexity =", round(perplexity(m), 1), "\n")
}


# Q2.3 - Visualise the top key words per topic using two methods

# Draws one word cloud. The margins are set to zero so the cloud gets the whole
# panel, otherwise the words get squeezed and some of them do not get plotted
draw_cloud <- function(df, i) {
  par(mar = c(0, 0, 3, 0))
  set.seed(2026)
  wordcloud(words = df$term, freq = df$probability,
            scale = c(2.8, 0.8), min.freq = 0, max.words = 30,
            random.order = FALSE, rot.per = 0.15,
            colors = brewer.pal(8, "Dark2"))
  title(paste("Word Cloud for Topic", i), cex.main = 1.4)
}

for (i in 1:num_topics) {
  topic_df <- topic_dfs[[i]]

  # Method 1 - bar plot of the word probabilities
  p <- ggplot(topic_df, aes(x = reorder(term, probability), y = probability)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Top", num_words, "Terms in Topic", i),
         x = "Term", y = "Probability") +
    theme_minimal()
  print(p)

  # Method 2 - word cloud of the same probabilities
  draw_cloud(topic_df, i)

  # Save both to file at a fixed size for the writeup
  ggsave(paste0("topic", i, "_barplot.png"), p, width = 6, height = 4, dpi = 150)
  png(paste0("topic", i, "_wordcloud.png"), width = 820, height = 580)
  draw_cloud(topic_df, i)
  dev.off()
}

par(mar = c(5, 4, 4, 2))  # put the margins back
