# Q3.1 - Scrape a website and build an R data structure from the clean text

library(rvest)
library(robotstxt)
library(tm)
library(stopwords)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

base_url  <- "https://www.sanews.gov.za"
index_url <- "https://www.sanews.gov.za/south-africa"
n_articles <- 12

# Fair use check. Ask robots.txt first instead of just assuming it is allowed.
# SAnews also lets media reuse its stories as long as it gets credited
paths_allowed(index_url)

# Grab the article links off the index page. The headlines are h2 elements
# wrapping a link to the story
index_page <- read_html(index_url)

# Hrefs come back relative on some pages and absolute on others, so knock the
# domain off first and then keep the ones that look like a story
story_links <- function(nodes) {
  href <- html_attr(nodes, "href")
  href <- href[!is.na(href)]
  href <- sub("^https?://[^/]+", "", href)
  unique(href[grepl("^/south-africa/.+", href)])
}

links <- story_links(html_elements(index_page, "h2 a"))

# If the h2 selector comes up empty the page layout has changed, so fall back
# to every link on the page
if (length(links) == 0) {
  links <- story_links(html_elements(index_page, "a"))
}

if (length(links) == 0) stop("No article links found, check the page layout")

links <- paste0(base_url, head(links, n_articles))
length(links)
head(links, 3)

# Pull one article and return its url, title and body text
get_article <- function(url) {
  Sys.sleep(1)   # one second between requests so we are not hammering the site
  page <- read_html(url)

  title <- html_text2(html_element(page, "h1"))

  # The story sits in the body field. Fall back to the article element if the
  # class name is not there, otherwise we would also scrape the menus and footer
  node <- html_element(page, ".field--name-body")
  if (is.na(node)) node <- html_element(page, "article")
  if (is.na(node)) node <- html_element(page, "body")

  paras <- html_text2(html_elements(node, "p"))
  paras <- paras[nchar(paras) > 40]   # drop captions, bylines and stray links

  data.frame(url = url,
             title = title,
             content = paste(paras, collapse = " "),
             stringsAsFactors = FALSE)
}

# Build the data frame, skipping anything that fails rather than stopping
articles <- data.frame()
for (u in links) {
  res <- try(get_article(u), silent = TRUE)
  if (!inherits(res, "try-error")) articles <- rbind(articles, res)
}

articles$words <- sapply(strsplit(articles$content, "\\s+"), length)

# Check what came back
dim(articles)
names(articles)
articles[, c("title", "words")]

write.csv(articles, "sanews_scraped.csv", row.names = FALSE)


# Q3.2 - Preprocess the text and strip out the non pertinent content

# Same cleaning approach as the text analysis practicals
clean_text <- function(txt) {
  out <- tolower(txt)
  out <- stringr::str_replace_all(out, "http\\S+", " ")          # urls
  out <- stringr::str_replace_all(out, "[^a-zA-Z\\s]", " ")      # non characters
  out <- stringr::str_replace_all(out, "[\\s]+", " ")            # extra spaces
  trimws(out, which = c("both"))
}

# Every article carries its date, so day and month names turn up in nearly all
# of them. The rest is media release filler that tells us nothing
custom_stopwords <- c(tolower(weekdays(as.Date("2024-07-08") + 0:6)),
                      tolower(month.name),
                      "said", "says", "will", "can", "may", "also", "told",
                      "added", "sanews", "gov", "www", "https", "read")

stw <- c(stopwords("en"), custom_stopwords)

# Tokenise one piece of text into words with the stop words taken out
tokenise <- function(txt) {
  w <- unlist(strsplit(clean_text(txt), "\\s+"))
  w <- w[!w %in% stw]
  w[nchar(w) > 2]
}

articles$clean <- sapply(articles$content, clean_text)

# Bag of words across the whole corpus
all_words <- unlist(lapply(articles$content, tokenise))
bow <- sort(table(all_words), decreasing = TRUE)

bow_df <- data.frame(Word = names(bow), Frequency = as.numeric(bow),
                     stringsAsFactors = FALSE)

# Check the cleaning worked
length(all_words)
nrow(bow_df)
head(bow_df, 15)


# Q3.3 - Summarise the collected text using two techniques, with visualisations

# Both techniques are extractive, they score every sentence and keep the highest
# scoring ones. The difference is how a word is weighted

# Split text into sentences. The abbreviations that end in a full stop get
# protected first, otherwise the split chops a sentence in half at something
# like "Act No. 2 of 2021"
split_sentences <- function(txt) {
  txt <- gsub("\\b(No|Act|Dr|Mr|Mrs|Ms|Prof|St|vs|etc)\\.", "\\1<DOT>", txt)
  s <- unlist(strsplit(txt, "(?<=[.!?])\\s+", perl = TRUE))
  s <- gsub("<DOT>", ".", s)
  s <- trimws(s)
  s[nchar(s) > 40]   # drop fragments
}

sentences <- unlist(lapply(articles$content, split_sentences))
length(sentences)

# Term frequency across the corpus
tf <- setNames(bow_df$Frequency, bow_df$Word)

# Document frequency, how many articles each word shows up in
df_counts <- table(unlist(lapply(articles$content, function(x) unique(tokenise(x)))))
df <- setNames(as.numeric(df_counts), names(df_counts))

# Inverse document frequency and TF-IDF
idf   <- log(nrow(articles) / df)
tfidf <- tf[names(idf)] * idf

# Score every sentence against a weighting, divided by length so long sentences
# do not win just by having more words in them
score_sentences <- function(sentences, weights) {
  sapply(sentences, function(s) {
    w <- tokenise(s)
    if (length(w) == 0) return(0)
    sum(weights[w], na.rm = TRUE) / length(w)
  })
}

# Technique 1 - term frequency scoring
tf_scores <- score_sentences(sentences, tf)

# Technique 2 - TF-IDF scoring
tfidf_scores <- score_sentences(sentences, tfidf)

n_keep <- 5

# Keep the top sentences but put them back in the order they appeared, otherwise
# the summary reads as a jumble
tf_pick    <- sort(order(tf_scores, decreasing = TRUE)[1:n_keep])
tfidf_pick <- sort(order(tfidf_scores, decreasing = TRUE)[1:n_keep])

cat("\n===== SUMMARY 1 - TERM FREQUENCY =====\n")
for (i in tf_pick) cat("*", sentences[i], "\n\n")

cat("\n===== SUMMARY 2 - TF-IDF =====\n")
for (i in tfidf_pick) cat("*", sentences[i], "\n\n")

# How much the two techniques agree
cat("Sentences chosen by both:", length(intersect(tf_pick, tfidf_pick)), "of", n_keep, "\n")


# Visualisation 1 - the 20 most frequent words
top_words <- head(bow_df, 20)
p1 <- ggplot(top_words, aes(x = reorder(Word, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Words in the Scraped SAnews Articles",
       x = "Word", y = "Frequency") +
  theme_minimal()
print(p1)
ggsave("q3_top_words.png", p1, width = 7, height = 5, dpi = 150)

# Visualisation 2 and 3 - the TF word cloud against the TF-IDF word cloud. TF
# just shows what comes up most, TF-IDF pushes up the words that are common
# here but rare across the rest of the articles
tf_df    <- data.frame(Word = names(tf), Frequency = as.numeric(tf),
                       stringsAsFactors = FALSE)
tfidf_df <- data.frame(Word = names(tfidf), Frequency = as.numeric(tfidf),
                       stringsAsFactors = FALSE)
tfidf_df <- tfidf_df[!is.na(tfidf_df$Frequency) & tfidf_df$Frequency > 0, ]

draw_cloud <- function(df, main_title) {
  par(mar = c(0, 0, 3, 0))
  set.seed(2026)
  wordcloud(words = df$Word, freq = df$Frequency,
            scale = c(3, 0.6), max.words = 60, min.freq = 0,
            random.order = FALSE, rot.per = 0.15,
            colors = brewer.pal(8, "Dark2"))
  title(main_title, cex.main = 1.4)
}

draw_cloud(tf_df, "Word Cloud by Term Frequency")
png("q3_tf_wordcloud.png", width = 820, height = 580)
draw_cloud(tf_df, "Word Cloud by Term Frequency")
invisible(dev.off())

draw_cloud(tfidf_df, "Word Cloud by TF-IDF")
png("q3_tfidf_wordcloud.png", width = 820, height = 580)
draw_cloud(tfidf_df, "Word Cloud by TF-IDF")
invisible(dev.off())

# What each measure puts at the top, so the two clouds can be compared
head(tf_df[order(-tf_df$Frequency), ], 10)
head(tfidf_df[order(-tfidf_df$Frequency), ], 10)

# Visualisation 4 - sentence scores with the chosen sentences marked
sc <- data.frame(Sentence = seq_along(sentences),
                 Score = tfidf_scores,
                 Chosen = ifelse(seq_along(sentences) %in% tfidf_pick, "Kept", "Dropped"))
p3 <- ggplot(sc, aes(x = Sentence, y = Score, fill = Chosen)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Dropped" = "grey70", "Kept" = "firebrick")) +
  labs(title = "TF-IDF Score of Every Sentence, with the Summary Sentences Marked",
       x = "Sentence number", y = "TF-IDF score") +
  theme_minimal()
print(p3)
ggsave("q3_sentence_scores.png", p3, width = 8, height = 4.5, dpi = 150)
