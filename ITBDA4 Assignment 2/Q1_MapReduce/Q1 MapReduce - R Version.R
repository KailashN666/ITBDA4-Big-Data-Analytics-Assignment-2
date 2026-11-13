# Q1 - MapReduce keyword search implemented in R
# Appendix version. The submitted project is the Java one that ran on Hadoop;
# this reproduces the same Map -> Shuffle/Sort -> Reduce flow in R and was used
# to cross-check that the cluster output was correct.
#
# Runs either way:
#   RStudio      - set corpus_dir and keyword below, then Source the script
#   Command line - Rscript "<this file>" <corpus_dir> <keyword> [word|substring]

# Q1.4 - Job configuration, the equivalent of the Hadoop Job object

# Settings used when running from RStudio
corpus_dir <- "C:/ITBDA4_Q1/docs"
keyword    <- "africa"
match_mode <- "word"   # "word" for whole words, "substring" to also match "African"

# Command line arguments override them, which is what keeps the keyword dynamic
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2) {
  corpus_dir <- args[1]
  keyword    <- args[2]
  if (length(args) >= 3) match_mode <- args[3]
}

job_conf <- list(
  input_dir      = corpus_dir,
  search_keyword = keyword,
  match_mode     = match_mode,
  case_sensitive = FALSE
)


# Q1.2 - Mapper
# Input  : key = file name, value = the whole document
# Output : a list of pairs, key = file name, value = "row,col"

map_function <- function(key, value, conf) {

  keyword <- tolower(conf$search_keyword)
  pairs <- list()
  lines <- strsplit(value, "\n", fixed = TRUE)[[1]]

  for (row in seq_along(lines)) {

    words <- strsplit(trimws(lines[row]), "[[:space:]]+")[[1]]
    if (length(words) == 0) next

    for (col in seq_along(words)) {

      # Strip surrounding punctuation so "Africa," still matches "africa"
      token <- tolower(gsub("^[[:punct:]]+|[[:punct:]]+$", "", words[col]))

      hit <- if (conf$match_mode == "substring") {
        grepl(keyword, token, fixed = TRUE)
      } else {
        token == keyword
      }

      if (hit) {
        pairs[[length(pairs) + 1]] <- list(key = key, value = paste(row, col, sep = ","))
      }
    }
  }
  pairs
}


# Shuffle and sort - Hadoop does this automatically, reproduced here so the
# full data flow is visible. Groups all values by their key.

shuffle_and_sort <- function(pairs) {
  grouped <- list()
  for (p in pairs) {
    grouped[[p$key]] <- c(grouped[[p$key]], p$value)
  }
  grouped[order(names(grouped))]
}


# Q1.3 - Reducer
# Input  : key = file name, values = all "row,col" strings for that file
# Output : one row per document with the count and every location

reduce_function <- function(key, values, conf) {

  positions <- do.call(rbind, lapply(values, function(v) {
    rc <- as.integer(strsplit(v, ",", fixed = TRUE)[[1]])
    data.frame(row = rc[1], column = rc[2])
  }))

  # Hadoop gives no guarantee on the order of values within a group, so sort
  positions <- positions[order(positions$row, positions$column), ]

  data.frame(
    document    = key,
    keyword     = conf$search_keyword,
    occurrences = nrow(positions),
    locations   = paste(sprintf("(%d,%d)", positions$row, positions$column), collapse = " "),
    stringsAsFactors = FALSE
  )
}


# Driver - runs Map, then Shuffle and Sort, then Reduce

run_mapreduce <- function(conf) {

  files <- list.files(conf$input_dir, pattern = "\\.txt$", full.names = TRUE)
  if (length(files) == 0) stop("No .txt files found in: ", conf$input_dir)

  cat("Documents :", length(files), "\n")
  cat("Keyword   :", conf$search_keyword, "\n")
  cat("Match mode:", conf$match_mode, "\n\n")

  # Map phase - one mapper per file, the same as one split per file in Hadoop
  all_pairs <- list()
  for (f in files) {
    content <- paste(readLines(f, warn = FALSE), collapse = "\n")
    all_pairs <- c(all_pairs, map_function(basename(f), content, conf))
  }
  cat("Mapper emitted", length(all_pairs), "intermediate key-value pairs\n\n")

  # Shuffle and sort
  grouped <- shuffle_and_sort(all_pairs)

  # Reduce phase
  results <- do.call(rbind, lapply(names(grouped), function(k) {
    reduce_function(k, grouped[[k]], conf)
  }))

  # Documents with no match are reported as zero so the whole corpus is covered
  missing <- setdiff(basename(files), results$document)
  if (length(missing) > 0) {
    results <- rbind(results, data.frame(
      document = sort(missing), keyword = conf$search_keyword,
      occurrences = 0L, locations = "-", stringsAsFactors = FALSE))
  }
  results[order(results$document), ]
}


final <- run_mapreduce(job_conf)
print(final, row.names = FALSE)

