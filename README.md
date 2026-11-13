# ITBDA4 Big Data Analytics Techniques - Assignment 2

Eduvos Online | Honours in Data Science (NQF 8) | EDUV11298859

Hadoop MapReduce, topic modelling, a web text mining proof of concept,
and an ETL pipeline into PostgreSQL.

## Layout

| Path | Contents |
|---|---|
| `ITBDA4 Assignment 2/Q1_MapReduce/` | Java MapReduce keyword search, the built jar, HDFS output, run evidence, R appendix version |
| `ITBDA4 Assignment 2/Q2_LDA/` | LDA topic modelling on the news corpus, bar plots and word clouds per topic |
| `ITBDA4 Assignment 2/Q3_WebText/` | SAnews scraper, TF and TF-IDF summarisation, scraped data and plots |
| `ITBDA4 Assignment 2/Q4_ETL/` | Eduvos programme scraper, cleaning and imputation, PostgreSQL load and queries |
| `news/sa_news/` | Q2 corpus, 10 articles |
| `map_reduce_test_docs/` | Q1 corpus, 7 documents |
| `ITBDA4 T14 - Assignment 2 ... Writeup.pdf` | Full writeup, Questions 1 to 4 |

The two corpora sit at the repository root rather than inside the code
folder because the news path is hardcoded in `Q2 LDA.R`.

## Running it

**Q1** needs Hadoop 3.3.6 running in pseudo-distributed mode:

    hadoop jar KeywordSearch.jar KeywordSearch /input_dir /output_dir africa

The keyword is the third argument and is never hardcoded. The R version
in the same folder runs standalone in RStudio.

**Q2, Q3 and Q4** are R scripts. Q2 needs `topicmodels`, `tm`, `wordcloud`,
`RColorBrewer`, `stopwords` and `ggplot2`. Q3 adds `rvest` and `robotstxt`.
Q4 adds `DBI` and `RPostgres`, and prompts for a PostgreSQL password.

Q3 scrapes live news, so each run returns whatever is currently published.
