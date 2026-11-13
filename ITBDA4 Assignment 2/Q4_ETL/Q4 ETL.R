# Q4.1 - Scrape the programme listing and build a data frame out of it

library(rvest)
library(robotstxt)
library(stringr)
library(DBI)
library(RPostgres)
library(ggplot2)

# Use case: Eduvos puts every qualification it offers on one listing page. I am
# scraping that page to build a dataset of what is on offer, by faculty and by
# level, which is the sort of thing you would need to compare a prospectus.

page_url <- "https://www.eduvos.com/qualifications/"

# Fair use check. Ask robots.txt before taking anything off the site
paths_allowed(page_url)

page <- read_html(page_url)

# Each programme title is a link through to /programmes/<slug>
cards <- html_elements(page, "a[href*='/programmes/']")
length(cards)

if (length(cards) == 0) stop("No programme links found, the page layout has changed")

raw <- data.frame(name = trimws(html_text2(cards)),
                  link = html_attr(cards, "href"),
                  stringsAsFactors = FALSE)

# This is the raw state and it is a mess, which is the point. There are nav
# links in here, the same programme repeated with a different ?id on the end,
# a mix of relative and absolute urls, and rows that are not qualifications
nrow(raw)
head(raw, 5)
sum(grepl("\\?", raw$link))          # how many carry a query string
sum(grepl("^https?://", raw$link))   # how many are absolute


# Q4.2 - Clean it up, fill in what is missing, write the csv

clean <- raw

# Knock the domain and the ?id off, otherwise the same programme comes through
# as several different rows
clean$link <- sub("^https?://[^/]+", "", clean$link)
clean$link <- sub("\\?.*$", "", clean$link)

# Whatever is left after /programmes/ is the slug. A row with nothing there is
# the section link itself, not a programme. The prefix has to come off before
# the trailing slash does, otherwise "/programmes/" never matches and the nav
# link survives all the way into the table
clean$slug <- sub("/$", "", sub("^/programmes/?", "", clean$link))
clean <- clean[nchar(clean$slug) > 0, ]
clean <- clean[nchar(clean$name) > 3, ]
clean <- clean[!duplicated(clean$slug), ]
nrow(clean)

# Delivery mode is tacked onto the end of the name after a dash, and the site
# writes it four or five different ways. Online Part Time, Distance Full Time,
# Distance (Online) Part Time, Campus Full Time. Distance is just their word
# for online so it gets folded in with it
mode_pattern <- regex("\\s*-\\s*(online|distance|campus)?[^-]*?(part|full)\\s*time\\s*$",
                      ignore_case = TRUE)
mode_txt <- str_extract(clean$name, mode_pattern)

online <- grepl("online|distance", mode_txt, ignore.case = TRUE) & !is.na(mode_txt)
part   <- grepl("part\\s*time",    mode_txt, ignore.case = TRUE) & !is.na(mode_txt)
full   <- grepl("full\\s*time",    mode_txt, ignore.case = TRUE) & !is.na(mode_txt)

clean$delivery_mode <- "Campus"
clean$delivery_mode[online & part]  <- "Online Part Time"
clean$delivery_mode[online & full]  <- "Online Full Time"
clean$delivery_mode[!online & part] <- "Campus Part Time"
clean$delivery_mode[!online & full] <- "Campus Full Time"

# Take the suffix back off so the name is just the qualification
clean$name <- str_trim(str_replace(clean$name, mode_pattern, ""))

# Field of study out of the name. The \\b on "in" matters, without it the match
# lands inside words and "Bachelor of Business Administration" comes back as
# "ess Administration"
clean$field <- str_trim(str_replace(clean$name, "^.*?(\\bin\\b|:)\\s*", ""))

# Regex to work the qualification level out of the name. This is the inferred
# numerical attribute, mapped onto the SA NQF levels
level_from_name <- function(nm) {
  n <- tolower(nm)
  if (str_detect(n, "doctor|phd"))                   return(10)
  if (str_detect(n, "master"))                       return(9)
  if (str_detect(n, "honours|postgraduate diploma|postgraduate certificate")) return(8)
  if (str_detect(n, "bachelor"))                     return(7)
  if (str_detect(n, "advanced diploma|diploma"))     return(6)
  if (str_detect(n, "higher certificate"))           return(5)
  return(NA)
}
clean$nqf_level <- sapply(clean$name, level_from_name)

# Faculty is not in the link text at all so it has to come off the name. Order
# matters a lot here. Law has to be the actual qualification or "Bachelor of
# Arts: Law and Politics" ends up under Law, and the science check has to sit
# above the management keywords or "Biotechnology Management" goes to Commerce
guess_faculty <- function(nm) {
  n <- tolower(nm)
  if (str_detect(n, "^bachelor of laws|^master of laws|\\bin law\\b|\\bof laws?\\b|\\bllb\\b|\\bllm\\b")) return("Law")
  if (str_detect(n, "information technology|information systems|data science|robotic|software|network|comput|cyber|web and ux")) return("Technology")
  if (str_detect(n, "bachelor of arts|^ba\\b|arts honours|art and design|graphic design|psycholog|media|education|teaching|social science|english|politics|communication")) return("Humanities and Arts")
  if (str_detect(n, "bioscience|biomedic|biotechnolog|\\bscience\\b"))  return("Applied Sciences")
  if (str_detect(n, "commerce|account|business|marketing|finance|tourism|human resource|management|managing|leadership|directorship|entrepreneur")) return("Commerce")
  return(NA)
}
clean$faculty <- sapply(clean$name, guess_faculty)

# What is still missing after all that
sum(is.na(clean$nqf_level))
sum(is.na(clean$faculty))
clean$name[is.na(clean$faculty) | is.na(clean$nqf_level)]

# Fill the gaps rather than dropping the rows. Faculty gets the most common
# value and level gets the median for its own faculty, which is the usual way
# of filling a categorical and a numeric column
modal_faculty <- names(sort(table(clean$faculty), decreasing = TRUE))[1]
clean$faculty[is.na(clean$faculty)] <- modal_faculty

for (f in unique(clean$faculty)) {
  rows <- clean$faculty == f
  clean$nqf_level[rows & is.na(clean$nqf_level)] <- round(median(clean$nqf_level[rows], na.rm = TRUE))
}

# Anything with no faculty to borrow from falls back to the overall median
clean$nqf_level[is.na(clean$nqf_level)] <- round(median(clean$nqf_level, na.rm = TRUE))

# A median can land halfway between two levels and turn the whole column into a
# double, and then Postgres will not take it against an INT column
clean$nqf_level <- as.integer(clean$nqf_level)

# Column order has to line up with the CREATE TABLE further down
cleaned <- clean[, c("name", "field", "faculty", "nqf_level", "delivery_mode", "link")]
cleaned <- cleaned[order(cleaned$faculty, cleaned$name), ]

dim(cleaned)
head(cleaned, 10)
table(cleaned$faculty)
table(cleaned$nqf_level)
table(cleaned$delivery_mode)

write.csv(cleaned, "eduvos_programmes_clean.csv", row.names = FALSE)


# Q4.3 - Load the csv into Postgres and run two investigations

# There is no R to Postgres code in the notes, the database work is all pgAdmin
# and the python notebook, so DBI and RPostgres for the load
con <- dbConnect(RPostgres::Postgres(),
                 host     = "localhost",
                 port     = 5432,
                 dbname   = "postgres",
                 user     = "postgres",
                 password = rstudioapi::askForPassword("Postgres password"))

dbExecute(con, "CREATE SCHEMA IF NOT EXISTS eduvos_schema;")
dbExecute(con, "DROP TABLE IF EXISTS eduvos_schema.Programme;")
dbExecute(con, "DROP TABLE IF EXISTS eduvos_schema.Faculty;")

# Two tables so the queries can join, the same way the join examples do

# Create Faculty Table
dbExecute(con, "
CREATE TABLE eduvos_schema.Faculty(
	FacultyID INT PRIMARY KEY,
	FacultyName VARCHAR(50) NOT NULL
);")

# Create Programme Table
dbExecute(con, "
CREATE TABLE eduvos_schema.Programme(
	ProgrammeID SERIAL PRIMARY KEY,
	ProgrammeName VARCHAR(200) NOT NULL,
	Field VARCHAR(150),
	FacultyID INT NOT NULL REFERENCES eduvos_schema.Faculty(FacultyID),
	NQFLevel INT,
	DeliveryMode VARCHAR(50),
	Link VARCHAR(300)
);")

# Read the csv back off disk so the load really is coming from the file
csv_data <- read.csv("eduvos_programmes_clean.csv", stringsAsFactors = FALSE)

faculty_tbl <- data.frame(facultyid   = seq_along(sort(unique(csv_data$faculty))),
                          facultyname = sort(unique(csv_data$faculty)),
                          stringsAsFactors = FALSE)

programme_tbl <- data.frame(
  programmename = csv_data$name,
  field         = csv_data$field,
  facultyid     = faculty_tbl$facultyid[match(csv_data$faculty, faculty_tbl$facultyname)],
  nqflevel      = csv_data$nqf_level,
  deliverymode  = csv_data$delivery_mode,
  link          = csv_data$link,
  stringsAsFactors = FALSE)

dbWriteTable(con, Id(schema = "eduvos_schema", table = "faculty"),   faculty_tbl,   append = TRUE)
dbWriteTable(con, Id(schema = "eduvos_schema", table = "programme"), programme_tbl, append = TRUE)

# Check it went in
dbGetQuery(con, "SELECT COUNT(*) AS Programmes FROM eduvos_schema.Programme;")
dbGetQuery(con, "SELECT * FROM eduvos_schema.Faculty ORDER BY FacultyID;")


# INVESTIGATION 1 - how many programmes each faculty offers at each level, with
# subtotals per faculty and a grand total on the end
inv1 <- "
SELECT 
	f.FacultyName,
	p.NQFLevel,
	COUNT(*) AS Programmes,
	GROUPING(f.FacultyName) AS GroupFaculty,
	GROUPING(p.NQFLevel) AS GroupLevel

FROM eduvos_schema.Programme p
INNER JOIN eduvos_schema.Faculty f
	ON p.FacultyID = f.FacultyID

GROUP BY ROLLUP(f.FacultyName,p.NQFLevel)
ORDER BY f.FacultyName, p.NQFLevel;"

res1 <- dbGetQuery(con, inv1)
res1


# INVESTIGATION 2 - how the programmes split across faculty and delivery mode
inv2 <- "
SELECT 
	f.FacultyName,
	p.DeliveryMode,
	COUNT(*) AS Programmes,
	ROUND(AVG(p.NQFLevel),1) AS AvgNQF

FROM eduvos_schema.Programme p
INNER JOIN eduvos_schema.Faculty f
	ON p.FacultyID = f.FacultyID

GROUP BY GROUPING SETS
(
	(f.FacultyName),
	(p.DeliveryMode),
	(f.FacultyName,p.DeliveryMode),
	()
)
ORDER BY f.FacultyName, p.DeliveryMode;"

res2 <- dbGetQuery(con, inv2)
res2

# A quick look at the faculty totals for the writeup. COUNT comes back from
# Postgres as a bigint, which lands in R as integer64, and ggplot has no scale
# for that type, so it needs converting first
fac_totals <- res1[res1$grouplevel == 1 & res1$groupfaculty == 0, c("facultyname", "programmes")]
fac_totals$programmes <- as.numeric(fac_totals$programmes)
p1 <- ggplot(fac_totals, aes(x = reorder(facultyname, programmes), y = programmes)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Eduvos Programmes by Faculty",
       x = "Faculty", y = "Number of programmes") +
  theme_minimal()
print(p1)
ggsave("q4_programmes_by_faculty.png", p1, width = 7, height = 5, dpi = 150)

dbDisconnect(con)


# Q4.4 - Push the whole thing to GitHub
#
# git init
# git add .
# git commit -m "ITBDA4 Assignment 2 - MapReduce, LDA, WebText API and ETL"
# git branch -M main
# git remote add origin https://github.com/KailashN666/ITBDA4-Big-Data-Analytics-Assignment-2.git
# git push -u origin main
