## Scrape first page of 
## Most Popular Feature Films Released 2017-01-01 to 2017-12-31

# Load packages ----
library(rvest)                         # webscraping
library(tidyverse)                     # data wrangling
library(broom)                         # model output

# Save URL ----
first_url <- "http://www.imdb.com/search/title?year=2017&title_type=feature&page=1&ref_=adv_nxt"

# Read page ----
page <- read_html(first_url)

# Scrape titles ----
titles <- page %>%
  html_nodes(".lister-item-header a") %>%
  html_text()

# Scrape ratings ----
ratings <- page %>%
  html_nodes(".ratings-imdb-rating strong") %>%
  html_text() %>%
  as.numeric()

# Scrape metascores ----
meta_scores <- page %>%
  html_nodes(".ratings-bar") %>%
  html_text() %>%
  str_remove_all("\\n") %>%            # remove all occurances of \n
  str_remove_all(" ") %>%              # remove all white spaces
  str_split("X") %>%                   # split at "X"
  map(2, .default = NA) %>%            # default to NA for missing metascores 
  str_remove("Metascore") %>%
  str_trim() %>%
  as.numeric()                         # will result in warning about NAs

# Scrape run times ----
run_times <- page %>%
  html_nodes(".text-muted .runtime") %>%
  html_text() %>%
  str_remove(" min") %>%               # remove " min"
  as.numeric()

# Scrape votes ----
votes <- page %>%
  html_nodes(".sort-num_votes-visible span:nth-child(2)") %>%
  html_text() %>%
  str_remove_all(",") %>%              # remove all commas in each entry
  as.numeric()

# Scrape grosses ----
grosses <- page %>%
  html_nodes(".sort-num_votes-visible") %>%
  html_text() %>%
  str_split("\\|") %>%
  map(2, .default = NA) %>%            # default to NA for missing grosses
  str_remove("Gross:") %>%
  str_remove_all("\\\n") %>%
  str_trim() %>%
  str_remove("\\$") %>%
  str_remove("M") %>%
  as.numeric()                         # will result in warning about NAs

# Save variables in a tibble
featured <- tibble(
  title = titles,
  rating = ratings,
  meta_score = meta_scores,
  run_time = run_times,
  vote = votes,
  gross = grosses
)

# Fit a model predicting rating ----
m <- lm(rating ~ meta_score + gross + run_time, data = featured)
tidy(m)
