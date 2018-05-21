## Scrape first three pages of 
## Most Popular Feature Films Released 2017-01-01 to 2017-12-31

# Load packages ----
library(rvest)                         # webscraping
library(tidyverse)                     # data wrangling
library(broom)                         # model output

# Construct URLs ----
urls <- "http://www.imdb.com/search/title?year=2017&title_type=feature&page=" %>%
  paste0(1:3, "&ref_=adv_nxt")

# Write a function that scrapes attributes of movies ----
# titles, ratings, meta scores, run times, votes, grosses
scrape_2017_movies <- function(url){
  
  page <- read_html(url)
  
  titles <- page %>%
    html_nodes(".lister-item-header a") %>%
    html_text()
  
  ratings <- page %>%
    html_nodes(".ratings-imdb-rating strong") %>%
    html_text() %>%
    as.numeric()
  
  meta_scores <- page %>%
    html_nodes(".ratings-bar") %>%
    html_text() %>%
    str_remove_all("\\n") %>%
    str_remove_all(" ") %>%
    str_split("X") %>%
    map(2, .default = NA) %>%
    str_remove("Metascore") %>%
    str_trim() %>%
    as.numeric()
  
  run_times <- page %>%
    html_nodes(".text-muted .runtime") %>%
    html_text() %>%
    str_replace("min", "") %>%
    as.numeric()
  
  votes <- page %>%
    html_nodes(".sort-num_votes-visible span:nth-child(2)") %>%
    html_text() %>%
    str_replace_all(",", "") %>%
    as.numeric()
  
  grosses <- page %>%
    html_nodes(".sort-num_votes-visible") %>%
    html_text() %>%
    str_split("\\|") %>%
    map(2, .default = NA) %>%
    str_replace_all("Gross:","") %>%
    str_replace_all("\\\n","") %>%
    str_trim() %>%
    str_replace_all("\\$","") %>%
    str_replace_all("M","") %>%
    as.numeric()
  
  tibble(
    title = titles,
    rating = ratings,
    meta_score = meta_scores,
    run_time = run_times,
    vote = votes,
    gross = grosses
  )
}

featured <- map_df(urls, scrape_2017_movies)

# Fit a model predicting rating from run time ----
library(broom)

m <- lm(rating ~ run_time + vote, data = featured)
tidy(m)
