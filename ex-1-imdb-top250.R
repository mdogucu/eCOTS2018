## Scrape IMDB top 250

# Load packages ----
library(rvest)                         # webscraping
library(tidyverse)                     # data wrangling and visualization
library(robotstxt)                     # checking bot permissions

# Check if a bot has permisson to access page ----
paths_allowed("http://www.imdb.com")
paths_allowed("http://www.facebook.com")

# Read the entire page ----
page <- read_html("http://www.imdb.com/chart/top")

# Scrape titles ----
titles <- page %>%
  html_nodes(".titleColumn a") %>%
  html_text()

# Scrape years ----
years <- page %>%
  html_nodes(".secondaryInfo") %>%
  html_text() %>%
  str_remove("\\(") %>%                # remove (
  str_remove("\\)") %>%                # remove )
  as.numeric()

# Scrape ratings ----
ratings <- page %>%
  html_nodes("#main strong") %>%
  html_text() %>%
  as.numeric()

# Save titles, years, and rating together in a tibble ----
imdb_top_250 <- tibble(
  title = titles, 
  year = years, 
  rating = ratings
)

# Visualize avg rating over time ----
imdb_top_250 %>%
  group_by(year) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(mapping = aes(x = year,y = avg_rating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
