# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)


# set url ----------------------------------------------------------------------

first_url <- "https://collections.ed.ac.uk/art/search/*:*/Collection:%22edinburgh+college+of+art%7C%7C%7CEdinburgh+College+of+Art%22?offset=0"

# read first page --------------------------------------------------------------

page <- read_html(first_url)

# scrape titles ----------------------------------------------------------------

titles <- page %>%
  html_nodes(".iteminfo") %>%
  html_node("h3 a") %>% ## this is retrieved using selector Gadget
  html_text() %>%
  str_squish() ## removes the long spaces between titles and year

titles

# scrape links -----------------------------------------------------------------

links <- page %>%
  html_nodes(".iteminfo") %>%
  html_node("h3 a") %>%
  html_attr("href") # get href attribute/links instead of text
  
links

# scrape artists ---------------------------------------------------------------

artists <- page %>%
  html_nodes(".iteminfo") %>%
  html_node(".artist") %>%
  html_text()

artists

# put together in a data frame -------------------------------------------------

first_ten <- tibble(
  Title = titles,
  Artist = artists,
  Link = links
)

first_ten

# scrape second ten paintings --------------------------------------------------

second_url <- "https://collections.ed.ac.uk/art/search/*:*/Collection:%22edinburgh+college+of+art%7C%7C%7CEdinburgh+College+of+Art%22?offset=10"

page_sec <- read_html(second_url)

# scrape artists, links, and titles

titles_sec <- page_sec %>%
  html_nodes(".iteminfo") %>%
  html_node(".record-title") %>% ## this is retrieved using selector Gadget
  html_text() %>%
  str_squish() ## removes the long spaces between titles and year

titles_sec

links_sec <- page_sec %>%
  html_nodes(".iteminfo") %>%
  html_node("h3 a") %>%
  html_attr("href") # get href attribute/links instead of text

links_sec

artists_sec <- page_sec %>%
  html_nodes(".iteminfo") %>%
  html_node(".artist") %>%
  html_text()

artists_sec

#

second_ten <- tibble(
  Title = titles_sec,
  Artist = artists_sec,
  Link = links_sec
)