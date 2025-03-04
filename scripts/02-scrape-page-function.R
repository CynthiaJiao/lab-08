# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)

# function: scrape_page --------------------------------------------------------

scrape_page <- function(url){
  page <- read_html(url)
    titles <- page %>%
      html_nodes(".iteminfo") %>%
      html_node("h3 a") %>% ## this is retrieved using selector Gadget
      html_text() %>%
      str_squish() ## removes the long spaces between titles and year
    links <- page %>%
      html_nodes(".iteminfo") %>%
      html_node("h3 a") %>%
      html_attr("href") # get href attribute/links instead of text
    artists <- page %>%
      html_nodes(".iteminfo") %>%
      html_node(".artist") %>%
      html_text()
    art <- tibble(
      Title = titles,
      Artist = artists,
      Link = links
    )
    return(art) ### what is returned needs to be stated HERE!!
    
}
  
  
# test scrape page function

scrape_page("https://collections.ed.ac.uk/art/search/*:*/Collection:%22edinburgh+college+of+art%7C%7C%7CEdinburgh+College+of+Art%22?offset=0")



scrape_page("https://collections.ed.ac.uk/art/search/*:*/Collection:%22edinburgh+college+of+art%7C%7C%7CEdinburgh+College+of+Art%22?offset=10")