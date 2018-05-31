### Web scraping
### Database 2017 creation

library("xml2")
library("rvest")
library("magrittr")

# IMDB TOP US GROSSING 2017: 50 MORE PROFITABLE MOVIES OF 2017 -------------

url <- "https://www.imdb.com/search/title?release_date=2017-01-01,2017-12-31&sort=boxoffice_gross_us,desc"
page <- read_html(url)

# Movies details
movie_nodes <- html_nodes(page, '.lister-item-header a') 
movie_link <- sapply(html_attrs(movie_nodes),`[[`,'href')
movie_link <- paste0("http://www.imdb.com", movie_link)
movie_crewlink <- gsub("[?]", "fullcredits?", movie_link) #Full crew links
movie_name <- html_text(movie_nodes)
movie_year <- rep(2017, 50)
movie_gross <- html_nodes(page, '.sort-num_votes-visible span:nth-child(5)') %>%
  html_text()

# CREATE DATAFRAME: TOP 2017 ----------------------------------------------

top_2017 <- data.frame(movie_name, movie_year, movie_gross, movie_crewlink, stringsAsFactors = FALSE)