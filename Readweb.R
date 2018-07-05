#Loading the rvest package
#https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
install.packages('rvest')
library('rvest')

#Specifying the url for desired website to be scrapped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'
#Reading the HTML code from the website
webpage <- read_html(url)
#Using CSS selectors to scrap the rankings section
rank_data_html <- html_nodes(webpage,'."yt-uix-tile-link yt-ui-ellipsis yt-ui-ellipsis-2 yt-uix-sessionlink      spf-link " ')
#Converting the ranking data to text
rank_data <- html_text(rank_data_html)
#Let's have a look at the rankings
head(rank_data)
#Using CSS selectors to scrap the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')
#Converting the title data to text
title_data <- html_text(title_data_html)
#Let's have a look at the title
head(title_data)

#############################################


url1 <- 'https://www.youtube.com/feed/trending'
webpage1 <- read_html(url1)
rank_data_html1<-html_nodes(webpage1,'.yt-uix-tile-link')
rank_data1 <- html_text(rank_data_html1)
#Let's have a look at the rankings
head(rank_data1)