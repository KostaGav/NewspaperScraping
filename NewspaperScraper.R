###Template to scrape news articles from different home pages using the internet archive

library(rvest)
library(tidyverse)
library(jsonlite)
library(Rcrawler)

#Create directories to store temporary data
dir.create(file.path("Scrape News articles"), showWarnings = FALSE)
dir.create(file.path("Scrape News articles/dataDump"), showWarnings = FALSE)

setwd("./Scrape News articles")

website_name <- "spiegel"

url_website <- "spiegel.de"

year <- 2014
year_to <- 2018

url <- paste0("http://web.archive.org/cdx/search/cdx?url=",url_website,"&matchType=url&&collapse=timestamp:8&limit=15000&filter=!mimetype:image/gif&filter=!mimetype:image/jpeg&from=", year, "01&to=", year_to, "12&output=json&limit=1")

#Retrieve JSONs from the web archive to get the relevant URLs
url_from_json <- as_data_frame(fromJSON(url))
names(url_from_json) <- lapply(url_from_json[1, ], as.character)
url_from_json <- url_from_json[-1,]

homepages <- c()

#Build correct URLs
for(i in 1:nrow(url_from_json)){
  homepages[i] <- paste0("http://web.archive.org/web/", url_from_json[i,2], "/", url_from_json[i,3])
}



articles_spiegel <- list()

###Use RCrawler
#Watch out, running this command takes about 5-6 days!
for(i in 1:length(homepages){
  setwd("./Scrape News articles/dataDump")
  Rcrawler(homepages[i], MaxDepth = 1, urlregexfilter = ".*www\\.spiegel\\.de.*html$", ExtractXpathPat = c("//h1", "//*[@class='article-section clearfix']"))
  
  if(exists("DATA")){
  #restructure data list
  dat <- DATA %>% 
    map_df(enframe) %>% 
    unnest() %>% 
    mutate(Id = rep(1:nrow(INDEX), each = 2)) %>% 
    arrange(name) %>% 
    spread(name, value) %>% 
    rename(title = `1`, article = `2`)
  
  #extract date of article and broader area (sports, politics etc...)
  INDEX <- INDEX %>% 
    mutate(Id = as.numeric(Id),
           date = str_match(Url,"/web/(\\w+?)/")[,2],
           date = parse_datetime(str_sub(date, 1, 8), "%Y%m%d"),
           area = str_match(Url,"www\\.spiegel\\.de/(\\w+?)/")[,2]
    ) %>% 
    as.tibble()
  
  #merge articles with meta data and clean up a bit
  dat_full <- INDEX %>% 
    mutate(Id = as.numeric(Id)) %>% 
    left_join(dat, by = c("Id")) %>% 
    mutate(article = str_replace_all(article, pattern = "\n", " "),
           article = str_replace_all(article, pattern = "\t", " "),
           article = str_replace_all(article, pattern = "\\s+", " "),
           article = str_remove_all(article, pattern = "<!--.*-->"),
           article = str_remove_all(article, pattern = "\\{.*\\}"),
           article = str_remove_all(article, pattern = "\\/\\/.*\\/\\/"),
           article = str_remove_all(article, pattern = "tl\\;dr.*")
           )
  
  articles_spiegel[[i]] <- dat_full
  
  print(paste0("Already ", i, " URLs scraped. There are ", length(fullUrlsDf) - i , " left to scrape"))
  
  #Remove html files to save space, using the Windows shell command
  #This command only works on Windows OS
  #For Mac or UNIX please replace with the respective system command
  #shell("del /S *.html")
  #Alternatively, you can directly remove the crawled data completely 
  unlink("./Scrape News articles/dataDump/*", recursive = TRUE, force = TRUE)
  
  rm(DATA, INDEX, dat, dat_full)
  } else {
    next
  }
}

#clear dataDump


#Get df from lists
content_spiegel <- do.call("rbind", articles_spiegel)

#Only keep newest version of the article
content_spiegel <- content_spiegel %>% 
  arrange(date) %>% 
  distinct(title, .keep_all = TRUE)

save(content_spiegel, file = "./Scrape News articles/articles_spiegel.RData")


