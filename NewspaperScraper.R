###Template to scrape news articles from different home pages using the internet archive

library(rvest)
library(tidyverse)
library(jsonlite)
library(Rcrawler)
library(lubridate)
library(httr)
library(stringr)

#Create directories to store temporary data
dir.create(file.path("Scrape News articles"), showWarnings = FALSE)
dir.create(file.path("Scrape News articles/dataDump"), showWarnings = FALSE)

setwd("./Scrape News articles")


website_name <- "faz"

url_website <- "faz.net"

year <- 2018

url <- paste0("http://web.archive.org/cdx/search/cdx?url=",url_website,"&matchType=url&&collapse=timestamp:8&limit=15000&filter=!mimetype:image/gif&filter=!mimetype:image/jpeg&from=", year, "01&to=", year, "10&output=json&limit=1")

url_from_json <- as_data_frame(fromJSON(url))
names(url_from_json) <- lapply(url_from_json[1, ], as.character)
url_from_json <- url_from_json[-1,]

homepages <- c()

#Download relevant URLs
for(i in 1:nrow(url_from_json)){
  homepages[i] <- paste0("http://web.archive.org/web/", url_from_json[i,2], "/", url_from_json[i,3])
}

fullUrls <- list()


for(i in 1:length(homepages)){
  possibleError <- tryCatch(
    r <- GET(homepages[i]),
    error = function(e) e
  )

  if(inherits(possibleError, "error")) next

  status <- status_code(r)
  if(status == 200){
    faz_html <- read_html(homepages[i])
    faz_urls <- faz_html %>%
      html_nodes("a") %>%
      html_attr("href")
    faz_urls <- faz_urls[grepl("www\\.faz\\.net", faz_urls)]

    faz_urlsFinal <- list()

    if(length(faz_urls)>0){
      for(j in 1:length(faz_urls)){
        if(!grepl("http://web.archive.org", faz_urls[j])){
          faz_urlsFinal[[j]] <- paste0("http://web.archive.org", faz_urls[j])
        } else
          faz_urlsFinal[[j]] <- faz_urls[j]
      }
    }

    faz_urlsFinal <- Reduce(c, faz_urlsFinal)

    fullUrls[[i]] <- faz_urlsFinal

    Sys.sleep(sample(1:5,1))
  } else{
    next
  }

}


fullUrlsComplete <- Reduce(c,fullUrls)

articles_faz <- list()

###Use RCrawler

oldw <- getOption("warn")
options(warn = -1)
#Watch out, running this command takes about 5-6 days!
for(i in 1:length(homepages)){
  setwd("./Scrape News articles/dataDump")
  Rcrawler(fullUrlsComplete[i], MaxDepth = 0, ExtractXpathPat = c("//*[@class='atc-HeadlineText']", "//*[@itemprop='articleBody']"))
  
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
           date = parse_datetime(str_sub(date, 1, 8), "%Y%m%d")
    ) %>% 
    as.tibble()
  
  #merge articles with meta data and clean up a bit
  dat_full <- INDEX %>% 
    mutate(Id = as.numeric(Id)) %>% 
    left_join(dat, by = c("Id")) %>% 
    mutate(article = str_replace_all(article, pattern = "\n", " "),
           article = str_replace_all(article, pattern = "\t", " "),
           article = str_replace_all(article, pattern = "\\s+", " ")
           )
  
  articles_faz[[i]] <- dat_full
  
  print(paste0("Already ", i, " homepages scraped. There are ", length(fullUrlsComplete) - i , " homepages left to scrape"))
  
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

options(warn = oldw)

#clear dataDump


#Get df from lists
content_faz <- do.call("rbind", articles_faz)

#Only keep latest version of the article
content_faz <- content_faz %>% 
  arrange(desc(date)) %>% 
  distinct(title, .keep_all = TRUE)

save(content_faz, file = "/home/ubuntu/scrapeArticles/articles_faz18.RData")


