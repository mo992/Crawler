rm(list=ls(all=T))
library(rvest)
library(dplyr)
options(timeout = 300)

# 給定關鍵字和日期區間, 輸出文章標題及連結
uastring <- "Chrome/58.0.3029.110"
theurl = "http://search.appledaily.com.tw/appledaily/search"
getpage = httr::POST(theurl, body = list(   searchMode = 'Adv',
                                            searchType = 'text',
                                            querystrA = '台電',
                                            source = 'twapple',
                                            sdate = '2017-06-01',
                                            edate = '2017-06-28',
                                            submit = '<unnamed>'
                                        ),
                     encode = 'form')
page <- read_html(getpage)
xmllinkname = xml_text(xml_find_all(page, "//li/div[@class='tbb']/h2/a"))
xmllink = xml_find_all(page, "//li/div[@class='tbb']/h2/a[@href]")
clearxmllink <- function(url){
url <- as.character(url)
url <- str_split(url,'"')[[1]][2]
}
xmllink = lapply(xmllink,clearxmllink)
xmllinklist = do.call('cbind',list(xmllinkname,xmllink))

# article crawler
getArticle <- function(url){
  pagerawdata <- read_html(url)
  content= pagerawdata %>% html_nodes('.articulum.trans') %>% 
    html_text() %>% 
    iconv(from='UTF-8', to='UTF-8')
  title <- pagerawdata %>% html_nodes('#h1') %>% 
    html_text() %>% 
    iconv(from='UTF-8', to='UTF-8')
  date <- pagerawdata %>% html_nodes('.gggs time') %>% 
    html_text() %>% 
    iconv(from='UTF-8', to='UTF-8')
  category <- pagerawdata %>% 
    iconv(from='UTF-8', to='UTF-8') %>%
    gsub(pattern='.*"keywords": \\["(.+?)"\\].*', '\\1', x=.)
  data.frame( title = title, content = content,
              date = date, category = category,
              stringsAsFactors = FALSE )
}

# try
result = list()
xmllink = unlist(xmllink)
for (i in 1:length(xmllink)){
   result[[i]] = getArticle(xmllink[i])
}
output = do.call('rbind',result)