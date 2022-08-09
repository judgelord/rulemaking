# This script builds functions to pull from the regulations.gov API, see http://regulationsgov.github.io/developers/console/#!/documents.json/documents_get_0

# grab your regulations.gov API key from a seperate file
# source("api-key.R") 
# or otherwise define it
# api_key <- api_key

library(httr)
library(jsonlite)
library(tidyverse)
library(magrittr)

source("api-key.R")


# defaults 
url  <- "https://api.regulations.gov"
sortby <- "postedDate" #docketId (Docket ID) docId (Document ID) title (Title) postedDate (Posted Date) agency (Agency) documentType (Document Type) submitterName (Submitter Name) organization (Organization)
page <- c(0, seq(1000)*rpp) # up to 1,000,0000 results

# this works:
if(FALSE){
  raw.result <- GET(url = "https://api.regulations.gov", 
                    path = paste0("/v4/comments?filter[searchTerm]=environmental%2Bjustice&api_key=", api_key))
}



search_keyword_page4 <- function(page = 1, 
                                 documenttype = "Rule", # default
                                 keyword, 
                                 lastModifiedDate = Sys.time() ){
  
  # sys.time to fit required format
  lastModifiedDate %<>% str_replace_all("[A-Z]", " ") %>% 
    str_replace(" ", "T") %>% 
    str_squish() %>% 
    str_c("Z")
  
  # format string (add quotes, replace space with unicode)
  search <- #keyword %>% 
    str_c("%22", keyword, "%22") %>% 
    str_replace(" ", "%2B")
  
  
  endpoint <- ifelse(documenttype == "Public Submission", "comments", "documents")
  
  # filter document types, unless endpoint is comments
  documentType <- ifelse(endpoint == "documents", 
                         str_c("&filter[documentType]=", documenttype),
                         "") %>% 
    str_replace(" ", "%2B")
    # previously required "&filter[documentType]=documents")
  
  path <- paste0("/v4/", endpoint,
                 "?page[number]=", page,
                 "&page[size]=250", # 250 is now the max
                 documentType,
                 #"&a=", agency, #FIXME provide empty string when agency is set to "all" (default)
                 "&sort=-lastModifiedDate,documentId",
                 "&filter[searchTerm]=", search,
                 #"&filter[lastModifiedDate][le]=", lastModifiedDate,
                 "&api_key=", api_key)
  

  # inspect path 
  # str_c("https://api.regulations.gov", path)
  
  raw.result <- GET(url = "https://api.regulations.gov", path = path)
  
  content <- fromJSON(rawToChar(raw.result$content))
  
  d <- content$data$attributes %>%  as_tibble()  %>%
    mutate(id = content$data$id,
           type = content$data$type,
           links = content$data$links$self,
           lastpage = content$meta$lastPage)
  
  #TODO loop this over batches of 5k documents
  # if(content$meta$lastPage){
  #   lastModifiedDate <-- content$data$attributes$lastModifiedDate %>% tail(1)
  #   #lastModifiedDate <-- Sys.time() %>% str_remove(" [A-Z]")
  # } 
  
  return(d)
}


# EXAMPLE 
d <- search_keyword_page4(keyword = "climate justice",
                          documenttype = "Rule",
                          lastModifiedDate =  Sys.time()) #NOT SYS DATE!!
d$lastModifiedDate
d$highlightedContent
