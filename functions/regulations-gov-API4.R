# This script builds functions to pull from the regulations.gov API v4

## EXAMPLES from https://open.gsa.gov/api/regulationsgov/#searching-for-comments
# https://api.regulations.gov/v4/comments?filter[searchTerm]=water&api_key=DEMO_KEY
# https://api.regulations.gov/v4/comments?filter[commentOnId]=09000064846eebaf
# &page[size]=250
# &page[number]=N
# &sort=lastModifiedDate,documentId
# &api_key=DEMO_KEY


# grab your regulations.gov API key from a separate file
# source("api-key.R") 

library(httr)
library(jsonlite)
library(tidyverse)
library(magrittr)


# defaults 
url  <- "https://api.regulations.gov"
rpp <- 250 # results per page
sortby <- "-postedDate" # decending posted date 
page <- 1:20
endpoint  <-  "comments" # all else = "documents"
lastModifiedDate = Sys.time() %>% str_replace_all("[A-Z]", " ") %>%  str_squish()
agency <- "CFPB"

#####################################
# ALL DOCUMENTS (i.e. not a keyword search, not limited to certian agencies) #
#####################################




#########
# https://api.regulations.gov/v4/comments?filter[agencyId]=GSA,EPA&page[size]=250&page[number]=N&sort=lastModifiedDate,documentId&api_key=DEMO_KEY

search_agency4 <- function(page = 1, 
                           agency = "CFPB",
                         documenttype = "Rule", # default
                         lastModifiedDate = Sys.time() ){
  
  # format date
  lastModifiedDate %<>% str_replace_all("[A-Z]", " ") %>%  str_squish()
  
  # backwards integration with v3 functions 
  endpoint = ifelse(documenttype %in% c("Public Submission", "comments"), "comments", "documents")
  
  path <- paste0("/v4/",
                 endpoint, 
                 "?filter[agencyId]=", agency,
                 "&filter[lastModifiedDate][le]=", lastModifiedDate,
                 "&page[size]=250", 
                 "&page[number]=", page,
                 "&sort=-lastModifiedDate,documentId",
                 "&api_key=", api_key)
  
  raw.result <- GET(url = url, path = path)

  
  # inspect path 
  str_c("https://api.regulations.gov", path)
  
  raw.result <- GET(url = url, path = path)
  
  content <- fromJSON(rawToChar(raw.result$content))
  
  d <- content$data$attributes %>%  as_tibble()  %>%
    mutate(id = content$data$id,
           type = content$data$type,
           links = content$data$links$self,
           lastpage = content$meta$lastPage)
  
  #TODO loop this over batches of 5k documents? 
  # (currently just done with map)
  # if(content$meta$lastPage){
  #   lastModifiedDate <-- content$data$attributes$lastModifiedDate %>% tail(1)
  #   #lastModifiedDate <-- Sys.time() %>% str_remove(" [A-Z]")
  # } 
  
  return(d)
}

if(FALSE){
  search_agency4(documenttype = "comments")
}



search_keyword_page4 <- function(page = 1, 
                                 documenttype = "Rule", # default
                                 keyword, 
                                 lastModifiedDate = Sys.time() ){
  
  
  lastModifiedDate %<>% str_replace_all("[A-Z]", " ") %>%  str_squish()
  
  # format (replace space with unicode)
  search <- #keyword %>% 
    str_c("%22", keyword, "%22") %>% 
    str_replace(" ", "%2B")
  
  
  endpoint = ifelse(documenttype == "Public Submission", "comments", "documents")
  
  documentType = ifelse(documenttype == "Public Submission", "", str_c("&filter[documentType]=", documenttype)) #"&filter[documentType]=documents")
  
  path <- paste0("/v4/", endpoint,
                 "?page[number]=", page,
                 "&page[size]=250", 
                 "&sort=-lastModifiedDate,documentId",
                 "&filter[searchTerm]=", search,
                 "&filter[lastModifiedDate][le]=", lastModifiedDate,
                 "&api_key=", api_key)
  
  # inspect path 
  str_c("https://api.regulations.gov", path)
  
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



#NOTRUN 
if(FALSE){
d <- search_keyword_page4(keyword = "climate justice",
                          documenttype = "Rule",
                          lastModifiedDate =  Sys.time()) #NOT SYS DATE!!
d$lastModifiedDate
d$highlightedContent
}


