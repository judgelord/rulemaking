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


#########

search_page4 <- function(page = 1, 
                         documenttype = "Rule", # default
                         lastModifiedDate = Sys.time() ){
  
  
  lastModifiedDate %<>% str_replace_all("[A-Z]", " ") %>%  str_squish()
  
  
  endpoint = ifelse(documenttype == "Public Submission", "comments", "documents")
  
  documentType = ifelse(documenttype == "Public Submission", "", str_c("&filter[documentType]=", documenttype)) #"&filter[documentType]=documents")
  
  path <- paste0("/v4/", endpoint,
                 "?page[number]=", page,
                 "&page[size]=250", 
                 documentType,
                 #"&a=", agency,
                 "&sort=-lastModifiedDate,documentId",
                 "&filter[lastModifiedDate][le]=", lastModifiedDate,
                 "&api_key=", api_key)
  
  # this works:
  if(FALSE){
    raw.result <- GET(url = "https://api.regulations.gov", 
                      path = paste0("/v4/comments?filter[searchTerm]=environmental%2Bjustice&api_key=", api_key))
  }
  
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





search_keyword_page4_comment_loop <- function(data_comments, data_comments1, keyword){

while(data_comments$lastModifiedDate %>% as.Date() %>% min() > as.Date("1993-01-01")){
  
  # next 5k
  data_2 <- map_dfr(.x = c(1:20),
                   .f = possibly(search_keyword_page4, otherwise = data_comments1),
                   keyword = keyword, 
                   documenttype = "Public Submission",
                   # starting at the last modified date (the function arranges by last modified date)
                   lastModifiedDate =  date
  )
  
  # if we get some, add them 
  if(nrow(data_2) > 0){
    print(nrow(data_2))
    data_comments$lastModifiedDate %>% min() %>% paste(" = old date") %>% print()
    data_2$lastModifiedDate %>% min()  %>% paste(" = current date")  %>% print() 
    # inspect
    
    data_2 %>% 
      ggplot() + 
      aes(x = as.Date(postedDate), fill = documentType) +
      geom_bar()
    
    ############
    # JOIN  #
    data_comments %<>% full_join(data_2)
    
    data_comments %>% 
      ggplot() + 
      aes(x = as.Date(postedDate), fill = documentType) +
      geom_bar()
    
    # Repeat above 
    # TODO make while loop in function 
    file = here::here("data", 
                      str_c("temp_data_comments", 
                            Sys.Date(), 
                            ".Rdata"))
    
    save(data_comments, file = file)
    
    
    # if we are getting stuck on the same date (i.e. if there are >5000 comments on a date)
    if(data_comments$lastModifiedDate %>%
       min()  == date ){
      beep() 
      
      # date rounded down to the nearist 20:00 hrs
      date <- data_comments$lastModifiedDate %>% min() %>% str_replace("T2.", "T20")
      
      if(data_comments$lastModifiedDate %>%
         min()  == date){
        # date rounded down to the nearist 1X:00 hrs
        date <- data_comments$lastModifiedDate %>% min() %>% str_replace("T2", "T1")
      } 
      if(data_comments$lastModifiedDate %>%
         min()  == date){
        # date rounded down to the nearist 10:00 hrs
        date <- data_comments$lastModifiedDate %>% min() %>% str_replace("T1.", "T10")
      }
      if(data_comments$lastModifiedDate %>%
         min()  == date){
        # date rounded down to the nearist 0X:00 hrs
        date <- data_comments$lastModifiedDate %>% min() %>% str_replace("T1", "T0")
      }
      if(data_comments$lastModifiedDate %>%
         min()  == date){
        # date rounded down to the nearist 0:00 hrs
        date <- data_comments$lastModifiedDate %>% min() %>% str_replace("T0.", "T00")
      }
    } else {
      # otherwise, the new date is the min
      date <- data_comments$lastModifiedDate %>% min() 
      beep(sound = 2)
    }
    date %>% paste(" = new date (should be current date unless the old date didn't change)") %>% print()
    
    
  } else{
    beep() 
    print(nrow(data_2))
  }
  
  Sys.sleep(50)
}

}
