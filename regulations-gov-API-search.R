# This script builds functions to pull from the regulations.gov API, see http://regulationsgov.github.io/developers/console/#!/documents.json/documents_get_0

# load packages
source("setup.R")

# grab your regulations.gov API key from a seperate file
source("api-key.R") 
# or otherwise define it
api_key <- api_key


# defaults 
url  <- "https://api.data.gov"
rpp <- 1000 # results per page
order <- "DESC" # DESC: Decending, ASC: Ascending 
sortby <- "postedDate" #docketId (Docket ID) docId (Document ID) title (Title) postedDate (Posted Date) agency (Agency) documentType (Document Type) submitterName (Submitter Name) organization (Organization)
page <- c(0, seq(1000)*rpp) # up to 1,000,0000 results
status <- "O" # O for open docket
n <- 2000 # max number of results
start <- 1000 # result number on which to resume partial search (e.g. due to api limits )
documenttype <- "N%2BPR%2BFR%2BPS%2BSR%2BO"
## N: Notice, 
## PR: Proposed Rule, 
## FR: Rule, 
## O: Other, 
## SR: Supporting & Related Material, 
## PS: Public Submission


#####################################
# ALL DOCUMENTS (i.e. not a keyword search, not limited to certian agencies) #
#####################################
search.docs <- function(documenttype, n) {
  documenttype <- gsub(", ", "%2B", documenttype)
  
  # create path
  path <- paste0("/regulations/v3/documents?api_key=", api_key, 
                 "&rpp=", rpp, 
                 "&so=", order, 
                 "&sb=", sortby, 
                 "&dct=", documenttype)
  
  path1 <- paste0(path, "&po=", page[round(start/1000)]) # page 1
  
  ######################
  # for > 1000 results #
  ######################
  # initialize
  raw.result <- GET(url = url, path = path1)
  raw.result$status_code == 200
  content <- fromJSON(rawToChar(raw.result$content))
  all <- as.data.frame(content[[1]])
  # loop over and bind additional pages 
  if(n>=2000){
  for(i in round(start/1000)+1:round(n/1000)){
    #tryCatch({
    raw.result <- GET(url = url, path = paste0(path, "&po=", page[i]))
    if(raw.result$status_code == 200){
      content <- fromJSON(rawToChar(raw.result$content))
      temp <- as.data.frame(content[[1]])
      all <- rbind(all, temp)
    }
    #}, error = function(err) {print(i)})
  }
  }
  all <- unique(all)
  return(all)
  if(raw.result$status_code != 200){ print(paste("Error: status code =", raw.result$status_code) ) }
}
##########################################################
##########################################################




########################################
# For select agencies # 
#######################
search.agency.docs <- function(agency, documenttype, n) {
  documenttype <- gsub(", ", "%2B", documenttype)
  agency <- gsub(", ", "%2B", agency)


# create path
path <- paste0("/regulations/v3/documents?api_key=", api_key, 
               "&rpp=", rpp, 
               "&a=", agency,
               "&so=", order, 
               "&sb=", sortby, 
               "&dct=", documenttype)

path1 <- paste0(path, "&po=", page[round(start/1000)]) # page 1

######################
# for > 1000 results #
######################
# initialize
raw.result <- GET(url = url, path = path1)
raw.result$status_code == 200
content <- fromJSON(rawToChar(raw.result$content))
all <- as.data.frame(content[[1]])
# loop over and bind additional pages 
if(n>=2000){
for(i in round(start/1000)+1:round(n/1000)){
  #tryCatch({
  raw.result <- GET(url = url, path = paste0(path, "&po=", page[i]))
  if(raw.result$status_code == 200){
    content <- fromJSON(rawToChar(raw.result$content))
    temp <- as.data.frame(content[[1]])
    all <- rbind(all, temp)
  }
  #}, error = function(err) {print(i)})
}
}
all <- unique(all)
return(all)
if(raw.result$status_code != 200){ print(paste("Error: status code =", raw.result$status_code) ) }
}




###########
# docket #
##########
search.docket <- function(docket, documenttype, n){
path <- paste0("/regulations/v3/documents?api_key=", api_key, 
               "&rpp=", rpp, 
               "&D=", docket,
               "&so=", order, 
               "&sb=", sortby, 
               "&dct=", documenttype)
path1 <- paste0(path, "&po=", page[round(start/1000)]) # page 1
raw.result <- GET(url = url, path = path1)
raw.result$status_code == 200
content <- fromJSON(rawToChar(raw.result$content))
all <- as.data.frame(content[[1]])
# loop over and bind additional pages 
if(n>=2000){
  for(i in round(start/1000)+1:round(n/1000)){
    #tryCatch({
    raw.result <- GET(url = url, path = paste0(path, "&po=", page[i]))
    if(raw.result$status_code == 200){
      content <- fromJSON(rawToChar(raw.result$content))
      temp <- as.data.frame(content[[1]])
      all <- rbind(all, temp)
    }
    #}, error = function(err) {print(i)})
  }
}
all <- unique(all)
return(all)
if(raw.result$status_code != 200){ print(paste("Error: status code =", raw.result$status_code) ) }
}
######################################
############################################################################








########################################################
# KEYWORD SEARCH #
##################
# create path function for keyword searches, comment out things like agency or status
search.keywords <- function(documenttype, keywords, n) {
  keywords <- gsub(" ", "%2B", keywords)
  documenttype <- gsub(", ", "%2B", documenttype)
  
  regs.gov.path <- function(documenttype, search){
    paste0("/regulations/v3/documents?api_key=", api_key, 
           "&rpp=", rpp, 
           #"&a=", agency,
           "&so=", order, 
           "&sb=", sortby, 
           "&s=", search, 
           #"&cp=", status,
           "&dct=", documenttype)
  }

# clear paths
paths <- NA

# make a vector of paths for each keyword
for(i in 1:length(keywords)){
  paths[i] <- regs.gov.path(
    documenttype = documenttype, # "N%2BPR%2BFR%2BPS%2BSR%2BO" # N%2BPR%2BFR%2BPS%2BSR%2BO = all docs, N: Notice PR: Proposed Rule FR: Rule O: Other SR: Supporting & Related Material PS: Public Submission
    search = keywords[i])
  return(paths)
}



######################
# function for each keyword path 
######################
search.keyword <- function(path) {
  # initialize with page 1
  rpp <- 1000
  page <- c(0, seq(1000)*rpp)
  raw.result <- GET(url = url, path = paste0(path, "&po=", page[round(start/1000)]))
  content <- fromJSON(rawToChar(raw.result$content))
  d <- as.data.frame(content[[1]])
  # loop over and bind additional pages 
  for(i in round(start/1000)+1:round(n/1000)){ # up to 10k results. MAY NEED TO CHANGE THIS BUT KEPT LOW TO AVOID LIMIT
    # tryCatch({
    if(raw.result$status_code == 200){ # only try pull if previous call was good
      raw.result <- GET(url = url, path = paste0(path, "&po=", page[i]))
    }
    if(raw.result$status_code == 200){ # only rbind if new call is good
      content <- fromJSON(rawToChar(raw.result$content))
      dtemp <- as.data.frame(content[[1]])
      if(ncol(dtemp)==ncol(d)){d <- rbind(d, dtemp)}
    }
    # }, error = function(err) {print(i)})  # may not need tryCatch because rbind throughs an error
  } # end loop
  return(d)
} # end keyword function


###################
# get keyword 1  #
##################
d <- search.keyword(paths[1])

# rename search results column
names(d)[names(d) == 'summary'] <- gsub("%22|%2B", "", keywords[1])

###################################
# repeat and merge any additional keywords #
###################################
if(length(keywords) > 1){
for(i in 2:length(paths)){
  temp <- search.keywords(paths[i])
  names(temp)[names(temp) == 'summary'] <- gsub("%22|%2B", " ", keywords[i])
  d <- full_join(d, temp)
}
}
# clean up strings 
# FIXME
# docs %<>% dplyr::mutate_all(str_replace(., pattern = "\n|\t|,", replacement =" "))

return(d)
} # end search.keywords() function 










# d %<>% filter(
#   !is.na(commentText)|
#     !is.na(summary)|
#     !is.na(court)|
#     #!is.na(lawsuit)|
#     !is.na(sue)|
#     !is.na(appeal)  
# )

# # remove tabs and line breaks 
# d$summary <- gsub("\n|\t|,", "", d$summary)
# d$environmentaljustice <- gsub("\n|\t|,", "", d$environmentaljustice)
# d$commentText <- gsub("\n|\t|,", "", d$commentText)





###############################
# One document (requires knowing the document id)

search.doc <- function(docID) {

  # create path (NOTE: for one document, the path is ...v3/document?..., not ...v3/documents?..)
  path <- paste0("/regulations/v3/document?api_key=", api_key, 
                 "&documentId=", docID)

  raw.result <- GET(url = url, path = path)
  raw.result$status_code == 200
  content <- fromJSON(rawToChar(raw.result$content))
  doc <- as.data.frame(content[[1]])
  
  if(raw.result$status_code != 200){ print(paste("Error: status code =", raw.result$status_code) ) }
  return(doc)
  }














##########################################################


# # Beginner example 
# # path to first page of up to 1000 results for first search term
# path1 <- paste0(path[1], "&po=", page[round(start/1000)]) # page 1
# 
# 
# # Execute API call for up to 1000 results from first term 
# raw.result <- GET(url = url, path = path1)
# raw.result$status_code == 200 # did it work? 
# 
# content <- fromJSON(rawToChar(raw.result$content))
# class(content) # make sure it's a list
# length(content) # of 2


# d <- as.data.frame(content[[1]])
# 
# d$summary <- gsub("\n", "", d$summary)
# d$commentText <- gsub("\n", "", d$commentText)
# write.csv(names(d), paste(documenttype, keywords, ".csv"))

