# install.packages(c("httr", "jsonlite", "lubridate", "tidyverse", "magrittr"))
library(httr)
library(jsonlite)
library(tidyverse)
library(magrittr)
options(stringsAsFactors = FALSE)

# Pull from regulations.gov API, see http://regulationsgov.github.io/developers/console/#!/documents.json/documents_get_0
url  <- "https://api.data.gov"
api_key <- "aynn8SLo5zdb2V0wqBKQwHQ5FmCLd2cIpWStzrZ0"
rpp <- 1000 # results per page
order <- "DESC" # DESC: Decending, ASC: Ascending 
sortby <- "postedDate" #docketId (Docket ID) docId (Document ID) title (Title) postedDate (Posted Date) agency (Agency) documentType (Document Type) submitterName (Submitter Name) organization (Organization)
page <- c(0, seq(1000)*rpp) # up to 1,000,0000 results
status <- "O" # O for open docket
keywords <- "all"
documenttype <- "variables"


##################
# KEYWORD SEARCH #
##################
# create path function, comment out things like agency or status
regs.gov.path <- function(documenttype, search) {
  paste0("/regulations/v3/documents?api_key=", api_key, 
               "&rpp=", rpp, 
               #"&a=", agency,
               "&so=", order, 
               "&sb=", sortby, 
               "&s=", search, 
               #"&cp=", status,
               "&dct=", documenttype)
}

# DEFINE SEAECH 
keywords  <- c("%22environmental%2Bjustice%22") # %2B is an encoded space, %22 is encoded quote, but does not seem to work for exact strings
documenttype <- c("PS%2BPR%2BFR") # "N%2BPR%2BFR%2BPS%2BSR%2BO" # N%2BPR%2BFR%2BPS%2BSR%2BO = all docs, N: Notice PR: Proposed Rule FR: Rule O: Other SR: Supporting & Related Material PS: Public Submission

path <- NA
for(i in 1:length(keywords)){
  path[i] <- regs.gov.path(
    documenttype = documenttype, # "N%2BPR%2BFR%2BPS%2BSR%2BO" # N%2BPR%2BFR%2BPS%2BSR%2BO = all docs, N: Notice PR: Proposed Rule FR: Rule O: Other SR: Supporting & Related Material PS: Public Submission
    search = keywords[i])
}

# path to first page of up to 1000 results for first search term
path1 <- paste0(path[1], "&po=", page[1]) # page 1


# Execute API call for up to 1000 results from first term 
raw.result <- GET(url = url, path = path1)
raw.result$status_code == 200 # did it work? 

content <- fromJSON(rawToChar(raw.result$content))
class(content) # make sure it's a list
length(content) # of 2


d <- as.data.frame(content[[1]])

d$summary <- gsub("\n", "", d$summary)
d$commentText <- gsub("\n", "", d$commentText)
write.csv(names(d), paste(documenttype, keywords, ".csv"))

######################
# function for > 1000 results #
######################
regs.gov.search <- function(path) {
  # initialize with page 1
  rpp <- 1000
  page <- c(0, seq(1000)*rpp)
  raw.result <- GET(url = url, path = paste0(path, "&po=", page[1]))
  content <- fromJSON(rawToChar(raw.result$content))
  d <- as.data.frame(content[[1]])
  # loop over and bind additional pages 
  for(i in 2:100){ # up to 10k results. MAY NEED TO CHANGE THIS BUT KEPT LOW TO AVOID LIMIT
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
} # end function


###################
# get keyword 1  #
##################
d <- regs.gov.search(path[1])
names(d)[16] <- gsub("%22|%2B", "", keywords[1])
head(d)






###################################
# merge multiple keyword searches #
###################################
for(i in 2:length(path)){
  names(d)[names(d) == 'summary'] <- gsub("%22|%2B", keywords[i])
d <- merge(d, regs.gov.search(path[i]), all = T)
}


d %<>% filter(
  !is.na(commentText)|
    !is.na(summary)|
    !is.na(court)|
    #!is.na(lawsuit)|
    !is.na(sue)|
    !is.na(appeal)  
  )

# remove tabs and line breaks 
d$summary <- gsub("\n|\t|,", "", d$summary)
d$environmentaljustice <- gsub("\n|\t|,", "", d$environmentaljustice)
d$commentText <- gsub("\n|\t|,", "", d$commentText)


# save csv
write.csv(d, paste(documenttype, keywords, ".csv"))




#####################################
# ALL DOCUMENTS (i.e. not a search) #
#####################################
documenttype <- "N%2BPR%2BFR" # N%2BPR%2BFR%2BPS%2BSR%2BO N: Notice PR: Proposed Rule FR: Rule O: Other SR: Supporting & Related Material PS: Public Submission
agency <- "SEC%2BFAA%2BFRA"

# create path
path <- paste0("/regulations/v3/documents?api_key=", api_key, 
               "&rpp=", rpp, 
               "&a=", agency,
               "&dct=", documenttype)

path1 <- paste0(path, "&po=", page[1]) # page 1

######################
# for > 1000 results #
######################
# initialize
raw.result <- GET(url = url, path = path1)
raw.result$status_code == 200
content <- fromJSON(rawToChar(raw.result$content))
all <- as.data.frame(content[[1]])
# loop over and bind additional pages 
for(i in 2:10){
  #tryCatch({
  raw.result <- GET(url = url, path = paste0(path, "&po=", page[i]))
  if(raw.result$status_code == 200){
    content <- fromJSON(rawToChar(raw.result$content))
    temp <- as.data.frame(content[[1]])
    all <- rbind(all, temp)
  }
  #}, error = function(err) {print(i)})
}
all %<>% unique()



###########
# docket #
##########
docket <- "EPA-HQ-OAR-2017-0355"
documenttype <- "PS" 
path <- paste0("/regulations/v3/documents?api_key=", api_key, 
               "&rpp=", rpp, 
               "&D=", docket,
               "&dct=", documenttype)

path1 <- paste0(path, "&po=", page[1]) # page 1
raw.result <- GET(url = url, path = path1)
raw.result$status_code == 200
content <- fromJSON(rawToChar(raw.result$content))
all <- as.data.frame(content[[1]])


head(all$commentText)




# top comments
top <- all %>% filter(numberOfCommentsReceived > 1) %>% arrange(desc(numberOfCommentsReceived))
cbind(top$documentId, top$numberOfCommentsReceived, top$title)
top$documentId

top$doc <- c("Mass Mail E1",
             "Mass Mail E10",
             "Mass Mail E2"
             
             "Mass Mail E3")

# 

top$numberOfCommentsReceived %<>% as.numeric()

(sum(top$numberOfCommentsReceived) -
  940 )/sum(top$numberOfCommentsReceived)

sum(top$numberOfCommentsReceived)

sum(grepl("support", all$commentText))
sum(grepl("support", all$commentText))
  

head(all$commentText[which(grepl("support", all$commentText))])


