# This script gets all documents (e.g. public comments) from regulations.gov

## load packages
source("setup.R")

# set defaults 
url  <- "https://api.data.gov"
rpp <- 1000 # 1000 = max results per page
order <- "DESC" # DESC = Decending, ASC = Ascending 
sortby <- "postedDate" #docketId (Docket ID) docId (Document ID) title (Title) postedDate (Posted Date) agency (Agency) documentType (Document Type) submitterName (Submitter Name) organization (Organization)
pages <- c(0, seq(100000)*rpp) # up to 100,000,0000 results
documenttype <- "PS" # "N%2BPR%2BFR%2BPS%2BSR%2BO"
## N: Notice, 
## PR: Proposed Rule, 
## FR: Rule, 
## O: Other, 
## SR: Supporting & Related Material, 
## PS: Public Submission

## add your API Key
source("api-key.R") 
api_key <- api_key

## initial API call (page 1, first 1000 results)
raw.result <- GET(
  url = url,
  path = paste0(
    "/regulations/v3/documents?api_key=", api_key,
    "&rpp=", rpp,
    "&so=", order,
    "&sb=", sortby,
    "&dct=", "PS", # searching Public Submissions
    "&po=", pages[101]
  )
)
raw.result$status_code

# extract content to list
content <- fromJSON(rawToChar(raw.result$content))
d <- as.data.frame(content[[1]])

# initialize
page <- 2
error <- 0

##################################################################################
# If adding to saved results, first run: load("comments.Rdata")

# loop until API fails for more than 1 hour or hits max page 
while(page < 1000) {
         while (error < 61) { # if returning errors for more than 1 hr
           # API call
           raw.result <- GET(
             url = url,
             path = paste0(
               "/regulations/v3/documents?api_key=",
               api_key,
               "&rpp=",
               rpp,
               "&so=",
               order,
               "&sb=",
               sortby,
               "&dct=",
               "PS",
               "&po=",
               pages[page]
             )
           )
           
           # API call error counter
           ifelse(raw.result$status_code != 200, error <- error + 1, error <- 0)
           
           # If call fails, wait a minute
           if(error > 0) {
             print(paste("Page", page, "error", raw.result$status_code, "for", error, "minutes"))
             Sys.sleep(60)}
           
           if(raw.result$status_code == 200){
           # extract content to list
           content <- fromJSON(rawToChar(raw.result$content))
           
           # merge with previous pages
           d %<>% full_join(as.data.frame(content[[1]]))
           
           print(paste("Page", page, "added"))
           page <- page +1
           }
         }
}
# loop over and bind additional pages 
save(d, page, file = "data/comments.Rdata")









