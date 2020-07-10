# This script gets all documents (e.g. public comments) from regulations.gov

## load packages
source("setup.R")

# set defaults 
url  <- "https://api.data.gov"
rpp <- 1000 # 1000 = max results per page
order <- "ASC" # DESC = Decending, ASC = Ascending 
sortby <- "postedDate" #docketId (Docket ID) docId (Document ID) title (Title) postedDate (Posted Date) agency (Agency) documentType (Document Type) submitterName (Submitter Name) organization (Organization)
pages <- c(1, (seq(100000)*rpp)+1) # up to 100,000,000 results
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
    "&po=", pages[1]
  )
)
raw.result$status_code

# extract content to list
content <- fromJSON(rawToChar(raw.result$content))
# make a data frame 
d <- as.data.frame(content[[1]])

# to strings 
#FIXME mutate_at/if
if("organization" %in% names(d)){d$organization %<>% as.character()}
if("commentDueDate" %in% names(d)){d$commentDueDate %<>% as.character()}
if("commentStartDate" %in% names(d)){d$commentStartDate %<>% as.character()}
if("postedDate" %in% names(d)){d$postedDate %<>% as.character()}

# initialize
page <- 2
error <- 0
skip <- NA

##################################################################################
# If adding to saved results, first run: load("data/comments.Rdata") 

# loop until API fails for more than 1 hour
while (error < 61) {
  # if returning errors for more than 1 hr
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
  ifelse(raw.result$status_code != 200, error <-
           error + 1, error <- 0)
  
  # If call fails, wait a minute
  if (error > 0) {
    message(print(paste("Error", raw.result$status_code,"on page", page, "- waiting", error,"minutes" )))
    Sys.sleep(60)
  }
  
  # If call works, mege in new data
  if (raw.result$status_code == 200) {
    # extract content to list
    content <- fromJSON(rawToChar(raw.result$content))
    # to data frame
    temp <- as.data.frame(content[[1]])
    if("organization" %in% names(temp)){temp$organization %<>% as.character()}
    if("commentDueDate" %in% names(temp)){temp$commentDueDate %<>% as.character()}
    if("commentStartDate" %in% names(temp)){temp$commentStartDate %<>% as.character()}
    if("postedDate" %in% names(temp)){temp$postedDate %<>% as.character()}
    
    # merge with previous pages silently
    suppressMessages(
    d %<>% full_join(temp)
    )
    
    message(paste("Page", page, "added at", Sys.time()))
    page <- page + 1
  }
  
  # If server error more than twice, skip
  if (raw.result$status_code == 500 & error > 1) {
    message(paste("Skipping page", page))
    skip <- c(skip, page)
    page <- page + 1
  }
  
  # save after each half million docs (it takes ~30 minutes to get 500k and you don't want to start over if you hit an error)
  if (grepl("000$|500$", page)){
    save(d, page, skip, file = paste0(page, "comments.Rdata") ) 
    d <- temp
  }
  
}# END LOOP 

# Save last comments
save(d, page, skip, file = paste0("lastcomments.Rdata") ) 
save.image()

save(d, file = "data/recentcomments.Rdata")



