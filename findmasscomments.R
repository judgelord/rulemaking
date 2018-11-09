# This script gets all comments from regulations.gov

## load packages
source("setup.R")

## get API call functions 
source("RegsGovAPI.R")

## add your API Key
source("api-key.R") 
api_key <- api_key

## initial API call
raw.result <- GET(
  url = url,
  path = paste0(
    "/regulations/v3/documents?api_key=", api_key,
    "&rpp=", rpp,
    "&so=", order,
    "&sb=", sortby,
    "&dct=", "PS",
    "&po=", page[start]
  )
)
raw.result$status_code

# extract content to list
content <- fromJSON(rawToChar(raw.result$content))
d <- as.data.frame(content[[1]])

# initialize
start <- 1

##################################################################################
# if resuming saved results: load("comments.Rdata")
# loop until api fails
while(raw.result$status_code == 200){
  
  # API call
raw.result <- GET(
  url = url,
  path = paste0(
    "/regulations/v3/documents?api_key=", api_key,
    "&rpp=", rpp,
    "&so=", order,
    "&sb=", sortby,
    "&dct=", "PS",
    "&po=", page[start]
  )
)
raw.result$status_code

# extract content to list
content <- fromJSON(rawToChar(raw.result$content))

# merge with previous pages
d %<>% full_join(as.data.frame(content[[1]]))

# next page 
start <- start + 1
}
# loop over and bind additional pages 
save(d, start, file = "data/comments.Rdata")




# format
d$numberOfCommentsReceived %<>% as.numeric()
d$postedDate %<>% as.Date()
d$commentStartDate %<>% as.Date()

# docket vars 
d %<>% group_by(docketId) %>% 
  mutate(numberOfCommentsUnique = n()) %>% 
  mutate(numberOfCommentsTotal = sum(numberOfCommentsReceived)) %>% 
  mutate(support = grepl(" support ", commentText) ) %>% 
  mutate(support = grepl(" oppose ", commentText) ) %>% 
  ungroup() %>% arrange(desc(numberOfCommentsReceived))

select(d, organization, numberOfCommentsReceived, numberOfCommentsUnique, numberOfCommentsTotal, agencyAcronym, title)


head(d$commentText[which(grepl(" support ", d$commentText))])


head(d$commentText)


            c("Mass Mail E1",
             "Mass Mail E10",
             "Mass Mail E2",
             "Mass Mail E3")

            





