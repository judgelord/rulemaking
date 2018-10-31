

# get all comments
documenttype <- "PS"

d <- search.docs(documenttype = "PS", n = 1000)

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
    "&dct=", documenttype,
    "&po=", page[start]
  )
)

# extract content to list
content <- fromJSON(rawToChar(raw.result$content))

# merge with previous pages
d %<>% full_join(as.data.frame(content[[1]]))

# next page 
start <- start + 1
}
# loop over and bind additional pages 
save(d, file = "data/comments.Rdata")




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

            





