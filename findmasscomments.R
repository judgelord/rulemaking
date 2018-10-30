

# get all comments
documenttype <- "PS"

d <- search.docs(documenttype = "PS", n = 1000)

# initialize
start <- 1

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
save(d, file = "comments.Rdata")






# top comments
top <- all %>% filter(numberOfCommentsReceived > 1) %>% arrange(desc(numberOfCommentsReceived))
cbind(top$documentId, top$numberOfCommentsReceived, top$title)
top$documentId

top$doc <- c("Mass Mail E1",
             "Mass Mail E10",
             "Mass Mail E2",
             "Mass Mail E3")

# 

top$numberOfCommentsReceived %<>% as.numeric()

(sum(top$numberOfCommentsReceived) -
    940 )/sum(top$numberOfCommentsReceived)

sum(top$numberOfCommentsReceived)

sum(grepl("support", all$commentText))
sum(grepl("support", all$commentText))


head(all$commentText[which(grepl("support", all$commentText))])





