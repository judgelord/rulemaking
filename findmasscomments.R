
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





