# data
load("data/masscomments.Rdata")
d <- mass 

# selecting ones most needed due to api limits
# FIXME 
d %<>% filter(grepl("attach", commentText), 
                 docketType == "Rulemaking") %>% 
  arrange(-numberOfCommentsReceived) 
# filter out docs we already have
d %<>% filter(!documentId %in% list.files("comments/") )
# /FIXME






# initialize and call api to get urls
docs <- search.doc(mass$documentId[1]) 
for(i in 1:dim(mass)[1]){ 
  doc <- search.doc(mass$documentId[i]) 
  docs %<>% full_join(doc) 
  Sys.sleep(2)} 

# save all urls 
save(docs, file ="data/attachment-urls.Rdata")

# select first url for now, due to api limits (spread this out to get all)
# FIXME
docs %<>% mutate(attach.url.1 = gsub(";.*", "", attach.url)) %>%
  mutate(attach.url.1 = gsub("Type=pdf.*", "Type=pdf", attach.url.1))

docs$attach.url.1[1]

# format for scraping rather than api 
docs %<>% mutate(attach.url.1 = gsub(".*download?", "https://www.regulations.gov/contentStreamer", attach.url.1)) 


# name output file
docs %<>% mutate(file = gsub(".*documentId=", "", attach.url.1)) %>%
  mutate(file = gsub("&contentType=", ".", file)) %>%
  mutate(file = gsub("&attachmentNumber=", "-", file))
docs$file[1]

docs$attach.url.1[1]

#################################
# DOWNLOAD 
download <- docs 

# files we don't have 
download %<>% filter(!file %in% list.files("comments/") ) %>%
  filter(attach.url.1 != "")

# loop over downloading attachments 
for(i in 1:dim(download)[1]){ 
  # download to comments folder 
  tryCatch({
    download.file(download$attach.url.1[i], 
                  destfile = paste0("comments/", download$file[i]) ) 
  },error = function(e) {
    print(e)
    print(i)
  })
  Sys.sleep(1)
}

##################
# READ TEXTS
# initialize and loop over downloaded attachments to read in texts
docs$attach.text = NA 
for(i in 1:dim(docs)[1]){
  # read / ocr text 
  text <- read_document(file[i]) 
  docs$attach.text[i] <- text 
}

