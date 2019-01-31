# load packages and functions 
library(here)
source(here("setup.R"))
source(here("regulations-gov-API-search.R"))
       
# load data from regulations.gov API search
load(here("data/masscomments.Rdata"))
d <- mass 

# selecting ones most needed for now due to api limits
# FIXME 
d %<>% filter(#grepl("attach", commentText), 
              docketType == "Rulemaking")
# /FIXME


# filter out docs already scraped
d %<>% filter(!stringr::str_detect(documentId, list.files("comments/") ))

## initialize and call api to get urls 
## (if only need pdfs, this is not necessary, just make all urls with the doc id and .pdf at the end)
docs <- search.doc(d$documentId[1]) 
for(i in 1:dim(d)[1]){ 
  doc <- search.doc(d$documentId[i]) # function from regulations-gov-API-search.R
  docs %<>% full_join(doc) 
  Sys.sleep(2)
} 

# save all urls 
save(docs, file ="data/attachment-urls.Rdata")


##############################################################
load("data/attachment-urls.Rdata")

docs %<>% filter( !is.na(attach.url), attach.url != "" ) %>% 
  mutate(attach.count = str_count(attach.url,";") )

max(docs$attach.count)


# select first url for now, due to api limits (spread this out to get all)
# FIXME
docs %<>% 
  mutate(attach.url.1 = attach.url) %>%
  mutate(attach.url.1 = gsub(";.*", "", attach.url)) %>%
  mutate(attach.url.1 = gsub("Type=pdf.*", "Type=pdf", attach.url.1))

docs$attach.url.1[1]

# format for scraping rather than api 
docs %<>% mutate(attach.url.1 = gsub(".*download?", "https://www.regulations.gov/contentStreamer", attach.url.1)) 

docs$attach.url.1[1]
# name output file
docs %<>% 
  mutate(file = gsub(".*documentId=", "", attach.url.1)) %>%
  mutate(file = gsub("&contentType=", ".", file)) %>%
  mutate(file = gsub("&attachmentNumber=", "-", file))
docs$file[1]

docs$attach.url.1[1]

#################################
# DOWNLOAD 
download <- docs 

# files we don't have 
download %<>% filter(!file %in% list.files("comments/") ) %>%
  filter(!attach.url.1 %in% c("","NULL"), !is.null(attach.url.1) )
dim(docs)
dim(download)
head(download$attach.url.1)

# loop over downloading attachments (regulations.gov blocks after 79?)
# With trycatch
# Sys.sleep(1200) # sleep for an hour
download %<>% filter(!file %in% list.files("comments/") ) 
errorcount <- 0
for(i in 1:dim(download)[1]){ 
  if(errorcount < 5){
  # download to comments folder 
  tryCatch({
    download.file(download$attach.url.1[i], 
                  destfile = paste0("comments/", download$file[i]) ) 
  },
  error = function(e) {
    errorcount<-(errorcount+1)
    print(errorcount)
    print(e)
  })
  print(i)
  Sys.sleep(1) # pausing between requests does not seem to help, but makes it easier to stop
    }}

##################

list.files("comments")
# pdftools::pdf_text("comments/WHD-2017-0002-138298-1.pdf")
# READ TEXTS
# initialize and loop over downloaded attachments to read in texts
files <- data.frame(fileId = as.character(paste0("comments/",list.files("comments"))), 
                    attach.text = NA)
dim(files)
files$fileId %<>% as.character()
files$fileId[1]

for(i in 1:dim(files)[1]){
  # read / ocr text 
  # text <-  textread::read_document(docs$file[i]) 
  tryCatch({
  text <-  pdftools::pdf_text(
    files$fileId[i]) 
  
  files$attach.text[i] <- text 
  },
  error = function(e) {
    print(e)
    print(i)
})}

