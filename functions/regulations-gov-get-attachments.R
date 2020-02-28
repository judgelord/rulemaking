# load packages and functions 
source("setup.R") # this file is located in the main directory https://github.com/judgelord/rulemaking
source("regulations-gov-API-search.R") # this file is located at https://github.com/judgelord/rulemaking/tree/master/functions
       
# load data from regulations.gov API search
load("data/allcomments-sample.Rdata)

# filter out docs already scraped (presumes you have a folder in this directory called "comments" where you keep scraped files
d %<>% filter(!stringr::str_detect(documentId, list.files("comments/") ))

## initialize and call api to get urls 
## (if only need pdfs, this is not necessary, just make all urls with the doc id and .pdf at the end)
docs <- search.doc(d$documentId[1]) 

docs <- map_dfr(d$documentId[1:100], search.doc)

# save all urls 
if(F){
save(docs, file ="data/attachment-urls.Rdata")
##################################################

##############################################################
load("data/attachment-urls.Rdata")
}

docs %<>% filter( !is.na(attach.url), attach.url != "" ) %>% 
  mutate(attach.count = 1 + str_count(attach.url,";") )

max(docs$attach.count)



docs %<>% 
  mutate(pdf = str_detect(attach.url, "pdf")) %>% 
  # split out attachments
  mutate(attach.url = str_split(attach.url, ";")) %>%
  unnest(attach.url)

docs %<>% 
  # drop tiffs where we have a pdf
  filter(!(pdf & str_detect(attach.url, "tiff"))) %>% 
  mutate(attach.url = str_replace(attach.url, "Type=pdf.*", "Type=pdf"))%>% 
  # switch API URL base to content streamer URL base 
  mutate(attach.url = str_replace(attach.url, 
                                  ".*download?", 
                                  "https://www.regulations.gov/contentStreamer"))

## Test 
docs$attach.url[1]

# name output file
docs %<>% 
  mutate(file = attach.url %>% 
           str_remove(".*documentId=") %>%
           str_replace("&contentType=", ".") %>%
           str_replace("&attachmentNumber=", "-"))

# Inspect
docs$file[1]
docs$attach.url[1]



#################################
# to DOWNLOAD 
download <- docs 

# files we don't have 
download %<>% filter(!file %in% list.files("comments/") ) %>%
  filter(!attach.url %in% c("","NULL"), !is.null(attach.url) )
dim(docs)
dim(download)
head(download)

# test
download.file(download$attach.url[1], 
              destfile = paste0("comments/", download$file[1]) ) 


# loop over downloading attachments 78 at a time (regulations.gov blocks after 78?)
for(i in 1:round(dim(download)[1]/78)){
  download %<>% filter(!file %in% list.files("comments/") ) 
  
  ## Reset error counter inside function 
  errorcount <<- 0
#for(i in 1:dim(download)[1]){ 
for(i in 1:78){ 
  if(errorcount < 5){
  # download to comments folder 
  tryCatch({ # tryCatch handles errors
    download.file(download$attach.url[i], 
                  destfile = paste0("comments/", download$file[i]) ) 
  },
  error = function(e) {
    errorcount<<-errorcount+1
    print(errorcount)
    if( str_detect(e[[1]][1], "cannot open URL") ){
      download$file[i] <<- "cannot open URL.csv" # this is a dummy file in the comments folder, which will cause this url to be filtered out
    }
    print(e)
  })
  print(i)
  #Sys.sleep(1) # pausing between requests does not seem to help, but makes it easier to stop failed calls
  }}
  Sys.sleep(600) # 10 min
}

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

