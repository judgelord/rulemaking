## This script downloads pdf attachments only
## The advantage of this approach is that it does not require using the API to get file names
## However, after it is run, one should run regulation-gov-get-attachments.R to get non-pdfs 


# load("ascending/allcomments.Rdata")
# load("data/mass.Rdata")
# docs <- filter(all, documentId %in% mass$documentId)

dim(d)
names(d)
head(d)
docs <- d %>% filter(org.comment)
dim(docs)

docs %<>% 
  mutate(file = str_c(documentId, "-1.pdf"),
         attach.url = str_c("https://www.regulations.gov/contentStreamer?documentId=",
                            documentId,
                            "&attachmentNumber=1&contentType=pdf"))


# Inspect
docs$file[1]
docs$attach.url[1]



#################################
# to DOWNLOAD 
download <- docs 
dim(download)

# files we don't have 
download %<>% filter(!file %in% list.files("comments/") ) %>%
  filter(!attach.url %in% c("","NULL"), !is.null(attach.url) )
dim(docs)
dim(download)
head(download$documentId)

# test
i <- 5
download.file(download$attach.url[i], 
              destfile = str_c("comments/", download$file[i]) ) 


# FIXME
# should use purrr walk() here and in get-attachments  


# loop over downloading attachments 78 at a time (regulations.gov blocks after 78?)
# need to run this loop about n/78 times to get everything
for(i in 1:round(dim(download)[1]/78)){
  # filter out files already downloaded
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
        errorcount <<- errorcount+1
        print(errorcount)
        if( str_detect(e[[1]][1], "cannot open URL") ){
          download$file[i] <<- "cannot open URL.csv" # this is a dummy file in the comments folder, which will cause this url to be filtered out for future runs of the loop
        }
        print(e)
      })
      print(i)
      Sys.sleep(1) # pausing between requests does not seem to help, but makes it easier to stop failed calls
    }}
  Sys.sleep(600) # 10 min
}

