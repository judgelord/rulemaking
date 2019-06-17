mass

docs <- mass 

mass$documentId


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

# files we don't have 
download %<>% filter(!file %in% list.files("comments/") ) %>%
  filter(!attach.url %in% c("","NULL"), !is.null(attach.url) )
dim(docs)
dim(download)
head(download)

# test
download.file(download$attach.url[1], 
              destfile = str_c("comments/", download$file[1]) ) 


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
