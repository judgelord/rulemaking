# check comments folder
directory <- "comments"
length(list.files(directory))

# load packages
source("setup.R")

## This script downloads pdf attachments only
## The advantage of this approach is that it does not require using the API to get file names
## However, after it is run, one should run regulation-gov-get-attachments.R on the fails to get non-pdfs 


load(here("data/masscomments.Rdata"))
dim(all)
names(all)

load("ascending/allcomments.Rdata")
dim(all)
names(all)

# urls for first attachments 
all %<>% 
  mutate(file = str_c(documentId, "-1.pdf"),
         attach.url = str_c("https://www.regulations.gov/contentStreamer?documentId=",
                            documentId,
                            "&attachmentNumber=1"),
         downloaded = file %in% list.files(here::here("comments")) )


# inspect missing files
all %>% 
  filter(!downloaded,
         #org.comment, # comments identified as a org comment 
         #!is.na(organization), # comments with an org name identified
         attachmentCount > 0) %>% 
         count(agencyAcronym) %>% knitr::kable()


# # NON-MASS COMMENTS ON MASS DOCKETS:
# d <- filter(all, docketId %in% mass$docketId)

# Comments from one agency
d <- filter(all, agencyAcronym %in% c("FDA"))

dim(d)
names(d)
head(d)



# subset to download
docs <- d %>% filter(#org.comment, # comments identified as a org comment 
                     #!is.na(organization), # comments with an org name identified
                     attachmentCount>0) # subset to those with attachment

# Inspect
dim(docs)
docs$file[1]
docs$attach.url[1]



#################################
# files we do have 
downloaded <- docs %>% filter(downloaded)

# inspect 
dim(downloaded)
head(downloaded)
sum(downloaded$numberOfCommentsReceived)
write.table(sum(downloaded$numberOfCommentsReceived), file = "data/downloaded.tex")

# to DOWNLOAD 
download <- docs 
dim(download)

# files we don't have 
download %<>% filter(!downloaded,
                     !attach.url %in% c("","NULL"), 
                     !is.null(attach.url) )

# inspect 
dim(docs)
names(download)
dim(download)
head(download)

# Load data on failed downloads 
load("data/comment_fails.Rdata")

# files that we have not already tried
download %<>% anti_join(fails)
# download %<>% filter(attachmentCount > 1)

# inspect 
dim(download)
head(download$attach.url)

# test
i <- 1
download.file(download$attach.url[i], 
              destfile = str_c("comments/", download$file[i]) ) 

# FIXME should use purrr walk() here and in get-attachments  


# loop over downloading attachments 78 at a time (regulations.gov blocks after 78?)
# run this loop about n/78 times to get everything
for(i in 1:round(dim(download)[1]/78)){
  n <- i*78
  N <- nrow(download)
  # filter out files already downloaded
  download %<>% filter(!file %in% list.files("comments/"),
                       !file %in% fails$file) 
  
  ## Reset error counter
  errorcount <<- 0
  
  for(i in 1:78){ 
    message(paste(download$file[i], "|", i, "of", 78, "|", n-78+i, "of", N, "downloaded at", Sys.time()))
    
    if(errorcount < 5){
      # download to comments folder 
      tryCatch({ # tryCatch handles errors
        download.file(download$attach.url[i], 
                      destfile = paste0("comments/", download$file[i]) ) 
      },
      error = function(e) {
        errorcount <<- errorcount+1
        message(paste("error", errorcount))
        if( str_detect(e[[1]][1], "cannot open URL") ){
          fails <<- rbind(fails, download$file[i])# this will cause this url to be filtered out for future runs of the loop
        }
        message(e)
        if(str_detect(e, "SSL connect error|500 Internal Server Error")){
          beepr::beep()
          Sys.sleep(600) # wait 10
          errorcount <<- 0 # reset error counter 
        }
      })

      Sys.sleep(.1) # pausing between requests does not seem to help, but makes it easier to stop failed calls
    }
    ## If 5 errors, wait and reset (sometimes you get "cannot open URL" 5x in a row)
    if(errorcount == 5){
      message("paused after 5 errors")
      beepr::beep()
      Sys.sleep(600) # wait 10 min
      errorcount <<- 0 # reset error counter 
    }
    
    } # end loop over batch
  

  # Save data on failed downloads 
  save(fails, file = "data/comment_fails.Rdata")
  message("Pausing to prevent regulations.gov security from blocking this IP address")
  Sys.sleep(500) # wait 10 min (5 min is not enough)
} # end main loop









