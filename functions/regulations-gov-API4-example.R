
# load functions
source("functions/regulations-gov-API4.R")

###################
# CFPB COMMENTS 

# init with first page of results, the most recent 250 comments 
CFPBcomments1 <- map_dfr(.x = c(1),
                            .f = search_agency4, agency = "CFPB",
                            documenttype = "Public Submission",
                            lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))


# page through 20 pages, the most recent 5k comments 
CFPBcomments <- map_dfr(.x = c(1:20),
                           .f = possibly(search_agency4, otherwise = CFPBcomments1),
                           documenttype = "Public Submission",
                           agency = "CFPB",
                           lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))

# was that all? 
unique(CFPBcomments$lastpage)


## begin loop, updating lastModifiedDate every batch of 5k
# (as of 2022-03-08, there were ~122k CFPB comment endpoints, so loop will run 24+ times)
#FIXME loop hangs on 2019-10-18 where > 5k comments were received in a 3 hour period
# and on 2019-10-16 where > 5k comments were recievedi in a 1 hour period
# (anti-hang correction did not deal with this because 0 comments were submitted on the rounded-down hour)

date <- CFPBcomments$lastModifiedDate %>% min()

# over 5k CFPB comments before 1 AM 
# date <- "2018-04-26T10:58:55Z"

while(CFPBcomments$lastModifiedDate %>% min() > as.Date("1993-01-01")){
  # next 5k
  CFPB2 <- map_dfr(.x = c(1:20),
                      .f = possibly(search_agency4, otherwise = CFPBcomments1),
                      documenttype = "Public Submission",
                      # starting at the last modified date (the function arranges by last modified date)
                      lastModifiedDate =  date
  )
  
  # if we get some, add them 
  if(nrow(CFPB2) > 0){
    print(nrow(CFPB2))
    CFPBcomments$lastModifiedDate %>% min() %>% paste(" = old date") %>% print()
    CFPB2$lastModifiedDate %>% min()  %>% paste(" = current date")  %>% print() 
    # inspect
    
    CFPB2 %>% 
      ggplot() + 
      aes(x = as.Date(postedDate), fill = documentType) +
      geom_bar()
    
    ############
    # JOIN  #
    CFPBcomments %<>% full_join(CFPB2)
    
    CFPBcomments %>% 
      ggplot() + 
      aes(x = as.Date(postedDate), fill = documentType) +
      geom_bar()
    
    # Repeat above 
    # TODO make while loop in function 
    file = here::here("data", 
                      str_c("CFPBcomments", 
                            Sys.Date(), 
                            ".Rdata"))
    
    save(CFPBcomments, file = file)
    
    
    # if we are getting stuck on the same date (i.e. if there are >5000 comments on a date)
    if(CFPBcomments$lastModifiedDate %>%
       min()  == date ){
      beep() 
      
      # date rounded down to the nearist 20:00 hrs
      date <- CFPBcomments$lastModifiedDate %>% min() %>% str_replace("T2.", "T20")
      
      if(CFPBcomments$lastModifiedDate %>%
         min()  == date){
        # date rounded down to the nearist 1X:00 hrs
        date <- CFPBcomments$lastModifiedDate %>% min() %>% str_replace("T2", "T1")
      } 
      if(CFPBcomments$lastModifiedDate %>%
         min()  == date){
        # date rounded down to the nearist 10:00 hrs
        date <- CFPBcomments$lastModifiedDate %>% min() %>% str_replace("T1.", "T10")
      }
      if(CFPBcomments$lastModifiedDate %>%
         min()  == date){
        # date rounded down to the nearist 0X:00 hrs
        date <- CFPBcomments$lastModifiedDate %>% min() %>% str_replace("T1", "T0")
      }
      if(CFPBcomments$lastModifiedDate %>%
         min()  == date){
        # date rounded down to the nearist 0:00 hrs
        date <- CFPBcomments$lastModifiedDate %>% min() %>% str_replace("T0.", "T00")
      }
    } else {
      # otherwise, the new date is the min
      date <- CFPBcomments$lastModifiedDate %>% min() 
      beep(sound = 2)
    }
    date %>% paste(" = new date (should be current date unless the old date didn't change)") %>% print()
    
    
  } else{
    beep() 
    print(nrow(CFPB2))
  }
  
  Sys.sleep(50)
}


# 
# save(CFPBcomments, file =  here::here("data", "CFPBcomments.Rdata"))
