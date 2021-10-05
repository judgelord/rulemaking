# climate
library(httr)
library(jsonlite)
library(tidyverse)
library(magrittr)
library(beepr)

source(here::here("api-key.R"))

source(here::here("functions", "regulations-gov-API-search.R"))

search_keyword_page <- search_keyword_page4

# climate NPRMs
# test with first 10k
climatePR <- map_dfr(.x = c(1:20),
                .f = search_keyword_page4,
                documenttype = "Proposed Rule",
                keyword = "climate change",
                lastModifiedDate = Sys.time())

climatePR %>%
  filter(!is.na(postedDate)) %>%
  arrange(postedDate) %>%
  head() %>%
  select(postedDate, lastpage)

# # up to 100k
climatePR100 <- map_dfr(.x = c(11:100),
                   .f = search_keyword_page,
                   documenttype = "Proposed Rule",
                   keyword = "climate change")

# # inspect
climatePR100 %>%
  filter(!is.na(postedDate)) %>%
  arrange(postedDate) %>%
  head() %>%
  select(postedDate, page)

climatePR %>% 
  mutate(year = str_sub(postedDate, 1,4) %>% as.numeric()) %>% 
  ggplot() + 
  aes(x = year, fill = documentType) +
  geom_bar()


#
climatePR %<>% full_join(climatePR100)

save(climatePR, file = here::here("data", 
                             str_c("climatePR", 
                                   Sys.Date(), 
                                   ".Rdata")))

##################################
# climate Rules 

# test with 1 batch of 250 
climateFR <- search_keyword_page4(keyword = "climate change",
                          documenttype = "Rule",
                          lastModifiedDate =  Sys.time()) #NOT SYS DATE!!

# test with first 10k
climateFR <- map_dfr(.x = c(1:20),
                .f = search_keyword_page,
                documenttype = "Rule",
                keyword = "climate change",
                lastModifiedDate <- Sys.time())

# if(content$meta$lastPage){
#   lastModifiedDate <-- content$data$attributes$lastModifiedDate %>% tail(1)
#   #lastModifiedDate <-- Sys.time() %>% str_remove(" [A-Z]")
# } 

# up to 100k
climateFR2 <- map_dfr(.x = c(1:20),
                 .f = search_keyword_page,
                 documenttype = "FR",
                 keyword = "climate change")

# inspect
climateFR2 %>% 
  filter(!is.na(postedDate)) %>% 
  arrange(postedDate) %>%
  head() %>%
  select(postedDate, page)

climateFR2 %>% count(documentType)

climateFR %>% 
  mutate(year = str_sub(postedDate, 1,4) %>% as.numeric()) %>% 
  ggplot() + 
  aes(x = year, fill = documentType) +
  geom_bar()

climateFR %<>% full_join(climateFR2)

climateFR %>% filter(is.na(postedDate)) %>% count(docketId, lastpage, sort= T)

save(climateFR, file = here::here("data", 
                             str_c("climateFR", 
                                   Sys.Date(), 
                                   ".Rdata")))



###################
# climate COMMENTS 
# initialize
climatecomments1 <- map_dfr(.x = c(1),
                       .f = search_keyword_page4,
                       documenttype = "Public Submission",
                       keyword = "climate change",
                       lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))


# first 5k
climatecomments <- map_dfr(.x = c(1:20),
                      .f = possibly(search_keyword_page4, otherwise = climatecomments1),
                      documenttype = "Public Submission",
                      keyword = "climate change",
                      lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))

unique(climatecomments$lastpage)


## begin loop (as of 2021-02-06, there were ~XXX climate comments, so repeat 5+ times)

date <- climatecomments$lastModifiedDate %>% min()

while(climatecomments$lastModifiedDate %>% min() > as.Date("2005-01-01")){
# next 5k
climate2 <- map_dfr(.x = c(1:20),
               .f = possibly(search_keyword_page4, otherwise = climatecomments1),
               documenttype = "Public Submission",
               keyword = "climate change",
               # starting at the last modified date (the function arranges by last modified date)
               lastModifiedDate =  date
                 #"2020-05-14T23:47:37Z"# there are more than 5k ej comenst on 2020-5-15
                 )

# if we get some, add them 
if(nrow(climate2) > 0){
  print(nrow(climate2))
  climatecomments$lastModifiedDate %>% min() %>% paste(" = old date") %>% print()
  climate2$lastModifiedDate %>% min()  %>% paste(" = current date")  %>% print() 
# inspect

climate2 %>% 
  ggplot() + 
  aes(x = as.Date(postedDate), fill = documentType) +
  geom_bar()

# JOIN 
climatecomments %<>% full_join(climate2)

climatecomments %>% 
  ggplot() + 
  aes(x = as.Date(postedDate), fill = documentType) +
  geom_bar()

# Repeat above 
# TODO make while loop in function 
file = here::here("data", 
                  str_c("climatecomments", 
                        Sys.Date(), 
                        ".Rdata"))

save(climatecomments, file = file)
beep(sound = 2)
Sys.sleep(50)

# if we are getting stuck on the same date
if(climatecomments$lastModifiedDate %>%
   min()  == date  &
   climate2$lastModifiedDate %>% 
   min()  == date){
  # date rounded down to the nearist 20:00 hrs
  date <- climatecomments$lastModifiedDate %>% min() %>% str_replace("T2.", "T20")
 
if(climate2$lastModifiedDate %>% 
     min()  == date){
    # date rounded down to the nearist 1X:00 hrs
    date <- climatecomments$lastModifiedDate %>% min() %>% str_replace("T2", "T1")
  } 
if(climate2$lastModifiedDate %>% 
       min()  == date){
      # date rounded down to the nearist 10:00 hrs
      date <- climatecomments$lastModifiedDate %>% min() %>% str_replace("T1.", "T10")
    }
if(climate2$lastModifiedDate %>% 
         min()  == date){
        # date rounded down to the nearist 0X:00 hrs
        date <- climatecomments$lastModifiedDate %>% min() %>% str_replace("T1", "T0")
      }
if(climate2$lastModifiedDate %>% 
           min()  == date){
          # date rounded down to the nearist 0:00 hrs
          date <- climatecomments$lastModifiedDate %>% min() %>% str_replace("T1.", "T10")
} } else {
  # otherwise, the new date is the min
  date <- climatecomments$lastModifiedDate %>% min() 
}
date %>% paste(" = new date (should be current date unless it was the same as the old date)") %>% print()

} else{
  beep()
  Sys.sleep(60)
  }

}

# # up to .5m if needed (but as of 2020, n = 41,591k)
# climate500 <- map_dfr(.x = c(101:500),
#                  .f = search_keyword_page,
#                  documenttype = "PS",
#                  keyword = "climate change")
# 
# climatecomments %<>% full_join(climate500)
# 
# save(climatecomments, file =  here::here("data", "climatecomments.Rdata"))