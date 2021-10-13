# cj
library(httr)
library(jsonlite)
library(tidyverse)
library(magrittr)

source(here::here("api-key.R"))

source(here::here("functions", "regulations-gov-API-search.R"))

search_keyword_page <- search_keyword_page4

# cj NPRMs
# test with first 10k
cjPR <- map_dfr(.x = c(1:20),
                .f = search_keyword_page4,
                documenttype = "Proposed Rule",
                keyword = "climate justice",
                lastModifiedDate = Sys.time())

cjPR %>%
  filter(!is.na(postedDate)) %>%
  arrange(postedDate) %>%
  head() %>%
  select(postedDate, lastpage)

# # up to 100k
cjPR100 <- map_dfr(.x = c(11:100),
                   .f = search_keyword_page,
                   documenttype = "Proposed Rule",
                   keyword = "climate justice")

# # inspect
cjPR100 %>%
  filter(!is.na(postedDate)) %>%
  arrange(postedDate) %>%
  head() %>%
  select(postedDate, page)

cjPR %>% 
  mutate(year = str_sub(postedDate, 1,4) %>% as.numeric()) %>% 
  ggplot() + 
  aes(x = year, fill = documentType) +
  geom_bar()


#
cjPR %<>% full_join(cjPR100)

save(cjPR, file = here::here("data", 
                             str_c("cjPR", 
                                   Sys.Date(), 
                                   ".Rdata")))

##################################
# cj Rules 

# test with 1 batch of 250 
cjFR <- search_keyword_page4(keyword = "climate justice",
                          documenttype = "Rule",
                          lastModifiedDate =  Sys.time()) #NOT SYS DATE!!

# test with first 10k
cjFR <- map_dfr(.x = c(1:20),
                .f = search_keyword_page,
                documenttype = "Rule",
                keyword = "climate justice",
                lastModifiedDate <- Sys.time())

# if(content$meta$lastPage){
#   lastModifiedDate <-- content$data$attributes$lastModifiedDate %>% tail(1)
#   #lastModifiedDate <-- Sys.time() %>% str_remove(" [A-Z]")
# } 

# up to 100k
cjFR2 <- map_dfr(.x = c(1:20),
                 .f = search_keyword_page,
                 documenttype = "Rule",
                 keyword = "climate justice")

# inspect
cjFR2 %>% 
  filter(!is.na(postedDate)) %>% 
  arrange(postedDate) %>%
  head() %>%
  select(postedDate, page)

cjFR2 %>% count(documentType)

cjFR %>% 
  mutate(year = str_sub(postedDate, 1,4) %>% as.numeric()) %>% 
  ggplot() + 
  aes(x = year, fill = documentType) +
  geom_bar()

cjFR %<>% full_join(cjFR2)

cjFR %>% filter(is.na(postedDate)) %>% count(docketId, lastpage, sort= T)

save(cjFR, file = here::here("data", 
                             str_c("cjFR", 
                                   Sys.Date(), 
                                   ".Rdata")))



###################
# cj COMMENTS 
# initialize
cjcomments1 <- map_dfr(.x = c(1),
                       .f = search_keyword_page4,
                       documenttype = "Public Submission",
                       keyword = "climate justice",
                       lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))


# first 5k
cjcomments <- map_dfr(.x = c(1:20),
                      .f = possibly(search_keyword_page4, otherwise = cjcomments1),
                      documenttype = "Public Submission",
                      keyword = "climate justice",
                      lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))

unique(cjcomments$lastpage)

## begin loop (as of 2021-02-06, there were ~XXX cj comments, so repeat 5+ times)
# next 5k
cj2 <- map_dfr(.x = c(1:20),
               .f = possibly(search_keyword_page4, otherwise = cjcomments1),
               documenttype = "Public Submission",
               keyword = "climate justice",
               # starting at the last modified date (the function arranges by last modified date)
               lastModifiedDate = #"2020-05-14T23:47:37Z")# there are more than 5k comenst on 2020-5-15
                 cjcomments$lastModifiedDate %>% min() )

# inspect
cj2 %>% 
  filter(!is.na(postedDate)) %>% 
  arrange(lastModifiedDate) %>%
  head() %>%
  select(lastModifiedDate, postedDate)

cj2 %>% 
  ggplot() + 
  aes(x = as.Date(postedDate), fill = documentType) +
  geom_bar()

# JOIN 
cjcomments %<>% full_join(cj2)

cjcomments %>% 
  ggplot() + 
  aes(x = as.Date(postedDate), fill = documentType) +
  geom_bar()

# Repeat above 
# TODO make while loop in function 

save(cjcomments, 
     file = here::here("data", 
                       str_c("cjcomments", 
                             Sys.Date(), 
                             ".Rdata")))

# # up to .5m if needed (but as of 2020, n = 41,591k)
# cj500 <- map_dfr(.x = c(101:500),
#                  .f = search_keyword_page,
#                  documenttype = "PS",
#                  keyword = "climate justice")
# 
# cjcomments %<>% full_join(cj500)
# 
# save(cjcomments, file =  here::here("data", "cjcomments.Rdata"))