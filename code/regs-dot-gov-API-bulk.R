library(httr)
library(jsonlite)
library(tidyverse)
library(magrittr)

source(here::here("functions", "regulations-gov-API-search.R"))

source(here::here("api-key.R"))

# RULES 
rules <- search.docs(documenttype = "FR" , 
                 n = 100000)
nrow(rules)
names(rules)
head(rules$postedDate)
tail(rules$postedDate)

save(rules, file = here::here("data", str_c("rules", Sys.Date(), ".Rdata")))

# NPRMS 
nprms <- search.docs(documenttype = "PR" , 
                  n = 100000)

nrow(nprms)
names(nprms)
head(nprms$postedDate)
tail(nprms$postedDate)

save(nprms, file = here::here("data", str_c("nprms", Sys.Date(), ".Rdata")))

notices <- search.docs(documenttype = "N" , 
                     n = 100000)

nrow(notices)
names(notices)
head(notices$postedDate)
tail(notices$postedDate)

save(notices, file = here::here("data", str_c("notices", Sys.Date(), ".Rdata")))


# Other 
other <- search.docs(documenttype = "O" , 
                       n = 100000)

nrow(other)
head(other$postedDate)
tail(other$postedDate)

save(other, file = here::here("data", str_c("other", Sys.Date(), ".Rdata")))

# SR 
support <- search.docs(documenttype = "SR" , 
                     n = 100000)

nrow(support)
head(support$postedDate)
tail(support$postedDate)

save(support, file = here::here("data", str_c("support", Sys.Date(), ".Rdata")))

# EJ

# EJ NPRMs
# test with first 10k
ejPR <- map_dfr(.x = c(1:10),
                .f = search_keyword_page,
                documenttype = "PR",
                keyword = "environmental justice")

# # up to 100k
# ejPR100 <- map_dfr(.x = c(11:100),
#                  .f = search_keyword_page,
#                  documenttype = "FR",
#                  keyword = "environmental justice")

# # inspect
# ejPR100 %>% 
#   filter(!is.na(postedDate)) %>% 
#   tail() %>% 
#   .$postedDate
# 
# ejPR %<>% full_join(ejPR100)

save(ejPR, file = here::here("data", "ejPR.Rdata"))

##################################
# EJ Rules 
# test with first 10k
ejFR <- map_dfr(.x = c(1:10),
                      .f = search_keyword_page,
                      documenttype = "FR",
                      keyword = "environmental justice")

# up to 100k
ejFR100 <- map_dfr(.x = c(11:100),
                 .f = search_keyword_page,
                 documenttype = "FR",
                 keyword = "environmental justice")

# inspect
ejFR100 %>% 
  filter(!is.na(postedDate)) %>% 
  tail() %>% 
  .$postedDate

ejFR %<>% full_join(ejFR100)

save(ejFR, file = here::here("data", "ejFR.Rdata"))



###################
# EJ COMMENTS 

# test with first 10k
ejcomments <- map_dfr(.x = c(1:10),
                      .f = search_keyword_page,
                      documenttype = "PS",
                      keyword = "environmental justice")

# up to 100k
ej100 <- map_dfr(.x = c(11:100),
                 .f = search_keyword_page,
                 documenttype = "PS",
                 keyword = "environmental justice")

# inspect
ej100 %>% 
  filter(!is.na(postedDate)) %>% 
  tail() %>% 
  .$postedDate

ejcomments %<>% full_join(ej100)

save(ejcomments, file = here::here("data", "ejcomments.Rdata"))

# # up to .5m if needed (but as of 2020, n = 41,591k)
# ej500 <- map_dfr(.x = c(101:500),
#                  .f = search_keyword_page,
#                  documenttype = "PS",
#                  keyword = "environmental justice")
# 
# ejcomments %<>% full_join(ej500)
# 
# save(ejcomments, file =  here::here("data", "ejcomments.Rdata"))
