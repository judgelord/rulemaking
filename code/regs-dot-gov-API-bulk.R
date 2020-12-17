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
ejPS <- search.keywords(keywords = c("environmental justice"),
                     documenttype = "PS",
                     n = 100000)

ejNPRM <- search.keywords(keywords = c("environmental justice"),
                        documenttype = "PR",
                        n = 100000)

ejFR <- search.keywords(keywords = c("environmental justice"),
                          documenttype = "FR",
                          n = 100000)

