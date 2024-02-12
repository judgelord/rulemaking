#### MERGE OCC DATA INTO SQLITE DATABASE ####
rm(list=ls())
gc()

library(DBI)
library(RSQLite)
library(tidyverse)

#######################################
# Database location on linux 
Master_location <- here::here("db", "comment_metadata.sqlite") 
dbListTables(master_con)
Master_Query <- dbSendQuery(master_con, 'SELECT * FROM comments_all')
Master <- dbFetch(Master_Query, n = -1)

comments <- Master

# drop non-informative vars 
if(FALSE){comments %<>% select(-number_of_comments_received)}

comments %<>% select( -document_status, 
                     -document_type,
                     -allow_late_comment)


save(comments, file = "db/comment_metadata_2020.Rdata")

names(Master)

update = F
if(update) {
# write combined
master_location <- here::here("db", "regs_dot_gov_2020.sqlite") 
master_con = dbConnect(SQLite(), dbname=master_location)
dbWriteTable(master_con, "comments", comments, overwrite = T)
dbWriteTable(master_con, "rules", rules, overwrite = T)
}

#######################################
# Database location mac
master_location <- here::here("data", "rulemaking_metadata.sqlite") 
master_con = dbConnect(SQLite(), dbname=master_location)
dbListTables(master_con)

update = F
if(update) {
dbWriteTable(master_con, "comments", comments_all, overwrite = T)
dbWriteTable(master_con, "actions", rules, overwrite = T)
}


Master_Query <- dbSendQuery(master_con, 'SELECT * FROM comments')

Master_Query <- dbSendQuery(master_con, 'SELECT * FROM comments WHERE agency_acronym == "OCC"')

Master <- dbFetch(Master_Query, n = -1)

names(Master)

master_con = dbConnect(SQLite(), dbname=master_location)

Master_Query <- dbSendQuery(master_con, "SELECT * FROM actions WHERE agency_acronym == 'OCC'")
dbFetch(Master_Query, n = -1)

Master_Query <- dbSendQuery(master_con, "SELECT * FROM attachments WHERE agency_acronym == 'OCC'")
dbFetch(Master_Query, n = -1)

Master_Query <- dbSendQuery(master_con, "SELECT * FROM comments WHERE agency_acronym == 'OCC'")

Master <- dbFetch(Master_Query, n = -1)

# ASSESS COMPLETENESS 
# Master %<>% mutate_all(str_squish)

Master %>% filter(nchar(organization) < 4, nchar(organization) > 2) %>% select(organization)

# View(Master)
cover <- function(x){
  (1- sum(is.na(x) | x == "" | nchar(x) < 3)/nrow(Master)) %>% scales::percent(accuracy = .01)
  }

cover(1)


kablebox <- . %>%  knitr::kable() %>% 
  kable_styling() %>% 
  scroll_box(height = "500px")

Master %>% group_by(agency_acronym) %>%  
  #summarise_all(is.na) %>% 
  summarise_all(cover) %>% kablebox() 

dbClearResult(Master_Query)
dbDisconnect(con)

