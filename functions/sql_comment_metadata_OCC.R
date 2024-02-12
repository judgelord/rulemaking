# This script creates a SQL table of OCC comment metadata 
# subset to Dodd-Frank dockets or RINs as identified by Davis Polk 
library(tidyverse)
library(magrittr)

# API version 
v4 = FALSE

## originally pulled from Devin's master data 
comments_all <- dbSendQuery(master_con, "SELECT * FROM comments WHERE agency_acronym == 'OCC'") |>
  dbFetch(Master_Query, n = -1)

## now pulling from new search of API v4
if(v4){
  load(here::here("data", "OCCcomments.Rdata"))
  comments_all <- OCCcomments 
}


nrow(comments_all)
head(comments_all)
names(comments_all)  

# Rename to fit https://docs.google.com/spreadsheets/d/1i8t_ZMAhjddg7cQz06Z057BqnNQEsC4Gur6p17Uz0i4/edit#gid=1357829693
names(comments_all)  <- names(comments_all) %>% 
  str_replace_all("([A-Z])", "_\\1") %>% 
  str_to_lower()

names(comments_all)

comments_all %<>% mutate(source = "regulations.gov")

if(v4){
  comments_all %<>% mutate(comment_id = id)
  comments_all %<>% rename(agency_acronym = agency_id)
  
  
  # V4 no longer returns organization or docket title 
  comments_all %<>% 
    mutate(docket_id = str_remove(comment_id, "-[0-9]*$"))
  
} else {
  # v3
  comments_all %<>% mutate(comment_id = document_id)
}

comments_all %<>% mutate(comment_url = str_c("https://www.regulations.gov/comment/", comment_id))

#FIXME v4 does not return comment due date 
count(comments_all, is.na(posted_date))
count(comments_all, is.na(comment_due_date))

comments_all %<>% mutate(late_comment = as.Date(posted_date) > as.Date(comment_due_date))

count(comments_all, is.na(late_comment))




vars_to_keep <- c("fr_document_id", # need this from rules table joined in by document_id - comment_id
                  "docket_title", # this may clash with docket title from attachments table
                  "docket_type",
                  "rin",
                  "attachment_count",
                  "comment_text",
                  "posted_date",
                  "submitter_name",
                  "organization",
                  "late_comment",
                  "allow_late_comment")




# FIXME trim down to minimial variables
comments_all %<>% select(source,
                         #fr_document_id, # need this from rules table joined in by document_id - comment_id
                         agency_acronym,
                         docket_id, 
                         comment_id = document_id,
                         comment_title = title, # rename
                         comment_url,
                         any_of(vars_to_keep) )



# filter to only OCC comments
comments_OCC <- comments_all %>% filter(agency_acronym == "OCC")
nrow(comments_OCC)
head(comments_OCC)


# save Rdata 
save(comments_OCC, file = here::here("data", "comment_metadata_OCC.Rdata"))

# convert dates to character 
comments_OCC %<>% mutate(across(where(is.numeric.Date), as.character) )


# Create RSQLite database for all OCC comment metadata 
library(DBI)
# install.packages("RSQLite")
1
library(RSQLite)
con <- dbConnect(RSQLite::SQLite(), here::here("data", "comment_metadata_OCC.sqlite"))

# check 
list.files("data")

dbListTables(con)
dbWriteTable(con, "comments", comments_OCC, overwrite = T)
dbListTables(con)

dbListFields(con, "comments")
# dbReadTable(con, "comments_OCC") # oops

# fetch results:
res <- dbSendQuery(con, "SELECT * FROM comments WHERE agency_acronym = 'OCC'")

dbFetch(res) %>% head()
dbClearResult(res)

dbDisconnect(con)


###################################################
# Subset to Davis Polk Dodd-Frank rules 
# load(here::here("data", "comment_metadata_OCC.Rdata"))
names(comments_OCC)

# Dodd-Frank rules from Davis Polk Data
df <- read_csv(here::here("data", "dockets_to_scrape.csv"))
names(df)
head(df)
df %<>% filter(str_detect(agency, "OCC"))

# Subset to Dodd-Frank rules (by docket or RIN)
df_rins <- df$RIN %>% na.omit() %>% unique()
df_dockets <- df$identifier %>% na.omit() %>% unique()

comments_OCC_df <- comments_OCC %>% 
  filter(docket_id %in% df_dockets | rin %in% df_rins)

# rins not in dockets to scrape
comments_OCC_df %>% 
  filter(!rin %in% df_rins) %>% 
  select(docket_id, rin) %>% 
  distinct()

# dockets not in dockets to scrape
comments_OCC_df %>% 
  filter(!docket_id %in% df_dockets) %>% 
  select(docket_id, rin) %>% 
  distinct() %>% knitr::kable()


comments_OCC_df$docket_id %>% unique()
comments_OCC_df$rin %>% unique()

# look back to see how many we matched 
matched <- df %>% filter(RIN %in% na.omit(comments_OCC_df$rin) | identifier %in% na.omit(comments_OCC_df$docket_id))
unmatched <- df %>% anti_join(matched)
unmatched %>% 
  select(RIN, identifier) %>% 
  distinct()

# # 0 comments 
# RIN       identifier   
# <chr>     <chr>        
# 1 1557-AD33 OCC-2011-0009
# 2 1557-AC99 OCC-2012-0002
# 3 1557-AD41 OCC-2011-0018
# 4 1557–AD42 OCC-2011-0010
# 5 1557-AD90 OCC-2014-0027
# 6 1557-AD34 OCC-2010-0021


# NOTE see checks against fed reg doc numbers in functions/sql_actions_metadata_OCC.R

# save Rdata 
save(comments_OCC_df, file = here::here("data", "comment_metadata_OCC_df.Rdata"))


# Create RSQLite database
con <- dbConnect(RSQLite::SQLite(), here::here("data", "comment_metadata_OCC_df.sqlite"))

# check 
list.files("data")

dbListTables(con)
dbWriteTable(con, "comments", comments_OCC_df, overwrite = T)
dbListTables(con)

dbListFields(con, "comments")

# fetch results:
res <- dbSendQuery(con, "SELECT * FROM comments WHERE agency_acronym = 'OCC'")

dbFetch(res) %>% head()
dbClearResult(res)
dbDisconnect(con)

