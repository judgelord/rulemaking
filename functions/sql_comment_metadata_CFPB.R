# This script creates a SQL table of CFPB comment metadata 
# it then subsets to Dodd-Frank dockets or RINs as identified by Davis Polk 

# required 
library(DBI)
library(RSQLite)
library(tidyverse)
library(magrittr)

# API version 
v4 = FALSE

## now pulling from new search of API v4
if(v4){
  #FIXME PULL FROM API4 SQL
  load(here::here("data", "CFPBcomments.Rdata"))
  comments_all <- CFPBcomments 
} else{
  ## originally pulled from Devin's master data 
  comments_all <- dbSendQuery(master_con, "SELECT * FROM comments WHERE agency_acronym == 'CFPB'") |>
    dbFetch(Master_Query, n = -1)
}


nrow(comments_all)
head(comments_all)
names(comments_all)  

# Rename to fit https://docs.google.com/spreadsheets/d/1i8t_ZMAhjddg7cQz06Z057BqnNQEsC4Gur6p17Uz0i4/edit#gid=1357829693
#FIXME Replace with janator?
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


# FIXME trim down to minimial variables
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

comments_all %<>% select(source,
                         #fr_document_id, # need this from rules table joined in by document_id - comment_id
                         agency_acronym,
                         docket_id, 
                         comment_id = document_id,
                         comment_title = title, # rename
                         comment_url,
                         any_of(vars_to_keep) )

# filter to only OCC comments
comments_CFPB <- comments_all %>% filter(agency_acronym == "CFPB")
nrow(comments_OCC)
head(comments_OCC)

# save Rdata 
save(comments_CFPB, file = here::here("data", "comment_metadata_CFPB.Rdata"))

# convert dates to character for SQL
comments_CFPB %<>% mutate(across(where(is.numeric.Date), as.character) )


# Create RSQLite database for all CFPB comment metadata 
con <- dbConnect(RSQLite::SQLite(), here::here("data", "comment_metadata_CFPB.sqlite"))

# check 
list.files("data")

dbListTables(con)
dbWriteTable(con, "comments", comments_CFPB, overwrite = T)
dbListTables(con)

dbListFields(con, "comments")

# fetch results:
res <- dbSendQuery(con, "SELECT * FROM comments WHERE agency_acronym = 'CFPB'")

dbFetch(res) %>% head()
dbClearResult(res)

dbDisconnect(con)


###################################################
# Subset to Davis Polk Dodd-Frank rules 
# load(here::here("data", "comment_metadata_CFPB.Rdata"))
names(comments_CFPB)

# Dodd-Frank rules from Davis Polk Data
df <- read_csv(here::here("data", "dockets_to_scrape.csv"))
names(df)
head(df)
df %<>% filter(str_detect(agency, "CFPB"))

# Subset to Dodd-Frank rules (by docket or RIN)
df_rins <- df$RIN %>% na.omit() %>% unique()
df_dockets <- df$identifier %>% na.omit() %>% unique()

comments_CFPB_df <- comments_CFPB %>% 
  filter(docket_id %in% df_dockets | rin %in% df_rins)

# rins not in dockets to scrape
comments_CFPB_df %>% 
  filter(!rin %in% df_rins) %>% 
  select(docket_id, rin) %>% 
  distinct()

# dockets not in dockets to scrape
comments_CFPB_df %>% 
  filter(!docket_id %in% df_dockets) %>% 
  select(docket_id, rin) %>% 
  distinct() %>% knitr::kable()


comments_CFPB_df$docket_id %>% unique()
comments_CFPB_df$rin %>% unique()

# look back to see how many we matched 
matched <- df %>% filter(RIN %in% na.omit(comments_CFPB_df$rin) | identifier %in% na.omit(comments_CFPB_df$docket_id))
unmatched <- df %>% anti_join(matched)
unmatched %>% 
  select(RIN, identifier) %>% 
  distinct()

# # 0 comments
# RIN       identifier    
# <chr>     <chr>         
#   1 NA        CFPB-2013-0038
# 2 3170-AA30 CFPB-2012-0040
# 3 NA        CFPB-2014-0030
# 4 3170-AA36 CFPB-2013-0006
# 5 NA        CFPB-2012-0042
# 6 NA        CFPB-2013-0034
# 7 NA        CFPB-2017-0026
# 8 NA        CFPB-2012-0043
# 9 NA        CFPB-2013-0035
# 10 NA        CFPB-2017-0027


# NOTE see checks against fed reg doc numbers in functions/sql_actions_metadata_CFPB.R

# save Rdata 
save(comments_CFPB_df, file = here::here("data", "comment_metadata_CFPB_df.Rdata"))


# Create RSQLite database
con <- dbConnect(RSQLite::SQLite(), here::here("data", "comment_metadata_CFPB_df.sqlite"))

# check 
list.files("data")

dbListTables(con)
dbWriteTable(con, "comments", comments_CFPB_df, overwrite = T)
dbListTables(con)

dbListFields(con, "comments")

# fetch results:
res <- dbSendQuery(con, "SELECT * FROM comments WHERE agency_acronym = 'CFPB'")

result <- dbFetch(res) 

result |> head()

count(result, posted_date, sort = T)

count(result, is.na(posted_date), sort = T)

dbClearResult(res)
dbDisconnect(con)

