# This script creates a SQL table of CFPB comment metadata 
# subset to Dodd-Frank dockets or RINs as identified by Davis Polk 

## originally pulled from Devin's master data 
# load(here::here("data", "comment_metadata.Rdata"))

## now pulling from new search of API v4 for shits and giggles
load(here::here("data", "CFPBcomments.Rdata"))

comments_all <- CFPBcomments 

nrow(comments_all)
head(comments_all)
names(comments_all)  

# Rename to fit https://docs.google.com/spreadsheets/d/1i8t_ZMAhjddg7cQz06Z057BqnNQEsC4Gur6p17Uz0i4/edit#gid=1357829693
names(comments_all)  <- names(comments_all) %>% 
  str_replace_all("([A-Z])", "_\\1") %>% 
  str_to_lower()

names(comments_all)



comments_all %<>% mutate(source = "regulations.gov")

# v3
comments_all %<>% mutate(comment_id = document_id)
# v4
comments_all %<>% mutate(comment_id = id)

comments_all %<>% mutate(comment_url = str_c("https://www.regulations.gov/comment", comment_id))

#FIXME v4 does not return comment due date 
comments_all %<>% mutate(late_comment = as.Date(posted_date) > as.Date(comment_due_date))

 # V4 no longer returns organization or docket title 




comments_all %<>% 
  mutate(docket_id = str_remove(comment_id, "-[0-9]*$"))


vars_to_keep <- c("fr_document_id", # need this from rules table joined in by document_id - comment_id
                  "docket_title", # this may clash with docket title from attachments table
                  "docket_type",
                  "rin",
                  "attachment_count",
                  "posted_date",
                  "submitter_name",
                  "organization",
                  "late_comment")

# FIXME trim down to minimial variables
comments_all %<>% select(source,
                         #fr_document_id, # need this from rules table joined in by document_id - comment_id
                         agency_acronym = agency_id,
                         docket_id, 
                         comment_id,
                         comment_title = title, # rename
                         comment_url,
                         any_of(vars_to_keep) )



# filter to only CFPB comments
comments_cfpb <- comments_all %>% filter(agency_acronym == "CFPB")
nrow(comments_cfpb)
head(comments_cfpb)


# save Rdata 
save(comments_cfpb, file = here::here("data", "comment_metadata_CFPB.Rdata"))


# Create RSQLite database for all CFPB comment metadata 
library(DBI)
install.packages("RSQLite")
1
library(RSQLite)
con <- dbConnect(RSQLite::SQLite(), here::here("data", "comment_metadata_CFPB.sqlite"))

# check 
list.files("data")

dbListTables(con)
dbWriteTable(con, "comments_cfpb", comments_cfpb, overwrite = T)
dbListTables(con)

dbListFields(con, "comments_cfpb")
# dbReadTable(con, "comments_cfpb") # oops

# fetch results:
res <- dbSendQuery(con, "SELECT * FROM comments_cfpb WHERE agency_acronym = 'CFPB'")

dbFetch(res) %>% head()
dbClearResult(res)

dbDisconnect(con)


###################################################
# Subset to Davis Polk Dodd-Frank rules 
# load(here::here("data", "comment_metadata_CFPB.Rdata"))
names(comments_cfpb)

# Dodd-Frank rules from Davis Polk Data
df <- read_csv(here::here("data", "dockets_to_scrape.csv"))
names(df)
head(df)
df %<>% filter(str_detect(agency, "CFPB"))

# Subset to Dodd-Frank rules (by docket or RIN)
df_rins <- df$RIN %>% na.omit() %>% unique()
df_dockets <- df$identifier %>% na.omit() %>% unique()

comments_cfpb_df <- comments_cfpb %>% 
  filter(docket_id %in% df_dockets | rin %in% df_rins)

# rins not in dockets to scrape
comments_cfpb_df %>% 
  filter(!rin %in% df_rins) %>% 
  select(docket_id, rin) %>% 
  distinct()

# dockets not in dockets to scrape
comments_cfpb_df %>% 
  filter(!docket_id %in% df_dockets) %>% 
  select(docket_id, rin) %>% 
  distinct() %>% knitr::kable()


comments_cfpb_df$docket_id %>% unique()
comments_cfpb_df$rin %>% unique()

# look back to see how many we matched 
matched <- df %>% filter(RIN %in% na.omit(comments_cfpb_df$rin) | identifier %in% na.omit(comments_cfpb_df$docket_id))
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


# save Rdata 
save(comments_cfpb_df, file = here::here("data", "comment_metadata_CFPB_df.Rdata"))


# Create RSQLite database
con <- dbConnect(RSQLite::SQLite(), here::here("data", "comment_metadata_CFPB_df.sqlite"))

# check 
list.files("data")

dbListTables(con)
dbWriteTable(con, "comments_cfpb_df", comments_cfpb_df, overwrite = T)
dbListTables(con)

dbListFields(con, "comments_cfpb_df")
# dbReadTable(con, "comments_cfpb") # oops

# fetch results:
res <- dbSendQuery(con, "SELECT * FROM comments_cfpb_df WHERE agency_acronym = 'CFPB'")

dbFetch(res) %>% head()
dbClearResult(res)
dbDisconnect(con)

