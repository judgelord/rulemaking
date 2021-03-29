# Create RSQLite database
library(DBI)
# install.packages("RSQLite")
1
library(RSQLite)
library(tidyverse)
library(magrittr)

# Create RSQLite database
con <- dbConnect(RSQLite::SQLite(), here::here("db", "regs_dot_gov.sqlite"))

# fetch results:
comments_cfpb <- dbGetQuery(con, "SELECT * FROM comments WHERE agency_acronym = 'CFPB'")

head(comments_cfpb)

# Refining for uniqueness 
d <- comments_cfpb

names(d)

# redundent 
d %<>% select(-late_comment)

# oddly there are duplicate comments with only different start dates
# FIXME I use comment_start_date to merge in fr_document_id below; selecting max here may leave some without a match below. Hopefully we can get fr_doc id from the new regulations.gov metatdata
d %<>% group_by(document_id) %>% 
  slice_max(comment_start_date)

d %<>% distinct()

# some dockets have more than one due date due to comment period extensions
d %<>% group_by(document_id) %>% 
  slice_max(comment_due_date)

# oddly, some comments have more than one posted date; maybe they were updated between my scrapes?
d %<>% group_by(document_id) %>% 
  slice_max(posted_date) %>% 
  ungroup()

# check for duplicate urls (primary key to attachments table)
look <- d %>% 
  add_count(document_id, sort = T) %>% 
  filter(n >1) %>% 
  arrange(document_id)

# inspect duplicates
head(look)


comments_cfpb <- d %>% ungroup()
#/dedupe


comments_cfpb %<>% rename(comment_title = title)

comments_cfpb %<>% select(#fr_document_id,
                          agency_acronym,
                          rin,
                          docket_id,
                          docket_title,
                          attachment_count,
                          document_id,
                          posted_date,
                          submitter_name,
                          comment_title,
                          organization,
                          #comment_url,
                          #late_comment,
                          comment_start_date,
                          comment_text
                          ) %>% 
  mutate(source = "regulations.gov",
         comment_url = str_c("https://www.regulations.gov/document/", document_id)
         ) %>% 
  distinct()

names(comments_cfpb)

# check for dupes
comments_cfpb %>% 
  add_count(comment_url) %>% 
  filter(n > 1) %>% 
  select(submitter_name, rin, posted_date) %>% 
  head() %>% knitr::kable()






###################################################
# Subset to Davis Polk Dodd-Frank rules 

# Dodd-Frank rules from Davis Polk Data
df <- read_csv(here::here("data", "dockets_to_scrape.csv"))
names(df)
head(df)

df_rins <- df$RIN %>% na.omit() %>% unique()
df_dockets <- df$identifier %>% na.omit() %>% unique()

# dockets with multiple fr docs
df %>% count(identifier, sort = T) 

# Subset to Dodd-Frank rules
# overinclusive subset?
comments_cfpb_df <- comments_cfpb %>% ungroup() %>% 
  filter(docket_id %in% df_dockets | rin %in% df_rins)




# rins not in dockets to scrape
comments_cfpb_df %>% 
  filter(!rin %in% df_rins, rin != "Not Assigned") %>% 
  select(docket_id, rin) %>% 
  distinct()

# dockets not in dockets to scrape
comments_cfpb_df %>% 
  filter(!docket_id %in% df_dockets) %>% 
  select(docket_id, rin) %>% 
  distinct() %>% knitr::kable()

# FIXME - investigate these 
# |docket_id      |rin          |
#   |:--------------|:------------|
#   |CFPB-2011-0040 |Not Assigned |
#   |CFPB-2014-0014 |7100-AD68    |
#   |CFPB-2015-0004 |3170-AA43    |
#   |CFPB-2016-0016 |3170-AA49    |


comments_cfpb_df$docket_id %>% unique()
comments_cfpb_df$rin %>% unique()

# look back to see how many we matched 
matched <- df %>% 
  filter(RIN %in% na.omit(comments_cfpb_df$rin) | identifier %in% na.omit(comments_cfpb_df$docket_id))

unmatched <- df %>% anti_join(matched) %>% filter(agency == 'CFPB')

unmatched %>% 
  select(identifier) %>% 
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


# 10 CFPB-2017-0027


# INVESTIGATE 
# 11 CFPB-2011-0008 26 comments NOT IN regulations.gov API DATA?
# 12 CFPB-2013-0001 1 comment
############################

unmatched %>% filter(docket_id %in% c("FPB-2011-0008", "CFPB-2013-0001"))


# Create RSQLite database
con <- dbConnect(RSQLite::SQLite(), here::here("db", "comment_metadata_CFPB_df.sqlite"))

# check 
list.files("db")

dbListTables(con)
dbWriteTable(con, "comments", comments_cfpb_df, overwrite = T)
dbListTables(con)
dbDisconnect(con)


# Create RSQLite database
con <- dbConnect(RSQLite::SQLite(), here::here("db", "metadata_CFPB_df.sqlite"))

# check 
list.files("db")

dbListTables(con)
dbWriteTable(con, "comments", comments_cfpb_df, overwrite = T)
dbListTables(con)

dbWriteTable(con, "actions", actions_cfpb %>% select(-fr_document_id_length, -fr_number), overwrite = T)
dbListTables(con)


dbListFields(con, "comments")
dbListFields(con, "actions")

# fetch results:
dbGetQuery(con, "SELECT * FROM actions WHERE docket_id = 'CFPB-2012-0029'") %>% head()
dbGetQuery(con, "SELECT * FROM comments WHERE docket_id = 'CFPB-2012-0029'") %>% head()
dbClearResult(res)
dbDisconnect(con)

