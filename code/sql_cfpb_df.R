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
nrow(d)
nrow(comments_cfpb)
names(d)

comments_cfpb <- d %>% ungroup()
#/dedupe

# fetch results:
rules_cfpb <- dbGetQuery(con, "SELECT * FROM rules WHERE agency_acronym = 'CFPB'")

head(rules_cfpb)

dbClearResult(res)

dbDisconnect(con)

dockets <- rules_cfpb %>% select(docket_id, fr_document_id, comment_start_date) 

dockets %>% count(docket_id,
                  comment_start_date, 
                  fr_document_id,
                  sort = T) %>% head()

nrow(dockets)
dockets %>% distinct() %>% nrow()

# crosswalk of docket + comment start --> fr_document_id
crosswalk <- comments_cfpb %>% ungroup() %>% left_join(dockets) %>% 
  select(docket_id, fr_document_id, comment_start_date) %>% distinct()
nrow(crosswalk)

crosswalk %>% count(docket_id,
                  comment_start_date, 
                  fr_document_id,
                  sort = T) %>% head()

crosswalk %>% filter(is.na(fr_document_id))

# to fill in NAs, include additional fr_document ids where start dated did not match
# join in alt fr_document id
crosswalk %<>% left_join(dockets %>% select(-comment_start_date) %>% rename(fr2 = fr_document_id) %>% distinct() )
nrow(crosswalk)


crosswalk %<>% 
  distinct() %>% 
  mutate(fr_document_id = coalesce(fr_document_id, fr2)) %>%
  select(-fr2) %>% 
  filter(!is.na(fr_document_id)) %>% 
  distinct()

# should be back to original size
nrow(crosswalk)


crosswalk %>% count(docket_id,
                    comment_start_date, 
                    fr_document_id,
                    sort = T) %>% head()

crosswalk %>% arrange(docket_id)

crosswalk %<>% filter(!is.na(comment_start_date))
nrow(crosswalk)

crosswalk %>% filter(fr_document_id == '2012-01728')
crosswalk %>% filter(fr_document_id == '2012-01726')

rules_cfpb %>% filter(fr_document_id == '2012-01726')

# summary  stats 
rules_cfpb %>% 
  arrange(-number_of_comments_received) %>% 
  select(number_of_comments_received, title) %>% 
  head()

comments_cfpb %>% count(docket_id, docket_title, sort = T)


crosswalk %<>% left_join(rules_cfpb %>% distinct(number_of_comments_received, 
                                                 #document_type, 
                                                 document_id, 
                                                 fr_document_id))

nrow(crosswalk)

crosswalk %>% count(fr_document_id, sort = T)

names(crosswalk)
crosswalk %<>% group_by(fr_document_id) %>% 
  slice_max(number_of_comments_received)

nrow(crosswalk)

crosswalk %>% count(fr_document_id, sort = T)

crosswalk %>% filter(fr_document_id == '2013-00736') %>% knitr::kable()

# by number of comments 
crosswalk %<>% group_by(fr_document_id) %>% 
  slice_max(comment_start_date) 

crosswalk %>% count(fr_document_id, sort = T)

# one regulations.gov id 
crosswalk %<>% group_by(fr_document_id) %>% 
  slice_max(document_id) 

crosswalk %>% count(fr_document_id, sort = T)
#####################
# JOIN comment data with crosswalk to add fr_document_id
# First, check for comment times in comment data
comments_cfpb %>% filter(is.na(comment_start_date))


comments_cfpb %<>% left_join(crosswalk)

#  should now have fr_document_id
names(comments_cfpb)

# missing fr_document_id
comments_cfpb %>% filter(is.na(fr_document_id))

comments_cfpb %<>% rename(comment_title = title)

comments_cfpb %<>% select(fr_document_id,
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
  select(fr_document_id, submitter_name, rin, posted_date, ) %>% 
  head() %>% knitr::kable()

# duplicates by federal register number 


# rename to match 
names(rules_cfpb)
actions_cfpb <- rules_cfpb %>% 
  select(fr_document_id,
         fr_number,
         #volume,
         #start_page,
         #page_length,
         agency_acronym,
         title,
         #action_description_fr,
         stage = document_type,
         #stage_n,
         #action,
         #joint, 
         #publication_date = posted_date,
         #action_url,
         reg_dot_gov_document_id = document_id,
         rin,
         docket_id)






###################################################
# Subset to Davis Polk Dodd-Frank rules 

# Dodd-Frank rules from Davis Polk Data
df <- read_csv(here::here("data", "dockets_to_scrape.csv"))
names(df)
head(df)

df_rins <- df$RIN %>% na.omit() %>% unique()
df_dockets <- df$identifier %>% na.omit() %>% unique()

df %<>% distinct(agency, identifier, document_number, REG_DOT_GOV_DOCNO)

df %>% distinct() %>% 
  count(agency, identifier, REG_DOT_GOV_DOCNO ,sort = T) %>%
  filter(n > 1) %>% knitr::kable()

df %<>% filter(str_detect(agency, "CFPB"))

df %>% count(REG_DOT_GOV_DOCNO, sort = T)

df %>% filter(is.na(REG_DOT_GOV_DOCNO))

# Subset to Dodd-Frank rules


names(comments_cfpb)

head(comments_cfpb$document_id)

# overinclusive subset
comments_cfpb_df <- comments_cfpb %>% ungroup() %>% 
  filter(docket_id %in% df_dockets | rin %in% df_rins | fr_document_id %in% df$document_number)

# dockets with multiple fr docs
df %>% count(identifier, sort = T) 


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

unmatched <- df %>% anti_join(matched)

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

############################




##########################
# Actions table 


actions_cfpb %<>% 
  mutate(fr_document_id_length = fr_document_id %>% nchar() ) 

actions_cfpb %>% filter(is.na(fr_document_id), 
                                !is.na(fr_number)) %>% 
  select(fr_number) %>% filter(nchar(fr_number) > 5)

# example fed reg ids
actions_cfpb %>% arrange(-fr_document_id_length) %>% 
  group_by(fr_document_id_length) %>%
  add_count(name = "n_of_length") %>% 
  slice(1) %>%
  select(n_of_length, fr_document_id, fr_number) %>% knitr::kable()

#FIXME edit document_number
actions_cfpb %<>% mutate(fr_document_id = ifelse(str_detect(fr_document_id, "^[0-9]{4}-[0-9]{4}$"),
                                                         fr_document_id %>% str_replace("-", "-0"),
                                                         fr_document_id)) 

# fed reg ids in dodd frank dockets to scrape 
df %>%
  mutate(fr_document_id_length = document_number %>% nchar() ) %>%
  arrange(-fr_document_id_length) %>% 
  group_by(fr_document_id_length) %>%
  add_count(name = "n_of_length") %>% 
  slice(1) %>%
  select(n_of_length, document_number) %>% knitr::kable()

df %>% #filter(str_detect(document_number, "-0")) %>% 
  filter(!document_number %in% actions_cfpb$fr_document_id)

#FIXME edit document_number
# df %<>% mutate(document_number = ifelse(str_detect(document_number, "-[0-9]{4}$"),
#                                        document_number %>% str_replace("-", "-0"),
#                                        document_number)) 
# 



# check against Dodd-Frank dockets to scrape

# merging on regulations.gov document id
df_min <- df %>% select(reg_dot_gov_document_id = 
                          REG_DOT_GOV_DOCNO,
                        #fr_document_id = 
                        document_number) %>% 
  distinct()

# failing 
df_min %>% left_join(actions_cfpb) %>% 
  filter(is.na(agency_acronym))

# mismatched federal reg docuemnt ID
df_min %>% left_join(actions_cfpb) %>% 
  filter(!is.na(agency_acronym),
         fr_document_id != document_number)%>% 
  select(docket_id, fr_document_id, document_number, reg_dot_gov_document_id) %>% knitr::kable()




# merging on fr_document_id
df_min <- df %>% 
  select(#document_id = 
    REG_DOT_GOV_DOCNO,
    fr_document_id = document_number ) %>% 
  distinct() 

df_min %<>% 
  mutate(fr_document_id = fr_document_id %>% 
           str_extract("[0-9]{4}-[0-9]{5}"))

# failing 
df_min %>% left_join(actions_cfpb) %>% 
  filter(is.na(agency_acronym))

# bad regs_gov docuemnt ID
df_min %>% left_join(actions_cfpb) %>% 
  filter(!is.na(agency_acronym),
         REG_DOT_GOV_DOCNO != reg_dot_gov_document_id)%>% 
  select(docket_id, fr_document_id, REG_DOT_GOV_DOCNO, reg_dot_gov_document_id) %>% knitr::kable()


# add actions to Dodd Frank dockets to scrape by FR numbers
df_min %<>% left_join(actions_cfpb %>% 
                        filter(#number_of_comments_received >0,
                               stage == "Proposed Rule",
                               !is.na(fr_document_id))) 

df_min %<>% 
  select(fr_document_id, docket_id) %>% 
  distinct()

multiples <- df_min %>% 
  distinct() %>% 
  count(docket_id, sort = T) %>%
  filter(n ==2)

actions_cfpb %>% 
  filter(docket_id %in% multiples$docket_id,
         #number_of_comments_received >0,
         stage == "Proposed Rule",
         !is.na(fr_document_id)) %>% 
  arrange(docket_id) %>%
  select(docket_id, rin, fr_document_id, reg_dot_gov_document_id) %>%
         #number_of_comments_received, comment_start_date, comment_due_date) %>%
  knitr::kable()







#### ???????? is this incomplete 




names(comments_cfpb_df)
# N
comments_cfpb_df %>% 
  left_join(df_min %>% 
              select(fr_document_id, docket_id))%>% 
  distinct() %>% nrow()
# target N
nrow(comments_cfpb_df)

# check for duplicatess
comments_cfpb_df %>% add_count(comment_url) %>% filter(n > 1) %>% 
  select(fr_document_id, submitter_name, rin, document_id, posted_date)

names(df)
# FIXME given differences in fr_document_id, this may remove too many 
comments_cfpb_df %<>% filter(fr_document_id %in% df$document_number)


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

