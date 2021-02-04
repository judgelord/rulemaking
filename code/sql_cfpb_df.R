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

dbClearResult(res)

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

crosswalk <- comments_cfpb %>% left_join(dockets) %>% 
  select(docket_id, fr_document_id, comment_start_date) %>% distinct()

crosswalk %>% count(docket_id,
                  comment_start_date, 
                  fr_document_id,
                  sort = T) %>% head()

crosswalk %<>% left_join(dockets %>% select(-comment_start_date) %>% rename(fr2 = fr_document_id) %>% distinct() )

crosswalk %<>% 
  distinct() %>% 
  mutate(fr_document_id = coalesce(fr_document_id, fr2)) %>%
  select(-fr2) %>% 
  filter(!is.na(fr_document_id)) %>% 
  distinct()

crosswalk %>% count(docket_id,
                    comment_start_date, 
                    fr_document_id,
                    sort = T) %>% head()

crosswalk %>% arrange(docket_id)

crosswalk %<>% filter(!is.na(comment_start_date))

comments_cfpb %<>% left_join(crosswalk)

names(comments_cfpb)
comments_cfpb %<>% select(fr_document_id,
                          agency_acronym,
                          rin,
                          docket_id,
                          docket_title,
                          attachment_count,
                          document_id,
                          posted_date,
                          submitter_name,
                          comment_title = title,
                          organization,
                          #comment_url,
                          #late_comment,
                          comment_text
                          ) %>% 
  mutate(source = "regulations.gov",
         comment_url = str_c("https://www.regulations.gov/document?D=", document_id)
         )

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
df %<>% filter(str_detect(agency, "CFPB"))

# Subset to Dodd-Frank rules
df_rins <- df$RIN %>% na.omit() %>% unique()
df_dockets <- df$identifier %>% na.omit() %>% unique()

comments_cfpb_df <- comments_cfpb %>% filter(docket_id %in% df_dockets | rin %in% df_rins)

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
