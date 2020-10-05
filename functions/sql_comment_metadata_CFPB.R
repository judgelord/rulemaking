# This script joins data on all comments from regulations.gov
# Completing missing from any one dataset
# Create R data
load(here::here("data", "comment_metadata.Rdata"))
nrow(comments_all)
head(comments_all)
names(comments_all)  

# Rename to fit https://docs.google.com/spreadsheets/d/1i8t_ZMAhjddg7cQz06Z057BqnNQEsC4Gur6p17Uz0i4/edit#gid=1357829693
names(comments_all)  <- names(comments_all) %>% 
  str_replace_all("([A-Z])", "_\\1") %>% 
  str_to_lower()

names(comments_all)

comments_all %<>% mutate(source = "regulations.gov")

comments_all %<>% mutate(comment_url = str_c("https://www.regulations.gov/document?D=", document_id))

comments_all %<>% mutate(late_comment = as.Date(posted_date) > as.Date(comment_due_date))

# FIXME trim down to minimial variables
comments_all %<>% select(source,
                         #fr_document_id, # need this from rules table joined in by document_id - comment_id
                         agency_acronym,
                         docket_id, 
                         docket_title, # this may clash with docket title from attachments table
                         # docket_type,
                         rin,
                         attachment_count,
                         posted_date,
                         submitter_name,
                         document_id,
                         comment_title = title, # rename
                         organization,
                         comment_url,
                         late_comment
                         )



# filter to only CFPB comments
comments_cfpb <- comments_all %>% filter(agency_acronym == "CFPB")
nrow(comments_cfpb)
head(comments_cfpb)


# save Rdata 
save(comments_cfpb, file = here::here("data", "comment_metadata_CFPB.Rdata"))


# Create RSQLite database
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
  filter(!docket_id %in% df_docekts) %>% 
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



#FIXME MOVE TO ANOTHER SCRIPT 
# join in fed reg from regs.gov
load(here::here("data", "AllRegsGovRules.Rdata"))

# Rename to fit https://docs.google.com/spreadsheets/d/1i8t_ZMAhjddg7cQz06Z057BqnNQEsC4Gur6p17Uz0i4/edit#gid=1357829693
names(d)  <- names(d) %>% 
  str_replace_all("([A-Z])", "_\\1") %>% 
  str_to_lower()

regs_dot_gov_actions <- d %>% mutate_all(as.character) %>%
  mutate(fr_document_id = fr_number %>%
           str_replace_all(" - |- |- | -| FR, ISSUE |, ISSUE #|  NO\\. | FR, NO. |, NO\\. |\\. NO. |, NO\\.| NO\\. |\\. NO | NO | FR |FR|\\):|\\(|\\)", "-") %>% 
           str_replace_all("E-", "E") %>% 
           # extract fed reg vol pattern
           str_extract("[0-9][0-9]+(-| |=)[0-9]+") %>%
           # replace space with dash
           str_replace(" |=", "-"),
         fr_document_id2 = fr_number %>%
           str_replace_all(" - |- |- | -| FR, ISSUE |, ISSUE #|  NO\\. | FR, NO. |, NO\\. |\\. NO. |, NO\\.| NO\\. |\\. NO | NO | FR |FR|\\):|\\(|\\)", "-") %>% 
           # extract fed reg vol pattern
           str_extract("(C|E|R|Z)[0-9]+(-| |=)[0-9]+") %>%
           # replace space with dash
           str_replace(" |=", "-") ) %>%
  mutate(fr_document_id = coalesce(fr_document_id, fr_document_id2))
  
regs_dot_gov_actions %<>% 
  mutate(fr_document_id_length = fr_document_id %>% nchar() ) 

regs_dot_gov_actions %>% filter(is.na(fr_document_id), 
                                !is.na(fr_number)) %>% 
  select(fr_number) %>% filter(nchar(fr_number) > 5)

# example fed reg ids
regs_dot_gov_actions %>% arrange(-fr_document_id_length) %>% 
  group_by(fr_document_id_length) %>%
  add_count(name = "n_of_length") %>% 
  slice(1) %>%
  select(n_of_length, fr_document_id, fr_number) %>% knitr::kable()

#FIXME edit document_number
regs_dot_gov_actions %<>% mutate(fr_document_id = ifelse(str_detect(fr_document_id, "^[0-9]{4}-[0-9]{4}$"),
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
  filter(!document_number %in% regs_dot_gov_actions$fr_document_id)

#FIXME edit document_number
# df %<>% mutate(document_number = ifelse(str_detect(document_number, "-[0-9]{4}$"),
#                                        document_number %>% str_replace("-", "-0"),
#                                        document_number)) 
# 

# save all actions
save(regs_dot_gov_actions, file = here::here("data", "regs_dot_gov_actions.Rdata"))
# /FIXME MOVE TO ANOTHER SCRIPT 

actions_cfpb <- regs_dot_gov_actions %>%
  filter(agency_acronym == "CFPB") 

# check against Dodd-Frank dockets to scrape

# merging on regulations.gov document id
df_min <- df %>% select(document_id = 
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
  select(docket_id, fr_document_id, document_number, document_id) %>% knitr::kable()




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
         REG_DOT_GOV_DOCNO != document_id)%>% 
  select(docket_id, fr_document_id, REG_DOT_GOV_DOCNO, document_id) %>% knitr::kable()

# add actions to Dodd Frank dockets to scrape FR numbers
df_min %<>% left_join(actions_cfpb %>% 
                        filter(number_of_comments_received >0,
                               document_type == "Proposed Rule",
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
         number_of_comments_received >0,
         document_type == "Proposed Rule",
         !is.na(fr_document_id)) %>% 
  arrange(docket_id) %>%
  select(docket_id, rin, fr_document_id, document_id, 
         number_of_comments_received, comment_start_date, comment_due_date) %>%
  knitr::kable()

names(comments_cfpb)
comments_cfpb_df %>% 
  filter(docket_id %in% multiples$docket_id) %>% 
  select(docket_id, rin) %>% distinct()



df_min %<>% 
  select(fr_document_id, docket_id) %>% 
  distinct()

df_min %>% add_count(docket_id, sort = T)

names(actions_cfpb)
actions_cfpb %>% select(number_of_comments_received, comment_start_date)
names(comments_cfpb_df)
# N
comments_cfpb_df %>% 
  left_join(df_min %>% 
              select(fr_document_id, docket_id))%>% 
  distinct() %>% nrow()
# target N
nrow(comments_cfpb_df)

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

