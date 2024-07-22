# This script joins data on all comments from regulations.gov and formats them for SQL

# Completing missing from any one dataset
load(here::here("ascending2/allcomments.Rdata"))
dim(all)
all2 <- all

load(here::here("ascending/allcomments.Rdata"))
dim(all)

comments_all <- all %>% full_join(all2)
dim(all)

# drop open for comment
comments_all %<>% select(-openForComment)

# Rename to fit https://docs.google.com/spreadsheets/d/1i8t_ZMAhjddg7cQz06Z057BqnNQEsC4Gur6p17Uz0i4/edit#gid=1357829693
names(comments_all)  <- names(comments_all) %>%
  str_replace_all("([A-Z])", "_\\1") %>%
  str_to_lower()
names(comments_all)

docket_info <- comments_all %>% select(docket_id,
                                       docket_type,
                                       docket_title,
                                       comment_start_date,
                                       comment_due_date,
                                       allow_late_comment) %>%
  distinct()

# should be the same if docket info is unique to docket_id
nrow(docket_info)
comments_all$docket_id %>% unique() %>% length()

# #FIXME
# # keeping these for now because docket info is not unique to docket_id
# #TODO investigate
# comments_all %<>% select(-docket_title, # in principle, this should go
#                          -docket_type, # in principle, this should go
#                          -comment_start_date,
#                          -comment_due_date,
#                          -allow_late_comment)

comments_all %>% head()

# an example obs for the docs
comments_all %>% filter(agency_acronym == "CFPB",
                        attachment_count > 0,
                        #docketType == "Rulemaking",
                        nchar(comment_text) > 200) %>%
  slice(1) %>%
  gather(key = "field", value = "example") %>% # knitr::kable()
  write_csv(path = "comment_metadata_example.csv")

comments <- comments_all
# Update R data
save(comments, file = "comments_metadata_2020.Rdata")
# load("comments_metadata.Rdata")
nrow(comments)
head(comments)

# LOAD RULES DATA FROM API
# load(here::here("data", "AllRegsGovRules.Rdata"))
# rules <- d %>% select(-X)

load(here::here("data", "rules_metadata.Rdata"))
names(rules)


# Reformat
names(rules)  <- names(rules) %>%
  str_replace_all("([A-Z])", "_\\1") %>%
  str_to_lower()
names(rules)
head(rules)

#FIXME lots more standardizing to do. Code for testing in sql_comment_metadata_CFPB
# THIS SHOULD BE A FUNCTION clean_fr_number
# standardize fr_document_id
rules %<>% #mutate_all(as.character) %>%
  mutate(fr_document_id = fr_number %>%
           as.character() %>%
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
names(rules)

rules %<>% select(-fr_document_id2)

rules %<>% select(-fr_document_id2)




# Update R data
save(rules, file = "rules_metadata.Rdata")


# load("rules_metadata.Rdata")
# load("comment_metadata.Rdata")
names(rules)
dim(rules)
rules$docket_id %>% unique() %>% length()

# Create RSQLite database
library(DBI)
library(RSQLite)
con <- DBI::dbConnect(SQLite(), here::here("regs_dot_gov.sqlite"))

# check to make sure it was created
list.files()

dbListTables(con)
dbWriteTable(con, "comments", comments, overwrite = T)
dbListTables(con)
dbWriteTable(con, "rules", rules, overwrite = T)
dbListTables(con)

# check var names
dbListFields(con, "comments")
dbListFields(con, "rules")

# fetch results for an agency
res <- DBI::dbSendQuery(con, "SELECT * FROM comments WHERE agency_acronym = 'CFPB'")
comments_CFPB <- dbFetch(res)
dbClearResult(res)

# fetch results for a docket
res <- DBI::dbSendQuery(con, "SELECT * FROM comments WHERE docket_id = 'CFPB-2018-0023'")
comments_docket <- dbFetch(res)
dbClearResult(res)

nrow(comments_docket)


# fetch results for a comment
res <- DBI::dbSendQuery(con, "SELECT * FROM comments WHERE document_id = 'CFPB-2018-0023-0006'")
comment_example <- dbFetch(res)
comment_example %>%
  mutate_all(as.character) %>%
  pivot_longer( everything() ) %>%
  knitr::kable()

dbClearResult(res)

comments$number_of_comments_received %>% sum()
rules$number_of_comments_received %>% sum()




##################################################################
# Create RSQLite database
con <- dbConnect(SQLite(), here::here("comment_metadata_CFPB.sqlite"))
# check
list.files()

dbListTables(con)
dbWriteTable(con, "comments", comments_all %>% filter(agency_acronym == "CFPB"), overwrite = T)
dbListTables(con)
