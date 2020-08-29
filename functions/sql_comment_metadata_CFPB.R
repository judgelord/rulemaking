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

# Davis Polk Data
df <- read_csv(here::here("data", "Rule_Progress.csv"))
df %<>% filter(str_detect(Agencies, "CFPB"))
df %<>% mutate(rin = str_extract(Citation, "RIN.*") %>% 
                 str_remove_all("RIN| ") %>% 
                 str_remove_all(" ") %>% 
                 str_replace("-|—|–", "zzz")%>%
                 str_remove(regex("\\W+")) %>% 
                 str_replace("zzz", "-"))

## correrctions to Davis Polk Data
df %<>% mutate(rin = ifelse(str_detect(Citation, 
                                       "12 CFR part 1041"), 
                            "3170-AA40",
                            rin))

# comments_cfpb %>% filter(str_detect(docket_title, "diversity policies and practices"))
df %<>% mutate(docket_id = ifelse(str_detect(Citation, 
                                       "Release No. 34-75050"), 
                                  "CFPB-2013-0029",
                            "other"))

df %<>% mutate(docket_id = ifelse(str_detect(Citation, 
                                             "3170-AA02"), 
                                  "CFPB-2011-0005",
                                  docket_id))

df %>% select(rin, Citation, docket_id)
df %>% select(rin, Citation, `Regulator Summary`) %>% filter(is.na(rin)) %>% knitr::kable()


# filter regulations.gov data
comments_cfpb_df <- comments_cfpb %>% filter(rin %in% df$rin | docket_id %in% df$docket_id)

comments_cfpb_df$docket_id %>% unique()
comments_cfpb_df$rin %>% unique()

# look back to see how many we matched 
matched <- df %>% filter(rin %in% comments_cfpb_df$rin | docket_id %in% comments_cfpb_df$docket_id)
unmatched <- df %>% anti_join(matched)
unmatched$rin %>% unique() 

unmatched %>% select(Citation, `Regulator Summary`)

# To investigate 
# 12 CFR Part 1082 [Docket No. CFPB-2011-0005] RIN 3170-AA02 State Official Notification Rule 

# 0 comments
"12 CFR Part 1090\nRIN 3170-AA30"

# In Unified agenda but not regulations.gov :
# Ability-to-Repay and Qualified Mortgage Standards Under the Truth in Lending
"3170-AA16" 
"12 CFR Part 1070\nRIN 3170-AA01"

# FED Rins:
"1557-AD62" 
"1557-AD64" 



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