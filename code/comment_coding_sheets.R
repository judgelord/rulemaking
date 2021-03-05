
source("setup.R")

# rules and proposed rules 
load(here::here("data", "rules_metadata.Rdata"))
rules %<>% as_tibble()
names(rules)
head(rules$posted_date)
tail(rules$posted_date)

rules$posted_date %<>% as.Date()

rules %<>% mutate(year = str_sub(posted_date, 0, 4) %>% as.integer())
unique(rules$year)

rules %<>% filter(year > 2000)

# inspect for completeness (metadata are missing notices for 2018 and 2019)
count(rules, year)
rules %>% filter(document_type =="Rule") %>% count(year)
rules %>% filter(document_type =="Proposed Rule") %>% count(year)

#FIXME
# Sampling
load(here::here("data", "ejdockets.Rdata"))
head(ejdockets)

rules %<>% filter(docket_id %in% ejdockets$docket_id)
nrow(rules)

done <- list.files(here::here("data", "datasheets")) %>% 
  str_remove("_.*")

rules %<>% filter(docket_id %in% done)
nrow(rules)
#/FIXME


# comments 
load(here::here("comment_metadata.Rdata"))
names(comments_all)

topdockets <- rules %>% 
  group_by(docket_id) %>% 
  mutate(number_of_comments_received = sum(number_of_comments_received)) %>%
  ungroup() %>% 
  filter(docket_type == "Rulemaking",
         number_of_comments_received > 0, # dockets with 100 comments 
         document_type == "Rule") %>% # dockets with a final rule (initially sampled all dockets)
  group_by(agency_acronym) %>%
  # agencie that have mass dockets
  mutate(max_comments = max(number_of_comments_received)) %>%
  #filter(max_comments > 100) %>%  
  slice_max(order_by = number_of_comments_received,
            n = 5,
            with_ties =F)

nrow(topdockets)
# ejdockets <- rules %>% 
#   ungroup() %>% 
#   filter(docket_type == "Rulemaking",
#          number_of_comments_received > 0,
#          docket_id %in% ej_dockets) 

topdockets$number_of_comments_received %>% head()

dim(topdockets)

count(topdockets, docket_id, number_of_comments_received)

agencies <- unique(rules$agency_acronym)

# agencies = c("EPA","ATF",
#              "NLRB",
#              "OFCCP",
#              "OJP",
#              "USCG",
#              "CIS",
#              "USCBP",
#              "PHMSA",
#              "DOS",
#              "ED",
#              "MSHA",
#              "BSEE",
#              "DOJ-CRT",
#              "DOL",
#              "BIA",
#              "FEMA",
#              "BLM",
#              "DOI",
#              "DEA",
#              "OSHA",
#              "DARS",
#              "DHS")

d <- comments_all %>% 
  # filter to top dockets
  filter(docket_id %in% topdockets$docket_id) %>% 
  # selecting agencies for hand codeing
  filter(agency_acronym %in% agencies) 

dim(d)



# filter to mass dockets
d %<>% group_by(docket_id) %>% 
  # mass dockets
  mutate(comments_on_docket = sum(number_of_comments_received), 
         max = max(number_of_comments_received) ) %>% 
  ungroup() %>% 
  filter(max > 99 | comments_on_docket > 999)
dim(d)

# apply auto-coding 
#FIXME with updated org_names from hand-coding 
source(here::here("code", "org_name.R"))
#FIXME source(here::here("code", "comment_position.R"))

save(d, file = here::here("data", "comments4datasheets.Rdata"))
# load(here::here("data", "comments4datasheets.Rdata"))
temp <- d
d <- temp

d %>% count(org_name, sort = T)

d %<>% mutate(org_name = ifelse(str_dct(title, "Chief, "),
                               str_remove(title, ".*Chief, "),
                               org_name),
             org_name = ifelse(str_dct(title, "Member of Congress|Senat|Rep\\.|Sen\\."),
                               title,
                               org_name),
             org_name = ifelse(str_dct(title, ", Counsel,"),
                               str_remove(title, ".*, Counsel,"),
                               org_name),
             org_name = ifelse(str_dct(title, "President,"),
                               str_remove(title, ".*President,"),
                               org_name),
             org_name = ifelse(str_dct(title, ", Chairman, "),
                               str_remove(title, ".*, Chairman, "),
                               org_name),
             org_name = ifelse(str_dct(title, "Chair,"),
                               str_remove(title, ".*Chair, "),
                               org_name),
             org_name = ifelse(str_dct(title, ", MPA, "),
                               str_remove(title, ".*, MPA, "),
                               org_name),
             org_name = ifelse(str_dct(title, ", Council Chair, "),
                               str_remove(title, ".*, Council Chair, "),
                               org_name),
             org_name = ifelse(str_dct(title, "Director, "),
                               str_remove(title, ".* Director, "),
                               org_name),
             org_name = ifelse(str_dct(title, "Commissioner, "),
                               str_remove(title, ".*Commissioner, "),
                               org_name),
             org_name = ifelse(str_dct(title, "Manager, "),
                               str_remove(title, ".*Manager, "),
                               org_name),
             org_name = ifelse(str_dct(title, "City of "),
                               str_remove(title, ".*City of "),
                               org_name),
             org_name = ifelse(str_dct(title, "Treasurer, "),
                               str_remove(title, ".*Treasurer, "),
                               org_name),
             org_name = ifelse(str_dct(title, "Consultant, "),
                               str_remove(title, ".*Consultant, "),
                               org_name),
             org_name = ifelse(str_dct(title, "Attorney General, "),
                               str_remove(title, ".*Attorney General, "),
                               org_name),
             org_name = ifelse(str_dct(title, "Engineer, "),
                               str_remove(title, ".*Engineer, "),
                               org_name),
             org_name = ifelse(str_dct(title, "Judge, "),
                               str_remove(title, ".*Judge, "),
                               org_name),
             org_name = ifelse(str_dct(title, "CEO,  "),
                               str_remove(title, ".*CEO, "),
                               org_name),
             org_name = ifelse(str_dct(title, ", Secretary, "),
                               str_remove(title, ".*, Secretary, "),
                               org_name),
             org_name = ifelse(str_dct(title, "Attorney, "),
                               str_remove(title, ".*Attorney, "),
                               org_name)
             )

d$title %<>% str_remove("Comment submitted by ")

d$org_name%<>% str_squish()

# filter down to org comments
d %<>% 
  group_by(docket_id, org_name) %>% 
  add_count(name = "org_total") %>% 
  ungroup() %>%
  arrange(-number_of_comments_received) %>% 
  filter(attachment_count > 0,
         str_detect(str_c(title, org_name), "Congress|Senat|Rep\\.|Sen\\.|Representative") |
         # individual names
         !str_detect(org_name, "^.\\.$|^.\\. |^\\w+ .\\.|illegible|no surname"),
         !str_detect(title, "illegible|surname|last name|forename|no name"),
         nchar(org_name) > 1) %>% 
  mutate(org_name = org_name %>% replace_na("NA") ) %>% 
  filter(number_of_comments_received > 99 | !org_name %in% c("NA", "na", "")) %>% 
  add_count(docket_id, name = "org_comments")

d %>% slice_max(org_comments) %>% count(org_name,title, sort = T) %>% 
  slice_sample(n = 100) %>% knitr::kable()# %>% pull(org_name) 

d %>% 
  #filter(n > 10, n < 20) %>% 
  count(docket_id, sort = T) %>% knitr::kable()


d %<>%  filter(org_comments < 1000)

## AUGMENT FUNCTION
# ad document name and link
d %<>% 
  mutate(file_1 = ifelse(attachment_count > 0,  
                         str_c(document_id, "-1.pdf"), 
                         NA),
         attachment_txt = ifelse(attachment_count > 0,  
                      str_c("https://ssc.wisc.edu/~judgelord/comment_text/",
                            document_id %>% str_remove("-.*$"), # agency folder
                            "/",
                            document_id %>% str_remove("-[A-z1-9]*$"), # docket folder
                            "/",
                            document_id,
                            "-1.txt"), 
                      NA),
         comment_url = str_c("https://www.regulations.gov/comment/",
                             document_id),
         docket_url = str_c("https://www.regulations.gov/docket/",
                            document_id %>% str_remove("-[0-9]*$")))

d$attachment_txt[1]
d$comment_url[1]
d$docket_url[1]

d %<>% rename(comment_title = title)
names(d)
## PREP SHEETS
d %<>% select(agency_acronym, 
              docket_id, 
              docket_url,
              docket_title, 
              document_id, 
              comment_url, 
              comment_text,
              attachment_txt,
              organization, 
              comment_title,
              attachment_count, 
              number_of_comments_received,
              org_name)

# add blanks
d %<>% mutate(position = "",
              position_certainty = "",
              comment_type = "",
              coalition_comment = "",
              coalition_type = "",
              # org_name = organization, # run scratchpad/orgnames.R until this is a function
              org_name_short = "",
              org_type = "",
              ask = "",
              ask1 = "",
              ask2 = "",
              ask3 = "",
              success = "",
              success_certainty = "",
              sucess1 = "",
              success2 = "",
              success3 = "",
              response = "",
              pressure_phrases = "",
              accept_phrases = "",
              compromise_phrases = "",
              reject_phrases = "",
              notes = "")

names(d)

# unique(d$organization)

count(d, organization, sort = T) %>% head()
count(d, org_name, sort = T) %>% head()

# create new directory if needed
if (!dir.exists(here::here("data", "datasheets") ) ){
  dir.create( here::here("data", "datasheets") )
}


write_comment_sheets <- function(docket){
  d %>% 
    filter(docket_id == docket) %>% 
    write_csv(file = here::here("data",
                                "datasheets",
                              #str_extract("^[A-Z]"), # agency  
                              str_c(docket, "_org_comments.csv")))
}


names(d)
d %<>% mutate(comment_type = ifelse(number_of_comments_received > 99, "mass", comment_type))

unique(d$docket_id)

walk(unique(d$docket_id), write_comment_sheets)



