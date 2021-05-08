
source("setup.R")



# comments 
load(here::here("data", "comment_metadata2020.Rdata"))
dim(comment_metadata)
ls()
names(comment_metadata)
names(comments_all)
dim(comments_all)

d <- comment_metadata 
d %<>% as_tibble()
d$posted_date %<>% as.Date()
max(d$posted_date, na.rm = T)
min(d$posted_date, na.rm = T)

d %<>% filter(number_of_comments_received > 9)


dim(d)

d %<>% mutate(agency_acronym = agency_id)
d %<>% mutate(document_id = id)

# apply auto-coding 
#FIXME with updated org_names from hand-coding 
source(here::here("code", "org_name.R"))
#FIXME source(here::here("code", "comment_position.R"))

save(d, file = here::here("data", "comments4masssheet.Rdata"))
# load(here::here("data", "comments4datasheets.Rdata"))
temp <- d
d <- temp
dim(d)
d %>% count(org_name, sort = T)

d %<>% mutate(org_name = ifelse(str_dct(title, "Chief,"),
                                str_remove(title, ".*Chief,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Member of Congress|Senat|Rep\\.|Sen\\.|House of Representatives"),
                                title,
                                org_name),
              org_name = ifelse(str_dct(title, ", Counsel,"),
                                str_remove(title, ".*, Counsel,"),
                                org_name),
              org_name = ifelse(str_dct(title, "President,"),
                                str_remove(title, ".*President,"),
                                org_name),
              org_name = ifelse(str_dct(title, ", Chairman,"),
                                str_remove(title, ".*, Chairman,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Chair,"),
                                str_remove(title, ".*Chair, "),
                                org_name),
              org_name = ifelse(str_dct(title, ", MPA,"),
                                str_remove(title, ".*, MPA,"),
                                org_name),
              org_name = ifelse(str_dct(title, ", Council Chair,"),
                                str_remove(title, ".*, Council Chair,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Director,"),
                                str_remove(title, ".*Director,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Commissioner,"),
                                str_remove(title, ".*Commissioner,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Manager,"),
                                str_remove(title, ".*Manager,"),
                                org_name),
              org_name = ifelse(str_dct(title, "City of"),
                                str_remove(title, ".*City of"),
                                org_name),
              org_name = ifelse(str_dct(title, "Treasurer,"),
                                str_remove(title, ".*Treasurer,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Consultant,"),
                                str_remove(title, ".*Consultant,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Attorney General,"),
                                str_remove(title, ".*Attorney General,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Engineer,"),
                                str_remove(title, ".*Engineer,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Judge,"),
                                str_remove(title, ".*Judge,"),
                                org_name),
              org_name = ifelse(str_dct(title, " CEO,"),
                                str_remove(title, ".*CEO,"),
                                org_name),
              org_name = ifelse(str_dct(title, ", Secretary,"),
                                str_remove(title, ".*, Secretary,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Attorney,"),
                                str_remove(title, ".*Attorney,"),
                                org_name),
              org_name = ifelse(str_dct(title, " \\(C.O\\),"),
                                str_remove(title, ".* \\(C.O\\),"),
                                org_name),
              org_name = ifelse(str_dct(title, " Chief Technology Officer,"),
                                str_remove(title, ".*Chief Technology Officer,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Governmental Affairs,"),
                                str_remove(title, ".*Governmental Affairs,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Mayor, "),
                                str_remove(title, ".*Mayor, "),
                                org_name),
              org_name = ifelse(str_dct(title, "Affairs,"),
                                str_remove(title, ".*Affairs,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Governor,"),
                                str_remove(title, ".*Governor,"),
                                org_name),
              org_name = ifelse(str_dct(title, " Operations,"),
                                str_remove(title, ".* Operations,"),
                                org_name),
              org_name = ifelse(str_dct(title, " Fellow,"),
                                str_remove(title, ".* Fellow,"),
                                org_name),
              org_name = ifelse(str_dct(title, "ember,"),
                                str_remove(title, ".*ember,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Board Member,"),
                                str_remove(title, ".*Board Member,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Chairman et al.,"),
                                str_remove(title, ".*Chairman et al.,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Officer,"),
                                str_remove(title, ".*Officer,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Secretary,"),
                                str_remove(title, ".*Secretary,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Chair of the Board,"),
                                str_remove(title, ".*Chair of the Board,"),
                                org_name)
)

d$title %<>% str_remove("Comment submitted by |Comment from |Comments from |Comment on |Comments|Submitted Electronically via eRulemaking Portal")

d$org_name%<>% str_squish()

d %<>% mutate(org_name = coalesce(org_name, title))

save(d, file = here::here("data", "comments4masssheet.Rdata"))
# load(here::here("data", "comments4datasheets.Rdata"))


d %>% count(org_name, sort = T)

d %>% count(is.na(org_name))



# random sample 
d %>% distinct(org_name,title) %>% slice_sample(n = 100) %>% knitr::kable()

d %>% distinct(docket_id, org_comments)%>% knitr::kable()

## AUGMENT FUNCTION
# ad document name and link
d %<>% 
  mutate(comment_url = str_c("https://www.regulations.gov/comment/",
                             document_id))

d$comment_url[1]

d %<>% mutate(comment_title = title)
names(d)

d %<>% 
  group_by(docket_id, org_name) %>% 
  summarise(comment_url = str_c(comment_url, collapse = " \n "),
            document_id = str_c(comment_url, collapse = " \n "),
            comments = sum(number_of_comments_received))

d %<>% arrange(-comments)

head(d)


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
dim(d)

# unique(d$organization)

count(d, organization, sort = T) %>% head()
count(d, org_name, sort = T) %>% head()

write_csv(d ,file = here::here("data","datasheets", "mass.csv"))



