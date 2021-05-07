
source("setup.R")



# comments 
load(here::here("data", "comment_metadata.Rdata"))
dim(comment_metadata)
ls()
names(comment_metadata)
names(comments_all)

d <- comment_metadata
d$posted_date %<>% as.Date()
max(d$posted_date, na.rm = T)
min(d$posted_date, na.rm = T)

d %<>% filter(number_of_comments_received > 99)


dim(d)



# filter to mass dockets
d %<>% group_by(docket_id) %>% 
  # mass dockets
  mutate(comments_on_docket = sum(number_of_comments_received), 
         max = max(number_of_comments_received) ) %>% 
  ungroup() %>% 
  filter(max > 99 | comments_on_docket > 999)
dim(d)

names(d)
d %<>% filter(attachment_count > 0,
              !str_detect(organization, "^.\\. |illegible|surname|last name|forename|no name|^unknown$"),
              !str_detect(title, "illegible|surname|last name|forename|no name") ) 
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

save(d, file = here::here("data", "comments4masssheet.Rdata"))
# load(here::here("data", "comments4datasheets.Rdata"))

# filter down to org comments
d %<>% 
  group_by(docket_id, org_name) %>% 
  add_count(name = "org_total") %>% 
  ungroup() %>%
  arrange(-number_of_comments_received) %>% 
  mutate(org_name = org_name %>% replace_na("NA") ) %>% 
  add_count(docket_id, name = "org_comments")


d %>% count(org_name, sort = T)

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

## PREP SHEETS
d %<>% select(#agency_acronym, 
  docket_id, 
  docket_url,
  #docket_title, 
  document_id, 
  posted_date,
  comment_url, 
  comment_text,
  #attachment_txt,
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

d %>% filter( organization  == "N/A") %>% distinct(org_name, number_of_comments_received)
d %>% filter(org_name == "unknown") %>% distinct(org_name, number_of_comments_received)

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





