
source("setup.R")
source("setup.R")

load(here::here("data", "native_org_comments.rdata"))

d <- native_org_comments

# clean up title 
d$title %<>% str_remove("Comment submitted by |Comment from |Comments from |Comment on |Comments|Submitted Electronically via eRulemaking Portal")

## AUGMENT FUNCTION
# ad document name and link
d %<>% 
  mutate(comment_url = str_c("https://www.regulations.gov/comment/",
                             document_id),
         docket_url = str_c("https://www.regulations.gov/docket/",
                            document_id %>% str_remove("-[0-9]*$")))

d$comment_url[1]
d$docket_url[1]

d %<>% rename(comment_title = title)
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
              org_name = Name,
              org_source = Source,
              org_website = Website,
              org_type = Type,
              org_emphasis = Emphasis,
              org_notes = Notes)

# correct 
d %<>% mutate(org_type = ifelse(str_dct(org_type, "Tribe"),
                                paste0("gov;", org_type),
                                paste0("ngo;", org_type)))


# add blanks
d %<>% mutate(org_name_short = "",
              position = "",
              position_certainty = "",
              comment_type = "org",
              coalition_comment = "",
              coalition_type = "",
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

d %<>% mutate(comment_type = ifelse(number_of_comments_received > 99, "mass", comment_type))

count(d, comment_type, sort = T) %>% head()

names(d)

# unique(d$organization)

count(d, docket_id, sort = T) %>% head()
count(d, organization, sort = T) %>% head()
count(d, org_name, sort = T) %>% head()

# create new directory if needed
if (!dir.exists(here::here("data", "datasheets") ) ){
  dir.create( here::here("data", "datasheets") )
}


d %<>% arrange(document_id)

write_csv(d, 
          file = here::here("data",
                            "datasheets",
                            "native_org_comments.csv"))



# how many dockets
unique(d$docket_id)
unique(d$agency)

sheet_write(d, "1wVvmKZ8KzY7LMP_cAvls21nw7oE05NOeUVWVoBUA0Hs",
            sheet = paste(Sys.Date(), "version"))


d %>% 
  mutate(agency = str_remove_all(docket_id, "-.*|_.*")) %>% 
  count(agency) %>% 
  ggplot() +
  aes(x= n, y = reorder(agency, n) )+ 
  geom_col() +
  labs(y = "",
       x = "Comments to Agency 2005-2020")

ggsave("native-comments-by-agency.png", width = 4, height = 9)

d %>% 
  mutate(agency = str_remove_all(docket_id, "-.*|_.*")) %>% 
  distinct(agency, docket_id) %>% 
  count(agency) %>% 
  ggplot() +
  aes(x= n, y = reorder(agency, n) )+ 
  geom_col() +
  labs(y = "",
       x = "Rules, 2005-2020")

ggsave("rules-by-agency.png", width = 4, height = 9)
