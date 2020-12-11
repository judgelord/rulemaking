
source("setup.R")
load("rules_metadata.Rdata")
names(rules)
load("comment_metadata.Rdata")
names(comments_all)

topdockets <- rules %>% 
  ungroup() %>% 
  filter(docket_type == "Rulemaking",
         number_of_comments_received > 0) %>% 
  group_by(agency_acronym) %>%
  slice_max(order_by = number_of_comments_received,
            n = 1,
            with_ties =F)

topdockets$number_of_comments_received %>% head()

dim(topdockets)

topdockets %>% count(number_of_comments_received)


d <- comments_all %>% filter(docket_id %in% topdockets$docket_id)
dim(d)



# filter to mass dockets
d %<>% group_by(docket_id) %>% 
  # mass dockets
  mutate(number_of_comments_on_docket = sum(number_of_comments_received),
         max = max(number_of_comments_received) ) %>% 
  ungroup() %>% 
  filter(max > 99 | number_of_comments_on_docket > 999)
dim(d)

# apply auto-coding 
#FIXME with updated org_names from hand-coding 
source(here::here("code", "org_name.R"))
source(here::here("code", "comment_position.R"))

# filter down to org comments
d %<>% 
  group_by(docket_id, org_name) %>% 
  add_count(name = "org_total") %>% 
  ungroup() %>%
  arrange(-number_of_comments_received) %>% 
  filter(attachment_count > 0) %>% 
  filter(!org_name %in% c("NA", "na", "Organization Unknown 1", "Organization Unknown 2", "Organization Unknown 3"),
         !is.na(org_name),
         #FIXME
         org_total < 5) %>% 
  add_count(docket_id)

d %>% 
  #filter(n > 10, n < 20) %>% 
  count(docket_id, sort = T) %>% knitr::kable()




## AUGMENT FUNCTION
# ad document name and link
d %<>% 
  mutate(file_1 = ifelse(attachment_count > 0,  
                         str_c(document_id, "-1.pdf"), 
                         NA),
         attachment_txt = ifelse(attachment_count > 0,  
                      str_c("https://ssc.wisc.edu/~judgelord/comment_text/",
                            document_id %>% str_remove("-.*$"), # agency 
                            "/",
                            document_id %>% str_remove("-.*?$"), # docket
                            "/",
                            document_id,
                            ".txt"), 
                      NA),
         comment_url = str_c("https://www.regulations.gov/contentStreamer?documentId=",
                             document_id),
         proposed_url = NA,
         final_url = NA)

d %<>% rename(comment_title = title)
names(d)
## PREP SHEETS
d %<>% select(agency_acronym, 
              docket_id, 
              docket_title, 
              document_id, 
              #proposed_url, #FIXME 
              #final_url,
              comment_url, 
              comment_text,
              attachment_txt,
              organization, 
              comment_title,
              attachment_count, 
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


# create new directory if needed
if (!dir.exists(here::here("data", "datasheets") ) ){
  dir.create( here::here("data", "datasheets") )
}


write_comment_sheets <- function(docket){
  d %>% 
    filter(docket_id == docket) %>% 
    write_csv(path = here::here("data",
                                "datasheets",
                              #str_extract("^[A-Z]"), # agency  
                              str_c(docket, "_org_comments.csv")))
}

unique(d$docket_id)

walk(unique(d$docket_id), write_comment_sheets)

