source("setup.R")
load("rules_metadata.Rdata")
names(rules)

combine_strings <- . %>% unique() %>% str_c(collapse = ";;;")

d <- rules %>% 
  #head(100) %>% 
  group_by(docket_id) %>% 
  # sum up numeric vars 
  mutate(attachment_count = sum(attachment_count),
         number_of_comments_received = sum(number_of_comments_received) ) %>% 
  # past unique values of string vars
  summarise_all(combine_strings) %>% 
  distinct() %>% 
  ungroup() 

dockets <- d 

save(dockets, here::here("data", "dockets_metadata.Rdata") )
  
# d %>% filter(str_detect(fr_number,";;;")) %>% select(fr_number)

d %<>% 
  ungroup() %>% 
  select(agency_acronym,
         docket_id, 
         # add vars 
         proposed_url = "",
         final_url = "", 
         proposed_direction = "",
         final_direction = "",
         final_relative_direction = "",
         issue1 = "",
         issue2 = "",
         issue3 = "",
         # combined rule vars just for extra info
         title,
         document_id,
         rin,
         fr_number,
         fr_document_id,
         posted_date,
         comment_start_date,
         comment_due_date, 
         attachment_count,
         number_of_comments_received)

d %>% count(docket_type)

d %<>% 
  filter(docket_type == "Rulemaking") # %>% arrange(number_of_comments_received)
  

