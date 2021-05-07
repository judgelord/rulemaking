source("setup.R")
load("data/rules_metadata.Rdata")
names(rules)
max(rules$posted_date, na.rm = T)

combine_strings <- . %>% unique() %>% str_c(collapse = ";;;")

rules %<>% filter(document_type %in% c("Proposed Rule", "Rule"))

rules %<>% mutate(document_id = id)

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
dockets_min <- d %>% select(docket_id, 
                            docket_type, 
                            number_of_comments_received, 
                            document_type,
                            posted_date,
                            comment_start_date,
                            comment_due_date)

save(dockets, file =  here::here("data", "dockets_metadata.Rdata") )
save(dockets_min, file =  here::here("data", "dockets_min.Rdata") )

# load(here::here("data", "dockets_metadata.Rdata"))

d  <- dockets

names(d)
dim(d)
  
# d %>% filter(str_detect(fr_number,";;;")) %>% select(fr_number)

d %<>% 
  ungroup() %>% 
  mutate(# add vars 
    proposed_url = "",
    final_url = "", 
    proposed_direction = "",
    final_direction = "",
    final_relative_direction = "",
    issue1 = "",
    issue2 = "",
    issue3 = "",
    coalitions = "") %>% 
  # order 
  select(agency_id,
         docket_type,
         number_of_comments_received,
         docket_id, 
         docket_title,
         # add vars 
         proposed_url,
         final_url, 
         proposed_direction,
         final_direction,
         final_relative_direction,
         issue1,
         issue2,
         issue3,
         coalitions,
         # combined rule vars just for extra info
         document_type,
         title,
         document_id,
         rin,
         fr_number,
         #fr_document_id = fr_number,
         posted_date,
         comment_start_date,
         comment_due_date, 
         attachment_count)

d %>% count(docket_type)

# d %<>% filter(docket_type == "Rulemaking") # %>% arrange(number_of_comments_received)

# write sheets
write_rule_sheets <- function(agency){
  d %>% 
    filter(agency_id == agency) %>% 
    write_csv(path = here::here("data",
                                "datasheets", 
                                str_c(agency, ".csv")))
}

unique(d$agency_id)

walk(unique(d$agency_id), write_rule_sheets)  

