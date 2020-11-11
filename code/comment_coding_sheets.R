
source("setup.R")
load("data/comment_metadata_CFPB.Rdata")
d <- comments_cfpb

d %<>% select(agency_acronym, 
              docket_id, 
              docket_title, 
              document_id, 
              comment_url, 
              organization, 
              comment_title,
              attachment_count)

d %<>% group_by(docket_id) %>% 
  #FIXME get this from data
  add_count(name = "number_of_comments_received") %>% 
  group_by(docket_id, organization) %>% 
  add_count(name = "org_total") %>% 
  ungroup() %>%
  arrange(-number_of_comments_received) %>% 
  filter(attachment_count > 0) %>% 
  filter(!organization %in% c("NA", "na", "Organization Unknown 1"),
         !is.na(organization),
         #FIXME
         org_total < 3) %>% 
  add_count(docket_id)

d %>% filter(n > 10, n < 20) %>% count(docket_id)
d$attachment_count

# add blanks
d %<>% mutate(position = "",
              position_certainty = "",
              comment_type = "",
              coalition_comment = "",
              coalition_type = "",
              org_name = "",
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
              agree_phrases = "",
              compromise_phrases = "",
              disagree_phrases = "",
              notes = "")

d %<>% select(-number_of_comments_received, -org_total)

unique(d$organization)

#FIXME 
write_csv(d, path = here::here("data/CFPB_org_comments.csv"))

saverules <- function(rule){
  d %>% 
    filter(docket_id == rule) %>% 
    write_csv(path = here::here("data",
                                "datasheets",
                              #str_sub(rule, 1, 4), 
                              str_c(rule, "_org_comments.csv")))
}

unique(d$docket_id)

walk(unique(d$docket_id), saverules)
