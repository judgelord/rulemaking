# Load packages and functions 
source("setup.R")
library("googledrive")
library("googlesheets4")

gs4_auth(email = "devin.jl@gmail.com")
drive_auth(email = "devin.jl@gmail.com")

# read google sheet
# ss <- drive_find(pattern = "finreg orgs", type = "spreadsheet")
# better
ss <- "1IV0V9C6hHlv7cQ9SGQFx1b06xbVMsich6wT9ncK6bhk"

library(googlesheets4)
known_orgs <- read_sheet(ss)

d <- tibble(file = list.files(here('comment_text'), recursive = T))

d %<>% mutate(comment_id = file %>% str_remove(".*/") %>% str_remove("-[0-9]*.txt"),
              docket_id = comment_id %>% str_remove("[0-9]*$"),
              agency = str_remove(docket_id, "-.*"))

d %<>% filter(agency %in% c("CFPB"))

# init/test 
file <- d$file[1]
orgs <- known_orgs$org_name %>% str_c("\\b")

orgs <- str_c("\\b", orgs)

detect_orgs <- function(file, orgs){
  text <- read_lines(here('comment_text', file)) %>% str_c(collapse = " ") %>% str_squish()
  
  found <- orgs[map2_lgl(text, orgs, str_dct)]
  
  return(found)
}

detect_orgs(d$file[1:2], orgs)

map(d$file[1:2], detect_orgs, orgs = orgs)

d %>% head() %>% 
  mutate(org_name = map(.x = file, .f = detect_orgs, orgs = orgs)) %>% 
  unnest(org_name) %>% 
  group_by(org_name) %>% 
  summarise(comment_id = comment_id %>% str_c(collapse = ";"))

d %<>% # head() %>% 
  mutate(org_name = map(.x = file, .f = detect_orgs, orgs = orgs)) %>% 
  unnest(org_name) %>% 
  group_by(org_name) %>% 
  summarise(comment_id = comment_id %>% str_c(collapse = ";")) %>% 
  arrange(org_name)
  

sheet_write(d, ss, sheet = "extracted")

# without cleaning org name
load(here("data", "orgs_all_cfpb.Rdata"))
d %<>% mutate(number_of_dockets = str_count(docket_id, ";;;"))
d %<>% arrange(-number_of_dockets)
d %<>% select(org_name, organization, comment_title, number_of_dockets, everything())
dim(d)
head(d)
sheet_write(d %>% filter(number_of_dockets>2) %>% select(-comment_text),
            ss, sheet = "metadata")


# with cleaned org name 
load(here("data", "orgs_all_cfpb2.Rdata"))

d %<>% mutate(number_of_dockets = str_count(docket_id, ";;;"))
d %<>% arrange(-number_of_dockets)
head(d)
d$docket_id[1]

d %<>% select(org_name, organization, comment_title, number_of_dockets, everything())

sheet_write(d %>% filter(number_of_dockets>2) %>% select(-comment_text), ss, sheet = "metadata_clean-ish")
