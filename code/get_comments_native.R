
source("setup.R")

# read from google sheet (requires authorization)
native_groups_raw <- read_sheet("1crWSexVnc1979ob5ql3_xATpF3K9f5Z7XpZ4QpaTe3Y")

# split out to one obs per moniker
native_groups <- native_groups_raw %>% 
  mutate(string = str_to_lower(`Text Strings`) %>% 
           str_split("\\|")
  ) %>% 
  unnest(string) %>% 
  mutate(string = str_squish(string)) %>% 
  distinct()

# inspect 
head(native_groups$string)

# custom clean function for these data 
clean <- . %>%  
  str_replace_all("-", "") %>% 
  str_replace_all("&", "and") %>%
  str_replace_all("confederated", "confederate")  %>% 
  str_replace_all("Pueblo de" ,"Pueblo of") %>% 
  str_replace_all("tribes", "tribe") %>% 
  str_replace_all("inter tribal","intertribal")

# apply function to strings to search for
native_groups$string %<>% clean()


# look at short strings 
native_groups %<>% 
  mutate(characters = nchar(string) )

native_groups %<>% filter(characters > 0)

native_groups %>%  arrange(characters) #%>% view()
# drop short strings

# drop duplicates 
native_groups %<>% distinct() %>% drop_na(string)

head(native_groups$string)

native_group_strings <- paste0(#"\b", 
  native_groups$string) %>% #, "\b") %>%
  paste0(collapse = "|")

#########
# load commentorg metadata 
load(here::here("data", "comment_metadata_orgs.rdata"))

# apply same function to data to search in 
orgs <- comment_metadata_orgs %>% 
  mutate(organization_clean = organization %>% 
           str_to_lower() %>% 
           str_squish() %>% 
           clean() %>%
           str_squish())

# drop strings that are shorter than any string we are searching for or are not orgs 
nonorgs <- "me, myself, and i|ladyfreethinker|not applicable|a youtuber|retired federal employee|the human race|the american people$|the people$|truck driver$"

orgs %<>% 
  filter(nchar(organization_clean) > 2,
         !str_detect(organization, nonorgs)) %>% 
  drop_na(organization_clean)
  
orgs %>% head(100)

# make a flag for commenting orgs in list of native orgs 
native_org_comments <- orgs %>% #head(10000) %>% 
  filter(str_detect(organization_clean, native_group_strings))

native_org_comments      

# identify matching substring
native_org_comments %<>% 
  mutate(string = str_extract(organization_clean, native_group_strings))

# join in metadata 
native_org_comments %<>% 
  left_join(native_groups)


# load comment metadata 
# load comment metadata 
load(here::here("data", "comment_metadata.rdata"))

# format for merging 
comment_metadata <- comments_all %>% 
  mutate(organization = str_to_lower(organization) %>% str_squish()) 

# join in metadata 
native_org_comments %<>% 
  left_join(comment_metadata)


# save 
save(native_org_comments,
     file = here::here("data", "native_org_comments.rdata"))

native_org_comments %>% add_count(Name, name = "n_comments") %>% distinct(organization, `Text Strings`, Name, n_comments ) %>% arrange(-n_comments) %>% #view
  write_csv(here::here("data", "native_org_matches.csv"))
        