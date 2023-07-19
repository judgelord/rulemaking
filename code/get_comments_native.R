
source("setup.R")

gs4_auth(email = "devin.jl@gmail.com")

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
  str_to_lower() %>% 
  str_replace_all("-", "") %>% 
  str_replace_all("&", "and") %>%
  str_replace_all("confederated", "confederate")  %>% 
  str_replace_all("pueblo de" ,"pueblo of") %>% 
  str_replace_all("tribes", "tribe") %>% 
  str_replace_all("inter tribal","intertribal")

# apply function to strings to search for
native_groups$string %<>% clean()

# inspect 
head(native_groups$string)

# look at short strings 
native_groups %<>% 
  mutate(characters = nchar(string) )

native_groups %<>% filter(characters > 0)

native_groups %>%  arrange(characters) #%>% view()
# drop short strings

# drop duplicates 
native_groups %<>% distinct() %>% drop_na(string)

head(native_groups$string)

native_group_strings <- paste0("\\b", 
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
nonorgs <- "me, myself, and i|not applicable|a youtuber|retired federal employee|the human race|the american people$|the people$|truck driver$|^human race|^attorney at law"

orgs %<>% 
  filter(nchar(organization_clean) > 2,
         !str_dct(organization, nonorgs)) %>% 
  drop_na(organization_clean)
  
orgs %>% head(100)

# make a flag for commenting orgs in list of native orgs 
native_org_comments <- orgs %>% #head(10000) %>% 
  filter(str_dct(organization_clean, native_group_strings))

native_org_comments      

# identify matching substring
native_org_comments %<>% 
  mutate(string = str_extract(organization_clean, native_group_strings))

# make sure strings have a unique match
native_groups %>% count(string, sort = T)

# join in metadata 
native_org_comments %<>% 
  left_join(native_groups, relationship = "many-to-many")


# load comment metadata 
if(!exists("comments_all")){
load(here::here("data", "comment_metadata.rdata"))
}

# format for merging 
comment_metadata <- comments_all %>% 
  mutate(organization = str_to_lower(organization) %>% str_squish()) 

# join in metadata 
native_org_comments %<>% 
  left_join(comment_metadata)


# save comment data
save(native_org_comments,
     file = here::here("data", "native_org_comments.rdata"))

native_org_comments %>% arrange(docket_id) %>%
  sheet_write("1HqI6MSMxCeMdcmrhivFlSmzeazpD-ioTjWcoa3GH4Fk",
              sheet = "matched_comments")

# create a list of matched orgs, with a count of comments per organization
native_org_commenters <- native_org_comments %>% add_count(Name, name = "n_comments") %>% distinct(organization, `Text Strings`, Name, n_comments ) %>% arrange(-n_comments) 

# Save list of matched orgs
native_org_commenters %>% #view
  write_csv(here::here("data", "native_org_matches.csv"))

native_org_commenters %>%
sheet_write("1HqI6MSMxCeMdcmrhivFlSmzeazpD-ioTjWcoa3GH4Fk",
            sheet = "matched_orgs")


########################
# OTHER CANDIDATE ORGS #
########################
search <- "tribe|tribal|reservation|rancheria|band of|indian(s| )|indigenous|\bnative|confed|pueblo|apache|mowhawk|ponca trib"

unmatched <- orgs %>% filter(str_dct(organization_clean, search),
                         !organization_clean %in% native_org_comments$organization_clean)%>% 
  distinct(organization_clean)

write_csv(unmatched, file = here::here("data", "native_org_not_matched.csv") )

sheet_write(unmatched, "1HqI6MSMxCeMdcmrhivFlSmzeazpD-ioTjWcoa3GH4Fk",
            sheet = "unmatched")


###################################
# FROM IRS 501(c) NONPROFITS DATA #
###################################
if(!exists("nonprofit_resources")){
load("~/Dropbox/FINREGRULEMAKE2/finreg/data/nonprofit_resources_clean.Rdata")
}

# clean 501(c) name in the same way we cleaned the list of native groups 
nonprofit_resources %<>% 
  mutate(organization_clean = name %>% 
           str_to_lower() %>% 
           str_squish() %>% 
           clean() %>%
           str_squish()) 

# subset to nonprofits that match a native group 
matched_ngos <- nonprofit_resources %>% 
  filter(str_dct(organization_clean, native_group_strings)) %>% 
  distinct(organization_clean)

# write matched ngos to google sheet
sheet_write(matched_ngos, "1H3n8kchClaeFoznD9QJadNKi5FSDvERziQiobVNLG0I",
            sheet = "matched_ngos")

# subset to nonprofits that DID NOT match a native group, but fit broader search pattern
unmatched_ngos <- nonprofit_resources %>% 
  filter(str_dct(organization_clean, search),
         !organization_clean %in% matched_ngos$organization_clean) %>% 
  distinct(organization_clean)

# write unmatched ngos to google sheet
sheet_write(unmatched_ngos, "1H3n8kchClaeFoznD9QJadNKi5FSDvERziQiobVNLG0I",
            sheet = "unmatched_ngos")

