# MASS
mass_raw <- read_sheet_c("1uM69A3MjX4-kHJSP6b5EZ33VKoNjefue3BqXDgLu0cY")

mass <- mass_raw %>% dplyr::select(any_of(c("docket_id", "comment_url", "comment_type", 
                                            "org_name", "org_name_short","org_type",
                                            "transparent", "coalition_comment", "coalition_type",	
                                            "position",	"position_certainty", #"success", 
                                            "docket_type"))) %>%   
  mutate(#comments = comments %>% str_squish() %>% as.numeric(),
    #success = success %>% str_squish()%>% as.numeric(),
    position = position %>% str_squish() %>% as.numeric(),
    comment_type = comment_type %>% replace_na("mass"),
    transparent = transparent %>% replace_na("")) %>%
  distinct()



# split back out to one obs per comment
mass %<>% mutate(comment_url = str_split(comment_url, "\n") ) %>% unnest(comment_url)

mass %<>% unnest(comment_url)

# should be less than 100 or the split did not work
max(nchar(mass$comment_url), na.rm = T)
mass %>% filter(is.na(comment_url))

# get id from url 
mass %<>% mutate(document_id = str_remove_all(comment_url, ".*=|.*/") %>% str_squish()) %>% 
  # drop url for better merging 
  dplyr::select(-comment_url)

head(mass$document_id)
mass %>% filter(is.na(document_id))

# should be ~ 26
max(nchar(mass$document_id), na.rm = T)

mass %>% arrange(-nchar(document_id)) %>% select(document_id)

# selected coded 
mass %<>% filter(!is.na(coalition_type) | !is.na(position) | !is.na(org_type))

# remove extra white space
mass %<>% mutate_all(str_squish)


# duplicates 
mass %>% 
  count(document_id, docket_id, sort = T) %>% 
  filter(n>1) %>% 
  group_by(docket_id) %>% 
  summarise(n = sum(n),
            document_id = str_c(document_id, collapse = '  ')) %>% 
  arrange(-n)


#FIXME this overwrites corrections, but the sheet collapses by org type, so corrections are not valid at id level
mass %<>% left_join(comments_min %>% distinct(id, number_of_comments_received),
                    by = c( "document_id" = "id"))

mass %<>% distinct()
# duplicates 
mass %>% 
  count(document_id, docket_id, sort = T) %>% 
  filter(n>1) %>% 
  group_by(docket_id) %>% 
  summarise(n = sum(n),
            document_id = str_c(document_id, collapse = '  ')) %>% 
  arrange(-n)

# remove extra white space and vonvert to chr
mass %<>% mutate_all(str_squish)