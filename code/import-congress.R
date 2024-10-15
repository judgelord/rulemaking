# congress
congress_raw <- read_sheet_c("1HBjG32qWVdf9YxfGPEJhNmSw65Z9XzPhHdDbLnc3mYc")

congress <- congress_raw %>% dplyr::select(any_of(c("docket_id", "comment_url", "comment_type", 
                                            "org_name", "org_name_short","org_type",
                                            #"transparent", 
                                            "coalition_comment", "coalition_type",	
                                            "position",	"position_certainty", #"success", 
                                            "docket_type"))) %>%   
  mutate(#comments = comments %>% str_squish() %>% as.numeric(),
    #success = success %>% str_squish()%>% as.numeric(),
    position = position %>% str_squish() %>% as.numeric() ) %>%
  distinct()



# split back out to one obs per comment
congress %<>% mutate(comment_url = str_split(comment_url, "\n") ) %>% unnest(comment_url)

congress %<>% unnest(comment_url)

# should be less than 100 or the split did not work
max(nchar(congress$comment_url), na.rm = T)
congress %>% filter(is.na(comment_url))

# get id from url 
congress %<>% mutate(document_id = str_remove_all(comment_url, ".*=|.*/") %>% str_squish()) %>% 
  # drop url for better merging 
  dplyr::select(-comment_url)

head(congress$document_id)
congress %>% filter(is.na(document_id))

# should be ~ 26
max(nchar(congress$document_id), na.rm = T)

congress %>% arrange(-nchar(document_id)) %>% dplyr::select(document_id)

# selected coded 
congress %<>% filter(!is.na(coalition_type) | !is.na(position) | !is.na(org_type))

# remove extra white space
congress %<>% mutate_all(str_squish)


# duplicates 
congress %>% 
  count(document_id, docket_id, sort = T) %>% 
  filter(n>1) %>% 
  group_by(docket_id) %>% 
  summarise(n = sum(n),
            document_id = str_c(document_id, collapse = '  ')) %>% 
  arrange(-n)


#FIXME this overwrites corrections, but the sheet collapses by org type, so corrections are not valid at id level
congress %<>% left_join(comments_min %>% distinct(id, number_of_comments_received),
                    by = c( "document_id" = "id"))

congress %<>% distinct()
# duplicates 
congress %>% 
  count(document_id, docket_id, sort = T) %>% 
  filter(n>1) %>% 
  group_by(docket_id) %>% 
  summarise(n = sum(n),
            document_id = str_c(document_id, collapse = '  ')) %>% 
  arrange(-n)

# remove extra white space and vonvert to chr
congress %<>% mutate_all(str_squish)

