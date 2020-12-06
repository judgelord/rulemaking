agency <- "CFPB"
docket <- "CFPB-2016-0025"
nprm <- "https://www.federalregister.gov/documents/full_text/xml/2016/07/22/2016-13490.xml"
# get txt file names from a directory, here called “comment_text”
comments <- tibble( path = list.files( here::here('comment_text', agency, docket), 
                                       full.names = T) )

# filter to file paths ending in txt
d <- comments %>%
  filter( str_detect(path, "txt")) %>%
  # in SQL, CFPB file names are regs_dot_gov_document_id, shortened to document_id for now
  mutate( document_id = path %>%
            str_remove(".*/")  %>%
            str_remove("\\..*") 
   ) #%>%
  # group_by(document_id, path)


source(here::here("functions", "clean_string.R"))


read_comment <- . %>%
  read_lines() #%>%
  #clean_string() 

read_comment(d$path[1]) 

d %<>% 
  head() %>% 
  mutate(text = path %>% map(read_comment)) %>%
  modify(as.character)
  
d$text[1]

