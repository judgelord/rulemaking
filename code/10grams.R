# rewriting old code for 10-gram window matching 

library(tidyverse)
library(magrittr)
library(tidytext)

#FIXME a new function to be applied to draft text as well
clean_string <- . %>% 
  str_c(collapse = " ") %>% 
  # make sure sentences are ended
  str_replace_all("\\t|\\\n", " ") %>% 
  str_replace_all("\\.([A-Z][a-z])", ". \\1") %>%
  # remvove numbers and specials (keep only text and basic punctuation)
  str_replace_all("[^[A-z]&^ &\\.&\\,&\\?&\\!&\\;&\\;]", " ") %>% 
  str_replace_all(" \\. ", ". ") %>%
  #str_replace_all("\\. .\\. ", ". ") %>% 
  # remove white space
  str_squish() 
  

# 1. Clean up text 
d$text %>% clean_string

# 2. Remove NPRM 10 grams
library(xml2)
xml_rule_text <- . %>% 
  read_xml() %>% 
  xml_children() %>% 
  xml_children() %>% 
  xml_text() %>% 
  str_squish() %>% 
  tibble::enframe(value = "text",  name = "id") 

pr <- xml_rule_text("https://www.federalregister.gov/documents/full_text/xml/2018/08/15/2018-17386.xml")

pr_text <- pr %>% summarise(text = text %>% clean_string() ) # fr_doc_id = 

# tengrams
#FIXME function 
tengram <- . %>% unnest_tokens(tengram, text, token = "ngrams", n = 10) %>% filter(!is.na(tengram))

pr_text %<>% tengram()

d %<>% tengram()

# remove pr tengrams
d %<>% 
  rownames_to_column() %>% 
  filter(!tengram %in% pr$tengram,
         # limit 10k wordds 
         as.numeric(rowname) < 10000) 

# 3. Save clean (first 10k?)

# 4. list with a logical vector for each comment with > x tengrams

#  two LWV letters 
d1 <- 
  read_table(here::here("comments", "NPS", dockets[1], comments[40]), col_names = "text") %>%
  mutate(text = clean_string(text)) %>%
  tengram()

d2 <- 
  read_table(here::here("comments", "NPS", dockets[1], comments[29]), col_names = "text") %>%
  mutate(text = clean_string(text)) %>%
  tengram() 


#FIXME meta function
read_grams <- . %>% {here::here("comments", .)} %>% 
  read_table(col_names = "text") %>%
  mutate(text = clean_string(text)) %>%
  tengram() %>% 
  as.list()


d <- tibble(files = list.files(here::here("comments"), recursive = T) ) %>% 
  filter(str_detect(files, "txt"))

d$files[5] %>% read_grams()


d %<>% 
  head() %>% 
  mutate(tengrams = files %>% map(possibly(read_grams, "error")) ) %>% 
  filter(tengrams != "error")


d$tengrams[1]


match <- function(text1, text2){
  unlist(text1) %in% unlist(text2)
}

# with this function, we can see which tengrams from text 1 are in text 2
match(d$tengrams[1], d$tengrams[2])

# if we do row-wise, then we only compare each document to itself 
map2(d$tengrams, d$tengrams, match)

# map one set to each other set of tengrams
map2(d$tengrams[1], d$tengrams, match) %>% enframe() %>% mutate(name = d$files) 

# a function to do this to each 
match2 <- . %>% map2(d$tengrams, match)

# map each document to all others
d %<>% 
  mutate(other_comment = list(d$files),
         other_commnet_match = tengrams %>% 
           map(match2) # %>% enframe() # %>% mutate(name = d$files) 
         )

# inspect
d
d$other_comment[1]
d$other_commnet_match[1]


