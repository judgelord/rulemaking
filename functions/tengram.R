library(tidytext)

# a function to parse a text in to tengrams
tengram <- . %>% unnest_tokens(tengram, text, token = "ngrams", n = 10) 

# a meta function to read document, clean it, and parse it into tengrams
read_grams <- . %>% 
  read_table(col_names = "text") %>%
  mutate(text = clean_string(text)) %>%
  tengram() %>% 
  rownames_to_column() %>% 
  filter(
    # limit to 10k wordds 
    as.numeric(rowname) < 10000) %>% 
  select(tengram) %>% 
  as.list()

# matching
match <- function(text1, text2){
  unlist(text1) %in% unlist(text2)
}

# ####################################################################################################
# # NOTES ON USING MATCHING FUCTIONS
# # with this function, we can see which tengrams from text 1 are in text 2
# match(d$tengrams[1], d$tengrams[2])
# 
# # if we map row-wise, then we only compare each document to itself 
# map2(d$tengrams, d$tengrams, match)
# 
# # but when we map one set of ngrams to each other set of ngrams, we match from each to each
# map2(d$tengrams[3], d$tengrams, match) %>% enframe() %>% mutate(name = d$reg_dot_gov_document_id) 
# ####################################################################################################

# a function to do this to each 
match2 <- . %>% map2(d$tengrams, match) #FIXME  supply list of tengrams as a var?

# a function to caputure just one other match (in this case, the diff with the NPRM)
match_tibble <- function(text1, text2){
  tibble(text = unlist(text1), 
         match = unlist(text1) %in% unlist(text2)
  )
}

# a function to extract the first word of a text string
word1 <- . %>% mutate(word = str_extract(text, "\\w+") )

# a function to reassemble text from first word of each tengram
tengram_match_to_text <- function(d){
  d %<>% 
    # mark transitions from new to new or new to new text
    mutate(trans = ifelse(match & !lead(match) | !match & lead(match), T, F)) %>% 
    # if a match, the next 9 are also a match 
    mutate(fix =  reduce(map(1:9, ~ lag(trans, ., 0)), `+`)) %>%
    mutate(match = ifelse(fix>0 & match == F, T, match)) %>% 
    # add elipses after transition words 
    mutate(word = ifelse(trans, str_c(word, "..."), word) ) %>%
    select(word, match)
  return(d)
}

