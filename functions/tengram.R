library(tidytext)

# a function to parse a text in to tengrams
tengram <- . %>% unnest_tokens(tengram, text, token = "ngrams", n = 10) 

# a meta function to read document, clean it, and parse it into tengrams
read_grams <- . %>% 
  read_table(col_names = "text") %>%
  summarise(text = clean_string(text)) %>%
  distinct() %>% 
  tengram() %>% 
  rownames_to_column() %>% 
  filter(as.numeric(rowname) < 10000) %>% 
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

# to do this to each 
match2 <- . %>% map2(d$tengrams, match) #FIXME  supply list of tengrams as a var?

# a function to caputure just one other match (in this case, the diff with the NPRM)
match_tibble <- function(text1, text2){
  tibble(ngram = unlist(text1), 
         match = unlist(text1) %in% unlist(text2)
  )
}

# a function to extract the first word of a text string
word1 <- . %>% mutate(word = str_extract(text, "\\w+") )

# a function to convert a vector of tengram matches to word matches
tengram_match <- function(match){
  temp <- tibble(match = match) %>% 
  # mark transitions from new to new or new to new text
  mutate(trans = ifelse(match & !lead(match) | !match & lead(match), T, F)) %>% 
  # if a match, the next 9 are also a match 
  mutate(fix =  reduce( map(1:9, ~ lag(trans, ., 0)), `+`)) %>%
  mutate(match = ifelse(fix > 0 & match == F, T, match) ) 
  
  return(temp$match)
}

# a function to reassemble text from first word of each tengram
tengram_match_dfr <- . %>% 
  # mark transitions from new to new or new to new text
  mutate(trans = ifelse(match & !lead(match) | !match & lead(match), T, F)) %>% 
  # if a match, the next 9 are also a match 
  mutate(fix =  reduce( map(1:9, ~ lag(trans, ., 0)), `+`)) %>%
  mutate(match = ifelse(fix > 0 & match == F, T, match) ) %>% 
  # remove vars we no longer need
  select(-trans, -fix) 

# for applying to nested data 
tengram_match_to_word <- . %>% unnest(match) %>% tengram_match()  %>% nest(match = c(match))

# a function to reassemble text from first word of each tengram
tengram_match_to_text <- . %>% 
  # mark transitions from new to new or new to new text
  mutate(trans = ifelse(match & !lead(match) | !match & lead(match), T, F)) %>% 
  # if a match, the next 9 are also a match 
  mutate(fix =  reduce( map(1:9, ~ lag(trans, ., 0)), `+`)) %>%
  mutate(match = ifelse(fix > 0 & match == F, T, match)) %>% 
  # add elipses after transition words 
  mutate(word = ifelse(fix > 0 & lead(fix) == 0, str_c(word, "..."), word) ) %>%
  # remove vars we no longer need
  select(-trans, -fix) 



