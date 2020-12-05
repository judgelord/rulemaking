# Functions required to sumarize a rule with summarize_sections()

# read rule from xml
xml_rule_text <- . %>% 
  read_xml() %>% 
  xml_children() %>% 
  xml_children() %>%
  xml_text() %>% 
  str_squish() %>% 
  tibble::enframe(value = "text",  name = "id") %>% 
  mutate(id = str_pad(id, 4, pad = "0") )

# split out sections
section <- . %>% 
  mutate(section = text %>%
           str_extract("^SUMMARY:|^([A-Z]*\\.|[1-9]\\.).*") %>% 
           str_sub(1,80) ) %>% 
  tidyr::fill(section, .direction = "down") %>%
  # add ... to headers that were cut off--this could be done later
  mutate( section = ifelse( nchar(section) == 80, 
                           str_c(section, "..."), 
                           section) )

# clean up (must be done after sectioning because it removes section numbers)
clean <- . %>%  
  # drop short texts
  filter(nchar(text) > 60) %>% 
  # identify headings as lines with a period in the beginning (also captures some footnotes)
  mutate(part = ifelse(str_detect(str_sub(text, 1, 5) ,'\\.'), 'head', 'text')) %>% 
  # idetify footnotes with a number and no period
  mutate(part = ifelse(text %>% str_detect("^[0-9](?!\\.)"), 'footnote', part) ) %>% 
  mutate(text = text %>% 
           # CAUTION: removes all text in parentheses
           str_remove_all("\\(.*\\)|[0-9]|_") %>% 
           str_remove_all("§") %>%
           str_squish() %>% 
           str_replace_all("\\.([A-Z][a-z])", ". \\1") %>%
           # remvove numbers and specials (keep only text and basic punctuation)
           str_replace_all("[^([A-z]& &'&\\.&\\,&\\?&\\!&\\;&\\;)]", " ") %>% 
           str_replace_all(" \\.", ". ") %>%
           # double commas  
           str_replace_all("(\\, \\,|\\,\\,|\\,\\.) ", ", ") %>% 
           # double periods 
           str_replace_all("(\\. \\.|\\.\\.|\\.\\,) ", ". ") %>% 
           str_squish() %>%
           # one character after a period 
           str_replace_all("\\. .\\. ", ". ") %>% 
           # remove white space
           str_replace_all(" \\,", ", ") %>%
           str_replace_all(" \\.", ". ") %>%
           str_squish() # %>% clean_string() # optional
  ) %>%
  filter(text != "") #filter out blank strings 


## For testing, let the section be the second section and max sentences = 2
# sections = text$section[2]
# max_sentences = 2

summarizeText <- function(sections, text, max_sentences) {
  
  sentences <- text %>%
    # slect section provided in function input
    filter(section == sections) %>%
    select(text) %>%
    unnest_sentences(output = sentences, input = text) %>%
    distinct() %>%
    mutate(textrank_id = row_number()) %>% 
    # textrank requires columns in order
    select(textrank_id, sentences)
  
  # select max sentences to summarize per section
  sentences %<>% filter(textrank_id <= max_sentences)
  
  # textrank needs a dictionary of words
  words <- unnest_tokens(sentences, output = word, input = 'sentences') %>% 
    distinct() %>% 
    anti_join(tidytext::stop_words)
  
  # textRank fails if you feed it only one sentence
  if(nrow(sentences) > 1){
    out <- textrank::textrank_sentences(data = sentences,
                                        terminology = words)
    # so we format the unranked sentences data frame as an alternative
  } else {
    out <- list(sentences = as.data.frame(sentences))
  }
  
  return(out)
  
}

summarize_sections <- function(text, n_sentences = 2, max_sentences = 100) {
  
  # section and clean text 
  text %<>% section() %>% clean() %>% 
    # drop headers and footnotes
    filter(!is.na(section), part == "text") %>%
    #FIXME make max_sentences depend on section length
    # group_by(section) %>% 
    add_count(section, name = "section_length") 
  
  # summarize, map summary function to each unique section
  text_summary <-  map(.x = text$section %>% unique() ,
                       .f = summarizeText, 
                       text = text, 
                       max_sentences = max_sentences) 
  
  # extract top n_sentences
  pull_sentences <- . %>% 
    .$sentences %>% 
    pull(sentence) %>% 
    .[1:n_sentences] %>% 
    str_to_sentence() %>%  
    str_c(collapse = " ")
  
  # make a data frame of section headers and n_sentences pulled from textrank output
  section_summary <- tibble(
    section = pull(text, section) %>% unique(), 
    summary = text_summary %>%
      #FIXME when this fails, it should default to the first sentence of the section. 
      map_chr(possibly(pull_sentences, otherwise = " ") ) ) 
  
  return(section_summary)
}