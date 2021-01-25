summarize_comments <- function(text, n_sentences = 2, max_sentences = 100, custom_stop_words, agency_name) {
  
  
  # not cleaning data here either; should we? I think it is better to do it in the above function so it is done on one comment at a time.
  
  # summarize, map summary function to each unique section
  text_summary <-  map(.x = text,
                       .f = summarizeText,
                       max_sentences = max_sentences,
                       custom_stop_words = custom_stop_words,
                       agency_name = agency_name) 
  
  # extract top n_sentences
  pull_sentences <- . %>% 
    .$sentences %>% 
    pull(sentence) %>% 
    .[1:n_sentences] %>% 
    str_to_sentence() %>%  #FIXME should define a custom function to fix sentence case with some common fixes like:
    str_replace_all(" i ", " I ") %>% 
    str_replace_all(" u\\.s\\. ", " U.S. ") %>% 
    str_replace_all("nprm", "NPRM") %>% 
    str_c(collapse = " ")
  
  # add n_sentences pulled from textrank output to data
  summary <- text_summary %>%
    #FIXME when this fails, it should default to the first sentence of the section. 
    map_chr(possibly(pull_sentences, otherwise = " ") ) 
  
  return(summary)
}
