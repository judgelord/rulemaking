summarizeText <- function(text, max_sentences, custom_stop_words, agency_name) {
  
  sentences <- tibble(text = text) %>%
    unnest_sentences(output = sentences, input = text) %>%
    distinct() %>%
    mutate(textrank_id = row_number()) %>% 
    # textrank requires columns in order
    select(textrank_id, sentences)
  
  #remove agency name string from textrank dictionary
  sentences %<>%
    mutate(sentences = str_remove_all(sentences, agency_name))
  
  # select max sentences to summarize per section
  sentences %<>% filter(textrank_id <= max_sentences)
  
  
  # textrank needs a dictionary of words
  words <- unnest_tokens(sentences, output = word, input = 'sentences') %>% 
    distinct() %>% 
    anti_join(tidytext::stop_words) %>% 
    anti_join(custom_stop_words) # remove custom stop words from textrank
  
  # inspect 
  count(words, word, sort = T) %>% filter(n>2) %>% pull(word)
  
  # textRank fails if you feed it only one sentence
  if(nrow(sentences) > 1){
    out <- textrank::textrank_sentences(data = sentences,
                                        terminology = words)
    
    # arrange by textrank 
    out$sentences %<>% arrange(-textrank)
    
    # so we format the unranked sentences data frame as an alternative
  } else {
    out <- list(sentences = as.data.frame(sentences))
  }
  
  #out %>%   knitr::kable() 
  
  return(out)
  
}