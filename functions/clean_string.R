# a function for cleaner, more comparable strings
clean_string <- . %>% 
  str_c(collapse = " ") %>% 
  # make sure sentences are ended
  str_replace_all("\\t|\\\n", " ") %>% 
  str_replace_all("\\.([A-Z][a-z])", ". \\1") %>%
  # remvove numbers and specials (keep only text and basic punctuation)
  str_replace_all("[^[A-z]&^ &\\.&\\,&\\?&\\!&\\;&\\;]", " ") %>% 
  str_replace_all(" \\.", ". ") %>%
  # double commas  
  str_replace_all("(\\, \\,) ", ", ") %>% 
  # double periods 
  str_replace_all("(\\. \\.) ", ". ") %>% 
  # one character after a period 
  str_replace_all("\\. .\\. ", ". ") %>% 
  # remove white space
  str_replace_all(" \\,", ". ") %>%
  str_replace_all(" \\.", ". ") %>%
  str_squish() 
