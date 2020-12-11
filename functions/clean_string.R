# a function for cleaner, more comparable strings
clean_string <- . %>% 
  str_c(collapse = " ") %>% 
  str_remove_all("Docket No\\. CFP-2016-0025 Consumer Financial Protection Bureau 1700 G Street, NW\\.") %>%
  # make sure sentences are ended
  str_replace_all("\\t|\\\n", " ") %>% 
  #str_replace_all("\\.([A-z])", ". \\1") %>%
  # remvove numbers and specials (keep only text and basic punctuation)
  str_remove_all("\\\\") %>% 
  str_replace_all("[^[A-z] \\.\\,\\?\\!\\;&\\;<>]", " ") %>% 
  str_squish() %>%
  str_replace_all(" (\\.|\\?|\\!||:|;)", "\\1 ") %>%
  str_replace_all(" , ", " ") %>% 
  # double commas  
  #str_replace_all("\\, \\, ", ", ") %>% 
  # double periods 
  #str_replace_all("\\. \\. ", ". ") %>% 
  # one character after a period 
  str_replace_all("\\. .\\. ", ". ") %>% 
  # remove white space
  str_squish() %>% 
  # str_replace_all(" \\,", ", ") %>%
  str_replace_all(" \\.", ". ") %>%
  str_squish() 
