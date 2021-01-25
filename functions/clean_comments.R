clean_comments <- . %>% 
  str_c(collapse = " ") %>% 
  # remove tab and line breaks
  str_replace_all("\\t|\\\n", " ") %>% 
  #str_remove_all(" ?(f|ht)(tp)(s?)(://)([A-z]*)[.|/](.*)") %>%
  #str_replace_all("\\s+", " ") %>% # removed by str_squish()
  #str_remove_all("^[0-9](?!\\.)") %>% #FIXME are not numbers removed below
  # add missing spaces after periods
  str_replace_all("\\.([A-z])", ". \\1") %>%
  # remvove numbers and specials (keep only text and basic punctuation)
  str_replace_all("[^[A-z] \\.\\,\\?\\!\\;&\\;\\']", " ") %>% 
  str_remove_all("\\[|\\]") %>%
  #str_remove_all("\\'") %>%
  str_squish() %>%
  #str_replace_all(" (\\.|\\?|\\!||:|;)", "\\1 ") %>%
  str_replace_all(" , ", " ") %>%
  str_replace_all("_", " ") %>%
  # double commas  
  #str_replace_all("\\, \\, ", ", ") %>%  # double punctuation not in summaries, uncomment if this shows up in summaries
  # double periods 
  #str_replace_all("\\. \\. ", ". ") %>% 
  # one character after a period 
  str_replace_all("\\. \\. \\. ", ". ") %>% 
  # remove white space
  str_squish() %>% 
  str_remove_all("pagebreak") %>% # Devin's OCR method adds this 
  # remove repeated periods
  #str_replace_all("\\.*", ". ") %>% # duplicated punctuation removed below
  # str_replace_all(" \\,", ", ") %>%
  str_replace_all(" \\.", ". ") %>%
  #remove space in 's
  str_replace_all(" \\'s ", "\\'s ") %>%
  #str_replace_all(" '", "'") %>% # we want to keep "'"
  # remove web addresses, wont capture urls with punctuation ("." or "_") in the middle
  # str_remove_all("www\\.[A-z]*\\.(com|org|net|gov|pdf)") %>%  # does not currently impact textrank, uncomment if urls start to skew summary
  # str_remove_all("http:www\\.[A-z]*\\.(com|org|net|gov|pdf)") %>%
  # str_remove_all("files\\.[A-z]*\\.(com|org|net|gov|pdf)") %>%
  #Removes duplicated puncuation
  str_replace_all("([[:punct:]])\\1+", "\\1") %>%
  str_squish() 