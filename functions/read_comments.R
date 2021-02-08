read_comments <- . %>%
  read_lines() %>%
  clean_comments() 