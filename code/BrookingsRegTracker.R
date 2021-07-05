

names(d)
names(regs)

regs %<>%
  rename(rin = RIN) %>% 
  left_join(d)

regtracker <- read_csv("data/Deregulation-tracker-data-Final.csv")

regtracker %<>% 
  mutate(frNumber = str_extract(`Proposed-link`, "20[0-9][0-9]-[0-9]{5}"))

regtracker$frNumber


regtracker %<>% 
  distinct() %>% 
  filter(!is.na(frNumber)) %>% 
  left_join(regs) %>% 
  select(rin, PUBLIC_COMMENT_URL, frNumber, title, numberOfCommentsReceived) %>% 
  distinct()
  