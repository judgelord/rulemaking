source("setup.R")
# new data 

#cj
load(here::here("data", "cjPR2021-10-04.Rdata"))
cjPR %<>% namingthings()
load(here::here("data", "cjFR2021-10-04.Rdata"))
cjFR %<>% namingthings()
load(here::here("data", "cjcomments2021-10-04.Rdata"))
cjcomments %<>% namingthings()


# inspect 
cjPR %>% 
  full_join(cjFR) %>% 
  filter(posted_date > as.Date("2000-01-01")) %>% 
  mutate(year = str_sub(posted_date, 1,4) %>% as.numeric()) %>% 
  ggplot() + 
  aes(x = year, fill = document_type) +
  geom_bar()

cjcomments %>% 
  filter(posted_date > as.Date("2000-01-01")) %>% 
  mutate(year = str_sub(posted_date, 1,4) %>% as.numeric()) %>% 
  ggplot() + 
  aes(x = year, fill = document_type) +
  geom_bar()

# save
save(cjPR, file = here::here("data", "cjPR.Rdata"))
save(cjFR, file = here::here("data", "cjFR.Rdata"))
save(cjcomments, file = here::here("data", "cjcomments.Rdata"))
