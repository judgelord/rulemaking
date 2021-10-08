source("setup.R")
# new data 

#climate
load(here::here("data", "climatePR2021-10-04.Rdata"))
climatePR %<>% namingthings()
load(here::here("data", "climateFR2021-10-04.Rdata"))
climateFR %<>% namingthings()
load(here::here("data", "climatecomments2021-10-06.Rdata"))
climatecomments %<>% namingthings()

# inspect 
climatePR %>% 
  full_join(climateFR) %>% 
  filter(posted_date > as.Date("2000-01-01")) %>% 
  mutate(year = str_sub(posted_date, 1,4) %>% as.numeric()) %>% 
  ggplot() + 
  aes(x = year, fill = document_type) +
  geom_bar()

climatecomments %>% 
  filter(posted_date > as.Date("2000-01-01")) %>% 
  mutate(year = str_sub(posted_date, 1,4) %>% as.numeric()) %>% 
  ggplot() + 
  aes(x = year, fill = document_type) +
  geom_bar()

# save
save(climatePR, file = here::here("data", "climatePR.Rdata"))
save(climateFR, file = here::here("data", "climateFR.Rdata"))
save(climatecomments, file = here::here("data", "climatecomments.Rdata"))
