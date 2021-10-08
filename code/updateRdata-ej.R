
# new data 

#ej
load(here::here("data", "ejPR2021-02-06.Rdata"))
ejPRnew<- ejPR %>% namingthings()
load(here::here("data", "ejFR2021-02-06.Rdata"))
ejFRnew <- ejFR %>% namingthings()
load(here::here("data", "ejcomments2021-02-06.Rdata"))
ejcommentsnew <- ejcomments %>% namingthings()

# old data
load(here::here("data", "rules_metadata.Rdata"))
load(here::here("data", "ejPR.Rdata"))
load(here::here("data", "ejFR.Rdata"))
load(here::here("data", "ejcomments.Rdata"))

ejFR %<>% namingthings()
ejPR %<>% namingthings()
ejcomments %<>% namingthings()


# join new

ejFR %<>% full_join(ejFRnew) %>% 
  #select(-page,-code, -`content[[1]]`, -message) %>% 
  distinct()

ejPR %<>% full_join(ejPRnew)  %>% 
  #select(-page,-code, -`content[[1]]`, -message) %>% 
  distinct()

ejcomments %<>% full_join(ejcommentsnew) %>% 
  #select(-code, -`content[[1]]`, -message) %>% 
  distinct()



# inspect 
ejPR %>% 
  full_join(ejFR) %>% 
  filter(posted_date > as.Date("2000-01-01")) %>% 
  mutate(year = str_sub(posted_date, 1,4) %>% as.numeric()) %>% 
  ggplot() + 
  aes(x = year, fill = document_type) +
  geom_bar()

ejcomments %>% 
  filter(posted_date > as.Date("2000-01-01")) %>% 
  mutate(year = str_sub(posted_date, 1,4) %>% as.numeric()) %>% 
  ggplot() + 
  aes(x = year, fill = document_type) +
  geom_bar()

# save
save(ejPRnew, file = here::here("data", "ejPRnew.Rdata"))
save(ejFRnew, file = here::here("data", "ejFRnew.Rdata"))
save(ejcommentsnew, file = here::here("data", "ejcommentsnew.Rdata"))