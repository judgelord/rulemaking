
# new data 

#ej
load(here::here("data", "ejPR2021-02-06.Rdata"))
ejPRnew<- ejPR %>% namingthings()
load(here::here("data", "ejFR2021-02-06.Rdata"))
ejFRnew <- ejFR %>% namingthings()
load(here::here("data", "ejcomments2021-02-06.Rdata"))
ejcommentsnew <- ejcomments %>% namingthings()

# all
load(here::here("data", "rules2020-12-17.Rdata"))
rulesnew <- rules %>% namingthings()

load(here::here("data", "nprms2020-12-17.Rdata"))
nprmsnew <- nprms %>% namingthings()

load(here::here("data", "notices2020-12-17.Rdata")) #FIXME THESE ARE ONLY 2020. go back for 2019 and 2018 
noticesnew <- notices %>% namingthings()

# old data
load(here::here("data", "rules_metadata.Rdata"))
load(here::here("data", "ejPR.Rdata"))
load(here::here("data", "ejFR.Rdata"))
load(here::here("data", "ejcomments.Rdata"))

ejFR %<>% namingthings()
ejPR %<>% namingthings()
ejcomments %<>% namingthings()
rules %<>% namingthings()


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

rules %<>% 
  full_join(rulesnew) %>% 
  full_join(nprmsnew) %>% 
  full_join(noticesnew) %>% 
  #select(-page,-code, -`content[[1]]`, -message) %>% 
  distinct() %>%
  arrange(rev(posted_date))

max(rules$posted_date)

rules %>% 
  filter(posted_date > as.Date("2000-01-01")) %>% 
  mutate(year = str_sub(posted_date, 1,4) %>% as.numeric()) %>% 
  ggplot() + 
  aes(x = year, fill = document_type) +
  geom_bar()

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

save(rules, file =  here::here("data", "rules_metadata.Rdata"))
save(ejPRnew, file = here::here("data", "ejPRnew.Rdata"))
save(ejFRnew, file = here::here("data", "ejFRnew.Rdata"))
save(ejcommentsnew, file = here::here("data", "ejcommentsnew.Rdata"))






# subset comments 

load("comment_metadata.Rdata")
names(comments_all)

org_comments <- comments_all %>% filter(!is.na(organization)) %>% count(organization, docket_id, number_of_comments_received)

head(org_comments)
dim(org_comments)

save(org_comments, file = here::here("data", "org_comments.Rdata"))



comments_min <- comments_all %>% distinct(document_id, organization,submitter_name, number_of_comments_received)

head(comments_min)
dim(comments_min)

save(comments_min, file = here::here("data", "comments_min.Rdata"))
