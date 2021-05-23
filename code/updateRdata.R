
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





dim(comment_metadata)
# subset comments 
load(here::here("data", "comment_metadata2020.Rdata"))
names(comment_metadata)

org_comments <- comment_metadata %>% filter(!is.na(organization)) %>% distinct(organization, docket_id, number_of_comments_received)

head(org_comments)
dim(org_comments)

save(org_comments, file = here::here("data", "org_comments.Rdata"))


# Minimal comment data 

class(comment_metadata$posted_date)
class(comment_metadata$comment_due_date)
class(comment_metadata$comment_start_date)

# to Date 
comment_metadata$comment_due_date %<>% as.Date()
comment_metadata$comment_start_date %<>% as.Date()

range(comment_metadata$posted_date, na.rm = T)
range(comment_metadata$comment_due_date, na.rm = T)
range(comment_metadata$comment_start_date, na.rm = T)

sum(is.na(comment_metadata$posted_date))
sum(is.na(comment_metadata$comment_due_date))
sum(is.na(comment_metadata$comment_start_date))


# on some older dockets, comments were uploaded later, so date posted not the date submitted, so I approximate with due date 
comment_metadata %<>%
  mutate(docket_date = str_extract(docket_id,"(19|20)[0-9][0-9]") %>% str_c("-01-01") %>% as.Date())

range(comment_metadata$docket_date, na.rm = T)

# make NA impossible dates
comment_metadata %<>% # head() %>%
  mutate(posted_date = if_else(posted_date < docket_date, as.Date(NA), posted_date),
         comment_start_date = if_else(comment_start_date < docket_date, as.Date(NA), comment_start_date),
         comment_due_date = if_else(comment_due_date < docket_date, as.Date(NA), comment_due_date)) #%>% select(ends_with("date"))

comment_metadata %<>%
  mutate(date = pmin(posted_date, comment_due_date, na.rm = T) %>% 
           # if still missing fill in comment start date (at least we should get the right admin)
           coalesce(comment_start_date, docket_date)) %>%
  ungroup() #%>% select(ends_with("date"))

comment_metadata %<>% arrange(-date)

comment_metadata %>% tail() %>% select(docket_id, ends_with("date"))

range(comment_metadata$date, na.rm = T)
range(comment_metadata$docket_date, na.rm = T)
sum(is.na(comment_metadata$docket_date))
sum(is.na(comment_metadata$date))

comment_metadata %>% filter(is.na(docket_date)) %>% #count(posted_date, sort = T) %>% 
  head(10)

# trim down to minimals vars 
comments_min <- comment_metadata %>% distinct(id, organization,submitter_name, number_of_comments_received, date)

head(comments_min)
dim(comment_metadata)
dim(comments_min)

comments_min %<>% filter(date >= as.Date("2005-01-01"))

save(comments_min, file = here::here("data", "comments_min.Rdata"))
class(comments_min)
comments_min %<>% as_tibble()

save(comment_metadata, file =  here::here("data", "comment_metadata2020.Rdata"))
