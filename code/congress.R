# load all comments 
load(here::here("data", "comment_meta_min.Rdata"))
names(comment_meta_min)

d <- comment_meta_min #%>% head(100000)
nrow(d)

# one string to search 
library(tidyr)
library(magrittr)
library(tidyverse)
d %<>% mutate(title_org = str_c(title, organization, sep =  " ") %>% replace_na(" "))

d %<>% mutate(house = str_dct(title_org, "Congressm|Congressw|Representative|Rep\\."))
unique(d$house)

d %<>% mutate(senate = str_dct(title_org, "Senat|Sen\\."))
unique(d$senate)

d %<>% mutate(congress = str_dct(title_org, "Congress"))
unique(d$congress)

comments_congress <- d %>% filter(house|senate|congress )
nrow(comments_congress)

comments_congress %>% count(title, sort = TRUE) %>% head(20)

comments_congress %>% count(organization, sort = TRUE) %>% head(20)


comments_congress %<>% mutate(agency = str_remove(id, "-.*"),
                              Year = str_sub(posted_date, 1, 4),
                              Chamber = ifelse(senate, "Senate", "Both"),
                              Chamber = ifelse(house, "House", Chamber)) 


save(comments_congress, file = here::here("data", "comment_congress.Rdata"))

load(here::here("data", "comments_congress.Rdata"))
d <- comments_congress

d$Year %<>% as.numeric()

breaks <- seq(2000, 2020,by = 2)

d %>% 
  filter(Year %>% as.numeric() > 2000,
         Year %>% as.numeric() < 2021) %>% 
  add_count(agency, name = "agency_n") %>%
  filter(agency_n > 88) %>% 
  count(Year, Chamber, agency, sort = TRUE) %>% 
  ggplot() +
  aes(x = Year, y = n, fill = Chamber) + 
  geom_col(position = "stack") + 
  facet_wrap("agency", scales = "free") + 
  labs(x = "" ,
       y = "Number of Rulemaking Comments from Members of Congress") + 
  scale_x_continuous(breaks = breaks) + 
  theme(axis.text.x = element_text(angle = 90),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())































