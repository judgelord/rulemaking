# load comment metadata 
load(here::here("data", "comment_metadata.rdata"))

comment_metadata_orgs <- comments_all %>% 
  mutate(organization = str_to_lower(organization) %>% str_squish()) %>% 
  count(organization, sort = T)  

nonorgs <- "na|n/a|no name|^unknown|^none$|^none |noneindividual|^my |^myself|^self$|seslf|private citizen|^citizen.$|^select$|^attorney$| owner$|christian$|catholic$|please select|the dodo|^other$|individual$|just me|anonymous|retired$|citizen$|citzen|^citizens$|citizen/consumer|^citizen, |^citizen of|^citizen at|^citizen -|^citizen and|me, myself and i|me myself and i|^no organization|concerned american|self.employed|self and|regulations.gov|public comment|private party|private owner|private citizan|^private$|personal (comment|opinion)| taxpayer$| voter$| human$|^concerned citizens$|^consumer$| consumer$| mom"

comment_metadata_orgs %<>% filter(!str_detect(organization, nonorgs))

head(comment_metadata_orgs, 100)

save(comment_metadata_orgs,
     file = here::here("data", "comment_metadata_orgs.rdata"))

write_csv(orgs, 
          file = here::here("data", "comment_metadata_orgs.csv"))
