# load comment metadata
load(here::here("data", "comment_metadata.rdata"))

comment_metadata_organization <- comment_metadata %>%
  mutate(organization = str_to_lower(organization) %>% str_squish()) %>%
  count(organization, sort = T)

nonorgs <- "^personal$|illegible|default organization name|^anonymous|individual$|myself|^mr$|^ms$|^mrs$|^mr.$|^ms.$|^mrs.$|me, myself, and i|not applicable|a youtuber|retired federal employee|the human race|the american people$|^the people$|truck driver$|^human race|^attorney at law|^na$|n/a|no name|^unknown|^none$|^none |noneindividual|^my |^myself|^self$|seslf|private citizen|^citizen.$|^select$|^attorney$| owner$|christian$|catholic$|please select|the dodo|^other$|individual$|just me|anonymous|retired$|citizen$|citzen|^citizens$|citizen/consumer|^citizen, |^citizen of|^citizen at|^citizen -|^citizen and|me, myself and i|me myself and i|^no organization|concerned american|self.employed|self and|regulations.gov|public comment|private party|private owner|private citizan|^private$|personal (comment|opinion)| taxpayer$| voter$| human$|^concerned citizens$|^consumer$| consumer$| mom"

comment_metadata_nonorgs <- comment_metadata_organization |> filter(str_detect(organization, nonorgs))

head(comment_metadata_nonorgs, 100)

comment_metadata_nonorgs |> mutate(nchar = nchar(organization)) |> arrange(nchar) |> head(100)

comment_metadata_orgs <- comment_metadata_organization |> filter(!str_detect(organization, nonorgs), nchar(organization) >1)

head(comment_metadata_orgs, 100)

save(comment_metadata_orgs,
     file = here::here("data", "comment_metadata_orgs.rdata"))

write_csv(orgs,
          file = here::here("data", "comment_metadata_orgs.csv"))
