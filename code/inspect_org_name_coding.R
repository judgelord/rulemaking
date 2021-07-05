#TESTING/TOOLS (function step by step)
#########################################################################################################################################################################################################

#org.comment
#######################

#sort by docket to see captured T, F, NA
docket <- d %>% 
  group_by(org.comment, docketId, .drop = F) %>%
  summarize(n=n()) %>% 
  arrange(docketId)

#find the missing org.comments
missing <- d %>% 
  select(mass, docketId, documentId, mass, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org, congress) %>% 
  filter(is.na(org.comment))

#check the true org.comments
true <- d %>% 
  select(mass, docketId, documentId, mass, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org, congress) %>% 
  filter(org.comment == T)

#check the false org.comments
false <- d %>% 
  select(mass, docketId, documentId, mass, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org, congress) %>% 
  filter(org.comment == F)

#check to see if there are org.comment where there is no org
noName <- d %>% 
  select(mass, congress,docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org_name) %>% 
  filter(org.comment == T, is.na(org_name))

#test frame to try out different strings to code T and F
test <- d %>% 
  select(mass, docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org_name) %>% 
  filter(str_dct(commenttext, ""))

#check the one word orgs that got lost 
lostorg_name <- d %>% 
  select(mass, docketId, documentId, mass, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org_name) %>% 
  filter(str_dct(organization, ".*"), !str_dct(organization, str_c("^none$", "^unknown$", "^individual$", "^citizen$", "self$", "not applicable", "^private$", "personal", "lover", "mr\\.$", "mrs\\.$", "^ms$",
                                                                   "retired", "^dr$", "^miss$", "^mr$", "^ms\\.$", "^mr\\.$", "^na$", "^me$", "^-$|^--$", "street$", "^happy$", "^r$", "^home$", "please select", "^brain$", "^no name$",
                                                                   "no one$", "^nol longer", "no organization$", "- select -", "- none - ","--none--", "concerned citizen$", "-none-", "select\\.\\.\\.", "send$", "^love$", "^n\\.a\\.$",
                                                                   "^\\.$", "not specified", "^other$", "foekf", "what a shame this is", "no affiliation", "^usa$", "^LFJJHK$", "none\\. plain ol' concerned citizen.", "anonymous anonymous",
                                                                   "the federal government institute", "individual taxpayer", "citizen of the united states|citizen of the US", "public land owner", "^taxpayer", "american citizen",
                                                                   "^U\\.S\\. citizen$", "^unknown", "^farmer$", 
                                                                   sep = "|")), is.na(org_name)) %>% 
  arrange(organization) %>% 
  count(organization)