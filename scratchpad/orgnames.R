source("setup.R")

# load(here("ascending/allcomments.Rdata"))
# d <- all


#function to change string remove to str_rm which is no longer case senstitive
str_rm_all <- function(string, pattern) {
  str_remove_all(string, regex(pattern, ignore_case = TRUE))
}

str_rpl <- function(string, pattern, replacement) {
  str_replace(string, regex(pattern, ignore_case = TRUE), replacement)
}

str_rm <- function(string, pattern) {
  str_remove(string, regex(pattern, ignore_case = TRUE))
}

## A sample of high-profile rules
load(here("data/masscomments.Rdata"))





#"FWS"   "HUD"   "NPS"   "OSHA"  "NHTSA" "NRC"   "BLM"   "FDA"   "BSEE"  "CMS"   "ED"    "CFPB"  "BOEM"  "FNS"   "ATF"   "EBSA"  "WHD"   "DOI"   "NOAA"  "IRS"   "OCC"   "EERE"  "OMB"   "FEMA"  "OSM"  
#"VA"    "SSA"   "HHS"   "FHWA"  "FAA"   "BIA"   "PHMSA" "OFCCP" "ACF"   "DOL"   "CDC"   "OPM"   "LMSO"  "OTS"   "USCIS" "CPSC"  "EEOC"  "DOD"   "ETA"   "MMS"  

d <- mass %>% filter(agencyAcronym == "FWS")
#later select later.. Forest Service

#all the agency list
unique(d$agencyAcronym)


length(unique(d$docketId))

d %<>% filter(docketType == "Rulemaking")
dim(d)
length(unique(d$docketId))
length(unique(d$agencyAcronym))

d %<>% group_by(docketId) %>% 
  mutate(docketTotal = sum(numberOfCommentsReceived)) %>% 
  ungroup()

d %<>% 
  # logical
  mutate(comment = nchar(commentText) > 140 ) %>% 
  mutate(comment = ifelse(is.na(comment), FALSE, comment)) %>%
  # text removing short phrases
  mutate(commenttext =
           ifelse( comment, commentText, NA)
         ) %>% 
  mutate(commentfirst140 = str_sub(commenttext,1,140)) %>% 
  mutate(commentlast140 = str_sub(commenttext,-140,-1))

sum(d$comment)
sum(!d$comment)
dim(d)
# count types 
d %<>% 
  group_by(docketId, commenttext) %>% 
  mutate(commentsIdentical = ifelse(comment, n(), NA) ) %>%
  ungroup() %>% 
  group_by(docketId, commentfirst140) %>% 
  mutate(commentsPartial = ifelse(comment, n(), NA) ) %>% 
  ungroup() %>% 
  group_by(docketId, commentlast140) %>% 
  mutate(commentsPartial = ifelse(comment, n(), commentsPartial) ) %>% 
  ungroup()
  
# NOTE: SOME OF THESE ARE DUPLICATES THAT NEED TO BE INVESTIGATED (IS REGULATIONS.GOV DOUBLE COUNTING OCCACIONALLY?)
# d %>% filter(commentsIdentical>2) %>% select(numberOfCommentsReceived, organization, commentText)
# d %>% filter(commentsPartial>9) %>% select(commentText)


sum(is.na(d$numberOfCommentsReceived))
sum(is.na(d$commentText))
sum(!is.na(d$commentText) & nchar(d$commentText)<20)

# define mass
  d %<>% 
    # default
    mutate(mass = "Yet to be classified") %>% 
    # unique comments
    mutate(mass = ifelse(comment & commentsIdentical == 1, "Unique", mass)) %>% 
   # medium batches (2-100)
    mutate(mass = ifelse(numberOfCommentsReceived>1 | commentsIdentical>1, "Medium batch", mass)) %>%
    # partially unique comments 
    mutate(mass = ifelse(comment & commentsIdentical==1 & commentsPartial>1, "Partially unique", mass)) %>%
    # bulk submissions over 99 
    mutate(mass = ifelse(numberOfCommentsReceived>99 | commentsIdentical>99, "Mass Comments", mass)) 

d  %<>% mutate(mass = ifelse(is.na(mass), "Yet to be classified", mass))

d %>% group_by(mass) %>% summarise(n = n(), total = sum(numberOfCommentsReceived))
  
  
# forms of comments
forms <- "email|Email|USB|Paper|paper|Web|web|Postcard|postcard|Change.org|eRulemaking Portal"
  
  d %<>% 
    mutate(commentform = tolower(str_extract(title, forms))) %>%
    mutate(commentform = ifelse(is.na(commentform), "unknown", commentform))
  
d$commentform %<>% stringr::str_replace("change.org", "web")
d$commentform %<>% stringr::str_replace("postcard", "paper")
d$commentform %<>% stringr::str_replace("usb", "web")
d %<>% 
  mutate(commentform = ifelse(comment == TRUE & numberOfCommentsReceived ==1 & commentform == "unknown",
                               "erulemaking portal", commentform))

unique(d$commentform)  
  
# extract org info 
preface <- ".*ponsored by |.*ponsoring organization |.*ubmitted by |.*omments from |.*ampaign from |.*on behalf of "

d %<>% 
  mutate(organization = 
           ifelse(grepl(preface,title) &
                    (is.na(organization) | str_detect(organization, "nknown") ),
                  gsub(preface,"",title), organization) ) %>%
  mutate(organization = ifelse( str_detect(organization, "nknown|^NA$|^n/a$"),NA, organization)) 

# clean orgs
# d %<>% mutate(organization = ifelse(is.na(organization), title, organization) )
# d$organization <- gsub(".*ponsored by |.*ponsoring organization |.*ampaign from |.*ubmitted by |.*omment from |.* on behalf of ", "", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*MoveOn.*", "MoveOn", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*CREDO.*", "CREDO Action", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*NRDC.*", "Natural Resources Defense Council", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Care2.*", "Care2", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Environmental Working|EWG.*", "Environmental Working Group", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Earthjustice.*", "Earthjustice", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Sierra Club.*", "Sierra Club", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Environmental Action.*", "Environmental Action", d$organization, ignore.case = TRUE)
d$organization <- gsub("Opportunties", "Opportunities", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*EDF.*", "Environmental Defense Fund", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Environmental Defense.*", "Environmental Defense Fund", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Monsanto.*", "Monsanto", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*American Lung Association.*", "American Lung Association", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Green For All.*", "Green For All", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Biological Diversity.*", "Center for Biological Diversity", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Audubon.*", "National Audubon Society", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Environment Maryland and Environment Virginia.*", "Environment Maryland and Environment Virginia", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Public Interest Research Group.*|.*tudent PIRG.*|.*PIRG..tudent.*", "Public Interest Research Group", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Unitarian*|unitarian.*", "Unitarian Congregations", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*American Policy Center.*", "American Policy Center", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Asbestos Disease Awareness Organization.*", "Asbestos Disease Awareness Organization", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Consumers Union.*", "Consumers Union", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*National Wildlife Federation.*", "National Wildlife Federation", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*350.*", "350", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Conservation Voters.*", "League of Conservation Voters", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Association for Gun Rights.*", "National Association for Gun Rights", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Friends of the Earth.*", "Friends of the Earth", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Center for Effective Government.*", "Center for Effective Government", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Interfaith Power and Light.*", "Interfaith Power and Light", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*Planned Parenthood.*", "Planned Parenthood", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*MomsRising|mom's rising.*", "MomsRising", d$organization, ignore.case = TRUE)
d$organization <- gsub(".*PEW.*", "PEW", d$organization, ignore.case = FALSE)
d$organization <- gsub(".*Farm Bureau.*", "Farm Bureau", d$organization, ignore.case = FALSE)
d$organization <- gsub(".*Organizing for Action.*", "Organizing For Action", d$organization, ignore.case = FALSE)
  
# second to last
d$organization <- gsub(".*members of ", "", d$organization, ignore.case = FALSE)
d$organization <- gsub(".*Change.org.*", "Change.org", d$organization, ignore.case = FALSE)
d$organization <- gsub(".*unknown.*|^N.A$", "unknown", d$organization, ignore.case = TRUE)
# do this last 
d$organization <- gsub("  ", " ", d$organization, ignore.case = TRUE)
d$organization <- gsub("^the |^ | $", "", d$organization, ignore.case = TRUE)
d$organization <- gsub("\\(.*", "", d$organization, ignore.case = TRUE)
d$organization <- gsub("\\. Sample.*|\\. \n.*|\n.*|\\,.*", "", d$organization, ignore.case = TRUE)
d$organization <- gsub(" et al.*| - .*", "", d$organization, ignore.case = TRUE)
d$organization <- gsub(" \\(.*| \\[.*", "", d$organization, ignore.case = TRUE)
d$organization <- gsub(" $", "", d$organization, ignore.case = TRUE)

################################################################################
#TO DO:
#create new org variable, that starts with organization and then builds on it 
#forest service, national research council, national oceangraphic and atmospheric administration and fish and wildlife service

#creating org variable
d %<>% 
  #bring over organization to org
  mutate(org = organization) %>% 
  #sponsored by
  mutate(org = ifelse(is.na(org) & grepl("sponsor... by [[:upper:]]", title, ignore.case = TRUE), 
                      str_rm_all(title, ".* sponsor... by|\\(.*"), 
                      org)) %>% 
  #association
  mutate(org = ifelse(is.na(org) & grepl(".* association", title, ignore.case = TRUE), 
                      str_rpl(title, "association .*", "Association"), 
                      org)) %>% 
  #cooperative
  mutate(org = ifelse(is.na(org) & grepl(".* cooperative", title, ignore.case = TRUE), 
                      str_rpl(title, "cooperative.*", "Cooperative"), 
                      org)) %>% 
  #by
  mutate(org = ifelse(is.na(org) & grepl("by", title, ignore.case = TRUE), 
                    str_rm_all(title, ".* by|\\(.*| et al|\\'s .*|Alexander Rony, |the "), 
                    org)) %>% 
    #fixing issues under "by"
    #FIXME
    mutate(org = ifelse(grepl("EPA-HQ-TRI-2005-0073", docketId), 
                    NA, 
                    org)) %>% 
  #mass mailer campaign 
  mutate(org = ifelse(is.na(org) & grepl(".*mass mailer campaign", title, ignore.case = TRUE), 
                    str_rm(title, "mass mailer campaign.*"), 
                    org)) %>% 
  #member mass email
  mutate(org = ifelse(is.na(org) & grepl(".*member mass email", title, ignore.case = TRUE), 
                      str_rm(title, "member mass email.*"), 
                      org)) %>% 
  #Missourians for a Balanced Energy Future
  mutate(org = ifelse(is.na(org) & grepl(".*Missourians for a Balanced Energy Future", title, ignore.case = TRUE), 
                   "Missourians for a Balanced Energy Future", 
                    org)) %>% 
  #CREDO Action
  mutate(org = ifelse(is.na(org) & grepl(".*CREDO Action", commenttext, ignore.case = TRUE), 
                    "CREDO Action", 
                    org)) %>% 
  #Public Citizen
  mutate(org = ifelse(is.na(org) & grepl(".*Public Citizen members", commenttext, ignore.case = TRUE), 
                      "Public Citizen", 
                      org)) %>% 
  #NPCA
  mutate(org = ifelse(is.na(org) & grepl(".*NPCA", title, ignore.case = TRUE), 
                    "National Parks Conservation Association", 
                    org)) %>% 
  #no-reply@democracyinaction.org
  #FIXME
  mutate(org = ifelse(is.na(org) & grepl("Melissa Drapeau <no-reply@democracyinaction.org>", commenttext, ignore.case = TRUE), 
                    str_rpl(commenttext, "Melissa Drapeau.*", "Democracy In Action"), 
                    org)) %>% 
  #American Lung Association
  mutate(org = ifelse(is.na(org) & grepl("See attached letter from 617 health professionals", commenttext, ignore.case = TRUE), 
                      str_rpl(commenttext, "See attached letter from 617 health professionals.*", "American Lung Association"), 
                        org)) %>% 
  #Power Shift Network
  #FIXME
  mutate(org = ifelse(is.na(org) & grepl("I am submitting the attached 1,418 comments on Docket EPA-HQ-OAR-2010-0505 collected by the Power Shift Network", commenttext, ignore.case = TRUE), 
                    str_rpl(commenttext, "I am submitting the attached 1,418 comments on Docket EPA-HQ-OAR-2010-0505 collected by the Power Shift Network.*", "Power Shift Network"), 
                    org)) %>% 
  #mass mail
  #creating other by getting rid of common phrasing of unknowns
  mutate(org = ifelse(is.na(org) & grepl("EPA", agencyAcronym, ignore.case = TRUE) & !grepl(str_c("This is a mass letter campaign.",
                                                                                                    "This is a mass postcard campaign.",
                                                                                                    "These comments were labeled incorrectly to refer",
                                                                                                    "This comment was labeled incorrectly to refer",
                                                                                                    "This is a mass e-mail campaign.",
                                                                                                    "This is a mass e-mail/letter campaign.",
                                                                                                    "This is a mass e-mail and letter campaign.",
                                                                                                    "This is a mass paper campaign.",
                                                                                                    "This is a mass e-mail and postcard campaign.",
                                                                                                    "This is a mass e-mail  and letter campaign.",
                                                                                                    "A sample PDF has been provided for review",
                                                                                                    sep = "|"),
                                                             commenttext, ignore.case = TRUE) & grepl(".", commenttext, ignore.case = TRUE), "other", org))

  
#create variable org.comment, result is T 
d %<>%
  mutate(org.comment) %>% 
  mutate(org.comment = ifelse(grepl("FWS-HQ-ES-2013-0073", docketID, ignore.case = TRUE), T, org.comment))
  

                                                            
#test for missing orgs not the unknown
showme <- d %>% 
  select(docketId, attachmentCount, agencyAcronym, title, commenttext, organization,org)

test <- d %>% 
  select(docketId, attachmentCount, agencyAcronym, title, commenttext, organization, org) %>% 
  filter(!grepl("unknown", title, ignore.case = TRUE), is.na(organization), is.na(org))

test <- d %>% 
  select(docketId, attachmentCount, agencyAcronym, title, commenttext, organization, org) %>% 
  filter( is.na(organization), is.na(org))


#running smaller test for speciifc rules 
test1 <- d %>% 
  select(docketId, attachmentCount, agencyAcronym, title, commenttext, organization, org) %>% 
  filter(grepl("comment from", title, ignore.case = TRUE), is.na(organization), is.na(org))

test2 <- d %>% 
  select(docketId, attachmentCount, agencyAcronym, title, commenttext, organization, org) %>% 
  filter(grepl("other", org, ignore.case = TRUE))

# The comments received are identical in content and format
test3 <- d %>% 
  select(docketId, attachmentCount, agencyAcronym, title, commenttext, organization, org) %>% 
  filter(!grepl(str_c("This is a mass letter campaign.", 
                     "This is a mass postcard campaign.",
                     "These comments were labeled incorrectly to refer",
                     "This comment was labeled incorrectly to refer",
                     "This is a mass e-mail campaign.",
                     "This is a mass e-mail/letter campaign.",
                     "This is a mass e-mail and letter campaign.",
                     "This is a mass paper campaign.",
                     "This is a mass e-mail and postcard campaign.",
                     sep = "|"), 
                     commenttext, ignore.case = TRUE) & grepl(".", commenttext, ignore.case = TRUE), 
         is.na(organization), is.na(org))



##########################################################################################################

unique(d$organization)[1:100]

d %<>% 
  mutate(organization = ifelse(organization %in% c("", "NA", "unknown"), NA, organization)) %>%
  group_by(organization) %>% 
  mutate(orgTotal = sum(numberOfCommentsReceived)) %>% 
  ungroup() %>%
  arrange(-orgTotal)

# inspect 
# unique(d$organization)[1:100]
d$orgTotal[1:20]
d$organization[1:20]
d$documentId[1:20]
d$commentText[1:20]


save = FALSE
if(save){
allcomments2 <- d
save(allcomments2, file ="ascending/allcomments2.Rdata") 

load(file ="ascending/allcomments2.Rdata") 
d <- allcomments2

#############################################################



toporgs <- d %>% 
  filter(!is.na(organization), organization != 'NA') %>% 
  filter(nchar(organization)<50) %>% 
  filter(orgTotal %in% sort(
    unique(d$orgTotal), 
    decreasing = T)[1:100] 
  ) %>% 
  arrange(orgTotal) 

save(toporgs, file = "data/toporgs.Rdata")






topdockets <- d %>% 
  filter(docketTotal %in% sort(
    unique(d$docketTotal), 
    decreasing = T)[1:10] 
    )

save(topdockets, file = "data/topdockets.Rdata")




###########################
# hand coding 
tocode <- filter(toporgs, docketType == "Rulemaking") %>% 
  select(numberPerDocket, numberOfCommentsReceived, organization, submitterName, title, commentText, docketTitle) %>%
  arrange(-numberPerDocket)

write.csv(tocode, here("data/tocode.csv"))


##################################
textcomments <- d %>% filter(docketType == "Rulemaking") %>%
  filter(comment == T)

textcomments %<>% group_by(docketId) %>% 
  mutate(docketTotal = sum(numberOfCommentsReceived)) %>% 
  ungroup()

d <- textcomments %>% 
  filter(docketTotal %in% sort(
    unique(textcomments$docketTotal), 
    decreasing = T)[9] 
  )
length(unique(d$commentText))
unique(d$organization)
length(unique(d$commentText))
unique(d$title)[1:20]
sum(d$numberOfCommentsReceived)
d$commentText[1:20]
unique(d$docketId)



save(textcomments, file = here("ascending/textcoments.Rdata"))

}

