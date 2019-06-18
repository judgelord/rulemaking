source("setup.R")


#load(here("ascending/allcomments.Rdata"))
load(here("data/allcomments.Rdata"))
load(here("data/masscomments.Rdata"))
load(here("data/comment_text_short.Rdata"))

#subsetting data from 'all' with matching docketId in 'mass'
d <- all %>% 
  filter(docketId %in% mass$docketId)

#merge comment_text by documentID value
d %<>% 
  left_join(comment_text_short)


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

str_dct <- function(string, pattern) {
  str_detect(string, regex(pattern, ignore_case = TRUE))
}

## A sample of high-profile rules
#load(here("data/masscomments.Rdata"))
#load(here("data/toporgs.RData"))

##LUCY
##Load in top org sample 
#load(here("data/topdockets.RData"))

#searching through EPA 
#d <- topdockets %>% filter(agencyAcronym == "EPA")

d <- d %>% filter(agencyAcronym == "NPS")
#looking through docket after
#group by docket, orgname
#summarize org.comment

#checking topdockets, 5 top dockets 
unique(d$docketId)

#FIXME
#d <- mass %>% filter(agencyAcronym == "EPA")


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



###all listed agency acronymns 
#"FWS"   "NPS"   "EERE"  "EPA"   "NHTSA" "FDA"   "OSHA"  "HUD"   "VA"    "IRS"   "NRC"   "CFPB"  "NOAA"  "OTS"   "HHS"   "USCIS" "CMS"   "ED"    "DOD"   "ETA"   "BLM"   "FNS"   "FAA"   "ATF"   "DOI"  
#[26] "OCC"   "EBSA"  "SSA"   "BSEE"  "OSM"   "BOEM"  "WHD"   "OMB"   "FEMA"  "FHWA"  "PHMSA" "BIA"   "OFCCP" "ACF"   "DOL"   "CDC"   "OPM"   "LMSO"  "CPSC"  "EEOC"  "MMS" 

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

##Org variable
d %<>% 
  mutate(org = NA) 

##broader rules
d %<>% 
  #sponsored by
  mutate(org = ifelse(is.na(org) & grepl("sponsor... by [[:upper:]]", title, ignore.case = TRUE), 
                      str_rm_all(title, ".* sponsor... by|\\(.*"), 
                      org)) %>% 
  #association
  mutate(org = ifelse(is.na(org) & grepl(".* association", title, ignore.case = TRUE) & !grepl("association of", title, ignore.case = TRUE), 
                      str_rpl(title, "association .*", "Association"), 
                      org)) %>% 
  #association of
  mutate(org = ifelse(is.na(org) & grepl(".*association of.*", title, ignore.case = TRUE), 
                      str_rpl(title, ".*association", "Association"), 
                      org)) %>% 
  #cooperative
  mutate(org = ifelse(is.na(org) & grepl(".* cooperative", title, ignore.case = TRUE), 
                      str_rpl(title, "cooperative.*", "Cooperative"), 
                      org)) %>% 
  # #by
  # mutate(org = ifelse(is.na(org) & grepl("by", title, ignore.case = TRUE), 
  #                   str_rm_all(title, ".* by|\\(.*| et al|\\'s .*|Alexander Rony, |the "), 
  #                   org)) %>% 
    #fixing issues under "by"
    # #FIXME
    # mutate(org = ifelse(is.na(org) & grepl("EPA-HQ-TRI-2005-0073", docketId), 
    #                 NA, 
    #                 org)) %>% 
  #mass mailer campaign 
  mutate(org = ifelse(is.na(org) & grepl(".*mass mailer campaign", title, ignore.case = TRUE), 
                    str_rm(title, "mass mailer campaign.*"), 
                    org)) %>% 
  #member mass email
  mutate(org = ifelse(is.na(org) & grepl(".*member mass email", title, ignore.case = TRUE), 
                      str_rm(title, "member mass email.*"), 
                      org)) %>% 
  #comment from
  mutate(org = ifelse(is.na(org) & grepl("comment from", title, ignore.case = TRUE), 
                      str_rm(title, ".*comment from"), 
                      org)) %>% 
  #schools
  mutate(org = ifelse(is.na(org) & grepl(".*school", title, ignore.case = TRUE), 
                    str_rpl(title, "school.*", "School"), 
                    org)) %>% 
  #church
  #FIXME, somehow doesn't work?
  mutate(org = ifelse(is.na(org) & grepl(".*church", title, ignore.case = TRUE), 
                    str_rpl(title, "church.*", "Church"), 
                    org)) %>% 
  #names
  mutate(org = ifelse(is.na(org) & grepl(".*, .*president, .*, .*", title, ignore.case = TRUE), 
                      str_rm(title, ".*, .*president, .*,"), 
                      org)) %>% 
  #names2
  mutate(org = ifelse(is.na(org) & grepl(".*, .*president,.*", title, ignore.case = TRUE), 
                    str_rm(title, ".*, .*president,"), 
                    org)) %>% 
  #bring over organization to org
  mutate(org = organization)


##Specific Cases
#Title
d %<>% 
mutate(org = ifelse(is.na(org) & grepl(".*WildEarth Guardians.*|.*WildEarthGuardians.*", title, ignore.case = TRUE), 
                    "WildEarth Guardians", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Care2.*", title, ignore.case = TRUE), 
                    "Care2", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Jane Goodall Institute.*", title, ignore.case = TRUE), 
                    "Jane Goodall Institute", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Humane Society International.*", title, ignore.case = TRUE), 
                    "Humane Society International", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Defenders of Wildlife.*|Defenders of Wildlife.*", title, ignore.case = TRUE), 
                    "Defenders of Wildlife", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*League of Conservation Voters.*", title, ignore.case = TRUE), 
                    "League of Conservation Voters", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Care2.*", title, ignore.case = TRUE), 
                    "Care2", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Sierra Club.*", title, ignore.case = TRUE), 
                    "Sierra Club", 
                     org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Environmental Action.*", title, ignore.case = TRUE), 
                    "Environmental Action", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*CascadiaWildlands.*", title, ignore.case = TRUE), 
                    "CascadiaWildlands", 
                     org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Cascadia Wildlands.*", title, ignore.case = TRUE), 
                    "Cascadia Wildlands", 
                     org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Sierra Club.*|SierraClub", title, ignore.case = TRUE), 
                    "Sierra Club", 
                     org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Biological Diversity.*|,*Center for Biological Diversity.*", title, ignore.case = TRUE), 
                    "Center for Biological Diversity", 
                     org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Colorado Wolf & Wildlife Center.*", title, ignore.case = TRUE), 
                    "Colorado Wolf & Wildlife Center", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Williams Community Forest Project.*", title, ignore.case = TRUE), 
                    "Williams Community Forest Project", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Binghamton Zoo.*", title, ignore.case = TRUE), 
                    "Binghamton Zoo", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl("Animal Rescute Site.*", title, ignore.case = TRUE), 
                    "Animal Rescue Site", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Big Game Forever.*", title, ignore.case = TRUE), 
                    "Big Game Forever", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*International Fund For Animal Welfare.*", title, ignore.case = TRUE), 
                    "International Fund For Animal Welfare", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*Missourians for a Balanced Energy Future", title, ignore.case = TRUE), 
                    "Missourians for a Balanced Energy Future", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*NPCA", title, ignore.case = TRUE), 
                    "National Parks Conservation Association", 
                    org)) %>% 
mutate(org = ifelse(is.na(org) & grepl(".*PEW", title, ignore.case = TRUE), 
                      "Pew Research Center", 
                      org))



#text
d %<>% 
  #International Fund for Animal Welfare
  mutate(org = ifelse(is.na(org) & grepl(".*International Fund For Animal Welfare.*", commenttext, ignore.case = TRUE), 
                      "International Fund For Animal Welfare", 
                      org)) %>% 
  #Endangered Species Coalition
  mutate(org = ifelse(is.na(org) & grepl(".*Endangered Species Coalition.*", commenttext, ignore.case = TRUE), 
                      "International Fund For Animal Welfare", 
                      org)) %>% 
  #Conservation Northwest
  mutate(org = ifelse(is.na(org) & grepl(".*Conservation Northwest.*", commenttext, ignore.case = TRUE), 
                      "Conservation Northwest", 
                      org)) %>% 
  #Save Our Environment, making caps matter because happens a lot in the text
  mutate(org = ifelse(is.na(org) & grepl(".*Save Our Environment.*", commenttext), 
                      "Save Our Environment", 
                      org)) %>%
  #Oregon Wild
  mutate(org = ifelse(is.na(org) & grepl(".*Oregon Wild.*", commenttext, ignore.case = TRUE), 
                      "Oregon Wild", 
                      org)) %>% 
  #Audobon California
  #FIXME
  mutate(org = ifelse(is.na(org) & grepl(".*Audobon California.*", commenttext, ignore.case = TRUE), 
                      "Audobon California", 
                      org)) %>% 
  #Care2
  mutate(org = ifelse(is.na(org) & grepl(".*Care2.*", commenttext, ignore.case = TRUE), 
                      "Care2", 
                      org)) %>% 
  #Sierra Club
  mutate(org = ifelse(is.na(org) & grepl(".*Sierra Club.*", commenttext, ignore.case = TRUE), 
                      "Sierra Club", 
                      org)) %>% 
  #CREDO Action
  mutate(org = ifelse(is.na(org) & grepl(".*CREDO Action", commenttext, ignore.case = TRUE), 
                    "CREDO Action", 
                    org)) %>% 
  #Public Citizen
  mutate(org = ifelse(is.na(org) & grepl(".*Public Citizen members", commenttext, ignore.case = TRUE), 
                      "Public Citizen", 
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
  #Trustees for Alaska
  mutate(org = ifelse(is.na(org) & grepl("Trustees for Alaska", commenttext, ignore.case = TRUE), "Trustees for Alaska", org)) %>% 
  #National Parks Conservation Association
  mutate(org = ifelse(is.na(org) & grepl("National Parks Conservation Association", commenttext, ignore.case = TRUE), "National Parks Conservation Association", org)) %>% 
  #Catharsis on the Mall
  mutate(org = ifelse(is.na(org) & grepl("catharsis on the mall", commenttext, ignore.case = TRUE), "Catharsis on the Mall", org)) %>% 
  #The November Project
  mutate(org = ifelse(is.na(org) & grepl("november project", commenttext, ignore.case = TRUE), "November Project", org))



#   #mass mail
#   #creating other by getting rid of common phrasing of unknowns
#   mutate(org = ifelse(is.na(org) & grepl("EPA", agencyAcronym, ignore.case = TRUE) & !grepl(str_c("This is a mass letter campaign.",
#                                                                                                     "This is a mass postcard campaign.",
#                                                                                                     "These comments were labeled incorrectly to refer",
#                                                                                                     "This comment was labeled incorrectly to refer",
#                                                                                                     "This is a mass e-mail campaign.",
#                                                                                                     "This is a mass e-mail/letter campaign.",
#                                                                                                     "This is a mass e-mail and letter campaign.",
#                                                                                                     "This is a mass paper campaign.",
#                                                                                                     "This is a mass e-mail and postcard campaign.",
#                                                                                                     "This is a mass e-mail  and letter campaign.",
#                                                                                                     "A sample PDF has been provided for review",
#                                                                                                     sep = "|"),
#                                                              commenttext, ignore.case = TRUE) & grepl(".", commenttext, ignore.case = TRUE), "other", org))

#org
##############################
#mutate org to lower
##org is who your mobilized by
d %<>% 
  mutate(org = tolower(org)) %>% 
  #mutate(org = ifelse(org %in% c("none", "unknown", "individual", "citizen", "self", "not applicable", "private", "personal", "bird lover", "private citizen", "retired"), NA , org))
  mutate(org = ifelse(str_dct(organization, str_c("^none$", "^unknown$", "^individual$", "citizen$", "self$", "not applicable", "^private$", "personal", "lover", "mr.$", "mrs.$", "ms$",
                                         "retired", "dr$", "miss$", "mr$", "ms.$", "mr.$", "^na$", "^me$", "^-$|^--$", "street$", "^happy$", "^r$", "^home$", "please select", "^brain$", "^no name$",
                                         "no one$", "^nol longer", "no organization$", "- select -", "- none - ","--none--", "concerned citizen$", "-none-", "select...", "send$", "^love$", "^n.a.$",
                                         "^.$", "not specified", "^other$", "foekf", "what a shame this is", "no affiliation", "^usa$", "LFJJHK"
                              sep = "|")), NA, org))

#congress
#############################


#create congress variable
d %<>%
  mutate(congress = NA) %>% 
  mutate(congress = ifelse(str_dct(title, "submitted by.*Senator|letter from.*senator"), T, congress)) %>% 
  mutate(congress = ifelse(str_dct(title, "submitted by.*representative|house of representatives|house of representative"), T, congress))


#org.comment
##############################
temp <- d
d <- temp

#create variable org.comment 
#org.comment, result is T
d %<>%
  mutate(org.comment = NA) %>% 
  #FIXME
  #mutate(org.comment = ifelse(grepl("FWS-HQ-ES-2013-0073", docketId, ignore.case = TRUE), T, org.comment)) %>% 
  mutate(org.comment = ifelse(grepl("EPA-HQ-OAR-2018-0283-4117", documentId, ignore.case = TRUE), T, org.comment))

#mass comment campaign, result is false
d %<>% 
  mutate(org.comment = ifelse(is.na(org.comment) & grepl(str_c("mass comment campaign", "mass postcard campaign", "mass mail Campaign",
                                                               "mass e-mail campaign", "Mass Mail Comment Campaign.", "mass e-mail/letter campaign",
                                                               "mass e-mail and letter campaign", "mass paper campaign", "Mass Mail",
                                                               "mass e-mail and postcard campaign","mass e-mail  and letter campaign",
                                                               "Mass signature campaign", "mass Comment campaing", "Mass comment Campaingn",
                                                              sep = "|"), title, ignore.case = TRUE), F, org.comment))


#finding submitted by names that are not associated with an organization
#notes: earthjustice miscaptures a few based on attachment count number because there are some
  #with high attachment counts that should be org.comment but others with high that aren't 
#SOMEHOW EVERYTHING IN FWS is getting marked as true 


#EPA 
d %<>% 
  #finding true 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by \\w+ \\w+") & str_dct(title, "director|CEO|president|manager|attorney") & attachmentCount >= 1, T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by") & str_dct(title, "natural resources defense council"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by \\w+ \\w+$") & agencyAcronym == "EPA" & attachmentCount > 1, T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by earthjustice"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by") & str_dct(title, "earthjustice") & str_dct(title,  str_c("lisa evans", "carrie apfel", "tyler smith",
                                                                                                                                                   "james pew", "jim pew", sep = "|")), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by") & str_dct(title, "earthworks"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by Sierra Club and Earthjustice"), T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by 350"), T, org.comment)) %>% 
  #finding false 
  mutate(org.comment = ifelse(is.na(org.comment) & congress == T, F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by [[:upper:]]. \\w+$"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "anonymous public comment"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "posted by error|wrong docket|duplicate"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "name illegible|name eligible"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by [[:upper:]] .*\\S$"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by [[:uppercase:]]. [[:uppercase:]]. \\w+$"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by [[:uppercase:]]. [[:uppercase:]].\\w+$"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by [[:uppercase:]]. and [[:uppercase:]].\\w+$") & attachmentCount <= 1, F, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(organization, "[[:upper:]]\\. \\w+$") & agencyAcronym == "EPA", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(organization, "[[:upper:]]\\.$") & agencyAcronym == "EPA" & attachmentCount <=1, F, org.comment))

#Fish & Wildlife Service 
d %<>% 
  #finding true
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, str_c("comment submitted by Chattahoochee Riverkeeper", "comment submitted by Black Warrior Riverkeeper", 
                                                                      "comment submitted by Hackensack", sep = "|")), T, org.comment)) %>% #check these
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "LLC") & attachmentCount >= 1, T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "[[:upper:]]\\. \\w+$") & str_dct(org, ".*") & agencyAcronym == "FWS", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "comments of the|on behalf of [[:uppercase:]]") & str_dct(org, ".*"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "as well as a letter") & str_dct(org, ".*") & agencyAcronym == "FWS", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "organizational comments"), T, org.comment)) %>% 
  #finding false
  mutate(org.comment = ifelse(is.na(org.comment) & numberOfCommentsReceived > 20, F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment from") & is.na(org), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "[[:upper:]]\\. \\w+$") & agencyAcronym == "FWS" & is.na(org), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "Submitted Electronically via eRulemaking Portal") & is.na(org), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "^\\w+$") & is.na(org) & agencyAcronym == "FWS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(organization, "none|concerned citizen"), F, org.comment)) %>% #this might need to change if they mention an org in text and its not captured in org
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "attached \\d.* comments|accept \\d.* comments"), F, org.comment)) %>%
  #fixing by docket
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(docketId, "FWS-HQ-ES-2013-0052") & str_dct(org, ".*"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(docketId, "FWS-HQ-ES-2013-0052") & str_dct(title, "Arkansas Game and Fish Commission"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(docketId, "FWS-HQ-ES-2013-0055") & str_dct(org, ".*"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(docketId, "FWS-HQ-ES-2016-0010") & str_dct(org, ".*"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(docketId, "FWS-HQ-ES-2016-0012") & str_dct(org, ".*"), T, org.comment)) %>% #pangolin...12-15
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(docketId, "FWS-HQ-ES-2016-0014") & str_dct(org, ".*"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(docketId, "FWS-HQ-ES-2016-0015") & str_dct(org, ".*") & !str_dct(commenttext, "public comments"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(docketId, "FWS-HQ-ES-2016-0017") & str_dct(org, ".*") & !str_dct(commenttext, "public comments"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(docketId, "FWS-HQ-ES-2016-0018") & str_dct(org, ".*") & !str_dct(commenttext, "public comments"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(docketId, "FWS-HQ-ES-2016-0021") & str_dct(org, ".*") & !str_dct(commenttext, "public comments"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(docketId, "FWS-HQ-ES-2016-0022") & str_dct(org, ".*") & !str_dct(commenttext, "public comments"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(docketId, "FWS-HQ-IA-2013-0091") & str_dct(org, ".*") & !str_dct(commenttext, "public comments") & !str_dct(commenttext, "letters from the activists") & !str_dct(org, "Katherine Anne Stansbury|Alan Feltman|Victoria Olson|Jon Swalby|Adrienne Metter|Brian Radcliffe|Jared Brenner|Paula Yurkovitch|Rebecca Marshall"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(docketId, "FWS-HQ-NWRS-2012-0086") & str_dct(org, ".*") & !str_dct(title, "public comments"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(docketId, "FWS-R1-ES-2011-0112") & str_dct(org, ".*") & !str_dct(title, "public comments"), T, org.comment)) %>% 
  #to increase speed, check back for accuracy?
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & !str_dct(commenttext, "public comments") & agencyAcronym == "FWS", T, org.comment))



whatiswrong <- d %>% 
  select(documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(org.comment, ".*"))


#National Park Service
#NA org for NPS
d %<>% 
  mutate(org = ifelse(str_dct(organization, "\\d\\d\\d\\d$") & agencyAcronym == "NPS", NA, org)) %>% 
  mutate(org = ifelse(str_dct(org, "\\d$") & !str_dct(org, "black youth project") & agencyAcronym == "NPS", NA, org))
  

d %<>% 
  #finding false
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "as one of the 1.3 million|as one of 1.3 million|an one of the 1.3 million") & str_dct(commenttext, "national parks conservation association") & agencyAcronym == "NPS", F, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(organization, str_c("none", "unknown", "individual", "citizen", "self", "not applicable", "private", "personal", "lover", "mr.", "mrs.", "ms",
                                                                        "retired", "dr", "miss ", "mr ", sep = "|")), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I urge you not to adopt the proposed rule .* impacts on public safety") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "as a citizen") & agencyAcronym == "NPS", F, org.comment)) %>% 
  #need to keep these mass comments just to NPS because they do not have orgs associated 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I am writing to respectfully encourage you not to") & agencyAcronym == "NPS", F, org.comment)) %>% #none of these seem to have attached orgs?
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I am opposed to the National Park Services plan") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I strongly urge you not to adopt the proposed rule") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I am writing to oppose the amendments to regulations") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "As a concerned citizen, animal activist, and a human being") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "concerned citizen") & agencyAcronym == "NPS", F, org.comment)) %>% #appears no org writen in the text
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "november project") & str_dct(commenttext, "member"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Ryan Zinke,"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Im writing today to demand that the Department of Interior,"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I'm writing today to demand that the Department of Interior,"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I am opposed to these new rules that act to restrict and burden the ability"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I am writing to express my opposition to the National Park Service's proposed regulations"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(mass, "mass comments"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(str_dct(org, "David's Tent|Davids Tent") & !str_dct(documentId, "NPS-2018-0007-49456"), F, org.comment)) %>% 
  #finding true
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & str_dct(organization, ".*") & agencyAcronym == "NPS", T, org.comment)) %>% #might be overbroad 925 observations
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "november project") & str_dct(commenttext, "co-leader"), T, org.comment))
  #leaving the rest as n/a, don't seem to have an associated org or org.comment 
  

na <- d %>% 
  select(docketId, documentId, mass, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(is.na(org.comment))

test <- d %>% 
  select(docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(commenttext, "I am writing to express my opposition to the National Park Service's proposed regulations"))

#NPS-2018-0007, NPS-2014-0004, NPS-2015-0006
docketTestTRUE <- d %>% 
  select(docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(docketId, "NPS-2018-0007"), org.comment == T)

docketTest <- d %>% 
  select(docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(docketId, "NPS-2018-0005"), is.na(org.comment))

false <- d %>% 
  select(documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(org.comment == F)

true <- d %>% 
  select(documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(org.comment == T)


##############################################
#Finding org.comment by docket

unique(d$docketId)

docketTEST <- d %>% 
  select(docketId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(docketId, "FWS-R7-NWRS-2014-0005"), org.comment == T) %>% 
  count(org, .drop = FALSE) %>% 
  arrange(-n)

docket <- d %>% 
  group_by(org.comment, docketId, .drop = F) %>%
  summarize(n=n()) %>% 
  arrange(docketId)

#count by org.comment and docket


docketTest <- d %>% 
  select(docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(docketId, "FWS-R1-ES-2011-0112"), org.comment == T)
         
         
         #is.na(org.comment))
#######################################################
  



#creating dataframe for org information, tribble
###################################################
#should be distinct if non profit?
orgInfo <- tribble(
  ~org, ~org.type, ~org.ej.community,
  "earthjustice", "ngo", "no", 
  "earthworks", "ngo", "no", 
  "riverkeeper", "ngo", "no", 
  "sierra club", "ngo", "no", 
  "black warrior riverkeeper", "ngo", "no",
  "cattahoochee riverkeeper", "ngo", "no",
  "hackensack riverkeeper", "ngo", "no", 
  "350", "ngo", "no", 
  "oregon wild", "ngo", "no",
  "defenders of wildlife", "ngo", "no",
  "conservation northwest", "ngo", "no",
  "center for biological diversity", "ngo", "no",
  "endangered species coalition", "ngo", "no", 
  "national parks conservation association", "ngo", "no"
)

#these are in addition to our official organization comments 

#join orgInfo into orginial dataset
###################################################
d %<>% 
  left_join(orgInfo) %>% 
  select(agencyAcronym, title, commenttext, organization, org.comment, org, org.type, org.ej.community)
  #filter(grepl("earthjustice", org, ignore.case = TRUE))


  
#Observations per docket for position
#####################################

#creating position variable
d %<>% 
  mutate(position = NA) %>% 
  #putting turtle species on international trade list, not org.comment
  mutate(position = ifelse (str_dct(documentId, "FWS-HQ-ES-2013-0052-0013"), "1", position)) %>% 
  mutate(position = ifelse (str_dct(documentId, "FWS-HQ-ES-2013-0052-0010"), "3", position)) %>% 
  mutate(position = ifelse (str_dct(documentId, "FWS-HQ-ES-2013-0052-0016"), "2", position)) %>% 
  #listing white rhino as threatened
  mutate(position = ifelse (str_dct(documentId, "FWS-HQ-ES-2013-0055-0577"), "3", position)) %>% 
  mutate(position = ifelse (str_dct(documentId, "FWS-HQ-ES-2013-0055-0580"), "2", position)) %>% 
  #reclassification of african elephant to endangered
  mutate(position = ifelse (str_dct(documentId, "FWS-HQ-ES-2016-0010-1483"), "1", position)) %>% 
  mutate(position = ifelse (str_dct(documentId, " FWS-HQ-ES-2016-0010-0446"), "3", position)) %>% 
  #pangolin
  mutate(position = ifelse (str_dct(documentId, "FWS-HQ-ES-2016-0012-0008"), "2", position)) %>% 
  #Ivory
  mutate(position = ifelse (str_dct(documentId, "FWS-HQ-IA-2013-0091-0817"), "4", position)) %>% 
  mutate(position = ifelse (str_dct(documentId, "FWS-HQ-IA-2013-0091-5613"), "1", position)) %>% 
  mutate(position = ifelse (str_dct(documentId, "FWS-HQ-IA-2013-0091-5613"), "1", position)) %>% 
  mutate(position = ifelse (str_dct(documentId, "FWS-HQ-IA-2013-0091-5719"), "2", position)) %>% 
  #regulations governing non-federal oil and gas development 
  mutate(position = ifelse (str_dct(documentId, "FWS-HQ-NWRS-2012-0086-0032"), "5", position)) %>% #confirm?
  #spotted owl
  mutate(position = ifelse (str_dct(documentId, "FWS-R1-ES-2011-0112-0882"), "1", position)) %>% 
  #hunting wildlife in Alaska
  mutate(position = ifelse (str_dct(documentId, "NPS-2018-0005-117264"), "1", position)) %>% 
  mutate(position = ifelse (str_dct(documentId, "NPS-2018-0005-116315"), "1", position)) %>% 
  mutate(position = ifelse (str_dct(documentId, "NPS-2018-0005-175662"), "3", position)) %>% #not org but pro
  #demonstration regulations
  mutate(position = ifelse (str_dct(documentId, "NPS-2018-0007-49456"), "1", position)) %>% 
  mutate(position = ifelse (str_dct(documentId, "NPS-2018-0007-49527"), "1", position)) %>% 
  











  


  

  
  


  
  




#Testing
##########################
example <- d %>% 
  select(documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(is.na(org.comment))
  
  
  count(org) %>% 
  arrange(-n)
  #find out large numbers of seen organizations

NPS <- d %>% 
  select(documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(org.comment == F)
    
#Test for org.comment
org.comment <- d %>% 
  select(docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, organization, org.comment, org, text_clean_short) %>% 
  filter(numberOfCommentsReceived > 20)



org.comment1 <- d %>% 
  select(docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(organization, "\\d\\d\\d\\d$"))
#these are in addition to our official

Docket <- d %>% 
  select(docketId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, congress, org.comment, org, submitterName) %>% 
  filter(grepl("EPA-HQ-OAR-2018-0283", docketId, ignore.case = TRUE) & is.na(org.comment) & is.na(congress))


true <- d %>% 
  select(docketId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(org.comment == T, attachmentCount == 0)
  




######################################################################################################################################################################################




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






#change


#Notes
###########################################################

#test for missing orgs not the unknown
showme <- d %>% 
  select(docketId, attachmentCount, agencyAcronym, title, commenttext, organization,org)

testUnknown <- d %>% 
  select(docketId, attachmentCount, agencyAcronym, title, commenttext, organization, org, numberOfCommentsReceived) %>% 
  filter(!grepl("unknown", title, ignore.case = TRUE), is.na(organization), is.na(org))

test <- d %>% 
  select(docketId, attachmentCount, agencyAcronym, title, commenttext, organization, org) %>% 
  filter(is.na(organization), is.na(org))


#running smaller test for speciifc rules 
test1 <- d %>% 
  select(docketId, attachmentCount, agencyAcronym, title, commenttext, organization, org) %>% 
  filter(grepl("Animal Rescute Site", title, ignore.case = TRUE), is.na(organization))

test2 <- d %>% 
  select(docketId, attachmentCount, agencyAcronym, title, commenttext, organization, org) %>% 
  filter(grepl("school", org, ignore.case = TRUE))

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
#before leave whatever is an org.comment = T, save a filter down version


#make a word   filter(str_dct(title, "^\\w+$"))

