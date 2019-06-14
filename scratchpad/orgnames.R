source("setup.R")


#load(here("ascending/allcomments.Rdata"))
load(here("data/allcomments.Rdata"))
load(here("data/masscomments.Rdata"))

#subsetting data from 'all' with matching docketId in 'mass'
d <- all %>% 
  filter(docketId %in% mass$docketId)

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

d <- d %>% filter(agencyAcronym == "FWS")
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
                      org))




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

#congress
#############################


#create congress variable
d %<>%
  mutate(congress = NA) %>% 
  mutate(congress = ifelse(str_dct(title, "submitted by.*Senator|letter from.*senator"), T, congress)) %>% 
  mutate(congress = ifelse(str_dct(title, "submitted by.*representative|house of representatives|house of representative"), T, congress))




#org.comment
##############################

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
none <- d %>% 
  select(docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(organization, "none"))

help <- d %>% 
  select(docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(title, "comment submitted by [[:uppercase:]]. [[:uppercase:]]. \\w+$"))
# 
 temp <- d
 d <- temp

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
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "[[:upper:]]\\. \\w+$") & agencyAcronym == "FWS" & str_dct(org, ".*"), T, org.comment)) %>% 
  #finding false
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment from") & is.na(org), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "[[:upper:]]\\. \\w+$") & agencyAcronym == "FWS" & is.na(org), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "Submitted Electronically via eRulemaking Portal") & is.na(org), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "^\\w+$") & is.na(org) & agencyAcronym == "FWS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(organization, "none"), F, org.comment))


  
  
#mutate org to lower
##org is who your mobilized by
d %<>% 
mutate(org = tolower(org)) %>% 
mutate(org = ifelse(org %in% c("none"), NA , org))


#creating dataframe for org information, tribble
orgInfo <- tribble(
  ~org, ~org.type, ~org.ej.community,
  "earthjustice", "ngo", "no", 
  "earthworks", "ngo", "no", 
  "riverkeeper", "ngo", "no", 
  "sierra club", "ngo", "no", 
  "black warrior riverkeeper", "ngo", "no",
  "cattahoochee riverkeeper", "ngo", "no",
  "hackensack riverkeeper", "ngo", "no", 
  "350", "ngo", "no"
)

#join orgInfo into orginial dataset
test <- d %>% 
  left_join(orgInfo) %>% 
  select(agencyAcronym, title, commenttext, organization, org.comment, org, org.type, org.ej.community) %>% 
  filter(grepl("earthjustice", org, ignore.case = TRUE))


  

  
# #testing in c formatting for while loop 
# while (org.comment == T)
# { 
#   mutate(org.name = org) %>% 
#   if(str_dct(title, "earthjustice")
#   {
#     mutate(org.type = "ngo" )
#     mutate(org.ej.community = "no")
#   }
# }
  



#Testing
##########################
example <- d %>% 
  select(documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(is.na(org.comment))%>% 
  count(organization) %>% 
  arrange(-n)
  #find out large numbers of seen organizations

  
    
#Test for org.comment
org.comment <- d %>% 
  select(docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(organization, "none"))

org.comment1 <- d %>% 
  select(docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(title, "Comment on FR Doc # 2011-21556"))

Docket <- d %>% 
  select(docketId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, congress, org.comment, org, submitterName) %>% 
  filter(grepl("EPA-HQ-OAR-2018-0283", docketId, ignore.case = TRUE) & is.na(org.comment) & is.na(congress))


true <- d %>% 
  select(docketId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(org.comment == T, attachmentCount == 0)
  
#need to make code to keep names without anything after
#needs to be after text
d %<>% 
  mutate(org.comment = ifelse(is.na(org) & grepl("comment submitted by [[:upper:]]\\w$", title, ignore.case = TRUE), F, org.comment))

str_detect("comment submitted by \\w \\w$")

association <- d %>% 
  select(documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(grepl("association,", title, ignore.case = TRUE))

#Finding org names


#Test for position
data %<>%
  mutate(position = NA) %>% 
  mutate(position = ifelse(is.na(position) & grepl(".*Audobon California.*", commenttext, ignore.case = TRUE), "Audobon California", org))


# position <- d %>% 
#   select(documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
#   filter(grepl("junk science", commenttext, ignore.case = TRUE))

#Finding org



unique(d$docketId)


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

