source("setup.R")

#load(here("ascending/allcomments.Rdata"))
load(here("data/allcomments.Rdata"))
load(here("data/masscomments.Rdata"))
load(here("data/comment_text_short.Rdata"))

#subsetting data from 'all' with matching docketId in 'mass'
#saving
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

str_ext <- function(string, pattern) {
  str_extract(string, regex(pattern, ignore_case = TRUE))
}
## A sample of high-profile rules
#load(here("data/masscomments.Rdata"))
#load(here("data/toporgs.RData"))

##LUCY
##Load in top org sample 
#load(here("data/topdockets.RData"))

#searching through EPA 
#d <- topdockets %>% filter(agencyAcronym == "EPA")

d <- d %>% filter(agencyAcronym == "CFPB")
#looking through docket after
#group by docket, orgname
#summarize org.comment

#checking topdockets, 5 top dockets 
unique(d$docketId)

#FIXME
#d <- mass %>% filter(agencyAcronym == "EPA")



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
# "IRS"   "CFPB"  "NOAA"  "OTS"   "HHS"   "USCIS" "CMS"   "ED"    "DOD"   "ETA"   "BLM"   "FNS"   "FAA"   "ATF"   "DOI"  
#[26] "OCC"   "EBSA"  "SSA"   "BSEE"  "OSM"   "BOEM"  "WHD"   "OMB"   "FEMA"  "FHWA"  "PHMSA" "BIA"   "OFCCP" "ACF"   "DOL"   "CDC"   "OPM"   "LMSO"  "CPSC"  "EEOC"  "MMS" 

#Notes: 
#############################
#completed agencies
#FWS #NPS #FDA #EERE #EPA #FDA #VA #OSHA

#messy agencies
#NHTSA #OSHA #HUD (CAN FIX) #NRC(all are marked in organization)
##############################

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

STARTOVER <- d
d <- STARTOVER


#Org variable
########################################################################################################
d %<>% 
  mutate(org = NA)

#broader rules
#############
d %<>% 
  #sponsored by
  mutate(org = ifelse(is.na(org) & grepl("sponsor... by [[:upper:]]", title, ignore.case = TRUE), 
                      str_rm_all(title, ".* sponsor... by|\\(.*"), 
                      org)) %>% 
  #association
  mutate(org = ifelse(is.na(org) & grepl(".*association|assn", title, ignore.case = TRUE) & !grepl("association of", title, ignore.case = TRUE), 
                      str_rpl(title, "association.*", "Association"), 
                      org)) %>% 
  #association of
  mutate(org = ifelse(is.na(org) & grepl(".*association of.*", title, ignore.case = TRUE), 
                      str_rpl(title, ".*association", "Association"), 
                      org)) %>% 
  #company
  mutate(org = ifelse(is.na(org) & grepl(".*company", title, ignore.case = TRUE), 
                      str_rpl(title, "company.*", "Company"), 
                      org)) %>%
  #conservancy
  mutate(org = ifelse(is.na(org) & grepl(".*conservancy", title, ignore.case = TRUE), 
                      str_rpl(title, ".*company", "conservancy"), 
                      org)) %>%
  #department
  mutate(org = ifelse(is.na(org) & grepl("department", title, ignore.case = TRUE), 
                      str_ext(title, ".*department.*"), 
                      org)) %>%
  #center
  mutate(org = ifelse(is.na(org) & grepl(".*Center", title, ignore.case = TRUE), 
                      str_ext(title,"\\w+ center.*"), 
                      org)) %>%
  #community1
  mutate(org = ifelse(is.na(org) & grepl("community", title, ignore.case = TRUE) & !str_dct(title, "community$"), 
                      str_ext(title, "\\w+ community.*"), 
                      org)) %>%
  #community2
  mutate(org = ifelse(is.na(org) & grepl("community$", title, ignore.case = TRUE), 
                      str_ext(title, "\\w+ \\w+ \\w+ community"), 
                      org)) %>%
  #cooperative
  mutate(org = ifelse(is.na(org) & grepl(".*cooperative", title, ignore.case = TRUE), 
                      str_rpl(title, "cooperative.*", "Cooperative"), 
                      org)) %>% 
  #commission
  mutate(org = ifelse(is.na(org) & grepl(".*commision|commission", title, ignore.case = TRUE), 
                      str_rpl(title, "commision.*|commission.*", "Commission"), 
                      org)) %>% 
  #co. 
  mutate(org = ifelse(is.na(org) & grepl(".*Co\\.$", title, ignore.case = TRUE), 
                      str_rm(title, "comment from"), 
                      org)) %>% 
  #corp 
  mutate(org = ifelse(is.na(org) & grepl("corp|corp.", title, ignore.case = TRUE), 
                      str_rm(title, "comment from"), 
                      org)) %>% 
  #Inc. 
  mutate(org = ifelse(is.na(org) & grepl(".*Inc\\..*|.*Inc.*", title, ignore.case = TRUE), 
                      str_rm(title, "comment from"), 
                      org)) %>% 
  #LLP
  mutate(org = ifelse(is.na(org) & grepl(".*LLP", title, ignore.case = TRUE), 
                      str_rpl(title, "LLP.*", "LLP"), 
                      org)) %>% 
  #LLC
  mutate(org = ifelse(is.na(org) & grepl(".*LLC|LC", title, ignore.case = TRUE), 
                      str_rpl(title, "LLC.*", "LLC"), 
                      org)) %>% 
  #coalition
  mutate(org = ifelse(is.na(org) & grepl("coalition", title, ignore.case = TRUE), 
                      str_ext(title, "\\w+ \\w+ \\w+ coalition"), 
                      org)) %>% 
  #institute
  mutate(org = ifelse(is.na(org) & grepl("institute|insitute", title, ignore.case = TRUE), 
                      str_ext(title, ".*institute"), 
                      org)) %>% 
  #academy
  mutate(org = ifelse(is.na(org) & grepl("academy of", title, ignore.case = TRUE), 
                      str_ext(title, "academy of.*"), 
                      org)) %>% 
  #society1 
  mutate(org = ifelse(is.na(org) & grepl("society$", title, ignore.case = TRUE), 
                      str_ext(title,  "\\w+ \\w+ \\w+ society"), 
                      org)) %>% 
  #society2
  mutate(org = ifelse(is.na(org) & grepl("society", title, ignore.case = TRUE), 
                      str_ext(title,  "\\w+ society \\w+ \\w+"), 
                      org)) %>% 
  #alliance
  mutate(org = ifelse(is.na(org) & grepl("alliance", title, ignore.case = TRUE), 
                      str_ext(title, ".*alliance"), 
                      org)) %>% 
  #growers
  mutate(org = ifelse(is.na(org) & grepl("growers", title, ignore.case = TRUE), 
                      str_ext(title, "\\w+ \\w+ \\w+ growers"), 
                      org)) %>% 
  #council, could use fixing
  mutate(org = ifelse(is.na(org) & grepl("council", title, ignore.case = TRUE), 
                      str_ext(title, ".* council"), 
                      org)) %>% 
  #mass mailer campaign 
  mutate(org = ifelse(is.na(org) & grepl(".*mass mailer campaign", title, ignore.case = TRUE), 
                    str_rm(title, "mass mailer campaign.*"), 
                    org)) %>% 
  #member mass email
  mutate(org = ifelse(is.na(org) & grepl(".*member mass email", title, ignore.case = TRUE), 
                      str_rm(title, "member mass email.*"), 
                      org)) %>% 
  #comment from
  mutate(org = ifelse(is.na(org) & grepl("comment from", title, ignore.case = TRUE) & agencyAcronym == "EPA", 
                      str_rm(title, ".*comment from"), 
                      org)) %>% 
  #schools
  mutate(org = ifelse(is.na(org) & grepl(".*school", title, ignore.case = TRUE), 
                    str_rpl(title, "school.*", "School"), 
                    org)) %>% 
  #farms
  mutate(org = ifelse(is.na(org) & grepl(".*farm", title, ignore.case = TRUE), 
                      str_ext(title, "\\w+ farm.*"), 
                      org)) %>%
  #federation1
  mutate(org = ifelse(is.na(org) & grepl("federation$", title, ignore.case = TRUE), 
                      str_ext(title, "\\w+ \\w+ \\w+ federation"), 
                      org)) %>% 
  #federation2
  mutate(org = ifelse(is.na(org) & grepl("federation", title, ignore.case = TRUE), 
                      str_ext(title, "\\w+ federation.*"), 
                      org)) %>% 
  #church
  mutate(org = ifelse(is.na(org) & grepl(".*church", title, ignore.case = TRUE), 
                    str_rpl(title, "church.*", "Church"), 
                    org)) %>% 
  #partnership
  mutate(org = ifelse(is.na(org) & grepl("partnership", title, ignore.case = TRUE), 
                      str_ext(title, ".*partnership"), 
                      org)) %>% 
  #partnership
  mutate(org = ifelse(is.na(org) & grepl("union", title, ignore.case = TRUE), 
                      str_ext(title, ".*union.*"), 
                      org)) %>% 
  #public health
  mutate(org = ifelse(is.na(org) & grepl("public health", title, ignore.case = TRUE), 
                      str_ext(title, ".*public health.*"), 
                      org)) %>% 
  #foundation
  mutate(org = ifelse(is.na(org) & grepl("foundation", title, ignore.case = TRUE), 
                      str_ext(title, ".*foundation.*"), 
                      org)) %>% 
  #foundation
  mutate(org = ifelse(is.na(org) & grepl("board of", title, ignore.case = TRUE), 
                      str_ext(title, "\\w+ board of.*"), 
                      org)) %>% 
  #university
  mutate(org = ifelse(is.na(org) & grepl("university", title, ignore.case = TRUE), 
                      str_ext(title, "\\w+ \\w+ \\w+ university"), 
                      org)) %>% 
  #city of
  mutate(org = ifelse(is.na(org) & grepl("city of", title, ignore.case = TRUE), 
                      str_ext(title, ".*city of.*"), 
                      org)) %>% 
  #city of
  mutate(org = ifelse(is.na(org) & grepl("commerce", title, ignore.case = TRUE), 
                      str_ext(title, ".*commerce.*"), 
                      org)) %>% 
  #names
  mutate(org = ifelse(is.na(org) & grepl(".*, .*president, .*, .*", title, ignore.case = TRUE), 
                      str_rm(title, ".*, .*president, .*,"), 
                      org)) %>% 
  #names2
  mutate(org = ifelse(is.na(org) & grepl(".*, .*president,.*", title, ignore.case = TRUE), 
                    str_rm(title, ".*, .*president,"), 
                    org)) %>%
  #names3
  mutate(org = ifelse(is.na(org) & grepl("president,.*", title, ignore.case = TRUE), 
                      str_rm(title, ".*president,"), 
                      org)) %>% 
  #testimony from
  mutate(org = ifelse(is.na(org) & grepl("testimony from.*", title, ignore.case = TRUE), 
                      str_rm(title, "testimony from"), 
                      org)) %>% 
  #request for an extension
  mutate(org = ifelse(is.na(org) & grepl("request for extension from .*", title, ignore.case = TRUE), 
                      str_rm(title, "request for extension from"), 
                      org)) %>% 
  #bring over organization to org
  mutate(org = ifelse(is.na(org), organization, org))



#Specific Cases
###############

#docket
d %<>% 
  #Western Energy Alliance, The Independent Petroleum Association of America, the American Exploration & Production Council
  mutate(org = ifelse(is.na(org) & grepl("NPS-2015-0006-0008", documentId, ignore.case = TRUE), 
                      "American Petroleum Institute, Western Energy Alliance, The Independent Petroleum Association of America, the American Exploration & Production Council", 
                      org)) %>% 
  #National Parks Conservation Association, the Natural Resources Defense Council, The Wilderness Society, Sierra Club, Park Rangers for Our Lands, the Ohio Environmental Council
  mutate(org = ifelse(is.na(org) & grepl("NPS-2015-0006-0015", documentId, ignore.case = TRUE), 
                      "National Parks Conservation Association; the Natural Resources Defense Council; The Wilderness Society; Sierra Club; Park Rangers for Our Lands; the Ohio Environmental Council", 
                      org)) %>% 
  #Trustees for Alaska, National Parks Conservation Association, Denali Citizens Council, Center for Biological Diversity, The Wilderness Society, Defenders of Wildlife, Copper Country Alliance, Northern Alaska Environmental Center, Alaska Center for the Environment, Natural Resources Defense Council, and Audubon Alaska
  mutate(org = ifelse(is.na(org) & grepl("NPS-2015-0006-0015", documentId, ignore.case = TRUE), 
                      "Trustees for Alaska; National Parks Conservation Association; Denali Citizens Council; Center for Biological Diversity; The Wilderness Society; Defenders of Wildlife; Copper Country Alliance; Northern Alaska Environmental Center; Alaska Center for the Environment; Natural Resources Defense Council; Audubon Alaska", 
                      org)) %>% 
  #Central Sierra Environmental Resource Center
  mutate(org = ifelse(is.na(org) & str_dct(documentId, "NPS-2014-0004-1150"), 
                      "Central Sierra Environmental Resource Center", 
                       org)) %>% 
  mutate(org = ifelse(is.na(org) & str_dct(documentId, "NPS-2014-0004-1159"), 
                      "Central Sierra Environmental Resource Center", 
                      org)) %>% 
  mutate(org = ifelse(is.na(org) & str_dct(documentId, "NPS-2014-0004-1163"), 
                      "Central Sierra Environmental Resource Center", 
                      org)) %>% 
  #the state of utah
  mutate(org = ifelse(is.na(org) & str_dct(documentId, "NPS-2015-0006-0018"), 
                    "The State of Utah", 
                    org))
  
#title
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
                      org)) %>% 
mutate(org = ifelse(is.na(org) & grepl("festival foods", title, ignore.case = TRUE), 
                      "Festival Foods", 
                      org)) %>% 
mutate(org = ifelse(is.na(org) & str_dct(title, "Union of Concerned Scientists"), 
                    "union of concerned scientists",
                     org)) %>% 
mutate(org = ifelse(is.na(org) & str_dct(title, "Kwik Trip"), 
                      "Kwik Trip",
                      org)) %>% 
mutate(org = ifelse(is.na(org) & str_dct(title, "ChangeLab Solutions"), 
                      "ChangeLab Solutions",
                      org)) %>% 
mutate(org = ifelse(is.na(org) & str_dct(title, "National Congress of American Indians"), 
                      "National Congress of American Indians",
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
  mutate(org = ifelse(is.na(org) & grepl(".*Public Citizen", commenttext, ignore.case = TRUE), 
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
  mutate(org = ifelse(is.na(org) & grepl("written by Trustees for Alaska", commenttext, ignore.case = TRUE), "Trustees for Alaska", org)) %>% 
  #National Parks Conservation Association
  mutate(org = ifelse(is.na(org) & grepl("National Parks Conservation Association", commenttext, ignore.case = TRUE), "National Parks Conservation Association", org)) %>% 
  #Catharsis on the Mall
  mutate(org = ifelse(is.na(org) & grepl("catharsis on the mall", commenttext, ignore.case = TRUE), "Catharsis on the Mall", org)) %>% 
  #The November Project
  mutate(org = ifelse(is.na(org) & grepl("november project", commenttext, ignore.case = TRUE), "November Project", org)) %>% 
  #The Board of Directors of the Society for Ethnomusicology
  mutate(org = ifelse(is.na(org) & grepl("The Board of Directors of the Society for Ethnomusicology", commenttext, ignore.case = TRUE), "The Board of Directors of the Society for Ethnomusicology", org)) %>% 
  #Doyon, Limited
  mutate(org = ifelse(is.na(org) & grepl("Doyon, Limited", commenttext, ignore.case = TRUE), "Doyon, Limited", org)) %>% 
  #Collier Resources Company
  mutate(org = ifelse(is.na(org) & grepl("Collier Resources", commenttext, ignore.case = TRUE), "Collier Resources", org)) %>% 
  #central sierra environmental resource center
  mutate(org = ifelse(is.na(org) & str_dct(commenttext, "Central Sierra Environmental Resource Center|CSERC"), "Central Sierra Environmental Resource Center", org)) %>% 
  #human society 
  mutate(org = ifelse(is.na(org) & str_dct(commenttext, "this is the humane society"), "Humane Society of the United States", org)) %>% 
  #congressional sportmen's foundation
  mutate(org = ifelse(is.na(org) & str_dct(commenttext, "Congressional Sportsmen's Foundation"), "Congressional Sportsmen's Foundation", org)) %>% 
  #Alaska Wilderness League 
  mutate(org = ifelse(is.na(org) & str_dct(commenttext, "Alaska Wilderness League"), "Alaska Wilderness League", org))
  #Union of Concerned Scientists

temp <- d
d <- temp 


na <- d %>% 
  select(mass, docketId, documentId, mass, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(is.na(org.comment), str_dct(org, ".*"))
#org
##############################
#mutate org to lower
##org is who your mobilized by
d %<>% 
  mutate(org = tolower(org)) %>% 
  mutate(org = str_remove(org, "^comment from|^from|^re |- comment$|-comment$|comment$|^request for extension|^request for an extension")) %>% 
  mutate(org = ifelse(str_detect(org, str_c("^none$", "^unknown$", "^individual$", "citizen$", "self$", "not applicable", "^private$", "personal", "lover", "mr.$", "mrs.$", "ms$",
                                         "retired", "dr$", "miss$", "mr$", "ms.$", "mr.$", "^na$", "^me$", "^-$|^--$", "street$", "^happy$", "^r$", "^home$", "please select", "^brain$", "^no name$",
                                         "no one$", "^nol longer", "no organization$", "- select -", "- none - ","--none--", "concerned citizen$", "-none-", "select...", "send$", "^love$", "^n.a.$",
                                         "^\\.$", "not specified", "^other$", "foekf", "what a shame this is", "no affiliation", "^usa$", "^LFJJHK$", "None. Plain ol' concerned citizen.", "anonymous anonymous",
                              sep = "|")), NA, org))


#congress
#############################
#create congress variable
d %<>%
  mutate(congress = NA) %>% 
  mutate(congress = ifelse(str_dct(title, "submitted by.*Senator|letter from.*senator"), T, congress)) %>% 
  mutate(congress = ifelse(str_dct(title, "submitted by.*representative|house of representatives|house of representative"), T, congress)) %>% 
  mutate(congress = ifelse(str_dct(documentId, "NPS-2015-0006-0003"), T, congress))
  
  

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
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment from") & is.na(org) & agencyAcronym == "FWS", F, org.comment)) %>% 
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


#National Park Service
#NA org for NPS
d %<>% 
  mutate(org = ifelse(str_dct(organization, "\\d\\d\\d\\d$") & agencyAcronym == "NPS", NA, org)) %>% 
  mutate(org = ifelse(str_dct(org, "\\d$") & !str_dct(org, "black youth project") & agencyAcronym == "NPS", NA, org))
  

d %<>% 
  #finding false
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(mass, "mass"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "as one of the 1.3 million|as one of 1.3 million|an one of the 1.3 million") & str_dct(commenttext, "national parks conservation association") & agencyAcronym == "NPS", F, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(organization, str_c("none", "unknown", "individual", "^citizen$", "self", "not applicable", "private", "personal", "lover", "mr.", "mrs.", "ms",
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
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Ryan Zinke,") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Im writing today to demand that the Department of Interior,") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I'm writing today to demand that the Department of Interior,") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I am opposed to these new rules that act to restrict and burden the ability") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I am writing to express my opposition to the National Park Service's proposed regulations") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I urge you not to adopt the proposed rule regarding recreational hunting") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Joel Hard, Deputy Regional Manager") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Support the NPS for rejecting State hunting rules that are inappropriate and unethical for National Preserves.") & agencyAcronym == "NPS", F, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Killing predators to increase game animal populations for hunters is inappropriate") & agencyAcronym == "NPS", F, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Killing predators to increase game animal populations for hunters is inappropriate") & agencyAcronym == "NPS", F, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "is to be commended") & agencyAcronym == "NPS", F, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "carnivores such as bears, wolves") & agencyAcronym == "NPS", F, org.comment)) %>%
  #finding true
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & str_dct(organization, ".*") & agencyAcronym == "NPS", T, org.comment)) %>% #might be overbroad 925 observations
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "november project") & str_dct(commenttext, "co-leader"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "on behalf of itself"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "the state of utah appreciates the opportunity"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "please accept the attached comments"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "on behalf of .* CSERC"), T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "attached.* comments"), T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(documentId, "NPS-2015-0006-0003"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(documentId, "NPS-2015-0006-0007"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(documentId, "NPS-2015-0006-0009"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(documentId, "NPS-2014-0004-1150"), T, org.comment))

#FDA
d %<>%
  #false
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "write-in campaign|write in campaign"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "multiple signatures"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "anonymous|Comment from Anonymous"), F, org.comment)) %>% 
  #true
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "Inc\\.$"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "Association$"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "Co\\.$"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "testimony from"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "request for extension"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "director|president|attorney"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "farm$"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "\\(") & agencyAcronym == "FDA", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "FDA", T, org.comment)) %>% 
  #specific true
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "National Association of County and City Health Officials") & agencyAcronym == "FDA", T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "Delaware Division of Public Health") & agencyAcronym == "FDA", T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "Michigan Department of Community Health") & agencyAcronym == "FDA", T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "Arizonans for Non-Smokers' Rights") & agencyAcronym == "FDA", T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "California Canning Peach Association") & agencyAcronym == "FDA", T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "George Washington University Regulatory Studies Center") & agencyAcronym == "FDA", T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "Snohomish County Children's Commission") & agencyAcronym == "FDA", T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "Center for Science in the Public Interest") & agencyAcronym == "FDA", T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "Campbell Soup Company") & agencyAcronym == "FDA", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "California Date Commission") & agencyAcronym == "FDA", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "UCSF-UC Hastings Consortium on Law, Science, and Health Policy") & agencyAcronym == "FDA", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "ILSI North America") & agencyAcronym == "FDA", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "California Strawberry Commission") & agencyAcronym == "FDA", T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "Citizens for Health") & agencyAcronym == "FDA", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "International Scientific Association for Probiotics and Prebiotics") & agencyAcronym == "FDA", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "International Food Additives Council") & agencyAcronym == "FDA", T, org.comment)) %>% 
#false
  mutate(org.comment = ifelse(is.na(org.comment) & attachmentCount == 0 & agencyAcronym == "FDA", F, org.comment))

#VA
d %<>%
  #false
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitter|comment submission") & is.na(org), F, org.comment)) %>% 
  #true
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "VA", T, org.comment))

#IRS
d %<>%
  #false
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, str_c("No public comment", "No comment submitted", "Withdraw needs additional information", "Explicit language",
                                                                        "incorrect information", "should be posted as a comment", "No comment public", "wrong Regulation", 
                                                                        "Incorrect information", "No attachment", "Removed due to language", "This public comment was redacted",
                                                                        "Posted to incorrect docket.", "incomplete information", 
                                                                                     sep = "|")), F, org.comment))
#CFPB
d %<>%
  #false
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "anonymous anonymous"), F, org.comment))

  



#filling in org after org.comment
d %<>%
  mutate(org = ifelse(is.na(org) & org.comment == T & str_dct(title, "\\("), 
                      str_ext(title, "\\w+ \\w+ \\w+ \\w+ \\("), org)) %>% 
  mutate(org = ifelse(str_detect(org, str_c("\\($",
                                            sep = "|")), NA, org))

  
  
#TESTING  
##########

unique(d$docketId)

na <- d %>% 
  select(mass, docketId, documentId, mass, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(is.na(org.comment), str_dct(org, ".*"))

true <- d %>% 
  select(mass, docketId, documentId, mass, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org, congress) %>% 
  filter(org.comment == T)

false <- d %>% 
  select(mass, docketId, documentId, mass, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org, congress) %>% 
  filter(org.comment == F)

noName <- d %>% 
  select(mass, congress,docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(org.comment == T, is.na(org))

test <- d %>% 
  select(mass, docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(title, "anonymous anonymous"))


#PUT DOCKET OPTIONS HERE
docketTestTRUE <- d %>% 
  select(rin, docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(docketId, "OSHA-2007-0072"), org.comment == T)


docketTestYES <- d %>% 
  select(congress, docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(docketId, "OSHA-2007-0072"), org.comment == T, str_dct(commenttext, "support"))

docketTestNO <- d %>% 
  select(congress, docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(docketId, "OSHA-2007-0072"), org.comment == T, str_dct(commenttext, "oppose"))


false <- d %>% 
  select(mass, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(org.comment == F)

true <- d %>% 
  select(mass, docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(org.comment == T)
#incorrecly marked false





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
  "national parks conservation association", "ngo", "no", 
  "doyon, limited", "corp", "no", 
  "the state of utah", "state gov", "no", 
  "central sierra environmental resource center", "ngo", "no", 
  "collier resources", "corp", "no", 
  "west energy alliance", "", "no", #fix
  "the independent petroleum association of america", "corp group", "no", 
  "the america exploration & production council", "corp group", "no", 
  "congressional sportsmen's foundation", "ngo", "no", 
  "american petroleum institute", "corp group", "no", 
  "prevention insitute", "ngo", "yes" #claims health equity community work
  
)

#these are in addition to our official organization comments 

#join orgInfo into orginial dataset
###################################################
d %<>% 
  left_join(orgInfo)
  #select(agencyAcronym, title, commenttext, organization, org.comment, org, org.type, org.ej.community, everything())
  #filter(grepl("earthjustice", org, ignore.case = TRUE))


  
#Observations per docket for position
#####################################

#creating position variable
d %<>% 
  mutate(position = NA) %>% 
  #putting turtle species on international trade list, not org.comment
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FWS-HQ-ES-2013-0052-0013"), "1", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FWS-HQ-ES-2013-0052-0010"), "3", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FWS-HQ-ES-2013-0052-0016"), "2", position)) %>% 
  #listing white rhino as threatened
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FWS-HQ-ES-2013-0055-0577"), "3", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FWS-HQ-ES-2013-0055-0580"), "2", position)) %>% 
  #reclassification of african elephant to endangered
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FWS-HQ-ES-2016-0010-1483"), "1", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, " FWS-HQ-ES-2016-0010-0446"), "3", position)) %>% 
  #pangolin
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FWS-HQ-ES-2016-0012-0008"), "2", position)) %>% 
  #Ivory
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FWS-HQ-IA-2013-0091-0817"), "4", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FWS-HQ-IA-2013-0091-5613"), "1", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FWS-HQ-IA-2013-0091-5613"), "1", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FWS-HQ-IA-2013-0091-5719"), "2", position)) %>% 
  #regulations governing non-federal oil and gas development 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FWS-HQ-NWRS-2012-0086-0032"), "5", position)) %>% #confirm?
  #spotted owl
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FWS-R1-ES-2011-0112-0882"), "1", position)) %>% 
  #altering rules for hunting wildlife in Alaska, permitting aggressive take and harvest practices #check thwaw
  ####GONE THROUGH, #YES are the ones I have looked at
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "NPS-2018-0005-160808"), "2", position)) %>% #YES
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "NPS-2018-0005-78196"), "1", position)) %>% #read articles, can't read from just commenttext
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "NPS-2018-0005-175662"), "3", position)) %>% #not org but pro
  #demonstration regulations
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "NPS-2018-0007-49456"), "1", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "NPS-2018-0007-49527"), "1", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "NPS-2018-0007-7913"), "5", position)) %>% 
  #sport hunting and trapping, NPS does not adopt state of alaskas take and harvest practices
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "NPS-2014-0004-1163"), "2", position)) %>% #support preventions, also dont want hunting dogs
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "NPS-2014-0004-2345"), "1", position)) %>% 
  #proposed rule and EIS on the revision of governing non-federal oil and gas development within the boundaries of units of the national park system
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "NPS-2015-0006-0018"), "5", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "NPS-2015-0006-0015"), "1", position)) %>% #assumption
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "NPS-2015-0006-0008"), "3", position)) #assumption
  
#correct now on
d %<>% 
#EERE
  #new energy conservation standards for manufactured housing
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "EERE-2009-BT-BC-0021-0440"), "1", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "EERE-2009-BT-BC-0021-0174"), "5", position)) %>% 
  #conservation standards for refrigerated beverage vending machines 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "EERE-2013-BT-STD-0022-0052"), "3", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "EERE-2013-BT-STD-0022-0051"), "5", position)) %>%  #assumption
#FDA
  #Food Labeling; Revision of the Nutrition and Supplement Facts #split across administrations
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2012-N-1210-2019"), "", position)) %>% #opposed to the new changes made on an old docket in Trump
  #Standards for the Growing, Harvesting, Packing, and Holding of Produce for Human Consumption
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2011-N-0921-19154"), "5", position)) %>% #National Onion #supports the extension, doesn't support the docket
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2011-N-0921-1257"), "2", position)) %>% #National Onion #doesn't support
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2011-N-0921-1235"), "2", position)) %>% 
  #General and Plastic Surgery Devices: Restricted Sale, Distribution, and Use of Sunlamp Products 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2015-N-1765-4781"), "5", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2015-N-1765-1002"), "2", position)) %>% 
  #Current Good Manufacturing Practice and Hazard Analysis and Risk-Based Preventive Controls For Human Food 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2011-N-0920-1752"), "2", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2011-N-0920-1182"), "4", position)) %>% 
  #Food Labeling; Nutrition Labeling of Standard Menu Items in Restaurants and Similar Retail Food Establishments #split across administrations
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2011-F-0172-1649"), "", position)) %>% #opposed to the delays made in Trump
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2011-F-0172-0457"), "4", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2011-F-0172-2860"), "3", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2011-N-0920-1152"), "3", position)) %>% 
  #prohibiting the extralabel use of cephalosporin antimicrobial drugs in food-producing animals
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2008-N-0326-0286"), "2", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2008-N-0326-0255"), "4", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2008-N-0326-0166"), "4", position)) %>% 
  #EIS- Investigational Use of Oxitec OX513A Mosquitoes
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2014-N-2235-1358"), "oppose", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2014-N-2235-1200"), "support", position)) %>% 
  #EIS- Preliminary Finding of No Significant Impact For a Genetically Engineered Atlantic Salmon
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2011-N-0899-1218"), "oppose", position)) %>%
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2011-N-0899-0737"), "support", position)) %>% #only support example could find
  #Deeming Tobacco Products To Be Subject to the Federal Food, Drug, and Cosmetic Act
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2014-N-0189-60210"), "5", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2014-N-0189-72301"), "2", position)) %>% 
  #Supplemental Applications Proposing Labeling Changes for Approved Drugs and Biological Products
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2013-N-0500-0055"), "4", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2013-N-0500-0019"), "2", position)) %>% 
  #Nicotine Exposure Warnings and Child-Resistant Packaging for Liquid Nicotine, Nicotine-Containing E-Liquid(s), and Other Tobacco Products
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2015-N-1514-0008"), "4", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2015-N-1514-0133"), "3", position)) %>% 
  #Menthol in Cigarettes, Tobacco Products
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2013-N-0521-0397"), "5", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "FDA-2013-N-0521-0377"), "4", position)) %>% 
#VA
  #AP44- Proposed Rule - Advanced Practice Registered Nurses
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "VA-2016-VHA-0011-60042"), "2", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "VA-2016-VHA-0011-216807"), "4", position)) %>% 
#OSHA
  #Tracking of Workplace Injuries and Illnesses #split administration
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "OSHA-2013-0023-2031"), "2", position)) %>% #commenting on the newer repeal of improve tracking of workplace injuries
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "OSHA-2013-0023-0240"), "2", position)) %>% #commenting on the older improve tracking of workplace injuries
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "OSHA-2013-0023-1387"), "4", position)) %>% #commenting on the older improve tracking of workplace injuries
  #Occupational Exposure to Beryllium, commenting on new proposal that revokes 2017 provisions
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "OSHA-H005C-2006-0870-2093"), "2", position)) %>% 
  #Occupational Exposure to Crystalline Silica, face significant risk
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "OSHA-2010-0034-1964"), "2", position)) %>% 
  mutate(position = ifelse(is.na(position) & str_dct(documentId, "OSHA-2010-0034-2166"), "4", position))


  
  
  
  
 

  


  


  
  
  




positionCheck <- d %>% 
  select(docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(documentId, "NPS-2018-0005-175662"))


position <- d %>% 
  select(docketId, documentId, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization, org.comment, org) %>% 
  filter(str_dct(docketId, "NPS-2018-0005"), org.comment == T)
  







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

