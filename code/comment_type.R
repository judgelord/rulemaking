

#FIXME this is now comment_type

#congress
#############################
#create congress variable
d %<>%
  mutate(congress = NA) %>% 
  mutate(congress = ifelse(str_dct(org_name, "house of representatives|^senate"), T, congress)) %>% 
  mutate(congress = ifelse(str_dct(org_name, "Congress of the United States"), T, congress)) %>%
  mutate(congress = ifelse(str_dct(org_name, "congress"), T, congress)) %>% 
  mutate(congress = ifelse(str_dct(title, "submitted by.*Senator|letter from.*senator"), T, congress)) %>% 
  mutate(congress = ifelse(str_dct(title, "submitted by.*representative|house of representatives|house of representative"), T, congress)) %>% 
  mutate(congress = ifelse(str_dct(document_id, "NPS-2015-0006-0003"), T, congress))


#create variable org.comment 
#coding congress for true
#coding listed as false
d %<>%
  mutate(org.comment = NA) %>% 
  mutate(org.comment = ifelse(congress == T, T, org.comment)) %>% 
  mutate(org.comment = ifelse(str_dct(organization, str_c("^none$", "^unknown$", "^individual$", "^citizen$", "self$", "not applicable", "^private$", "personal", "lover", "^mr\\.$", "^mrs\\.$", "^ms$", 
                                                          "retired", "^dr$", "^miss$", "^mr$", "^ms\\.$", "^mr\\.$", "^na$", "^me$", "^-$|^--$", "street$", "^happy$", "^r$", "^home$", "please select", "^brain$", "^no name$",
                                                          "^no one$", "^nol longer", "no organization$", "- select -", "- none - ","--none--", "concerned citizen$", "-none-", "select\\.\\.\\.", "send$", "^love$", "^n\\.a\\.$",
                                                          "^\\.$", "not specified", "^other$", "foekf", "what a shame this is", "no affiliation", "^usa$", "^LFJJHK$", "none\\. plain ol' concerned citizen.", "anonymous anonymous",
                                                          "the federal government institute", "individual taxpayer", "citizen of the united states|citizen of the US", "public land owner", "^taxpayer", "american citizen",
                                                          "^U\\.S\\. citizen$", "^unknown", "^farmer$", "^f u$", "^no$", "\\?", 
                                                          sep = "|")), F, org.comment))

#mass comment campaign results in false
d %<>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, str_c("mass comment campaign", "mass postcard campaign", "mass mail Campaign",
                                                                        "mass e-mail campaign", "Mass Mail Comment Campaign.", "mass e-mail/letter campaign",
                                                                        "mass e-mail and letter campaign", "mass paper campaign", "Mass Mail",
                                                                        "mass e-mail and postcard campaign","mass e-mail  and letter campaign",
                                                                        "Mass signature campaign", "mass Comment campaing", "Mass comment Campaingn",
                                                                        sep = "|")), F, org.comment))

#EPA 
d %<>% 
  #finding true 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by \\w+ \\w+") & str_dct(title, "director|CEO|president|manager|attorney|chief executive officer|executive coordinator") & attachmentCount >= 1, T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by") & str_dct(title, "natural resources defense council"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by \\w+ \\w+$") & agencyAcronym == "EPA" & attachmentCount > 1, T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by earthjustice"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by") & str_dct(title, "earthjustice") & str_dct(title,  str_c("lisa evans", "carrie apfel", "tyler smith",
                                                                                                                                                   "james pew", "jim pew", sep = "|")), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by") & str_dct(title, "earthworks"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by Sierra Club and Earthjustice"), T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by 350"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "corporation|inc\\.|co\\.|corp\\.|LLC|LLP|coalition|institute|association|alliance"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "joint comments"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "^.* submits"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "^.* submits"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, "city of|mayor"), T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "hereby submit"), T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "association comments"), T, org.comment)) %>%
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
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(organization, "[[:upper:]]\\.$") & agencyAcronym == "EPA" & attachmentCount <=1, F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & attachmentCount >= 50, F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by [[:alpha:]]\\. \\w+ \\w+"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by [[:alpha:]]\\. \\w+-\\w+"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitted by [[:alpha:]]\\. [[:alpha:]]\\. \\w+ \\w+$"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "please enter your comment here"), F, org.comment))


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
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & str_dct(title, "Submitted Electronically via eRulemaking Portal") & agencyAcronym == "FWS", T, org.comment))
#finding false
mutate(org.comment = ifelse(is.na(org.comment) & numberOfCommentsReceived > 20, F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment from") & is.na(org_name) & agencyAcronym == "FWS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "[[:upper:]]\\. \\w+$") & agencyAcronym == "FWS" & is.na(org_name), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "Submitted Electronically via eRulemaking Portal") & is.na(org_name), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "^\\w+$") & is.na(org_name) & agencyAcronym == "FWS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(organization, "none|concerned citizen"), F, org.comment)) %>% #this might need to change if they mention an org_name in text and its not captured in org
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
#NA org_name for NPS
d %<>% 
  mutate(org_name = ifelse(str_dct(organization, "\\d\\d\\d\\d$") & agencyAcronym == "NPS", NA, org_name)) %>% 
  mutate(org_name = ifelse(str_dct(org, "\\d$") & !str_dct(org, "black youth project") & agencyAcronym == "NPS", NA, org_name))

d %<>% 
  #finding false
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(mass, "mass"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "as one of the 1.3 million|as one of 1.3 million|an one of the 1.3 million") & str_dct(commenttext, "national parks conservation association") & agencyAcronym == "NPS", F, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I urge you not to adopt the proposed rule .* impacts on public safety") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "as a citizen") & agencyAcronym == "NPS", F, org.comment)) %>% 
  #need to keep these mass comments just to NPS because they do not have orgs associated 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I am writing to respectfully encourage you not to") & agencyAcronym == "NPS", F, org.comment)) %>% #none of these seem to have attached orgs?
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I am opposed to the National Park Services plan") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I strongly urge you not to adopt the proposed rule") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I am writing to oppose the amendments to regulations") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "As a concerned citizen, animal activist, and a human being") & agencyAcronym == "NPS", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "concerned citizen") & agencyAcronym == "NPS", F, org.comment)) %>% #appears no org_name writen in the text
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
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment submitter|comment submission") & is.na(org_name), F, org.comment)) %>% 
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

#ATF
d %<>%
  #false
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment on") & agencyAcronym == "ATF" & is.na(org_name), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & !str_dct(title, "comment on") & agencyAcronym == "ATF" & is.na(org_name), F, org.comment)) %>% 
  #true
  mutate(org.comment = ifelse(is.na(org.comment) & !str_dct(title, "comment on") & str_dct(org, ".*") & agencyAcronym == "ATF", T, org.comment))

#OCC
d %<>%
  #false
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & agencyAcronym == "OCC", F, org.comment)) %>% 
  #true
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "OCC", T, org.comment))

#BSEE, EBSA
d %<>%
  #false
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & str_dct(commenttext, "Dear Director Angelle"), F, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & str_dct(commenttext, "BPs Deepwater Horizon tragedy killed 11 people"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & str_dct(title, "comment on") & agencyAcronym == "BSEE", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & is.na(organization) & str_dct(title, "comment") & agencyAcronym == "EBSA", F, org.comment)) %>% 
  #true
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, "[A-z]") & str_dct(title, "[A-z]") & str_dct(title, org_name) & agencyAcronym == "EBSA", T, org.comment))

#LMSO, FHWA, BIA, OPM
d %<>%
  #false
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Human resource") & agencyAcronym == "LMSO", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I am writing to express my opposition to the Labor Department")& agencyAcronym == "LMSO", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I am writing to endorse the comments submitted to") & agencyAcronym == "FHWA", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & agencyAcronym == "FHWA", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "\\w+, \\w+$") & agencyAcronym == "BIA", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "\\w+, \\w+ \\w+$") & agencyAcronym == "BIA", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "[[:digit:]][[:digit:]] - \\w+$") & agencyAcronym == "BIA", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & is.na(organization) & agencyAcronym == "OPM", F, org.comment))

#PHMSA, CDC
d %<>%
  #true
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "PHMSA", T, org.comment)) %>% 
  #false
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I'm concerned that some of our most treasured nation") & agencyAcronym == "PHMSA", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Thank you for taking steps to better protect our neighborhood") & agencyAcronym == "PHMSA", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Dear U.S. Department of Transportation") & agencyAcronym == "PHMSA", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I have copied the summary, below, as disseminated") & agencyAcronym == "PHMSA", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "PHMSA,") & agencyAcronym == "PHMSA", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Im writing to express my grave concerns") & agencyAcronym == "PHMSA", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Im writing to express my concerns about") & agencyAcronym == "PHMSA", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I'm|Im |I am") & agencyAcronym == "PHMSA", F, org.comment))

#OFCCP, ACF, ETA, DOD, EEOC
d %<>%
  #true
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "OFCCP", T, org.comment)) %>% 
  #false
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "User error. No file attached."), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "withdrawn"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "Distinct comments on compensation tool submitted by AAUW") & agencyAcronym == "OFCCP", F, org.comment)) %>%  #how do i make it so one is different? check in on if even want that
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "It is outrageous that gender-based wage") & agencyAcronym == "OFCCP", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Please consider the administrative burden") & agencyAcronym == "OFCCP", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I wish to express my strong support for the Office") & agencyAcronym == "OFCCP", F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(organization, "not provided"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(title, "comment on") & is.na(org_name) & is.na(organization), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & agencyAcronym == "ETA", F, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & agencyAcronym == "DOD", F, org.comment)) %>% 
  #true
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "ACF", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "ETA", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "DOD", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "EEOC", T, org.comment))

#EBSA, OSM, DOI, MMS, OPM, LMSO, CPSC
#repetitive but going through each agency to make sure case applies
d %<>%
  #false
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & is.na(organization) & agencyAcronym == "EBSA" , F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & is.na(organization) & agencyAcronym == "DOI" , F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & is.na(organization) & agencyAcronym == "OSM" , F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & is.na(organization) & agencyAcronym == "MMS" , F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & is.na(organization) & agencyAcronym == "LMSO" , F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & is.na(organization) & agencyAcronym == "CPSC" , F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "I support President Obama and the Secretary of the Interior|I am a non-Hawaiian who strongly supports"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "Kkua the actions and hopes of 125,000|Ali'i Nui Mo'i \\(King/High Chief\\)"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, "email") & agencyAcronym == "DOI", F, org.comment)) %>% 
  #true
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "EBSA", T, org.comment)) %>%
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "OSM", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "MMS", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "OPM", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "CPSC", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & agencyAcronym == "LMSO", T, org.comment)) %>% #"casual" org_name comments(?)
  mutate(org.comment = ifelse(is.na(org.comment) & !str_dct(org, "email") & str_dct(org, ".*") & agencyAcronym == "DOI", T, org.comment))

#ED, SSA
d %<>%
  #false
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, "at pima medical institute"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, "with the"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, "staff at|attended|faculty"), F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & is.na(organization) & agencyAcronym == "ED" , F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & is.na(organization) & agencyAcronym == "SSA" , F, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(commenttext, "To Whom It May Concern") & agencyAcronym == "SSA", F, org.comment))

#NOAA
d %<>% 
  mutate(org.comment = ifelse(is.na(org.comment) & str_dct(org, ".*") & str_dct(organization, ".*") & agencyAcronym == "NOAA", T, org.comment)) %>% 
  mutate(org.comment = ifelse(is.na(org.comment) & is.na(org_name) & is.na(organization) & agencyAcronym == "NOAA", F, org.comment))




#creating dataframe for org_name information, tribble
###################################################
#should be distinct if non profit?
orgInfo <- tribble(
  ~org_name, ~org_type, ~org_ej,
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
  "west energy alliance", "corp group", "no", #check, starting as ngo?
  "the independent petroleum association of america", "corp group", "no", 
  "the america exploration & production council", "corp group", "no", 
  "congressional sportsmen's foundation", "ngo", "no", 
  "american petroleum institute", "corp group", "no", 
  "prevention insitute", "ngo", "yes", #claims health equity community work
  "gun owners of america", "ngo", "no"
)

#join orgInfo into orginial dataset
###################################################
#FIXME this will fail due to title case 
d %<>% 
  left_join(orgInfo)