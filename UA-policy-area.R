
regs <- read.csv("Data/OIRA and UA.csv")

# consolidate ORIA and UA DATES
regs %<>% mutate(UnifiedAgendaDate = if_else(!is.na(DATE_RECEIVED), DATE_RECEIVED, UnifiedAgendaDate))
sum(is.na(regs$UnifiedAgendaDate))

# select one observation per RIN - WHY? 
regs %<>%
  dplyr::group_by(RIN) %>%
  dplyr::top_n(n = 1, UnifiedAgendaDate) %>%
  dplyr::ungroup()# %>%



# MOVE OR DELETE THIS #################################




##############################################################
########### THIS IS WHAT BELONGS IN THIS FILE ################

############# FIX THIS TO BE A VAR RATHER THAN SUBSET ##################
transportation <- regs[which(grepl("^21", regs$RIN)),]

ag <- regs[which(grepl("^05", regs$RIN)),]
                       
#commerce and treasury 
commerce <- regs[which(grepl("^06|^15", regs$RIN)),]
                       
defense <- regs[which(grepl("^07", regs$RIN)),]

edu <- regs[which(grepl("^18", regs$RIN)),]
                        
energy <- regs[which(grepl("^19", regs$RIN)),]

env <- regs[which(grepl("^20", regs$RIN)),]
env$type <- "Environmental"

govops <- regs[which(grepl("^23", regs$RIN)),]


# HHS and VA
health <- regs[which(grepl("^09|^29", regs$RIN)),]
                           
# State and International Trade Admin
foreign <- regs[which(grepl("^14|10070|^35100|^0625", regs$RIN)),]

#Labor
labor <- regs[which(grepl("^12", regs$RIN)),]
                          
justice <- regs[which(grepl("^11", regs$RIN)),]

# interior and Forest Service 
lands <- regs[which(grepl("^10|^0596", regs$RIN)),]

welfare <- regs[which(grepl("^16|^0938", regs$RIN)),]

civilrights <- regs[which(grepl("^0507|^2529|^35000", regs$RIN)),]

social=rbind(edu, health, labor, justice, welfare, civilrights)
social$type <- "Social Policy"

economic <- rbind(transportation, ag, commerce, energy, lands) 
economic$type <- "Economic Policy"

foreign <- rbind(defense, foreign) 
foreign$type <- "Foreign Policy"

regtype <- rbind(social, economic, foreign, env)







#############################################
######### TIMELINES #####################

# pick an agency
d <- transportation

# UA all 
class(d$WithdrawalPublished)
class(d$enddate)
d$enddate<-as.Date(d$UnifiedAgendaDate)
d$WithdrawalPublished<-as.Date(d$WITHDRAWAL)

ggplot(d, aes(x=RIN)) +
  ggtitle('Significant DOT Rulemaking Projects 1981-2016 (Unified Agenda, N = 4,888)')+ 
  xlab("RIN")+
  ylab("Year")+
  #facet_grid(agency ~ .) +
  #geom_linerange(aes(ymin=initiated, ymax=enddate, color="black"), size=.1) +
  geom_point(aes(y=enddate, color="black"), shape = 15, size=1) + 
  geom_point(aes(y=WithdrawalPublished, color="red"), shape = 4, size=.5) + 
  coord_flip() +  
  scale_y_date(lim = c(as.Date("1981-08-11"), as.Date("2016-06-01")),
               breaks=date_breaks(width = "2 year"), labels = date_format("%y"),
               minor_breaks=NULL)+
  scale_color_identity("",guide="legend",
                       labels=c("Rule Published",
                                "Rule Withdrawn")) +
  theme(axis.text.y =
          element_text(size  =2,
                       angle = 0,
                       hjust = 1,
                       vjust = 1)) 

length(transportation$RIN)




##############################################################

# POTTER DATA (I THINK)
ggplot(x %>% filter(grepl("^21", RIN))) +
  ggtitle('Significant DOT Rulemaking Projects 1994-2014 (N = 21,387)') + 
  xlab("RIN")+
  ylab("Months")+
  #facet_grid(agency ~ .) +
  #geom_linerange(aes(ymin=initiated, ymax=enddate, color="black"), size=.1) +
  geom_point(aes(x=RIN, y=time), shape = 15, size=.5, color="black") + 
  #geom_point(aes(y=WithdrawalPublished, color="red"), shape = 4, size=1) + 
  coord_flip() +  
  theme(axis.text.y =
          element_text(size  =2,
                       angle = 0,
                       hjust = 1,
                       vjust = 1)) 

