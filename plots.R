
# load packages
library(here)
source(here("setup.R"))

library(gridExtra) 

# load all regulations.gov rules
load(here("data/AllRegsGovRules.Rdata"))






# d %<>% select(-postedDate)
d %<>% unique()
d %<>% filter(!is.na(documentType), documentType != "NA")


# vars 
names(d)

# format as date
d$commentDueDate %<>% as.Date()
d$postedDate %<>% as.Date()
d$commentStartDate %<>% as.Date()


d$openForComment %<>% as.logical()

d %<>% mutate(year = substr(postedDate, 1, 4))
d$year %<>% as.numeric()

# # INSPECT (head tables)
# d %>% group_by(agencyAcronym) %>% count() %>% arrange(-n)
# d %>% filter(docketType == "Rulemaking") %>%  group_by(docketId) %>% count() %>% arrange(-n)

# over 10k comments
d %>% filter(numberOfCommentsReceived > 10000)



######################
# NUMBER OF COMMENTS #
######################
# histogram of number of comments 
ggplot(d %>% filter(year > 1993, numberOfCommentsReceived < 1000, numberOfCommentsReceived > 0)) + 
  geom_histogram(aes(numberOfCommentsReceived), binwidth = 100)



# TYPEpoint log log 
count <- d %>% filter(year > 1993, !is.na(commentStartDate)) %>% # filter(documentType == "Proposed Rule") %>% #filter(numberOfCommentsReceived > 0) %>% 
  group_by(documentType) %>%
  mutate(mean = round(mean(numberOfCommentsReceived))) %>% 
  mutate(median = median(numberOfCommentsReceived)) %>% 
  mutate(zero = sum(numberOfCommentsReceived == 0)) %>%
  mutate(total = n()) %>%
   group_by(numberOfCommentsReceived, documentType, mean, median, zero, total) %>% tally() %>% arrange(numberOfCommentsReceived) %>%
 #  summarize(unique(zero))
  ggplot() +
  geom_point(aes(
    x = numberOfCommentsReceived, 
    y = n,
    color = documentType, fill = documentType),
    alpha = .2) +
  geom_vline(aes(xintercept = mean, color = documentType), linetype="dashed") +
  geom_label(aes(x = mean, y = zero, label = paste(documentType, "mean =", mean, "comments"), color = documentType), check_overlap = TRUE, vjust = -.1) +
  geom_text(aes(x = mean, y = zero, label = paste0(zero,"/", total, " = ", round(zero/total*100),"%", " have no comments"), color = documentType), check_overlap = TRUE, vjust = 1.1) +
  scale_x_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x) ) ) + 
  scale_y_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x) ) ) + 
  labs(title = paste("All regulations.gov documents open for public comment 1994-2017"),#, sum(d$documentType == "Proposed Rule"),"proposed rules"),
       x = "Number of Comments", 
       y = "Number of Documents", 
       color = "", fill = "") + 
  theme_bw() +
  theme(legend.position="none")


  
  # TYPE DENSITY LOG LOG 
density <-  d %>% filter(year > 1993, !is.na(commentStartDate)) %>% # filter(documentType == "Proposed Rule") %>% #filter(numberOfCommentsReceived > 0) %>% 
    group_by(documentType) %>%
    mutate(mean = round(mean(numberOfCommentsReceived))) %>% 
    mutate(median = median(numberOfCommentsReceived)) %>% 
    mutate(zero = sum(numberOfCommentsReceived == 0)) %>%
    mutate(total = n()) %>%
    #  summarize(unique(zero))
    ggplot() +
    geom_density(aes(
      x = numberOfCommentsReceived, 
      #y = ..count..,
      color = documentType, fill = documentType),
      alpha = .2, position = "identity") +
    geom_vline(aes(xintercept = mean, color = documentType), linetype="dashed") +
#    geom_label(aes(x = mean, y = zero, label = paste(documentType, "mean =", mean, "comments"), color = documentType), check_overlap = TRUE, vjust = -.1) +
#    geom_text(aes(x = mean, y = zero, label = paste0(zero,"/", total, " = ", round(zero/total*100),"%", " have no comments"), color = documentType), check_overlap = TRUE, vjust = 1.1) +
    scale_x_continuous(trans = "log10",
                       breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x) ) ) + 
#    scale_y_continuous(trans = "log10",
#                       breaks = trans_breaks("log10", function(x) 10^x),
#                       labels = trans_format("log10", math_format(10^.x) ) ) + 
    labs(#title = paste("All regulations.gov documents accepting public comments, 1994-2017"),#, sum(d$documentType == "Proposed Rule"),"proposed rules"),
         x = "Number of Comments", 
         y = "Density", 
         color = "", fill = "") + 
    theme_bw() +
    theme(legend.position="none")

   grid.arrange(count, density, ncol = 1)
  
   
   
regsgov <- d
regsgov  %>% mutate(RIN = rin)

load(here("data/OIRAandUA.Rdata"))
   
regs <- d

d <- full_join(regsgov, regs)
   
d$MAJOR %<>% gsub("Yes", "Major Rule", .)
d$MAJOR %<>% gsub("No", "Non-major Rule", .)
d$MAJOR[which(d$MAJOR == "Undetermined")] <- NA
unique(d$PRIORITY_CATEGORY)

# MAJOR point log log 
count <- d %>% filter(year > 1993, !is.na(commentStartDate), !is.na(MAJOR)) %>% filter(documentType == "Proposed Rule") %>% #filter(numberOfCommentsReceived > 0) %>% 
  group_by(MAJOR) %>%
  mutate(mean = round(mean(numberOfCommentsReceived))) %>% 
  mutate(median = median(numberOfCommentsReceived)) %>% 
  mutate(zero = sum(numberOfCommentsReceived == 0)) %>%
  mutate(total = n()) %>%
  group_by(numberOfCommentsReceived, MAJOR, mean, median, zero, total) %>% tally() %>% arrange(numberOfCommentsReceived) %>%
  #  summarize(unique(zero))
  ggplot() +
  geom_point(aes(
    x = numberOfCommentsReceived, 
    y = n,
    color = MAJOR, fill = MAJOR),
    alpha = .2) +
  geom_vline(aes(xintercept = mean, color = MAJOR), linetype="dashed") +
  geom_label(aes(x = mean, y = zero, label = paste(MAJOR, "mean =", mean, "comments"), color = MAJOR), check_overlap = TRUE, vjust = -.1) +
  geom_text(aes(x = mean, y = zero, label = paste0(zero,"/", total, " = ", round(zero/total*100),"%", " have no comments"), color = MAJOR), check_overlap = TRUE, vjust = 1.1) +
  scale_x_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x) ) ) + 
  scale_y_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x) ) ) + 
  labs(title = paste("All regulations.gov documents open for public comment 1994-2017"),#, sum(d$MAJOR == "Proposed Rule"),"proposed rules"),
       x = "Number of Comments", 
       y = "Number of Documents", 
       color = "", fill = "") + 
  theme_bw() +
  theme(legend.position="none")



# MAJOR DENSITY LOG 
density <-  d %>% filter(year > 1993, !is.na(commentStartDate)) %>%  filter(MAJOR == "Proposed Rule") %>% #filter(numberOfCommentsReceived > 0) %>% 
  group_by(MAJOR) %>%
  mutate(mean = round(mean(numberOfCommentsReceived))) %>% 
  mutate(median = median(numberOfCommentsReceived)) %>% 
  mutate(zero = sum(numberOfCommentsReceived == 0)) %>%
  mutate(total = n()) %>%
  #  summarize(unique(zero))
  ggplot() +
  geom_density(aes(
    x = numberOfCommentsReceived, 
    #y = ..count..,
    color = MAJOR, fill = MAJOR),
    alpha = .2, position = "identity") +
  geom_vline(aes(xintercept = mean, color = MAJOR), linetype="dashed") +
  #    geom_label(aes(x = mean, y = zero, label = paste(MAJOR, "mean =", mean, "comments"), color = MAJOR), check_overlap = TRUE, vjust = -.1) +
  #    geom_text(aes(x = mean, y = zero, label = paste0(zero,"/", total, " = ", round(zero/total*100),"%", " have no comments"), color = MAJOR), check_overlap = TRUE, vjust = 1.1) +
  scale_x_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x) ) ) + 
  #    scale_y_continuous(trans = "log10",
  #                       breaks = trans_breaks("log10", function(x) 10^x),
  #                       labels = trans_format("log10", math_format(10^.x) ) ) + 
  labs(#title = paste("All regulations.gov documents accepting public comments, 1994-2017"),#, sum(d$MAJOR == "Proposed Rule"),"proposed rules"),
    x = "Number of Comments", 
    y = "Density", 
    color = "", fill = "") + 
  theme_bw() +
  theme(legend.position="none")

grid.arrange(count, density, ncol = 1)
   











   

   
   
  
  
# PRIORITY_CATEGORY point log log 
count <- d %>% filter(year > 1993, !is.na(commentStartDate), !is.na(PRIORITY_CATEGORY), PRIORITY_CATEGORY != "Info./Admin./Other") %>% 
  filter(documentType == "Proposed Rule") %>% #filter(numberOfCommentsReceived > 0) %>% 
  group_by(PRIORITY_CATEGORY) %>%
  mutate(mean = round(mean(numberOfCommentsReceived))) %>% 
  mutate(median = median(numberOfCommentsReceived)) %>% 
  mutate(zero = sum(numberOfCommentsReceived == 0)) %>%
  mutate(total = n()) %>%
  group_by(numberOfCommentsReceived, PRIORITY_CATEGORY, mean, median, zero, total) %>% tally() %>% arrange(numberOfCommentsReceived) %>%
  #  summarize(unique(zero))
  ggplot() +
  geom_point(aes(
    x = numberOfCommentsReceived, 
    y = n,
    color = PRIORITY_CATEGORY, fill = PRIORITY_CATEGORY),
    alpha = .2) +
  geom_vline(aes(xintercept = mean, color = PRIORITY_CATEGORY), linetype="dashed") +
  geom_label(aes(x = mean, y = zero, label = paste0(PRIORITY_CATEGORY, ": mean = ", mean, ", median = ", median,", comments"), color = PRIORITY_CATEGORY), check_overlap = TRUE, vjust = -.1) +
  geom_text(aes(x = mean, y = zero, label = paste0(zero,"/", total, " = ", round(zero/total*100),"%", " have no comments"), color = PRIORITY_CATEGORY), check_overlap = TRUE, vjust = 1.1) +
  scale_x_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x) ) ) + 
  scale_y_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x) ) ) + 
  labs(title = paste("Regulations.gov Proposed Rules in Unified Agenda open for public comment 1994-2017"),#, sum(d$PRIORITY_CATEGORY == "Proposed Rule"),"proposed rules"),
       x = "Number of Comments", 
       y = "Number of Documents", 
       color = "", fill = "") + 
  theme_bw() +
  theme(legend.position="none")



# PRIORITY_CATEGORY DENSITY LOG 
density <-  d %>% filter(year > 1993, !is.na(commentStartDate), !is.na(PRIORITY_CATEGORY), PRIORITY_CATEGORY != "Info./Admin./Other") %>%
  filter(documentType == "Proposed Rule") %>% #filter(numberOfCommentsReceived > 0) %>% 
  group_by(PRIORITY_CATEGORY) %>%
  mutate(mean = round(mean(numberOfCommentsReceived))) %>% 
  mutate(median = median(numberOfCommentsReceived)) %>% 
  mutate(zero = sum(numberOfCommentsReceived == 0)) %>%
  mutate(total = n()) %>%
  #  summarize(unique(zero))
  ggplot() +
  geom_density(aes(
    x = numberOfCommentsReceived, 
    #y = ..count..,
    color = PRIORITY_CATEGORY, fill = PRIORITY_CATEGORY),
    alpha = .2, position = "identity") +
  geom_vline(aes(xintercept = mean, color = PRIORITY_CATEGORY), linetype="dashed") +
  #    geom_label(aes(x = mean, y = zero, label = paste(PRIORITY_CATEGORY, "mean =", mean, "comments"), color = PRIORITY_CATEGORY), check_overlap = TRUE, vjust = -.1) +
  #    geom_text(aes(x = mean, y = zero, label = paste0(zero,"/", total, " = ", round(zero/total*100),"%", " have no comments"), color = PRIORITY_CATEGORY), check_overlap = TRUE, vjust = 1.1) +
  scale_x_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x) ) ) + 
  #    scale_y_continuous(trans = "log10",
  #                       breaks = trans_breaks("log10", function(x) 10^x),
  #                       labels = trans_format("log10", math_format(10^.x) ) ) + 
  labs(#title = paste("All regulations.gov documents accepting public comments, 1994-2017"),#, sum(d$PRIORITY_CATEGORY == "Proposed Rule"),"proposed rules"),
    x = "Number of Comments", 
    y = "Density", 
    color = "", fill = "") + 
  theme_bw() +
  theme(legend.position="none")

grid.arrange(count, density, ncol = 1)
  
  



























# number of rules and promosed rules 
d %>% 
    filter(year > 1993, documentType %in% c("Rule", "Proposed Rule")) %>% 
ggplot() +
  geom_bar(aes(
             x=year, 
             fill = documentType), position = "dodge") +  
  labs(x = "", y = "", title = "") + 
  theme(axis.text.x  = element_text(angle=45, vjust=0.5), legend.title = element_blank()) #+




# number of comments with acronym labels 
ggplot(d %>% filter(numberOfCommentsReceived > 0, 
                    numberOfCommentsReceived < 1000000, 
                    # documentType %in% c("Public Comments"),
                    year != "NA", year > 1993)) + 
  geom_point(aes(
    x = postedDate, 
    y = numberOfCommentsReceived, 
    color = documentType
  ), alpha = .5) +
  geom_text(d = d %>% filter(numberOfCommentsReceived > 250000, 
                             numberOfCommentsReceived < 1000000, 
                             # documentType %in% c("Public Comments"),
                             year != "NA", year > 1993),
            aes(postedDate, numberOfCommentsReceived, 
                label = agencyAcronym), vjust = 0, hjust = 1, size = 3) +
  labs(x = "", y = "", title = "") + 
  theme(axis.text.x  = element_text(angle=45, vjust=0.5), legend.title = element_blank()) +
  scale_x_date(labels = date_format("%Y"), date_breaks = "1 year")






ggplot(d %>% filter(
  !is.na(postedDate), 
  #documentType %in% c("Rule", "Proposed Rule"),
  year > 1993
)) +
  geom_point(aes(
    x = postedDate, 
    y = numberOfCommentsReceived, 
    color = documentType
  )) + facet_grid(~agencyAcronym)




d %<>% 
  group_by(year, documentType) %>% 
  mutate(AvgComments = mean(numberOfCommentsReceived)) %>%
  ungroup()



ggplot(d %>% filter(documentType == "Proposed Rule", 
                    year != "NA", 
                    year > 1993, 
                    numberOfCommentsReceived >0), 
       aes(x = factor(year), y = numberOfCommentsReceived)) + 
  geom_boxplot() + labs(x = "", y = "", title = "") + 
  theme(axis.text.x  = element_text(angle=45, vjust=0.5), legend.title = element_blank()) +
  scale_y_continuous(limit = c(0, 100)) 


ggplot(d %>% filter(documentType == "Proposed Rule", 
                    year != "NA", 
                    year > 2005, 
                    numberOfCommentsReceived >0), 
       aes(x = postedDate, y = numberOfCommentsReceived)) + 
  geom_point(alpha = .1) + 
  scale_y_continuous(limit = c(0, 1000)) 



ggplot(d %>% group_by(year) %>% filter(year != "NA", year > 1993, numberOfCommentsReceived <100000)) + 
  geom_point(aes(x = year, y = AvgComments, color = documentType))  + labs(x = "", y = "", title = "") + 
  theme(axis.text.x  = element_text(angle=45, vjust=0.5), legend.title = element_blank()) 


ggplot(d %>% group_by(year) %>% filter(year != "NA", year > 1993, numberOfCommentsReceived > 0)) + 
  geom_violin(scale = "area", aes(x = year, y = numberOfCommentsReceived, 
                                  alpha = 1)) + labs(x = "", y = "", title = "") + 
  theme(axis.text.x  = element_text(angle=45, vjust=0.5), legend.title = element_blank()) +
  scale_y_continuous(limit = c(0, 100))

unique(d$AvgComments)

plot(n)











###########################################################################




# for comments data
d %>% group_by(organization) %>% count() %>% arrange(-n)


d %<>% mutate(year = substr(postedDate, 1, 4))
d$year %<>% as.numeric()

# # check for errors
d$commentStartDate[which(d$year == 2099)]

d$year %<>% {gsub("2099", "2009", .)}

d$organization %<>% as.factor() %<>% droplevels()
levels(d$organization)

d$documentType %<>% {gsub("Submission", "Comments", .)}



ggplot()+
  geom_point(d = d %>% 
               filter(year > 1993, documentType %in% c("Public Comments")) %>%
               group_by(docketId) %>% 
               top_n(1) %>%
               ungroup() %>%
               group_by(year, documentType) %>%
               count() %>% 
               ungroup(),
             aes(x = year, y = n, color = documentType))