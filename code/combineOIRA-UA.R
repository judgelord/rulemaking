source("setup.R")
library(zoo)
# To skip downloading R packages and XML files from reginfo.gov
DownloadNow = FALSE
# To skip parsing XML files
ParseNow = FALSE

#################################################################
# Get OIRA reports from http://www.reginfo.gov/public/do/XMLReportList
#################################################################
if(DownloadNow){
  # At this time the .zip archive on reginfo.gov included 1981-2014
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=PREVIOUS_YEAR_EO_RULE_COMPLETED.zip", "reports/reginfo.zip")
# 
  #download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=EO_RULE_COMPLETED_2016.xml","reports/EO_RULE_COMPLETED_2015.xml")
  # 2021 completed 
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=EO_RULE_COMPLETED_YTD.xml","reports/EO_RULE_COMPLETED_YTD.xml")
  # under review
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=EO_RULES_UNDER_REVIEW.xml", "reports/EO_RULES_UNDER_REVIEW.xml")
  unzip("reports/reginfo.zip")
}

# Find XML files
files <- paste0("reports/", list.files(here("reports"), pattern = "EO_RULE"))

# Function to parse XML files to lists:
parse_xml <-function(FileName) {
  doc1 <- xmlParse(FileName) 
  doc <- xmlToDataFrame(homogeneous = FALSE, stringsAsFactors = FALSE, nodes=getNodeSet(doc1,"//REGACT"))
} 

# Apply function to lists and combine into dataframe:
if(ParseNow){
  OIRAraw <- plyr::ldply(files, parse_xml)
}
# Restore dataframe retaining raw parsed data
OIRA <- OIRAraw

##################################################################################################

unique(OIRA$STAGE)

# change names of stages to match Unified Agenda
OIRA$STAGE <- gsub("Final Rule.*", "Final Rule", OIRA$STAGE)

# correct mis-labeled stages
OIRA$STAGE[which(OIRA$DATE_RECEIVED=="2010-12-01" & OIRA$RIN=="2120-AJ00")] <- "SNPRM"
OIRA$STAGE[which(OIRA$DATE_RECEIVED=="2011-08-01" & OIRA$RIN=="2105-AD96")] <- "SNPRM"
OIRA$STAGE[which(OIRA$DATE_RECEIVED=="2006-05-17" & OIRA$RIN=="2120-AG87")] <- "SNPRM"
OIRA$STAGE[which(OIRA$DATE_RECEIVED=="2008-03-27" & OIRA$RIN=="2120-AI70")] <- "SNPRM"
OIRA$STAGE[which(OIRA$DATE_RECEIVED=="2001-06-14" & OIRA$RIN=="2120-AG87")] <- "Final Rule/2"
OIRA$STAGE[which(OIRA$DATE_RECEIVED=="2008-09-10" & OIRA$RIN=="2120-AG87")] <- "Final Rule/3"
OIRA$STAGE[which(OIRA$DATE_RECEIVED=="2016-03-24" & OIRA$RIN=="2120-AJ87")] <- "Final Rule/2"
OIRA$STAGE[which(OIRA$DATE_RECEIVED=="2009-12-17" & OIRA$RIN=="2126-AA89")] <- "Final Rule/2"
OIRA$STAGE[which(OIRA$DATE_RECEIVED=="2011-04-14" & OIRA$RIN=="2126-AB02")] <- "Final Rule/2"
OIRA$STAGE[which(OIRA$DATE_RECEIVED=="2010-04-15" & OIRA$RIN=="2126-AB21")] <- "Final Rule/2"
OIRA$STAGE[which(OIRA$DATE_RECEIVED=="2009-06-10" & OIRA$RIN=="2127-AJ37")] <- "Final Rule/2"
OIRA$STAGE[which(OIRA$DATE_RECEIVED=="2013-12-25" & OIRA$RIN=="2127-AK43")] <- "Final Rule/2"
OIRA$STAGE[which(OIRA$DATE_RECEIVED=="2010-09-24" & OIRA$RIN=="2137-AE13")] <- "Final Rule/2"

# delete problem observations
length(OIRA$STAGE)
#OIRA <- OIRA[-which(OIRA$RIN=="2137-AE44" & OIRA$DATE_RECEIVED=="2010-10-05"),]
#OIRA <- OIRA[-which(OIRA$RIN=="2126-AA22" & OIRA$DATE_RECEIVED=="2013-02-16"),]
length(OIRA$STAGE)

# select unique observations
OIRA %<>% distinct() %>% arrange(RIN)


# Dates for:
# ANPRM
OIRA$ANPRM_RECIEVED = NA
OIRA$ANPRM_COMPLETED = NA
OIRA$ANPRM_PUBLISHED = NA
# NPRM
OIRA$NPRM_RECIEVED = NA
OIRA$NPRM_COMPLETED = NA
OIRA$NPRM_PUBLISHED = NA
# SNPRM
OIRA$SNPRM_RECIEVED = NA
OIRA$SNPRM_COMPLETED = NA
OIRA$SNPRM_PUBLISHED = NA
# IFR
OIRA$IFR_RECIEVED = NA
OIRA$IFR_COMPLETED = NA
OIRA$IFR_PUBLISHED = NA
# Final
OIRA$FINAL_RECIEVED = NA
OIRA$FINAL_COMPLETED = NA
OIRA$FINAL_PUBLISHED = NA

# FIXME
# would be faster with mutate(if_else())
for(i in 1:dim(OIRA)[1]){
  # ANPRM
  if(OIRA$STAGE[i]=="Prerule"){OIRA$ANPRM_RECIEVED[i] <- OIRA$DATE_RECEIVED[i]}
  if(OIRA$STAGE[i]=="Prerule"){OIRA$ANPRM_COMPLETED[i] <- OIRA$DATE_COMPLETED[i]}
  if(OIRA$STAGE[i]=="Prerule"){OIRA$ANPRM_PUBLISHED[i] <- OIRA$DATE_PUBLISHED[i]}
  # NPRM
  if(OIRA$STAGE[i]=="Proposed Rule"){OIRA$NPRM_RECIEVED[i] <- OIRA$DATE_RECEIVED[i]}
  if(OIRA$STAGE[i]=="Proposed Rule"){OIRA$NPRM_COMPLETED[i] <- OIRA$DATE_COMPLETED[i]}
  if(OIRA$STAGE[i]=="Proposed Rule"){OIRA$NPRM_PUBLISHED[i] <- OIRA$DATE_PUBLISHED[i]}
  # SNPRM
  if(OIRA$STAGE[i]=="SNPRM"){OIRA$SNPRM_RECIEVED[i] <- OIRA$DATE_RECEIVED[i]}
  if(OIRA$STAGE[i]=="SNPRM"){OIRA$SNPRM_COMPLETED[i] <- OIRA$DATE_COMPLETED[i]}
  if(OIRA$STAGE[i]=="SNPRM"){OIRA$SNPRM_PUBLISHED[i] <- OIRA$DATE_PUBLISHED[i]}
  # IFR
  if(OIRA$STAGE[i]=="Interim Final Rule"){OIRA$IFR_RECIEVED[i] <- OIRA$DATE_RECEIVED[i]}
  if(OIRA$STAGE[i]=="Interim Final Rule"){OIRA$IFR_COMPLETED[i] <- OIRA$DATE_COMPLETED[i]}
  if(OIRA$STAGE[i]=="Interim Final Rule"){OIRA$IFR_PUBLISHED[i] <- OIRA$DATE_PUBLISHED[i]}
  # Final
  if(OIRA$STAGE[i]=="Final Rule"){OIRA$FINAL_RECIEVED[i] <- OIRA$DATE_RECEIVED[i]}
  if(OIRA$STAGE[i]=="Final Rule"){OIRA$FINAL_COMPLETED[i] <- OIRA$DATE_COMPLETED[i]}
  if(OIRA$STAGE[i]=="Final Rule"){OIRA$FINAL_PUBLISHED[i] <- OIRA$DATE_PUBLISHED[i]}
}


# Copy new vars to all with same RIN
OIRA %<>% arrange((DATE_RECEIVED)) %>%
  # ANPRM
  transform(ANPRM_RECIEVED = ave(ANPRM_RECIEVED, RIN, FUN = CopyIfNA)) %>%
  transform(ANPRM_COMPLETED = ave(ANPRM_COMPLETED, RIN, FUN = CopyIfNA)) %>%
  transform(ANPRM_PUBLISHED = ave(ANPRM_PUBLISHED, RIN, FUN = CopyIfNA)) %>%
  # NPRM
  transform(NPRM_RECIEVED = ave(NPRM_RECIEVED, RIN, FUN = CopyIfNA)) %>%
  transform(NPRM_COMPLETED = ave(NPRM_COMPLETED, RIN, FUN = CopyIfNA)) %>%
  transform(NPRM_PUBLISHED = ave(NPRM_PUBLISHED, RIN, FUN = CopyIfNA)) %>%
  # SNPRM
  transform(SNPRM_RECIEVED = ave(SNPRM_RECIEVED, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRM_COMPLETED = ave(SNPRM_COMPLETED, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRM_PUBLISHED = ave(SNPRM_PUBLISHED, RIN, FUN = CopyIfNA)) %>%
  # IFR
  transform(IFR_RECIEVED = ave(IFR_RECIEVED, RIN, FUN = CopyIfNA)) %>%
  transform(IFR_COMPLETED = ave(IFR_COMPLETED, RIN, FUN = CopyIfNA)) %>%
  transform(IFR_PUBLISHED = ave(IFR_PUBLISHED, RIN, FUN = CopyIfNA)) %>%
  # FINAL
  transform(FINAL_RECIEVED = ave(FINAL_RECIEVED, RIN, FUN = CopyIfNA)) %>%
  transform(FINAL_COMPLETED = ave(FINAL_COMPLETED, RIN, FUN = CopyIfNA)) %>%
  transform(FINAL_PUBLISHED = ave(FINAL_PUBLISHED, RIN, FUN = CopyIfNA))


OIRA %<>% arrange(desc(DATE_RECEIVED)) %>%
  # ANPRM
  transform(ANPRM_RECIEVED = ave(ANPRM_RECIEVED, RIN, FUN = CopyIfNA)) %>%
  transform(ANPRM_COMPLETED = ave(ANPRM_COMPLETED, RIN, FUN = CopyIfNA)) %>%
  transform(ANPRM_PUBLISHED = ave(ANPRM_PUBLISHED, RIN, FUN = CopyIfNA)) %>%
  # NPRM
  transform(NPRM_RECIEVED = ave(NPRM_RECIEVED, RIN, FUN = CopyIfNA)) %>%
  transform(NPRM_COMPLETED = ave(NPRM_COMPLETED, RIN, FUN = CopyIfNA)) %>%
  transform(NPRM_PUBLISHED = ave(NPRM_PUBLISHED, RIN, FUN = CopyIfNA)) %>%
  # SNPRM
  transform(SNPRM_RECIEVED = ave(SNPRM_RECIEVED, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRM_COMPLETED = ave(SNPRM_COMPLETED, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRM_PUBLISHED = ave(SNPRM_PUBLISHED, RIN, FUN = CopyIfNA)) %>%
  # IFR
  transform(IFR_RECIEVED = ave(IFR_RECIEVED, RIN, FUN = CopyIfNA)) %>%
  transform(IFR_COMPLETED = ave(IFR_COMPLETED, RIN, FUN = CopyIfNA)) %>%
  transform(IFR_PUBLISHED = ave(IFR_PUBLISHED, RIN, FUN = CopyIfNA)) %>%
  # FINAL
  transform(FINAL_RECIEVED = ave(FINAL_RECIEVED, RIN, FUN = CopyIfNA)) %>%
  transform(FINAL_COMPLETED = ave(FINAL_COMPLETED, RIN, FUN = CopyIfNA)) %>%
  transform(FINAL_PUBLISHED = ave(FINAL_PUBLISHED, RIN, FUN = CopyIfNA))





















####################################################################
# Unified Agenda data https://www.reginfo.gov/public/do/eAgendaXmlReport
###################################################################
if(DownloadNow){
  download.file("https://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_202104.xml","reports/REGINFO_RIN_DATA_202104.xml")
  download.file("https://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_202010.xml","reports/REGINFO_RIN_DATA_202010.xml")
  download.file("https://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_202004.xml","reports/REGINFO_RIN_DATA_202004.xml")
  download.file("https://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201910.xml","reports/REGINFO_RIN_DATA_201910.xml")
  download.file("https://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201904.xml","reports/REGINFO_RIN_DATA_201904.xml")
  download.file("https://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201810.xml","reports/REGINFO_RIN_DATA_201810.xml")
  download.file("https://www.reginfo.gov/public/do/XMLViewFileAction?f=2018-SPRING-RIN-DATA.xml","reports/REGINFO_RIN_DATA_201804.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201704.xml","reports/REGINFO_RIN_DATA_201704.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201710.xml","reports/REGINFO_RIN_DATA_201710.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201704.xml","reports/REGINFO_RIN_DATA_201704.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201610.xml","reports/REGINFO_RIN_DATA_201610.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201604.xml","reports/REGINFO_RIN_DATA_201604.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201510.xml","reports/REGINFO_RIN_DATA_201510.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201504.xml","reports/REGINFO_RIN_DATA_201504.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201410.xml","reports/REGINFO_RIN_DATA_201410.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201404.xml","reports/REGINFO_RIN_DATA_201404.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201310.xml","reports/REGINFO_RIN_DATA_201310.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201304.xml","reports/REGINFO_RIN_DATA_201304.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_2012.xml","reports/REGINFO_RIN_DATA_2012.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201110.xml","reports/REGINFO_RIN_DATA_201110.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201104.xml","reports/REGINFO_RIN_DATA_201104.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201010.xml","reports/REGINFO_RIN_DATA_201010.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_201004.xml","reports/REGINFO_RIN_DATA_201004.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200910.xml","reports/REGINFO_RIN_DATA_200910.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200904.xml","reports/REGINFO_RIN_DATA_200904.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200810.xml","reports/REGINFO_RIN_DATA_200810.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200804.xml","reports/REGINFO_RIN_DATA_200804.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200710.xml","reports/REGINFO_RIN_DATA_200710.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200704.xml","reports/REGINFO_RIN_DATA_200704.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200610.xml","reports/REGINFO_RIN_DATA_200610.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200604.xml","reports/REGINFO_RIN_DATA_200604.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200510.xml","reports/REGINFO_RIN_DATA_200510.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200504.xml","reports/REGINFO_RIN_DATA_200504.xml")
  ############################## there is an error in the 2004 data related to RIN 1084-AA00 ###################
  #download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200410.xml","reports/REGINFO_RIN_DATA_200410.xml")
  #download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200404.xml","reports/REGINFO_RIN_DATA_200404.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200310.xml","reports/REGINFO_RIN_DATA_200310.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200304.xml","reports/REGINFO_RIN_DATA_200304.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200210.xml","reports/REGINFO_RIN_DATA_200210.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200204.xml","reports/REGINFO_RIN_DATA_200204.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200110.xml","reports/REGINFO_RIN_DATA_200110.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200104.xml","reports/REGINFO_RIN_DATA_200104.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200010.xml","reports/REGINFO_RIN_DATA_200010.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_200004.xml","reports/REGINFO_RIN_DATA_200004.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_199910.xml","reports/REGINFO_RIN_DATA_199910.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_199904.xml","reports/REGINFO_RIN_DATA_199904.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_199810.xml","reports/REGINFO_RIN_DATA_199810.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_199804.xml","reports/REGINFO_RIN_DATA_199804.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_199710.xml","reports/REGINFO_RIN_DATA_199710.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_199704.xml","reports/REGINFO_RIN_DATA_199704.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_199610.xml","reports/REGINFO_RIN_DATA_199610.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_199604.xml","reports/REGINFO_RIN_DATA_199604.xml")
  download.file("http://www.reginfo.gov/public/do/XMLViewFileAction?f=REGINFO_RIN_DATA_199510.xml","reports/REGINFO_RIN_DATA_199510.xml")
}

# Find saved Unified Agenda reports
files <- paste0("reports/",
  list.files(here("reports"), pattern = "REGINFO_RIN_DATA_") 
  )


files <- files[!str_detect(files, "200404")]

# A function to parse XML files to lists:
parse_xml <-function(FileName) {
  doc1 <- xmlParse(FileName) 
  doc <- xmlToDataFrame(homogeneous = FALSE, stringsAsFactors = FALSE, nodes=getNodeSet(doc1,"//RIN_INFO"))
} 
# Apply this function to files and combine into dataframe
if(ParseNow){
  UnifiedAgendaRaw <- plyr::ldply(files, parse_xml)
}
# Restore dataframe without re-parsing
UnifiedAgenda <- UnifiedAgendaRaw


# format publication date
UnifiedAgenda$PUBLICATION <- gsub("T.*|U.*", "", UnifiedAgenda$PUBLICATION)
UnifiedAgenda$PUBLICATION <- paste(UnifiedAgenda$PUBLICATION,"01")
UnifiedAgenda$PUBLICATION <- as.Date(as.character(UnifiedAgenda$PUBLICATION), "%Y%m%d")

names(UnifiedAgenda) <- gsub("PUBLICATION", "UnifiedAgendaDate", names(UnifiedAgenda))

# change name of STAGE to match OIRA reports
names(UnifiedAgenda) <- gsub("RULE_STAGE", "STAGE", names(UnifiedAgenda))

UnifiedAgenda$STAGE %<>%
  gsub("Final Rule.*", "Final Rule", .) %>%
  gsub("Proposed Rule.*", "Proposed Rule", .) %>%
  gsub("Prerule.*", "Prerule", .) %>%
  gsub("Long-Term Actions", "Undetermined", .)

UnifiedAgenda$TIMETABLE_LIST %<>% 
  gsub("ANPRM", "A.N.P.R.M.", .) %>%
  gsub("Second NPRM|Supplemental NPRM|SNPRM","S.N.P.R.M.", .)

##############################################################################################
#   #  working on rewrite with dplyr #
######################################
# UnifiedAgenda %<>%
#   mutate(STAGE = ifelse(
#     STAGE=="Long-Term Actions" |
#       STAGE=="Completed Actions" & 
#       grepl("withdraw|withdrawn", TIMETABLE_LIST, ignore.case = T)
#     , "Withdrawal", STAGE)) %>%
#   mutate(STAGE = ifelse(
#     grepl("interim final rule", TIMETABLE_LIST, ignore.case = T) &
#       !grepl("final rule{2,}", TIMETABLE_LIST, ignore.case = T)
#     , "Interim Final Rule", STAGE)) %>%
#   mutate(STAGE = ifelse(
#     STAGE=="Completed Actions" &
#       grepl("final rule", TIMETABLE_LIST, ignore.case = T)
#     , "Final Rule", STAGE)) %>%
#   mutate(STAGE = ifelse(
#     grepl("final rule", STAGE, ignore.case = T) &
#       grepl("final rule", TIMETABLE_LIST, ignore.case = T) &
#       sum(str_count(TIMETABLE_LIST, "Final Rule"))>sum(str_count(TIMETABLE_LIST, "Interim Final Rule"))
#     , "Final Rule", STAGE))

###################################################################

############################
# RENAMING STAGES #
###################

# This might need to be revisited
for(i in 1:dim(UnifiedAgenda)[1]){
  if(
    UnifiedAgenda$STAGE[i]=="Long-Term Actions" &
    grepl("withdraw|withdrawn", UnifiedAgenda$TIMETABLE_LIST[i], ignore.case = T)
  ){
    UnifiedAgenda$STAGE[i] <- "Withdrawal"
  }
  if(
    UnifiedAgenda$STAGE[i]=="Completed Actions" &
    grepl("withdraw|withdrawn", UnifiedAgenda$TIMETABLE_LIST[i], ignore.case = T)
  ){
    UnifiedAgenda$STAGE[i] <- "Withdrawal"
  }
  if(
    grepl("interim final rule", UnifiedAgenda$TIMETABLE_LIST[i], ignore.case = T) && 
    !grepl("final rule{2,}", UnifiedAgenda$TIMETABLE_LIST[i], ignore.case = T)
  ){
    UnifiedAgenda$STAGE[i] <- "Interim Final Rule"
  }
  if(
    UnifiedAgenda$STAGE[i]=="Completed Actions" &
    grepl("final rule", UnifiedAgenda$TIMETABLE_LIST[i], ignore.case = T)
  ){
    UnifiedAgenda$STAGE[i] <- "Final Rule"
  }
  if(
    UnifiedAgenda$STAGE[i]=="Completed Actions" &
    grepl("final action", UnifiedAgenda$TIMETABLE_LIST[i], ignore.case = T)
  ){
    UnifiedAgenda$STAGE[i] <- "Final Rule"
  }
  if(
    grepl("final rule", UnifiedAgenda$STAGE[i], ignore.case = T) &&
    grepl("final rule", UnifiedAgenda$TIMETABLE_LIST[i], ignore.case = T) &&
    sum(str_count(UnifiedAgenda$TIMETABLE_LIST[i], "Final Rule"))>sum(str_count(UnifiedAgenda$TIMETABLE_LIST[i], "Interim Final Rule"))
  ){
    UnifiedAgenda$STAGE[i] <- "Final Rule"
  }
}



####################
# CORRECTIONS #
###############

# Correct stages - Please help with this! 
# Corrections for DOT 2007 - 2017 (only corrections as of Dec 2018)
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2007-10-01" & UnifiedAgenda$RIN=="2105-AD63")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2009-04-01" & UnifiedAgenda$RIN=="2105-AD72")] <- "Prerule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2007-04-01" & UnifiedAgenda$RIN=="2120-AG87")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2010-10-01" & UnifiedAgenda$RIN=="2120-AJ15")] <- "Final Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2010-10-01" & UnifiedAgenda$RIN=="2120-AJ34")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2009-04-01" & UnifiedAgenda$RIN=="2120-AJ38")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2011-04-01" & UnifiedAgenda$RIN=="2120-AJ69 ")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2012-10-01" & UnifiedAgenda$RIN=="2120-AK04")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2012-10-01" & UnifiedAgenda$RIN=="2120-AK05")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2016-04-01" & UnifiedAgenda$RIN=="2120-AK09")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2013-10-01" & UnifiedAgenda$RIN=="2120-AK26")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2010-10-01" & UnifiedAgenda$RIN=="2125-AF24")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2010-04-01" & UnifiedAgenda$RIN=="2125-AF34")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2011-10-01" & UnifiedAgenda$RIN=="2125-AF38")] <- "Withdrawal"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2003-04-01" & UnifiedAgenda$RIN=="2126-AA64")] <- "Interim Final Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2011-10-01" & UnifiedAgenda$RIN=="2126-AA64")] <- "Final Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2008-04-01" & UnifiedAgenda$RIN=="2127-AK00")] <- "Other"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2008-10-01" & UnifiedAgenda$RIN=="2127-AK11")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2009-10-01" & UnifiedAgenda$RIN=="2127-AK45")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2010-04-01" & UnifiedAgenda$RIN=="2127-AK50")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2010-04-01" & UnifiedAgenda$RIN=="2127-AK52")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2010-10-01" & UnifiedAgenda$RIN=="2127-AK73")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2011-10-01" & UnifiedAgenda$RIN=="2127-AK75")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2012-10-01" & UnifiedAgenda$RIN=="2127-AL29")] <- "Interim Final Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2013-04-01" & UnifiedAgenda$RIN=="2127-AL29")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2015-04-01" & UnifiedAgenda$RIN=="2127-AL55")] <- "Prerule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2008-10-01" & UnifiedAgenda$RIN=="2130-AB69")] <- "Interim Final Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2008-04-01" & UnifiedAgenda$RIN=="2130-AB91")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2011-10-01" & UnifiedAgenda$RIN=="2130-AC27")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2015-04-01" & UnifiedAgenda$RIN=="2130-AC47")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2016-04-01" & UnifiedAgenda$RIN=="2130-AC53")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2011-04-01" & UnifiedAgenda$RIN=="2132-AB01")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2011-04-01" & UnifiedAgenda$RIN=="2132-AB02")] <- "Prerule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2012-10-01" & UnifiedAgenda$RIN=="2132-AB03")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2012-10-01" & UnifiedAgenda$RIN=="2132-AB13")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2011-04-01" & UnifiedAgenda$RIN=="2137-AE44")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2010-10-01" & UnifiedAgenda$RIN=="2137-AE63")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2013-10-01" & UnifiedAgenda$RIN=="2137-AE91")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2014-04-01" & UnifiedAgenda$RIN=="2137-AF00")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2014-04-01" & UnifiedAgenda$RIN=="2137-AF08")] <- "Prerule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2008-04-01" & UnifiedAgenda$RIN=="2137-AF12")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2015-04-01" & UnifiedAgenda$RIN=="2105-AD90")] <- "Terminated"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2013-10-01" & UnifiedAgenda$RIN=="2105-AE08")] <- "Final Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2016-04-01" & UnifiedAgenda$RIN=="2105-AE33")] <- "Final Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2014-10-01" & UnifiedAgenda$RIN=="2120-AK57")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2016-04-01" & UnifiedAgenda$RIN=="2127-AK95")] <- "Final Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2014-10-01" & UnifiedAgenda$RIN=="2127-AL55")] <- "Prerule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2014-10-01" & UnifiedAgenda$RIN=="2130-AC47")] <- "Final Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2016-04-01" & UnifiedAgenda$RIN=="2130-AC47")] <- "Withdrawal"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2015-04-01" & UnifiedAgenda$RIN=="2137-AF08")] <- "Prerule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2015-10-01" & UnifiedAgenda$RIN=="2105-AE33")] <- "Interm Final Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2014-04-01" & UnifiedAgenda$RIN=="2130-AC47")] <- "Proposed Rule"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2001-04-01" & UnifiedAgenda$RIN=="2120-AG87")] <- "Final Rule/2"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2009-10-01" & UnifiedAgenda$RIN=="2120-AG87")] <- "Final Rule/3"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2016-04-01" & UnifiedAgenda$RIN=="2120-AJ87")] <- "Final Rule/2"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2009-10-01" & UnifiedAgenda$RIN=="2126-AA89")] <- "Final Rule/2"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2011-04-01" & UnifiedAgenda$RIN=="2126-AB02")] <- "Final Rule/2"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2010-10-01" & UnifiedAgenda$RIN=="2126-AB21")] <- "Final Rule/2"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2009-10-01" & UnifiedAgenda$RIN=="2127-AJ37")] <- "Final Rule/2"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2014-04-01" & UnifiedAgenda$RIN=="2127-AK43")] <- "Final Rule/2"
UnifiedAgenda$STAGE[which(UnifiedAgenda$UnifiedAgendaDate=="2011-04-01" & UnifiedAgenda$RIN=="2137-AE13")] <- "Final Rule/2"

#####################
# SUBSET # 
##########
# select the most recent observation for each RIN at each Stage
UnifiedAgenda %<>%
  dplyr::group_by(RIN, STAGE) %>%
  dplyr::top_n(1, UnifiedAgendaDate) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(RIN)










##########################################
# EXTRACT DATES #
#################

# change 00 days to 01 in TIMETABLE
UnifiedAgenda$TIMETABLE_LIST <- gsub("/00/", "/01/", UnifiedAgenda$TIMETABLE_LIST)

# change Interim Final Rule to IFR, to make extraction easier 
UnifiedAgenda$TIMETABLE_LIST <- gsub("Interim Final Rule", "IFR", UnifiedAgenda$TIMETABLE_LIST)

# Extract dates from TIMETABLE
library(stringi)
UnifiedAgenda %<>%
  # ANPRM
  mutate(ANPRM = gsub("A.N.P.R.M.","", str_extract(TIMETABLE_LIST, "A.N.P.R.M.[0-9]{2}/[0-9]{2}/[0-9]{4}"))) %>%
  mutate(ANPRMcomment = gsub("A.N.P.R.M. Comment Period End","", str_extract(UnifiedAgenda$TIMETABLE_LIST, "A.N.P.R.M. Comment Period End[0-9]{2}/[0-9]{2}/[0-9]{4}"))) %>%
  mutate(ANPRMfedreg = gsub("A.N.P.R.M.[0-9]{2}/[0-9]{2}/[0-9]{4}|[A-Z]$","", str_extract(UnifiedAgenda$TIMETABLE_LIST, "A.N.P.R.M.[0-9]{2}/[0-9]{2}/[0-9]{6} FR [0-9]{4}."))) %>%
  # NPRM
  mutate(NPRM = gsub("NPRM","", str_extract(UnifiedAgenda$TIMETABLE_LIST, "NPRM[0-9]{2}/[0-9]{2}/[0-9]{4}"))) %>%
  mutate(NPRMcomment = gsub("NPRM Comment Period End","", str_extract(UnifiedAgenda$TIMETABLE_LIST, "NPRM Comment Period End[0-9]{2}/[0-9]{2}/[0-9]{4}"))) %>%
  mutate(NPRMfedreg = gsub("NPRM[0-9]{2}/[0-9]{2}/[0-9]{4}|[A-Z]$","", str_extract(UnifiedAgenda$TIMETABLE_LIST, "NPRM[0-9]{2}/[0-9]{2}/[0-9]{6} FR [0-9]{4}."))) %>%
  # SNPRM
  mutate(SNPRM = gsub("S.N.P.R.M.","", str_extract(UnifiedAgenda$TIMETABLE_LIST, "S.N.P.R.M.[0-9]{2}/[0-9]{2}/[0-9]{4}"))) %>%
  mutate(SNPRMcomment = gsub("S.N.P.R.M. Comment Period End","", str_extract(UnifiedAgenda$TIMETABLE_LIST, "S.N.P.R.M. Comment Period End[0-9]{2}/[0-9]{2}/[0-9]{4}"))) %>%
  mutate(SNPRMfedreg = gsub("S.N.P.R.M.[0-9]{2}/[0-9]{2}/[0-9]{4}|[A-Z]$","", str_extract(UnifiedAgenda$TIMETABLE_LIST, "S.N.P.R.M.[0-9]{2}/[0-9]{2}/[0-9]{6} FR [0-9]{4}."))) %>%
  # Interim Rule
  mutate(IFR = gsub("(IFR (IFR)|IFR)","", str_extract(UnifiedAgenda$TIMETABLE_LIST, "(IFR (IFR)|IFR)[0-9]{2}/[0-9]{2}/[0-9]{4}"))) %>%
  mutate(IFRcomment = gsub("(IFR (IFR)|IFR) Comment Period End","", str_extract(UnifiedAgenda$TIMETABLE_LIST, "(IFR (IFR)|IFR) Comment Period End[0-9]{2}/[0-9]{2}/[0-9]{4}"))) %>%
  mutate(IFReffective = gsub("(IFR (IFR)|IFR) Effective","", str_extract(UnifiedAgenda$TIMETABLE_LIST, "(IFR (IFR)|IFR) Effective[0-9]{2}/[0-9]{2}/[0-9]{4}"))) %>%
  mutate(IFRfedreg = gsub("(IFR (IFR)|IFR)[0-9]{2}/[0-9]{2}/[0-9]{4}|[A-Z]$","", str_extract(UnifiedAgenda$TIMETABLE_LIST, "(IFR (IFR)|IFR)[0-9]{2}/[0-9]{2}/[0-9]{6} FR [0-9]{4}."))) %>%
  # Withdrawal
  mutate(WITHDRAWAL = gsub("(Withdraw|withdraw)[A-Za-z ]*","", str_extract(UnifiedAgenda$TIMETABLE_LIST, "(Withdraw|withdraw)[A-Za-z ]*[0-9]{2}/[0-9]{2}/[0-9]{4}"))) %>%
  # FINAL (DOES THIS WORK NOW?), NOT COMPLETELY, SEE 270
  mutate(FINAL = ifelse(
    grepl("Final Rule", UnifiedAgenda$TIMETABLE_LIST, ignore.case = T),
    gsub("Final Rule","", stri_extract_last_regex(UnifiedAgenda$TIMETABLE_LIST, "Final Rule[0-9]{2}/[0-9]{2}/[0-9]{4}")),
    NA)) %>%
  mutate(FINALeffective = ifelse(
    grepl("Final Rule", UnifiedAgenda$TIMETABLE_LIST, ignore.case = T), 
    gsub("Final Rule Effective","", stri_extract_last_regex(UnifiedAgenda$TIMETABLE_LIST, "Final Rule Effective[0-9]{2}/[0-9]{2}/[0-9]{4}")), 
    NA)) %>%
  mutate(FINALfedreg = ifelse(
    grepl("Final Rule", UnifiedAgenda$TIMETABLE_LIST, ignore.case = T),
    gsub("Rule[0-9]{2}/[0-9]{2}/[0-9]{4}|[A-Z]$","", stri_extract_last_regex(UnifiedAgenda$TIMETABLE_LIST, "Rule[0-9]{2}/[0-9]{2}/[0-9]{6} FR [0-9]{4}.")), 
    NA))

# Inspect date variables vs timetable for completeness
timetable <- select(UnifiedAgenda, TIMETABLE_LIST, ANPRM, NPRM, IFR, FINAL, WITHDRAWAL)





# Extract dates from DEADLINE
UnifiedAgenda %<>%
  # NPRM
  mutate(StatutoryNPRM = gsub("StatutoryNPRM","", str_extract(LEGAL_DLINE_LIST, "StatutoryNPRM[0-9]{2}/[0-9]{2}/[0-9]{4}"))) %>%
  mutate(JudicialNPRM = gsub("JudicialNPRM","", str_extract(LEGAL_DLINE_LIST, "JudicialNPRM[0-9]{2}/[0-9]{2}/[0-9]{4}"))) %>%
  # Final
  mutate(StatutoryFinal = gsub("StatutoryFinal","", str_extract(LEGAL_DLINE_LIST, "StatutoryFinal[0-9]{2}/[0-9]{2}/[0-9]{4}"))) %>%
  mutate(JudicialFinal = gsub("JudicialFinal","", str_extract(LEGAL_DLINE_LIST, "JudicialFinal[0-9]{2}/[0-9]{2}/[0-9]{4}")))

# format dates
UnifiedAgenda$ANPRM <-
  as.Date(as.character(UnifiedAgenda$ANPRM), "%m/%d/%Y")
UnifiedAgenda$ANPRMcomment <-
  as.Date(as.character(UnifiedAgenda$ANPRMcomment), "%m/%d/%Y")
UnifiedAgenda$NPRM <-
  as.Date(as.character(UnifiedAgenda$NPRM), "%m/%d/%Y")
UnifiedAgenda$NPRMcomment <-
  as.Date(as.character(UnifiedAgenda$NPRMcomment), "%m/%d/%Y")
UnifiedAgenda$SNPRM <-
  as.Date(as.character(UnifiedAgenda$SNPRM), "%m/%d/%Y")
UnifiedAgenda$SNPRMcomment <-
  as.Date(as.character(UnifiedAgenda$SNPRMcomment), "%m/%d/%Y")
UnifiedAgenda$IFR <-
  as.Date(as.character(UnifiedAgenda$IFR), "%m/%d/%Y")
UnifiedAgenda$IFReffective <-
  as.Date(as.character(UnifiedAgenda$IFReffective), "%m/%d/%Y")
UnifiedAgenda$WITHDRAWAL <-
  as.Date(as.character(UnifiedAgenda$WITHDRAWAL), "%m/%d/%Y")
UnifiedAgenda$FINAL <-
  as.Date(as.character(UnifiedAgenda$FINAL), "%m/%d/%Y")
UnifiedAgenda$FINALeffective <-
  as.Date(as.character(UnifiedAgenda$FINALeffective), "%m/%d/%Y")
UnifiedAgenda$StatutoryNPRM <-
  as.Date(as.character(UnifiedAgenda$StatutoryNPRM), "%m/%d/%Y")
UnifiedAgenda$JudicialNPRM <-
  as.Date(as.character(UnifiedAgenda$JudicialNPRM), "%m/%d/%Y")
UnifiedAgenda$StatutoryFinal <-
  as.Date(as.character(UnifiedAgenda$StatutoryFinal), "%m/%d/%Y")
UnifiedAgenda$JudicialFinal <-
  as.Date(as.character(UnifiedAgenda$JudicialFinal), "%m/%d/%Y")

# format abstract
UnifiedAgenda$ABSTRACT %<>%
  gsub(".*<p>","",.) %>%
  gsub("</p>.*","",.)

# format title
UnifiedAgenda$RULE_TITLE %<>%
  gsub("[0-9]*|[:upper:]*$","",.)

# format agency
UnifiedAgenda$AGENCY %<>%
  gsub("[0-9]","",.)

UnifiedAgenda$AGENCY %<>%
  gsub("[A-Z]*$","",.)

# Copy new vars to all with the same RIN 
UnifiedAgenda %<>% 
  group_by(RIN) %>% 
  #ANPRM
  mutate(ANPRM = paste(rev(tail(ANPRM))[1], collapse = ";"))%>% 
  mutate(ANPRMcomment = paste(rev(tail(ANPRMcomment))[1], collapse = ";"))%>% 
  mutate(ANPRMfedreg = paste(ANPRMfedreg, collapse = ";"))%>% 
  # NPRM
  mutate(NPRM = paste(rev(tail(NPRM))[1], collapse = ";")) %>%  
  mutate(NPRMcomment = paste(rev(tail(NPRMcomment))[1], collapse = ";")) %>%  
  mutate(NPRMfedreg = paste(NPRMfedreg, collapse = ";")) %>%  
  mutate(NPRMjudicial = paste(JudicialNPRM, collapse = ";")) %>%
  mutate(NPRMstatutory = paste(StatutoryNPRM, collapse = ";")) %>%  
  # SNPRM
  mutate(SNPRM = paste(rev(tail(SNPRM))[1], collapse = ";")) %>%  
  mutate(SNPRMcomment = paste(rev(tail(SNPRMcomment))[1], collapse = ";")) %>%  
  mutate(SNPRMfedreg = paste(SNPRMfedreg, collapse = ";")) %>%
  # IFR
  mutate(IFR = paste(rev(tail(IFR))[1], collapse = ";")) %>%  
  mutate(IFRcomment = paste(rev(tail(IFRcomment))[1], collapse = ";")) %>%  
  mutate(IFRfedreg = paste(IFRfedreg, collapse = ";")) %>%
  # FINAL
  mutate(FINAL = paste(rev(tail(FINAL))[1], collapse = ";")) %>%  
  # [no final comment? check timtable]
  mutate(FINALeffective = paste(rev(tail(FINALeffective))[1], collapse = ";")) %>%  
  mutate(FINALfedreg = paste(FINALfedreg, collapse = ";")) %>%  
  mutate(FINALjudicial = paste(JudicialFinal, collapse = ";")) %>%
  mutate(FINALstatutory = paste(StatutoryFinal, collapse = ";")) %>% 
  # WITHDRAWAL
  mutate(WITHDRAWAL = paste(rev(tail(WITHDRAWAL))[1], collapse = ";")) %>% 
  ungroup()
  


# Copy new vars to all with the same RIN (replaced with faster mutate() code above)
# UnifiedAgenda %<>% arrange((UnifiedAgendaDate)) %>%
#   transform(ANPRM = ave(ANPRM, RIN, FUN = CopyIfNA)) %>%
#   transform(ANPRMcomment = ave(ANPRMcomment, RIN, FUN = CopyIfNA)) %>%
#   transform(ANPRMfedreg = ave(ANPRMfedreg, RIN, FUN = CopyIfNA)) %>%
#   transform(NPRM = ave(NPRM, RIN, FUN = CopyIfNA)) %>%
#   transform(NPRMcomment = ave(NPRMcomment, RIN, FUN = CopyIfNA)) %>%
#   transform(NPRMfedreg = ave(NPRMfedreg, RIN, FUN = CopyIfNA)) %>%
#   transform(SNPRM = ave(SNPRM, RIN, FUN = CopyIfNA)) %>%
#   transform(SNPRMcomment = ave(SNPRMcomment, RIN, FUN = CopyIfNA)) %>%
#   transform(SNPRMfedreg = ave(SNPRMfedreg, RIN, FUN = CopyIfNA)) %>%
#   transform(IFR = ave(IFR, RIN, FUN = CopyIfNA)) %>%
#   transform(IFRcomment = ave(IFRcomment, RIN, FUN = CopyIfNA)) %>%
#   transform(IFRfedreg = ave(IFRfedreg, RIN, FUN = CopyIfNA)) %>%
#   transform(IFReffective = ave(IFReffective, RIN, FUN = CopyIfNA)) %>%
#   transform(WITHDRAWAL = ave(WITHDRAWAL, RIN, FUN = CopyIfNA)) %>%
#   transform(FINAL = ave(FINAL, RIN, FUN = CopyIfNA)) %>%
#   transform(FINALeffective = ave(FINALeffective, RIN, FUN = CopyIfNA)) %>%
#   transform(FINALfedreg = ave(FINALfedreg, RIN, FUN = CopyIfNA)) %>%
#   transform(StatutoryNPRM = ave(StatutoryNPRM, RIN, FUN = CopyIfNA)) %>%
#   transform(JudicialNPRM = ave(JudicialNPRM, RIN, FUN = CopyIfNA)) %>%
#   transform(StatutoryFinal = ave(StatutoryFinal, RIN, FUN = CopyIfNA)) %>%
#   transform(JudicialFinal = ave(JudicialFinal, RIN, FUN = CopyIfNA))
# 
# UnifiedAgenda %<>% arrange(desc(UnifiedAgendaDate)) %>%
#   transform(ANPRM = ave(ANPRM, RIN, FUN = CopyIfNA)) %>%
#   transform(ANPRMcomment = ave(ANPRMcomment, RIN, FUN = CopyIfNA)) %>%
#   transform(ANPRMfedreg = ave(ANPRMfedreg, RIN, FUN = CopyIfNA)) %>%
#   transform(NPRM = ave(NPRM, RIN, FUN = CopyIfNA)) %>%
#   transform(NPRMcomment = ave(NPRMcomment, RIN, FUN = CopyIfNA)) %>%
#   transform(NPRMfedreg = ave(NPRMfedreg, RIN, FUN = CopyIfNA)) %>%
#   transform(SNPRM = ave(SNPRM, RIN, FUN = CopyIfNA)) %>%
#   transform(SNPRMcomment = ave(SNPRMcomment, RIN, FUN = CopyIfNA)) %>%
#   transform(SNPRMfedreg = ave(SNPRMfedreg, RIN, FUN = CopyIfNA)) %>%
#   transform(IFR = ave(IFR, RIN, FUN = CopyIfNA)) %>%
#   transform(IFRcomment = ave(IFRcomment, RIN, FUN = CopyIfNA)) %>%
#   transform(IFRfedreg = ave(IFRfedreg, RIN, FUN = CopyIfNA)) %>%
#   transform(IFReffective = ave(IFReffective, RIN, FUN = CopyIfNA)) %>%
#   transform(WITHDRAWAL = ave(WITHDRAWAL, RIN, FUN = CopyIfNA)) %>%
#   transform(FINAL = ave(FINAL, RIN, FUN = CopyIfNA)) %>%
#   transform(FINALeffective = ave(FINALeffective, RIN, FUN = CopyIfNA)) %>%
#   transform(FINALfedreg = ave(FINALfedreg, RIN, FUN = CopyIfNA)) %>%
#   transform(StatutoryNPRM = ave(StatutoryNPRM, RIN, FUN = CopyIfNA)) %>%
#   transform(JudicialNPRM = ave(JudicialNPRM, RIN, FUN = CopyIfNA)) %>%
#   transform(StatutoryFinal = ave(StatutoryFinal, RIN, FUN = CopyIfNA)) %>%
#   transform(JudicialFinal = ave(JudicialFinal, RIN, FUN = CopyIfNA))








#UnifiedAgenda %<>%  dplyr::select(RIN, AGENCY, RULE_TITLE, ABSTRACT, STAGE, UnifiedAgendaDate, ANPRM, ANPRMcomment, NPRM, NPRMcomment, SNPRM, SNPRMcomment, IFR, IFReffective, FINAL, FINALeffective, TIMETABLE_LIST, CFR_LIST, ANPRMfedreg, NPRMfedreg, SNPRMfedreg, IFRfedreg, FINALfedreg, everything())
#UnifiedAgenda <- read.csv("Unified Agenda.csv")
#OIRA <- read.csv("OIRA.csv")
thusfar <- UnifiedAgenda
UnifiedAgenda <- thusfar









######################################
# Merge Unified Agenda and OIRA reports
##################################
OIRA %<>% mutate(MAJOR_OIRA= MAJOR) %>% select(-MAJOR)
regs <- merge(UnifiedAgenda, OIRA, by=c("RIN","STAGE"), all=TRUE)
regs <- full_join(UnifiedAgenda, OIRA)

# FIXME
# this works fine, but mutate(if_else()) would be faster
for(i in 1:dim(regs)[1]){
  if(!grepl("-", regs$ANPRM_PUBLISHED[i])){regs$ANPRM_PUBLISHED[i] <- as.character(regs$ANPRM[i])}
  if(!grepl("-", regs$NPRM_PUBLISHED[i])){regs$NPRM_PUBLISHED[i] <- as.character(regs$NPRM[i])}
  if(!grepl("-", regs$SNPRM_PUBLISHED[i])){regs$SNPRM_PUBLISHED[i] <- as.character(regs$SNPRM[i])}
  if(!grepl("-", regs$IFR_PUBLISHED[i])){regs$IFR_PUBLISHED[i] <- as.character(regs$IFR[i])}
  if(!grepl("-", regs$FINAL_PUBLISHED[i])){regs$FINAL_PUBLISHED[i] <- as.character(regs$FINAL[i])}
  if(is.na(regs$TITLE[i])){regs$TITLE[i] <- regs$RULE_TITLE[i]}
}

# delete duplicate variables (those created from timetable list, which were just merged with key dates above)
regs$ANPRM <- regs$NPRM <- regs$SNPRM <- regs$IFR <- regs$FINAL <- regs$RULE_TITLE <- NULL

# inspect timetable vs key date variables for completeness
timetable <- select(regs, RIN, TIMETABLE_LIST, ANPRM_PUBLISHED, NPRM_PUBLISHED, IFR_PUBLISHED, FINAL_PUBLISHED)





regs$outcomedate = ""



# Outcome and outcome date 
regs %<>% mutate(outcome = if_else(!is.na(FINAL_PUBLISHED), 1, 0)) # final is 1
regs %<>% mutate(outcomedate = if_else(!is.na(FINAL_PUBLISHED), FINAL_PUBLISHED, outcomedate))
regs %<>% mutate(outcome = if_else(!is.na(WITHDRAWAL), 2, outcome)) # withdrawal is 2
regs %<>% mutate(outcomedate = if_else(!is.na(WITHDRAWAL), WITHDRAWAL, outcomedate))
# ADD TERMINATION?

# year 
regs$endyear = as.numeric(substring(as.character(regs$outcomedate),1,4))

# how long from NPRM to ...
regs %<>% mutate(NPRMtoFinal = as.numeric(as.Date(FINAL_PUBLISHED) - as.Date(NPRM_PUBLISHED)))

regs %<>% mutate(NPRMtoWithdrawal = as.numeric(as.Date(WITHDRAWAL) - as.Date(NPRM_PUBLISHED)))

#########################################################################
# Save 

# note that .csv files are too large for git
if(F){
  # write out UA
  write.csv(UnifiedAgenda, file="data/Unified Agenda.csv")
  
  # write out OIRA
  write.csv(OIRA, file="data/OIRA.csv")
  
  # write out combined
  write.csv(regs, file="data/OIRAandUA.csv")

  
}

if(F){
save(OIRA, file = "data/OIRA.Rdata")
save(UnifiedAgenda, file = "data/UnifiedAgenda.Rdata")
save(regs, file = "data/OIRAandUA.Rdata")
}
