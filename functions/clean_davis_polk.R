# A script to clean the Davis Polk Dodd-Frank spreadsheet

df %<>% mutate(rin = str_extract(Citation, "RIN.*") %>% 
                 str_remove_all("RIN| ") %>% 
                 str_remove_all(" ") %>% 
                 str_replace("-|—|–", "zzz")%>%
                 str_remove(regex("\\W+")) %>% 
                 str_replace("zzz", "-"))


## correrctions to Davis Polk Data
df %<>% mutate(rin = ifelse(str_detect(Citation, 
                                       "12 CFR part 1041"), 
                            "3170-AA40",
                            rin))

# comments_cfpb %>% filter(str_detect(docket_title, "diversity policies and practices"))
df %<>% mutate(docket_id = ifelse(str_detect(Citation, 
                                             "Release No. 34-75050"), 
                                  "CFPB-2013-0029",
                                  "other"))

df %<>% mutate(docket_id = ifelse(str_detect(Citation, 
                                             "3170-AA02"), 
                                  "CFPB-2011-0005",
                                  docket_id))

df %>% select(rin, Citation, docket_id)
df %>% select(rin, Citation, `Regulator Summary`) %>% filter(is.na(rin)) %>% knitr::kable()


# filter regulations.gov data
comments_cfpb_df <- comments_cfpb %>% filter(rin %in% df$rin | docket_id %in% df$docket_id)

# In Unified agenda but not regulations.gov :
# Ability-to-Repay and Qualified Mortgage Standards Under the Truth in Lending
"3170-AA16" 
"12 CFR Part 1070\nRIN 3170-AA01"

# FED Rins:
"1557-AD62" 
"1557-AD64" 

