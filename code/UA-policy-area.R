# source("setup.R")
#load(here("data", "OIRAandUA.Rdata"))





##############################################################
########### Policy Agendas Project Codes ################

############# FIX THIS TO BE A VAR RATHER THAN SUBSET ##################
regs %<>% mutate(policy_code = 
                   case_when(
                     str_detect(RIN, "^21") ~ "Transportation",
                     str_detect(RIN, "^05") ~ "Agriculture",
                     str_detect(RIN, "^06|^15") ~ "Commerce & Treasury",
                     str_detect(RIN, "^07") ~ "Defense",
                     str_detect(RIN, "^18") ~ "Education",
                     str_detect(RIN, "^19") ~ "Energy",
                     str_detect(RIN, "^20") ~ "Environmental",
                     str_detect(RIN, "^23") ~ "Government Operations",
                     str_detect(RIN, "^14|10070|^35100|^0625") ~ "Foreign Policy",
                     str_detect(RIN, "^12") ~ "Labor",
                     str_detect(RIN, "^10|^0596") ~ "Public Lands",
                     str_detect(RIN, "^16|^0938") ~ "Welfare",
                     str_detect(RIN, "^11") ~ "Justice",
                     str_detect(RIN, "^0507|^2529|^35000") ~ "Civil Rights"
                   ))

# regs %>% count(policy_code)

regs %<>% mutate(policy_type = case_when(
  policy_code %in% c("Education",
                     "Health",
                     "Labor",
                     "Justice",
                     "Welfare",
                     "Civil Rights") ~ "Social Policy",
  policy_code %in% c("Transportation",
                     "Agriculture",
                     "Commerce",
                     "Energy",
                     "Lands") ~ "Economic Policy",
  policy_code %in% c("Environmental") ~ "Environmental Policy",
  policy_code %in% c("Defense", "Foreign Policy") ~ "Foreign Policy"
))

# regs %>% count(policy_type)