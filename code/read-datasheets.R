# this script loads in hand-coded data 
# right now it loads in CFPB-2016-0025, but it will be expanded to a more general function

library(googlesheets4)
library(readtext)

gs4_auth(
  email = gargle::gargle_oauth_email()
)


#Read in handcoded asks
# table1 <- read_sheet("https://docs.google.com/spreadsheets/d/1jdQjknkJ4LlTMaBUVRUoJwF34C0ose1NJznhPlctciI/edit")
# table2 <- read_sheet("https://docs.google.com/spreadsheets/d/1Z0hkac2hlNzBKYJzE3aGjGhM8D00rZcPPBRrllD6rwI/edit#gid=1545760618")
# table3 <- read_sheet("https://docs.google.com/spreadsheets/d/1a1pPe70tiB-a8YRIHGBuKZzXC3R3wBwCKtEJFatiyAE/edit")

#Join asks into one df
# NOTE: try something like by = everything() 
asksData <- 
  left_join(read_sheet("https://docs.google.com/spreadsheets/d/1jdQjknkJ4LlTMaBUVRUoJwF34C0ose1NJznhPlctciI", col_types = "c"), # Hope
            read_sheet("https://docs.google.com/spreadsheets/d/1Z0hkac2hlNzBKYJzE3aGjGhM8D00rZcPPBRrllD6rwI", col_types = "c")) %>%#, # Ana
            #by = c("proposed_url", "final_url", "comment_url", "document_id", "organization", "position_certainty", "comment_type", "coalition_comment", "coalition_type", "org_name", "org_name_short", "org_type", "ask", "ask1", "ask2", "ask3", "success", "success_certainty", "sucess1", "success2", "success3", "response", "pressure_phrases", "compromise_phrases", "notes")) %>%
  left_join(read_sheet("https://docs.google.com/spreadsheets/d/1a1pPe70tiB-a8YRIHGBuKZzXC3R3wBwCKtEJFatiyAE", col_types = "c")) %>% #, # Hailey
            #by = c("proposed_url", "final_url", "comment_url", "document_id", "organization", "position_certainty", "comment_type", "coalition_comment", "coalition_type", "org_name", "org_name_short", "org_type", "ask", "ask1", "ask2", "ask3", "success", "success_certainty", "sucess1", "success2", "success3", "response", "pressure_phrases", "compromise_phrases", "notes")) %>%
  left_join(read_sheet("https://docs.google.com/spreadsheets/d/1GCz6ewgJGUYMo2mN6BdPoECMoT34xaumoyR9YP51pNw", col_types = "c")) %>% # Samia
  left_join(read_sheet("https://docs.google.com/spreadsheets/d/1aPygOmPG336m78d1XRt94Nv-MOwpxZR_nIVK7m8mKrY", col_types = "c")) %>% # Max
  left_join(read_sheet("https://docs.google.com/spreadsheets/d/1rqofFHs8LbdND0-gJ1hPD6VNCQKSWDOBy-YAmTjgtXY", col_types = "c")) %>% 
  left_join(read_sheet("https://docs.google.com/spreadsheets/d/1GDYeu8biQHMcgfcrz0CXxosMYZwPUlWTQm5VMC_YO6w", col_types = "c")) # Jackson

asksData %<>% 
  # selected coded obs
  filter(!is.na(ask)) %>% 
  # one obs per document_id
  group_by(document_id) %>% 
  top_n(1)

coded <- asksData %>% ungroup()

save(coded, file =  here::here("data", "CFPB-2016-0025-coded.Rdata"))
