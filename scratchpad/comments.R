library(here)
source(here("setup.R"))

# MOVE TO SUMMARY 

# orgs 
load(here("data/toporgs.Rdata"))

toporgs$mass %<>% 
  str_replace("Not unique", "Medium batch")

toporgs %>%
  mutate(topdockets = ifelse(docketTotal %in% sort(unique(toporgs$docketTotal), decreasing = T)[1:10],
        docketId, NA) ) %>%
  filter(orgTotal %in% sort(
    unique(toporgs$orgTotal), 
    decreasing = T)[1:10] 
  ) %>% 
  filter(!is.na(mass)) %>% 
  ggplot() +
  geom_col(aes(x = organization, y = numberOfCommentsReceived/1000)) +
  coord_flip()+
  facet_grid(. ~ mass, scales = "free_x") + 
  labs(x = "", y = "Number of Comments (thousands)", fill = "") + theme_bw() 


# dockets 
load(here("data/topdockets.Rdata"))

unique(topdockets$docketTitle)
# short docket titles
topdockets$docketTitle %<>%
  str_replace(
    "Repeal of Carbon Dioxide Emission Guidelines for Existing Stationary Sources: Electric Utility Generating Units; Emission Guidelines for Greenhouse Gas Emissions from Existing Electric Utility Generating Units; Revisions to Emission Guideline Implementing Regulations; Revisions to New Source Review Program",
    "2017 Clean Power Plan Repeal"
  ) %>%
  str_replace(
    "Payday, Vehicle, Title and Certain High-Cost Installment Loans",
    "2016 Payday Loans"
  ) %>%
  str_replace(
    "Standards of Performance for Greenhouse Gas Emissions from Existing Sources: Electric Utility Generating Units",
    "2013 Clean Power Plan"
  ) %>%
  str_replace(
    "Greenhouse Gas New Source Performance Standard for Electric Generating Units",
    "2011 Greenhouse Gas NSPS"
  ) %>%
  str_replace(
    "Oil and Natural Gas Sector -- New Source Performance Standards, National Emission Standards for Hazardous Air Pollutants, and Control Techniques Guidelines",
    "2010 Oil and Gas NSPS"
  ) %>%
  str_replace(
    "Removing the Gray Wolf from the List of Endangered and Threatened Wildlife and Maintaining Protections for the Mexican Wolf by Listing It as Endangered",
    "2013 Gray Wolf removal"
  ) %>%
  str_replace(
    ".*of the 2019-2024 Draft Proposed Outer Continental Shelf .OCS. Oil and Gas Leasing Program and Notice of Intent .NOI. to Prepare a Programmatic Environmental Impact Statement .EIS.",
    "2017 Outer Continental Shelf Oil and Gas Leasing"
  ) %>%
  str_replace(
    "Review of Certain National Monuments Established Since 1996; Notice of Opportunity for Public Comment",
    "2017 National Monuments Review"
  ) %>%
  str_replace(
    "Standards of Performance for Greenhouse Gas Emissions for New Stationary Sources: Electric Utility Generating Units",
    "2013 Greenhouse Gas Emissions for New Stationary Sources"
  ) %>%
  str_replace(
    "Request for Comments on the Proposed 2017-2022 OCS Oil and Gas Leasing Program.*",
    "2016 Outer Continental Shelf Oil and Gas Leasing")  

topdockets$mass %<>% 
  str_replace("Not unique", "Small batch")
  

topdockets %<>% 
  group_by(organization) %>%
  mutate(orgTotal = sum(numberOfCommentsReceived)) %>% 
  ungroup()

topdockets %>% 
  filter(!is.na(mass)) %>% 
  mutate(toporgs = ifelse(orgTotal %in% sort(unique(topdockets$orgTotal), decreasing = T)[1:20],
                             organization, NA) ) %>%
  ggplot() +
  geom_col(aes(x = docketTitle, y = numberOfCommentsReceived/1000, fill = toporgs)) +
  coord_flip() +
  facet_grid(. ~ mass, scales = "free_x") + 
  labs(x = "", y = "Number of Comments (thousands)", fill = "") + 
  theme_bw()

topdockets %>% group_by(docketId, organization) %>% summarise(n = sum(numberOfCommentsReceived)) %>% arrange(-n)


