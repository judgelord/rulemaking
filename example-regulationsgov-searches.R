
## Example searches using API call functions ##

## load packages
source("setup.R")

## get API call functions 
source("RegsGovAPI.R")

# add your API Key
source("api-key.R") 
api_key <- api_key

# Select n most recently posted results (see the RegsGovAPI.R script to change defaults for n most/least ____ results)
n = 2000

## Select document types from this list. Seperate multiple types with a comma and a space:
## N: Notice, 
## PR: Proposed Rule, 
## FR: Rule, 
## O: Other, 
## SR: Supporting & Related Material, 
## PS: Public Submission

# Search 
d <- search.docs(documenttype = "PS" , 
                 n = 1000)

d$postedDate %<>% as.Date()
d$commentStartDate %<>% as.Date()

ggplot(d) + geom_histogram(aes(x = postedDate, 
                              fill = documentType))
names(d)

# Select agencies, again seperating multiple agencies with a comma and a space
# Search
d <- search.agency.docs(agency = "SEC, FAA, FRA", 
                        documenttype= "N, PR, FR",
                        n = 2000)

d$postedDate %<>% as.Date()
d$commentStartDate %<>% as.Date()

ggplot(d) + geom_histogram(aes(x = postedDate,
                               fill = agencyAcronym)) + theme_minimal()

# Select docket
docket <- "EPA-HQ-OAR-2017-0355"
# Search
d <- search.docket(docket = "EPA-HQ-OAR-2017-0355", 
             documenttype = "PS",
             n = 2000)

d$postedDate %<>% as.Date()
d$commentStartDate %<>% as.Date()

ggplot(d) + geom_histogram(aes(x = postedDate, 
                               fill = documentType)) +  theme_minimal() +
  labs(title = docket)


# keywords 
## Note that keyword serches return an additional feature "summary" which is text before and after
search.keywords(keywords = "environmental justice",
                documenttype = "PS",
                n = 2000)

d$postedDate %<>% as.Date()
d$commentStartDate %<>% as.Date()

ggplot(d) + geom_histogram(aes(x = postedDate, 
                               fill = documentType)) +  theme_minimal() 







