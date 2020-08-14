# This script saves "allcomments.Rdata"--all comments posted to regulations.gov
# It joins Rdata subsets of comment metadata from the regulations.gov API 
# These data are created by "functions/regulations-gov-get-all-comments.R"
source(here::here("setup.R"))
directory <- "ascending2"

load(here("ascending2", "lastcomments.Rdata"))
all <- d

load(here(directory,  "13000comments.Rdata"))
all %<>% full_join(d)

load(here(directory, "12500comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "12000comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "11500comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "11000comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "10500comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "10000comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "9500comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "9000comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "8500comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "8000comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "7500comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "7000comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "6500comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "6000comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "5500comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "5000comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "4500comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "4000comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "3500comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "3000comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "2500comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "2000comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "1500comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "1000comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "500comments.Rdata"))
all %<>% full_join(d)

dim(all)

#####################################
# save all
save(all, 
     file = here(directory,  "allcomments.Rdata"))

#####################################
# save comments with non-attachment text only to data folder
textcomments <- filter(all, nchar(commentText)> 240)
dim(textcomments)
save(textcomments,
     file = here(directory,  "textcomments.Rdata"))

#####################################
# save mass comment campaigns only to data folder
mass <- filter(all, numberOfCommentsReceived > 99)
save(mass,
     file = "data/masscomments.Rdata")


