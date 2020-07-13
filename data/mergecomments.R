# This script saves "allcomments.Rdata"--all comments posted to regulations.gov
# It joins Rdata subsets of comment metadata from the regulations.gov API 
# These data are created by "functions/regulations-gov-get-all-comments.R"
source(here::here("setup.R"))
load(here("ascending/lastcomments.Rdata"))
all <- d

load(here("ascending/13000comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/12500comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/12000comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/11500comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/11000comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/10500comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/10000comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/9500comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/9000comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/8500comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/8000comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/7500comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/7000comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/6500comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/6000comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/5500comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/5000comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/4500comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/4000comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/3500comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/3000comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/2500comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/2000comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/1500comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/1000comments.Rdata"))
all %<>% full_join(d)

load(here("ascending/500comments.Rdata"))
all %<>% full_join(d)

dim(all)

#####################################
# save all
save(all, 
     file = "ascending/allcomments.Rdata")

#####################################
# save comments with non-attachment text only to data folder
textcomments <- filter(all, nchar(commentText)> 240)
dim(textcomments)
save(textcomments,
     file = "ascending/textcomments.Rdata")

#####################################
# save mass comment campaigns only to data folder
mass <- filter(all, numberOfCommentsReceived > 99)
save(mass,
     file = "data/masscomments.Rdata")


