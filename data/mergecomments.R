library(here)
source(here("setup.R"))
load(here("ascending/lastcomments.Rdata"))
all <- d

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

#####################################
# save all
save(all, 
     file = "ascending/allcomments.Rdata")

#####################################
# save mass comment campaigns only to data folder
mass <- filter(all, numberOfCommentsReceived > 99)
save(mass,
     file = "data/masscomments.Rdata")
