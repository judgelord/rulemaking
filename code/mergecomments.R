# This script saves "allcomments.Rdata"--all comments posted to regulations.gov
# It joins Rdata subsets of comment metadata from the regulations.gov API 
# These data are created by "functions/regulations-gov-get-all-comments.R"
source(here::here("setup.R"))

# third pull
directory <- "ascending3"

load(here(directory, "lastcomments.Rdata"))
head(d$postedDate)
all <- d

load(here(directory,  "14000comments.Rdata"))
all %<>% full_join(d)

load(here(directory,  "13500comments.Rdata"))
all %<>% full_join(d)

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
# second pull 
#####################################
directory <- "ascending2"

load(here(directory, "lastcomments.Rdata"))
all %<>% full_join(d)

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


all %>% count(openForComment)
all %>% count(documentStatus)
all %>% count(allowLateComment)
all %>% count(documentType)

dim(all)

all %<>% select(-openForComment, 
                -allowLateComment, 
                -documentType) %>%
  distinct()

# save all
save(all, 
     file = here("data",  "allcomments2020.Rdata"))

comment_metadata <- all %>% namingthings() 
names(comment_metadata)
dim(comment_metadata)
head(comment_metadata)

# save all
save(comment_metadata, 
     file = here("data",  "comment_metadata2020.Rdata"))

comment_meta_min <- comment_metadata %>% 
  distinct(docket_id, id, posted_date, organization, title, submitter_name, 
           attachment_count, number_of_comments_received)

# save minimal set
save(comment_meta_min, 
     file = here("data",  "comment_meta_min.Rdata"))

#####################################
# save comments with non-attachment text only to data folder

textcomments <- filter(comment_metadata, nchar(comment_text)> 240)
dim(textcomments)
save(textcomments,
     file = here("data",  "textcomments.Rdata"))

#####################################
# save mass comment campaigns only to data folder
mass <- filter(comment_metadata, number_of_comments_received > 99)
dim(mass)
save(mass,
     file = here("data",  "comment_meta_mass.Rdata"))



