# This script joins data on all comments from regulations.gov
# Completing missing from any one dataset
load(here::here("ascending2/allcomments.Rdata"))
dim(all)
all2 <- all

load(here::here("ascending/allcomments.Rdata"))
dim(all)

all %<>% full_join(all2)
dim(all)

comments_all<- all %>% 
  mutate(file1 = ifelse(attachmentCount > 0,  
                        str_c(documentId, "-1.pdf"), 
                        NA),
         url1 = ifelse(attachmentCount > 0,  
                       str_c("https://www.regulations.gov/contentStreamer?documentId=",
                            documentId,
                            "&attachmentNumber=1"), 
                       NA))

comments_all %>% head() 

comments_all %>% filter(agencyAcronym == "CFPB",
                        attachmentCount > 0,
                        #docketType == "Rulemaking",
                        nchar(commentText) > 200) %>% 
  slice(1) %>% 
  gather(key = "field", value = "example") %>% kable()
  write_csv(path = "comment_metadata_example.csv")


# Create R data
save(comments_all, file = "comment_metadata.Rdata")
nrow(comments_all)
head(comments_all)

# Create RSQLite database
con <- dbConnect(SQLite(), here::here("comment_metadata.sqlite"))

# check 
list.files()

dbListTables(con)
dbWriteTable(con, "comments_all", comments_all, overwrite = T)
dbListTables(con)

dbListFields(con, "comments_all")
# dbReadTable(con, "comments_all") # oops

# fetch results:
res <- dbSendQuery(con, "SELECT * FROM comments_all WHERE agencyAcronym = 'CFPB'")

dbFetch(res)
dbClearResult(res)


# Create RSQLite database
con <- dbConnect(SQLite(), here::here("comment_metadata_CFPB.sqlite"))
# check 
list.files()

dbListTables(con)
dbWriteTable(con, "comments_all", comments_all %>% filter(agencyAcronym == "CFPB"), overwrite = T)
dbListTables(con)
