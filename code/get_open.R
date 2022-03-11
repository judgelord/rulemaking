

source("setup.R")

#FIXME, split into function-based scripts for packaging
source("functions/regulations-gov-API4.R")

# source(here::here("code", "search_page4.R"))
source(here::here("code", "search_keyword_page4.R"))


# NPRMS 
pr <- map_dfr(.x = c(1:20),
                .f = search_page4,
                documenttype = "Proposed Rule",
                lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))


pr_open <- pr %>% filter(openForComment)


ejpr <- map_dfr(.x = c(1:20),
                .f = search_keyword_page4,
                documenttype = "Proposed Rule",
                keyword = "environmental justice",
                lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))




climatepr <- map_dfr(.x = c(1:20),
                            .f = search_keyword_page4,
                            documenttype = "Proposed Rule",
                            keyword = "climate change",
                            lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))






# init 
ejcomments1 <- map_dfr(.x = c(1),
                            .f = search_keyword_page4,
                            documenttype = "Public Submission",
                            keyword = "environmental justice",
                            lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))


# first 5k
ejcomments <- map_dfr(.x = c(1:20),
                           .f = possibly(search_keyword_page4, otherwise = ejcomments1),
                           documenttype = "Public Submission",
                           keyword = "environmental justice",
                           lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))

ggplot() + 
        aes(x = ejcomments$postedDate %>% as.Date() ,
            y = 0) +
        geom_jitter()

# looping over the rest - you can stop this at any time when you think you have gone far enough back in time 
date <- ejcomments$lastModifiedDate %>% min()

search_keyword_page4_comment_loop(ejcomments, ejcomments1, keyword = "environmental justice")

# object data_comments is saved as temp_data_comments[date].Rdata
load(here::here("data",  str_c("temp_data_comments", Sys.Date(), ".Rdata")))

# join the first 5k with the rest
ejcomments %<>% full_join(data_comments)

# CLIMATE 
climatecomments1 <- map_dfr(.x = c(1),
                            .f = search_keyword_page4,
                            documenttype = "Public Submission",
                            keyword = "climate change",
                            lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))


# first 5k
climatecomments <- map_dfr(.x = c(1:20),
                           .f = possibly(search_keyword_page4, otherwise = climatecomments1),
                           documenttype = "Public Submission",
                           keyword = "climate change",
                           lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))

ggplot() + 
        aes(x = climatecomments$postedDate %>% as.Date() ,
            y = 0) +
        geom_jitter()


# looping over the rest - you can stop this at any time when you think you have gone far enough back in time 
date <- climatecomments$lastModifiedDate %>% min()

search_keyword_page4_comment_loop(climatecomments, climatecomments1, keyword = "climate change")

# object data_comments is saved as temp_data_comments[date].Rdata
load(here::here("data",  str_c("temp_data_comments", Sys.Date(), ".Rdata")))

# join the first 5k with the rest
climatecomments %<>% full_join(data_comments)


save(pr_open, 
     ejpr, 
     climatepr, 
     ejcomments,# (just 5 k )
     climatecomments,# (just 5 k )
     file = here::here("data", str_c("open-", Sys.Date(), ".Rdata")))
