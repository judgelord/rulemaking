
# NPRMS 
pr <- map_dfr(.x = c(1),
                .f = search_page4,
                documenttype = "Proposed Rule",
                lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))


pr_open <- pr %>% filter(openForComment)


ejpr <- map_dfr(.x = c(1),
                .f = search_keyword_page4,
                documenttype = "Proposed Rule",
                keyword = "environmental justice",
                lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))




climatepr <- map_dfr(.x = c(1),
                            .f = search_keyword_page4,
                            documenttype = "Proposed Rule",
                            keyword = "climate change",
                            lastModifiedDate = Sys.time() %>% str_remove(" [A-Z]"))







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




save(pr_open, 
     ejpr, 
     climatepr, 
     ejcomments,# (just 5 k )
     climatecomments,# (just 5 k )
     file = here::here("data", "open.Rdata"))
