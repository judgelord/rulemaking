last <- d
all <- last 

load("500comments.Rdata")
all %<>% full_join(d)

load("1000comments.Rdata")
all %<>% full_join(d)

load("1500comments.Rdata")
all %<>% full_join(d)

load("2000comments.Rdata")
all %<>% full_join(d)

load("2500comments.Rdata")
all %<>% full_join(d)

load("3000comments.Rdata")
all %<>% full_join(d)

load("3500comments.Rdata")
all %<>% full_join(d)

load("4000comments.Rdata")
all %<>% full_join(d)

load("4500comments.Rdata")
all %<>% full_join(d)

load("5000comments.Rdata")
all %<>% full_join(d)

load("5500comments.Rdata")
all %<>% full_join(d)

load("6000comments.Rdata")
all %<>% full_join(d)

load("6500comments.Rdata")
all %<>% full_join(d)

load("7000comments.Rdata")
all %<>% full_join(d)

load("7500comments.Rdata")
all %<>% full_join(d)

load("8000comments.Rdata")
all %<>% full_join(d)

load("8500comments.Rdata")
all %<>% full_join(d)

load("9000comments.Rdata")
all %<>% full_join(d)

save(all, file = "allcomments.Rdata")
#####################################
load("allcomments.Rdata")

total <- sum(all$numberOfCommentsReceived)
save( total , file = "totalcomments.tex")

rins <- length(unique(all$rin))
save( rins, file = "RINcount.tex")

massRINs <- filter(all, numberOfCommentsReceived > 100)

total <- sum(massRINs$numberOfCommentsReceived)
total
save( total , file = "masscommentsTotal.tex")

massRINs <- unique(massRINs$rin)
total <-  length(unique(massRINs$rin))
total
save( total , file = "massRINcount.tex")


massRINs <- filter(all, rin %in% massRINs)

save( massRINs , file = "massRINs.Rdata")

total <- sum(massRINs$numberOfCommentsReceived)
total
save( total , file = "massRINsTotal.tex")

