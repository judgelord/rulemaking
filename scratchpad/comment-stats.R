source(here("setup.R"))

# Load all comments as all
load(here("data/allcomments.Rdata"))

total <- sum(all$numberOfCommentsReceived)
save( total , file = "totalcomments.tex")

rins <- length(unique(all$rin))
save( rins, file = "RINcount.tex")

massRINs <- filter(all, numberOfCommentsReceived > 99)

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

