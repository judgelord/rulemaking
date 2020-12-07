
agencies <- d$agencyAcronym %>% unique()

dir <- "agencies"

if (!dir.exists(here::here(dir) ) ){
  dir.create(here::here(dir), showWarnings = FALSE)
}

makefolder <- function(agency){
if (!dir.exists(here::here(dir, agency) ) ){
  dir.create(here::here(dir, agency), showWarnings = FALSE)
}}

makefolder(agencies[1])

walk(agencies, makefolder)

