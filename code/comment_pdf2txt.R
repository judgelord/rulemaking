# pdf2txt (pdfminer) with system commands
# e.g. system("pdf2txt.py -o comments/NPS-2018-0007-0293-1.txt comments/NPS-2018-0007-0293-1.pdf")

library(tidyverse)
library(here)

# files in comments folder
files <- list.files(here::here("comments"))

# a tibble of info to make command line commands
d <- tibble(file = files) %>% 
  mutate(agency = str_extract(file, "[A-Z]*"),
         docket = str_extract(file, "[A-Z]*-[0-9]*-[0-9]*"),
         newfile = str_replace(file, "pdf", "txt"),
         command = str_c("pdf2txt.py -o comments/", agency, "/", docket, "/", newfile, 
                         " comments/", file) )
  
  
# a frunction to make a directory for a docket
make_dir <- function(docket){
  
  agency = str_extract(docket, "[A-Z]*")
  
  # create new directories if needed
  if (!dir.exists(here("comments", agency, docket) ) ){
    dir.create(here("comments", agency), showWarnings = FALSE)
    dir.create(here("comments", agency, docket))
  }
}

# make directories for all dockets
walk(unique(d$docket), make_dir)
  
system(d$command[6])

walk(d$command, possibly(system, otherwise = print("nope")))


?reticulate::repl_python
