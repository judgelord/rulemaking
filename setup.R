options(stringsAsFactors = FALSE)

requires <- c("gmailr", 
              "dplyr", 
              "magrittr",
              "XML",
              "stringi",
              "stringr", 
              "here",
              "tidyverse")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )

library(tidyverse)
library(dplyr) # in case tydyverse fails (problem on linux)
library(magrittr)
library(XML)
library(stringr)
library(stringi)

library(here)


# function to fill NAs
CopyIfNA <- function(x, na.rm = FALSE, ...) na.locf(x, na.rm = na.rm, ...)
