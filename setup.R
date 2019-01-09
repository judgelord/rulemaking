options(stringsAsFactors = FALSE)

requires <- c("gmailr", 
              "dplyr", 
              "ggplot2", 
              "gdata", 
              "magrittr",
              "XML",
              "stringr", 
              "stargazer",
              "visreg",
              "reshape2",
              "scales",
              "magrittr",
              "XML",
              "stringi",
              "stringr", 
              "here",
              "gridExtra",
              "httr",
              "jsonlite",
              "tidyverse")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
rm(requires, to_install)

library(tidyverse)
library(dplyr) # in case tydyverse fails (problem on linux)
library(ggplot2); theme_set(theme_bw())
library(magrittr)
library(XML)
library(stringr)
library(stargazer)
library(reshape2)
library(scales)
library(here)
library(gridExtra)
library(httr)
library(jsonlite)


# function to fill NAs
CopyIfNA <- function(x, na.rm = FALSE, ...) na.locf(x, na.rm = na.rm, ...)
