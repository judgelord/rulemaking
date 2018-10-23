options(stringsAsFactors = FALSE)

requires <- c("gmailr", 
              "dplyr", 
              "ggplot2", 
              "gdata", 
              "magrittr",
              "XML",
              "stats",
              "zoo",
              "stringi",
              "stringr", 
              "stargazer",
              "visreg",
              "reshape2",
              "scales",
              "here",
              "tidyverse")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )

library(tidyverse)
library(dplyr) # in case tydyverse fails (problem on linux)
library(ggplot2)
library(magrittr)
library(XML)
library(stringr)
library(stringi)
library(stats)
library(zoo)
library(stargazer)
library(visreg)
library(reshape2)
library(scales)
library(here)


# function to fill NAs
CopyIfNA <- function(x, na.rm = FALSE, ...) na.locf(x, na.rm = na.rm, ...)

