options(stringsAsFactors = FALSE)

requires <- c("gmailr", 
              "dplyr", 
<<<<<<< HEAD
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
=======
              "magrittr",
              "XML",
              "stringi",
              "stringr", 
>>>>>>> eabd462b71798f61eac51b51d134cc3f6613f2e5
              "here",
              "tidyverse")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )

library(tidyverse)
library(dplyr) # in case tydyverse fails (problem on linux)
<<<<<<< HEAD
library(ggplot2)
=======
>>>>>>> eabd462b71798f61eac51b51d134cc3f6613f2e5
library(magrittr)
library(XML)
library(stringr)
library(stringi)
<<<<<<< HEAD
library(stats)
library(zoo)
library(stargazer)
library(visreg)
library(reshape2)
library(scales)
=======

>>>>>>> eabd462b71798f61eac51b51d134cc3f6613f2e5
library(here)


# function to fill NAs
CopyIfNA <- function(x, na.rm = FALSE, ...) na.locf(x, na.rm = na.rm, ...)
<<<<<<< HEAD

=======
>>>>>>> eabd462b71798f61eac51b51d134cc3f6613f2e5
