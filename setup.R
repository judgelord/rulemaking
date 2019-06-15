options(stringsAsFactors = FALSE)

requires <- c("gmailr", 
              "tidyverse", 
              "dplyr",
              "gdata", 
              "magrittr",
              "reshape2",
              "scales",
              "magrittr",
              "XML",
              "stringr", 
              "here",
              "gridExtra",
              "httr",
              "jsonlite",
              "tm",
              "tidytext",
              "topicmodels",
              "textfeatures",
              "cleanNLP",
              # "clusters",
              # "rjags",
              # "bayesmix",
              # "MCMCpack",
              "gtools",
              #"textreadr",
              "tidyverse")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
rm(requires, to_install)

library(tidyverse)
library(dplyr) # in case tydyverse fails (problem on linux)
library(ggplot2); theme_set(theme_bw())
options(
  ggplot2.continuous.color = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_color_discrete <- function(...)
  scale_color_viridis_d(...)
scale_fill_discrete <- function(...)
  scale_fill_viridis_d(...)
library(gridExtra)
library(jsonlite)

library(magrittr)
library(XML)
library(stringr)
library(reshape2)
library(scales)
library(here)
library(httr)
library(tm)
library(tidytext)
library(topicmodels)
library(textfeatures)
library(cleanNLP)
# library(textreadr)

knitr::opts_chunk$set(echo = TRUE, 
                      cache = FALSE, 
                      fig.width=8.5, fig.align = 'center', fig.path='Figs/',
                      warning=FALSE, message=FALSE)


# function to fill NAs
# FIXME 
# replace with fill()
CopyIfNA <- function(x, na.rm = FALSE, ...) na.locf(x, na.rm = na.rm, ...)

