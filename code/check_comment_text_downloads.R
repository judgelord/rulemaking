library(here)
library(tidyverse)
library(magrittr)


 files <- here::here() |>  str_replace("rulemaking", "Dropbox (University of Michigan)/judgelord/comment_text") |>
   list.files(recursive = T)

 files |>
   head()

 agencies <- files |> str_remove("/.*") |> unique()
 head(agencies)

 dockets <- files |> str_remove(".*?/")  |> str_remove("/.*") |> unique()
 head(dockets)
