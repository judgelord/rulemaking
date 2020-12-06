# rewriting old code for 10-gram window matching 

# load libraries and functions
source(here::here("setup.R"))

# 1. Clean up text 

# 2. Remove NPRM 10 grams

# 3. Save clean (first 10k?)

# 4. list with a logical vector for each comment with > x tengrams

#  two LWV letters at 29 and 40

agency <- "NPS"
docket <- "NPS-2018-0007"
nprm <- "https://www.federalregister.gov/documents/full_text/xml/2018/08/15/2018-17386.xml"

# get txt files from a directory with here() 
comments <- tibble( path = list.files(here::here("comment_text", 
                                                 agency, 
                                                 docket),
                                      full.names = T )) 

comments %<>% top_n(5)  #FIXME just a few comments for now

# a function to parse comments into 10-word phrases and identify matching phrases in other comments of the NPRM
comment_tengrams <- function(nprm, comments){
  
  # read in rule text from federal register
  # see https://github.com/judgelord/rulemaking/blob/master/functions/xml_rule_text.R
  pr_text <- xml_rule_text(nprm) %>% 
    summarise(text = text %>% clean_string() ) %>%  # fr_document_id = 
    tengram() %>% 
    filter(!is.na(tengram))
  
  # make a data frame out of file paths ending in txt
  d <- comments %>% 
    filter( str_detect(path, "txt")) %>% 
    # in SQL, CFPB file names are regs_dot_gov_document_id, shortened to document_id for now
    mutate( document_id = path %>% 
              str_remove(".*/")  %>% 
              str_remove("\\..*") 
    )
  
  # parse each document with the read_grams function 
  # see https://github.com/judgelord/rulemaking/blob/master/functions/tengram.R
  d %<>% 
    mutate(tengrams = path %>% map(possibly(read_grams, 
                                            otherwise = list(tengram = "404error")
                                            )
                                   ) 
           ) #%>% filter(tengrams != "404error") # if you want to drop comments that fail to read
  
  # map each document to all others
  d %<>% 
    mutate(
      text = tengrams %>% 
        # diff with the nprm
        map2(list(pr_text$tengram), match_tibble) %>% 
        # reassemble text from the first word of each tengram
        map(word1),
      # diff with all other comments 
      reuse = tibble(document_id2 = list(document_id),
                     reuse = tengrams %>% map(match2)
                     )
      ) %>% 
    # turn the tibble of lists into list of tibbles
    group_by(document_id) %>% 
    mutate(reuse = reuse %>% flatten() %>% as_tibble() %>% list() ) 
  
  # drop variables we no longer need
  d %<>%  select(-path, -tengrams)
  
  return(d)
} # end function







# test function
d <- comment_tengrams(comments, nprm)

# inspect
save(d, file = here::here("data", str_c(docket, "-tengrams.Rdata")))




# TODO apply tengram_match_to_text












################################################################################
# tests 

# copied from the NPRM
unlist(d$text[[3]]$word)[unlist(d$text[[3]]$match)] %>% paste(collapse =" ")
#FIXME with purrr


d$reuse
# reuse with itself should be all TRUE
# reuse for document 3
d$reuse[[3]]$reuse[[1]][[3]]

d %>% 
  filter(document_id == d$document_id[3]) %>% # select a file
  select(-text) %>% # select just the doc name and reuse table (otherwise unnest duplicates the text table for every reuse observation)
  unnest(reuse) %>%             # unnest reuse tibble
  unnest(document_id2, reuse) %>% # unnest document_id and reuse lists 
  unnest(reuse) %>% # unnest reuse logical
  group_by(document_id, document_id2) %>% 
  summarise(percent_match = sum(reuse)/n() )

d$text
d %>% filter(nrow(text) > 1)
