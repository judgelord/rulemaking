# could save a bit of unlisting: https://rdrr.io/github/enricoschumann/esutils/man/pdf2txt.html

# pdftotext
files <- list.files(here("comments")) # %>% head()



#FIXME move to functions 
library(pdftools)

# a function to read pdf into a string
pdf_to_text <- function(file){
  message(file)
  # default value is NA
  text <- NA
  # if the file is a pdf, run pdf_text
  if(str_detect(file, "pdf")){
    text <- pdf_text(here("comments", file))  %>% 
      # collapse the list of pages 
      # FIXME 
      unlist() %>% 
      paste(collapse = "\n<pagebreak>\n") %>% 
      unlist() %>% 
      as.character() %>% 
      paste(sep = "\n<pagebreak>\n")
  }
  return(text)
}

# a function to read pdfs and save a txt file
pdf_to_txt <- function(file){
agency <- str_extract(file, "[A-Z]*")

docket <- str_extract(file, "[A-Z]*-[0-9]*-[0-9]*")

# create new directories if needed
if (!dir.exists(here("comments", agency, docket) ) ){
  dir.create(here("comments", agency), showWarnings = FALSE)
  dir.create(here("comments", agency, docket))
}

# save txt file
write_file(pdf_to_text(file), 
           path = here("comments", agency, docket, str_replace(file, "pdf", "txt") ) )
}

length(files)
walk(files, possibly(pdf_to_txt, otherwise = print("nope")))

list.files(here::here("comments", "ACF", "ACF-2015-0002"))
