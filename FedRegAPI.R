install.packages("federalregister")
library(federalregister)
pi_search(type='RULE', fields=c('document_number','raw_text_url'), version = 'v3')


fr_get("2014-05323")

fr_search(term = "environmental justice", fields=NULL, per_page=NULL, page=NULL,
          order='relevance', version='v1', getopts = NULL)

clinton <- fr_search(presidential_document_type='executive_order', president='william-j-clinton', per_page=1000)

fr_search(presidential_document_type='executive_order',
                    fields=c('executive_order_number','president','raw_text_url','document_number'),
                   per_page=30)
          

fr_search(term='climate', publication_date=list(gte='2013-01-01',lte='2017-03-31'), fields = )




arecord <- fr_get('E9-1719')
full <- httr::content(httr::GET(arecord[[1]]$raw_text_url), "text", encoding = "UTF-8")
cat(substring(full, 1, 2000))




ej <- read.csv("ej.csv")
names(ej$comment_url)
head(ej)
tail(ej)
ej$agency_names


Will someone from the UCF School of Public Administration be updating the status of this search on #PsJMinfo? 
