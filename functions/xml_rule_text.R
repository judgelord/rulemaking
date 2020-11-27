library(xml2)
xml_rule_text <- . %>% 
  read_xml() %>% 
  xml_children() %>% 
  xml_children() %>% 
  xml_text() %>% 
  str_squish() %>% 
  tibble::enframe(value = "text",  name = "id") 