#Observations per docket for position
#####################################
# findDocket <- d %>% 
#   select(docketId, document_id, attachmentCount, numberOfCommentsReceived, agencyAcronym, title, commenttext, organization) %>% 
#   filter(document_id == "NPS-2015-0006-0008")

#creating position variable
d %<>% 
  #   mutate(position = NA) %>% 
  #FWS
  #putting turtle species on international trade list, not org.comment
  mutate(position = ifelse(is.na(position) & document_id == "FWS-HQ-ES-2013-0052-0067", "4", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "FWS-HQ-ES-2013-0052-0016", "5", position)) %>% #not an org.comment
  #listing white rhino as threatened
  mutate(position = ifelse(is.na(position) & document_id == "FWS-HQ-ES-2013-0055-0577", "4", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "FWS-HQ-ES-2013-0055-0580", "4", position)) %>% 
  #reclassification of african elephant to endangered
  mutate(position = ifelse(is.na(position) & document_id == "FWS-HQ-ES-2016-0010-1483", "1", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "FWS-HQ-ES-2016-0010-0446", "4", position)) %>% #check that this is now org.comment TRUE
  #pangolin
  mutate(position = ifelse(is.na(position) & document_id == "FWS-HQ-ES-2016-0012-0008", "4", position)) %>% 
  #Ivory
  mutate(position = ifelse(is.na(position) & document_id == "FWS-HQ-IA-2013-0091-0817", "1", position)) %>% #they want more specifics, so they are opposed, intersting one
  mutate(position = ifelse(is.na(position) & document_id == "FWS-HQ-IA-2013-0091-5613", "3", position)) %>% #check, instruments
  mutate(position = ifelse(is.na(position) & document_id == "FWS-HQ-IA-2013-0091-5719", "4", position)) %>% 
  #regulations governing non-federal oil and gas development 
  mutate(position = ifelse(is.na(position) & document_id == "FWS-HQ-NWRS-2012-0086-0032", "4", position)) %>% #confirm?
  #spotted owl
  mutate(position = ifelse(is.na(position) & document_id == "FWS-R1-ES-2011-0112-0882", "2", position)) %>%
  #NPS
  #altering rules for hunting wildlife in Alaska, permitting aggressive take and harvest practices
  mutate(position = ifelse(is.na(position) & document_id == "NPS-2018-0005-160808", "2", position)) %>%
  mutate(position = ifelse(is.na(position) & document_id == "NPS-2018-0005-78196", "1", position)) %>% #read articles, can't read from just commenttext
  mutate(position = ifelse(is.na(position) & document_id == "NPS-2018-0005-175662", "3", position)) %>% #not an org.comment
  #demonstration regulations
  mutate(position = ifelse(is.na(position) & document_id == "NPS-2018-0007-49456", "2", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "NPS-2018-0007-49527", "2", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "NPS-2018-0007-7913", "1", position)) %>% 
  #sport hunting and trapping, NPS does not adopt state of alaskas take and harvest practices
  mutate(position = ifelse(is.na(position) & document_id == "NPS-2014-0004-1163", "5", position)) %>% #support preventions, also doesn't want hunting dogs
  #proposed rule and EIS on the revision of governing non-federal oil and gas development within the boundaries of units of the national park system
  mutate(position = ifelse(is.na(position) & document_id == "NPS-2015-0006-0018", "3", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "NPS-2015-0006-0015", "1", position)) %>% #assumption (? just put them in to look further into)
  mutate(position = ifelse(is.na(position) & document_id == "NPS-2015-0006-0008", "5", position)) %>% #assumption (? just put them in to look further into)
  #EERE
  #new energy conservation standards for manufactured housing
  mutate(position = ifelse(is.na(position) & document_id == "EERE-2009-BT-BC-0021-0440", "1", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "EERE-2009-BT-BC-0021-0174", "5", position)) %>% 
  #conservation standards for refrigerated beverage vending machines 
  mutate(position = ifelse(is.na(position) & document_id == "EERE-2013-BT-STD-0022-0052", "3", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "EERE-2013-BT-STD-0022-0051", "5", position)) %>%  #assumption
  #FDA
  #Food Labeling; Revision of the Nutrition and Supplement Facts #split across administrations
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2012-N-1210-2019", "", position)) %>% #opposed to the new changes made on an old docket in Trump
  #Standards for the Growing, Harvesting, Packing, and Holding of Produce for Human Consumption
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2011-N-0921-19154", "5", position)) %>% #National Onion #supports the extension, doesn't support the docket
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2011-N-0921-1257", "2", position)) %>% #National Onion #doesn't support
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2011-N-0921-1235", "2", position)) %>% 
  #General and Plastic Surgery Devices: Restricted Sale, Distribution, and Use of Sunlamp Products 
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2015-N-1765-4781", "5", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2015-N-1765-1002", "2", position)) %>% 
  #Current Good Manufacturing Practice and Hazard Analysis and Risk-Based Preventive Controls For Human Food 
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2011-N-0920-1752", "2", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2011-N-0920-1182", "4", position)) %>% 
  #Food Labeling; Nutrition Labeling of Standard Menu Items in Restaurants and Similar Retail Food Establishments #split across administrations
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2011-F-0172-1649", "", position)) %>% #opposed to the delays made in Trump
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2011-F-0172-0457", "4", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2011-F-0172-2860", "3", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2011-N-0920-1152", "3", position)) %>% 
  #prohibiting the extralabel use of cephalosporin antimicrobial drugs in food-producing animals
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2008-N-0326-0286", "2", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2008-N-0326-0255", "4", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2008-N-0326-0166", "4", position)) %>% 
  #EIS- Investigational Use of Oxitec OX513A Mosquitoes
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2014-N-2235-1358", "oppose", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2014-N-2235-1200", "support", position)) %>% 
  #EIS- Preliminary Finding of No Significant Impact For a Genetically Engineered Atlantic Salmon
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2011-N-0899-1218", "oppose", position)) %>%
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2011-N-0899-0737", "support", position)) %>% #only support example could find
  #Deeming Tobacco Products To Be Subject to the Federal Food, Drug, and Cosmetic Act
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2014-N-0189-60210", "5", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2014-N-0189-72301", "2", position)) %>% 
  #Supplemental Applications Proposing Labeling Changes for Approved Drugs and Biological Products
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2013-N-0500-0055", "4", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2013-N-0500-0019", "2", position)) %>% 
  #Nicotine Exposure Warnings and Child-Resistant Packaging for Liquid Nicotine, Nicotine-Containing E-Liquid(s), and Other Tobacco Products
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2015-N-1514-0008", "4", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2015-N-1514-0133", "3", position)) %>% 
  #Menthol in Cigarettes, Tobacco Products
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2013-N-0521-0397", "5", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "FDA-2013-N-0521-0377", "4", position)) %>% 
  #VA
  #AP44- Proposed Rule - Advanced Practice Registered Nurses
  mutate(position = ifelse(is.na(position) & document_id == "VA-2016-VHA-0011-60042", "2", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "VA-2016-VHA-0011-216807", "4", position)) %>% 
  #OSHA
  #Tracking of Workplace Injuries and Illnesses #split administration
  mutate(position = ifelse(is.na(position) & document_id == "OSHA-2013-0023-2031", "2", position)) %>% #commenting on the newer repeal of improve tracking of workplace injuries
  mutate(position = ifelse(is.na(position) & document_id == "OSHA-2013-0023-0240", "2", position)) %>% #commenting on the older improve tracking of workplace injuries
  mutate(position = ifelse(is.na(position) & document_id == "OSHA-2013-0023-1387", "4", position)) %>% #commenting on the older improve tracking of workplace injuries
  #Occupational Exposure to Beryllium, commenting on new proposal that revokes 2017 provisions
  mutate(position = ifelse(is.na(position) & document_id == "OSHA-H005C-2006-0870-2093", "2", position)) %>% 
  #Occupational Exposure to Crystalline Silica, face significant risk
  mutate(position = ifelse(is.na(position) & document_id == "OSHA-2010-0034-1964", "2", position)) %>% 
  mutate(position = ifelse(is.na(position) & document_id == "OSHA-2010-0034-2166", "4", position)) %>% 
  #ATF
  #Bump-Stock-Type Devices  
  mutate(position = ifelse(is.na(position) & document_id == "ATF-2018-0001-34129", "4", position)) %>%
  mutate(position = ifelse(is.na(position) & document_id == "ATF-2018-0001-34129", "4", position))