This is a project to pull and merge various data on US federal agency rulemaking.

## Regulations.gov (rule metadata and public comments)

I have collected two datasets from regulations.gov, one for all rules, proposed rules, and notices and a second for all public comments:

- Metadata for all rules, proposed rules, and notices on regulations.gov ([.Rdata](https://github.com/judgelord/rulemaking/blob/master/data/AllRegsGovRules.Rdata))

```
 [1] "agencyAcronym"           
 [2] "allowLateComment"        
 [3] "attachmentCount"         
 [4] "commentDueDate"          
 [5] "commentStartDate"        
 [6] "docketId"                
 [7] "docketTitle"             
 [8] "docketType"              
 [9] "documentId"              
[10] "documentStatus"          
[11] "documentType"            
[12] "frNumber"                
[13] "numberOfCommentsReceived"
[14] "openForComment"          
[15] "postedDate"              
[16] "rin"                     
[17] "title" 
```

- Metadata for all public comments on regulations.gov ([.SQLite](https://drive.google.com/file/d/1hSl9MxjzO4R40QjFoh8TPmbCAUpJW372/view?usp=sharing),[.Rdata](https://github.com/judgelord/rulemaking/blob/master/data/allcomments-sample.Rdata))

**Using SQL**: For example, to get metadata for all CFPB comments (including those without attachments): 

`SELECT * FROM comments_all WHERE agencyAcronym = 'CFPB')`

| field                    | example content                                                                                                                                                                                                                       |
|--------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| agencyAcronym            | CFPB                                                                                                                                                                                                                          |
| allowLateComment         | FALSE                                                                                                                                                                                                                         |
| attachmentCount          | 1                                                                                                                                                                                                                             |
| commentDueDate           | NA                                                                                                                                                                                                                            |
| commentStartDate         | NA                                                                                                                                                                                                                            |
| commentText              | Please accept the attached comments on behalf of the Online Lenders Alliance regarding  BCFP Trial Disclosure Programs Docket No. CFPB-2018-0023  Thank You   Michael Day  Policy Director  Online Lenders Alliance   file(s) |
| docketId                 | CFPB-2018-0023                                                                                                                                                                                                                |
| docketTitle              | Policy to Encourage Trial Disclosure Programs                                                                                                                                                                                 |
| docketType               | Nonrulemaking                                                                                                                                                                                                                 |
| documentId               | CFPB-2018-0023-0006                                                                                                                                                                                                           |
| documentStatus           | Posted                                                                                                                                                                                                                        |
| documentType             | Public Submission                                                                                                                                                                                                             |
| numberOfCommentsReceived | 1                                                                                                                                                                                                                             |
| openForComment           | FALSE                                                                                                                                                                                                                         |
| postedDate               | 2018-10-11T00:00:00-04:00                                                                                                                                                                                                     |
| submitterName            | Michael Day                                                                                                                                                                                                                   |
| title                    | Comment Submitted by Michael Day, OLA                                                                                                                                                                                         |
| rin                      | NA                                                                                                                                                                                                                            |
| organization             | OLA                                                                                                                                                                                                                           |
| file1                    | CFPB-2018-0023-0006-1.doc                                                                                                                                                                                                     |
| url1                     | https://www.regulations.gov/contentStreamer?documentId=CFPB-2018-0023-0006&attachmentNumber=1                                                                                                                                 |

## Unified Agenda of Regulatory and Deregulatory Actions

- [XML versions available from 1995 until present](https://www.reginfo.gov/public/do/eAgendaXmlReport)
- [Transformed to Rdata. Some errors corrected for 2000-2018](https://github.com/judgelord/rulemaking/blob/master/data/UnifiedAgenda.Rdata)

```
 [1] "ABSTRACT"                
 [2] "ADDITIONAL_INFO"         
 [3] "AGENCY"                  
 [4] "AGENCY_CONTACT_LIST"     
 [5] "ANPRM"                   
 [6] "ANPRMcomment"            
 [7] "ANPRMfedreg"             
 [8] "CFR_LIST"                
 [9] "CHILD_RIN_LIST"          
[10] "COMPLIANCE_COST"         
[11] "ENERGY_AFFECTED"         
[12] "EO_13771_DESIGNATION"    
[13] "FEDERALISM"              
[14] "FINAL"                   
[15] "FINALeffective"          
[16] "FINALfedreg"             
[17] "FINALjudicial"           
[18] "FINALstatutory"          
[19] "FURTHER_INFO_URL"        
[20] "GOVT_LEVEL_LIST"         
[21] "IFR"                     
[22] "IFRcomment"              
[23] "IFReffective"            
[24] "IFRfedreg"               
[25] "INTERNATIONAL_INTEREST"  
[26] "JudicialFinal"           
[27] "JudicialNPRM"            
[28] "LEGAL_AUTHORITY_LIST"    
[29] "LEGAL_DLINE_LIST"        
[30] "LEGAL_DLINE_OVERALL_DESC"
[31] "MAJOR"                   
[32] "NAICS_LIST"              
[33] "NPRM"                    
[34] "NPRMcomment"             
[35] "NPRMfedreg"              
[36] "NPRMjudicial"            
[37] "NPRMstatutory"           
[38] "PARENT_AGENCY"           
[39] "PARENT_RIN"              
[40] "PRINT_PAPER"             
[41] "PRIORITY_CATEGORY"       
[42] "PROCUREMENT"             
[43] "PUBLIC_COMMENT_URL"      
[44] "REINVENT_GOVT"           
[45] "RELATED_AGENCY_LIST"     
[46] "RELATED_RIN_LIST"        
[47] "RFA_REQUIRED"            
[48] "RFA_SECTION_610_REVIEW"  
[49] "RIN"                     
[50] "RIN_STATUS"              
[51] "RPLAN_ENTRY"             
[52] "RPLAN_INFO"              
[53] "RULE_TITLE"              
[54] "SIC_DESC"                
[55] "SMALL_ENTITY_LIST"       
[56] "SNPRM"                   
[57] "SNPRMcomment"            
[58] "SNPRMfedreg"             
[59] "STAGE"                   
[60] "StatutoryFinal"          
[61] "StatutoryNPRM"           
[62] "TIMETABLE_LIST"          
[63] "UNFUNDED_MANDATE_LIST"   
[64] "UnifiedAgendaDate"       
[65] "WITHDRAWAL" 
```

## Office of Information and Regulatory Affairs (ORIA) Reports

- [XML versions available from 1981 until present](http://www.reginfo.gov/public/do/XMLReportList)
- [Transformed to Rdata. Some errors corrected.](https://github.com/judgelord/rulemaking/blob/master/data/OIRA.Rdata)
```
 [1] "AGENCY_CODE"                    
 [2] "ANPRM_COMPLETED"                
 [3] "ANPRM_PUBLISHED"                
 [4] "ANPRM_RECIEVED"                 
 [5] "DATE_COMPLETED"                 
 [6] "DATE_PUBLISHED"                 
 [7] "DATE_RECEIVED"                  
 [8] "DECISION"                       
 [9] "DODD_FRANK_ACT"                 
[10] "ECONOMICALLY_SIGNIFICANT"       
[11] "EXPEDITED_REVIEW"               
[12] "FEDERALISM_IMPLICATIONS"        
[13] "FINAL_COMPLETED"                
[14] "FINAL_PUBLISHED"                
[15] "FINAL_RECIEVED"                 
[16] "HEALTH_CARE_ACT"                
[17] "HOMELAND_SECURITY"              
[18] "IFR_COMPLETED"                  
[19] "IFR_PUBLISHED"                  
[20] "IFR_RECIEVED"                   
[21] "INTERNATIONAL_IMPACTS"          
[22] "LEGAL_DEADLINE"                 
[23] "MAJOR_OIRA"                     
[24] "NPRM_COMPLETED"                 
[25] "NPRM_PUBLISHED"                 
[26] "NPRM_RECIEVED"                  
[27] "REGULATORY_FLEXIBILITY_ANALYSIS"
[28] "RIN"                            
[29] "SMALL_ENTITIES_AFFECTED"        
[30] "SNPRM_COMPLETED"                
[31] "SNPRM_PUBLISHED"                
[32] "SNPRM_RECIEVED"                 
[33] "STAGE"                          
[34] "TCJA"                           
[35] "TITLE"                          
[36] "UNFUNDED_MANDATES"
```

More about these data in [Why Do Agencies (sometimes) Get So Much Mail? Lobbying Coalitions, Mass Comments, and Political Information in Bureaucratic Policymaking](https://judgelord.github.io/research/whymail/), [Data and Methods Analyzing Special Interests Influence in Rulemaking](https://link.springer.com/article/10.1057%2Fs41309-020-00094-w), and this [collaborative repository](https://github.com/libgober/regdata/blob/master/README.md)
