This is a project to collect and join various data on US federal agency rulemaking.

## Regulations.gov (rule metadata and public comments)

I have collected two datasets from regulations.gov, one for all rules, proposed rules, and notices and a second for all public comments:

### 1. Metadata for all rules, proposed rules, and notices on regulations.gov ([.Rdata](https://drive.google.com/file/d/1LrafvpLDC2vBjO8DxEyCsGCEXhlwlKEe/view?usp=sharing),[SQL](https://drive.google.com/file/d/1hSl9MxjzO4R40QjFoh8TPmbCAUpJW372/view?usp=sharing))

|name                        |value                                                                                                                    |
|:---------------------------|:------------------------------------------------------------------------------------------------------------------------|
|agency_acronym              |FDA                                                                                                                      |
|allow_late_comment          |0                                                                                                                        |
|attachment_count            |0                                                                                                                        |
|comment_due_date            |NA                                                                                                                       |
|comment_start_date          |NA                                                                                                                       |
|docket_id                   |FDA-1976-N-0020                                                                                                          |
|docket_title                |ANTIBIOTIC DRUGS/ISOLATION & DIFF OF MICROORGANSM CLI USE                                                                |
|docket_type                 |Nonrulemaking                                                                                                            |
|document_id                 |FDA-1976-N-0020-0005                                                                                                     |
|document_status             |Posted                                                                                                                   |
|document_type               |Notice                                                                                                                   |
|fr_number                   |NA                                                                                                                       |
|number_of_comments_received |0                                                                                                                        |
|open_for_comment            |0                                                                                                                        |
|posted_date                 |1978-07-10T00:00:00-04:00                                                                                                |
|rin                         |NA                                                                                                                       |
|title                       |Part 433 - Exemptions from Antibiotic Certification and Labeling Requirements - Notice of Confirmation of Effective Date |
|fr_document_id              |NA                                                                                                                       |

### 2. Metadata for all public comments on regulations.gov ([.Rdata](https://drive.google.com/file/d/1iryaZo4W4-mPnsNC535HPl2KbSt5RKav/view?usp=sharing),[SQL](https://drive.google.com/file/d/1hSl9MxjzO4R40QjFoh8TPmbCAUpJW372/view?usp=sharing))

|name                        |value                                                                        |
|:---------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|agency_acronym              |CFPB                                                                        |
|allow_late_comment          |0                                                                        |
|attachment_count            |1                                                                        |
|comment_due_date            |NA                                                                        |
|comment_start_date          |NA                                                                        |
|comment_text                |Please accept the attached comments on behalf of the OnlineLenders Alliance regarding  BCFP Trial Disclosure Programs Docket No. CFPB-2018-0023 Thank You Michael Day Policy Director Online Lenders Alliance |
|docket_id                   |CFPB-2018-0023                                                                        |
|docket_title                |Policy to Encourage Trial Disclosure Programs                                                                        |
|docket_type                 |Nonrulemaking                                                                        |
|document_id                 |CFPB-2018-0023-0006                                                                        |
|document_status             |Posted                                                                        |
|document_type               |Public Submission                                                                        |
|number_of_comments_received |1                                                                        |
|posted_date                 |2018-10-11T00:00:00-04:00                                                                        |
|submitter_name              |Michael Day                                                                        |
|title                       |Comment Submitted by Michael Day, OLA                                                                        |
|rin                         |NA                                                                        |
|organization                |OLA                                                                        |
| attachment_1                    | CFPB-2018-0023-0006-1.doc                                                                                                                                                                                                     |
| comment_url                     | https://www.regulations.gov/document?D=CFPB-2018-0023-0006                                                                                                                                 |

### Both tables are also available in [SQL](https://drive.google.com/file/d/1hSl9MxjzO4R40QjFoh8TPmbCAUpJW372/view?usp=sharing) ([instructions for using SQL](https://judgelord.github.io/rulemaking/sql))

For example, to get metadata for all CFPB rules:

`SELECT * FROM rules WHERE agency_acronym = 'CFPB')`

and all comments on those rules (including those without attachments): 

`SELECT * FROM comments WHERE agency_acronym = 'CFPB')`

In R, you can query SQL databases with the `DBI` and `RSQLite` packages: 

```
library(DBI)
library(RSQLite)

con <- DBI::dbConnect(RSQLite::SQLite(), here::here("db", "regs_dot_gov.sqlite"))

# results for a comment, CFPB-2018-0023-0006
dbGetQuery(con, "SELECT * FROM comments WHERE docket_id = 'CFPB-2018-0023-0006'")
```

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
