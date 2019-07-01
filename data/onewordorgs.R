
#list of one word org name
orgs <- str_spl(
c("350
3M
AARP
ACD
ACM
Acrisure
ACS
ACUITY
Acushnetompany
Aetna
AFSCME
API
BHHS
biofuelwatch
care2
earthjustice
earthworks
emblemHealth
greenpeace
HMS
Humana
microsoft
motionfirst
^NAR$
oceana
offshore systems
oneDigital
pennenvironment
regence
SkyTruth
Transamerica
TriNet
Walmart
waterlegacy
WSCHR
WSUSA
WTP
WZF
XeniumHR
YDL
YRMC"), "\n"
) %>% 
  unlist()

orgsShort <- str_c(orgs, collapse = "|")


#Advantix Systems
#REMAX Select Properties/KGD INC.


