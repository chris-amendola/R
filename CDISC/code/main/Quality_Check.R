library(haven)

install_head<-'C:\\Users\\camendola\\Google Drive\\CDISCr'

# ADSL
adsl_sas<-read_sas("~/CDISC/CDISC_BOOK/chapter 7/adsl.sas7bdat")
load(file = paste(install_head,"\\Data\\output\\adsl.Rdata",sep=''))

adsl_sas<-adsl_sas[order(adsl_sas$USUBJID),]
adsl<-adsl[order(adsl$USUBJID),]

# I am ignoring the issue with row
all_equal(adsl,adsl_sas)

# ADEF
adef_sas<-read_sas("~/CDISC/CDISC_BOOK/chapter 7/adef.sas7bdat")
load(file = paste(install_head,"\\Data\\output\\adef.Rdata",sep=''))

# Accepting the integer versus numeric difference for now
all_equal(adef,adef_sas)

#ADAE
adae_sas<-read_sas("~/CDISC/CDISC_BOOK/chapter 7/adae.sas7bdat")
load(file = paste(install_head,"\\Data\\output\\adae.Rdata",sep=''))
all_equal(adae,adae_sas)

# Have needed to peek at these
#xraw_dose<-read_sas("~/CDISC/CDISC_BOOK/chapter 3/dosing.sas7bdat")
#xdemo<-read_sas("~/CDISC/CDISC_BOOK/chapter 3/demographic.sas7bdat")