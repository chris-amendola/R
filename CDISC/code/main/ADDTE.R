###############
###############
###############

install_head<-'C:\\Users\\camendola\\Google Drive\\CDISCr'

# Libraries
source(paste(install_head,'\\code\\main\\library_setup.R',sep=''))

# Local Functions
source(paste(install_head,'\\code\\func_lib\\base_lib.R',sep=''))

# Create Metadata objects from excel file
source(paste(install_head,'\\code\\main\\adam_metadata_setup.R',sep=''))

# Load ADSL 
load(file = paste(install_head,"\\Data\\output\\adsl.Rdata",sep=''))
pre.adtte<-adsl[,c( 'STUDYID'
                   ,'USUBJID'
                   ,'SITEID'
                   ,'COUNTRY'
                   ,'AGE'
                   ,'AGRGR1'
                   ,'SEX'
                   ,'RACE'
                   ,'RANDDT'
                   ,'TRT01P'
                   ,'ITTFL'
                   ,'TRTENDT')]
# Load ADEF
load(file = paste(install_head,"\\Data\\output\\adef.Rdata",sep=''))
# Look for CHGS
sel.adef<-adef[  adef$PARAMCD=='XPPAIN' 
              & adef$VISITNUM>0 
              & (adef$CHG>0 | adef$CHG<0) 
              ,c( 'USUBJID'
                 ,'PARAMCD'
                 ,'CHG'
                 ,'ADT'
                 ,'VISITNUM'
                 ,'XPSEQ')]
