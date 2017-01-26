###############
###############
###############

install_head<-'C:\\Users\\camendola\\Google Drive\\CDISCr'

# Libraries
source(paste(install_head,'\\code\\main\\library_setup.R',sep=''))

# Load in adsl and lb data
load(file = paste(install_head,"\\Data\\output\\lb.Rdata",sep=''))
load(file = paste(install_head,"\\Data\\output\\adsl.Rdata",sep=''))

# Select from ADSL
adsl.sel<-adsl[,c( 'USUBJID'
                   ,'SITEID'
                   ,'COUNTRY'
                   ,'AGE'
                   ,'AGEGR1'
                   ,'AGEGR1N'
                   ,'SEX'
                   ,'RACE'
                   ,'TRTSDT'
                   ,'TRT01A'
                   ,'TRT01AN'
                   ,'SAFFL')]

# Transform the LB data
lb.sel<-lb %>%
        select( USUBJID
               ,LBTESTCD
               ,LBCAT
               ,LBORRESU
               ,LBTEST
               ,LBORRES
               ,VISIT
               ,VISITNUM
               ,LBDY
               ,LBDTC)%>%
        mutate(PARAM=paste(LBTEST,LBORRESU,sep=' ;')) %>%
        rename( PARAMCD=LBTESTCD
               ,AVAL=LBORRES
               ,AVISIT=VISIT
               ,AVISITN=VISITNUM
               ,ADY=LBDY
               ,ADT=LBDTC) %>%
        select( USUBJID
               ,PARAMCD
               ,PARAM
               ,AVAL
               ,AVISIT
               ,AVISITN
               ,ADY
               ,ADT)

