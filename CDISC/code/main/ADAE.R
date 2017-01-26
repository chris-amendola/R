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

# Select from ADSL
load(file = paste(install_head,"\\Data\\output\\adsl.Rdata",sep=''))
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

#Load AE
load(file = paste(install_head,"\\Data\\output\\ae.Rdata",sep=''))

# Bring selected columns from adsl and AE data together
adae.pre<-merge( adsl.sel
                ,ae
                ,by.x='USUBJID'
                ,by.y='USUBJID')

names(adae.pre)[names(adae.pre)=="TRT01A"] <- "TRTA"
names(adae.pre)[names(adae.pre)=="TRT01AN"] <- "TRTAN"

adae.pre$ASTDT<-as.Date(adae.pre$AESTDTC)
adae.pre$ASTDY<-as.Date(adae.pre$AESTDTC)-as.Date(adae.pre$TRTSDT)
adae.pre$AENDT<-as.Date(adae.pre$AEENDTC)
adae.pre$AENDY<-as.Date(adae.pre$AEENDTC)-as.Date(adae.pre$TRTSDT)

adae.pre$CQ01NAM <- ifelse(  grepl('PAIN|pain', adae.pre$AEDECOD) 
                            | toupper(adae.pre$AEDECOD) == 'HEADACHE'
                          ,'PAIN EVENT',' ' )

aereln.fmt<-setNames(c('0','1','2'),c('NOT RELATED','POSSIBLY RELATED','PROBABLY RELATED'))
aesevn.fmt<-setNames(c('1','2','3'),c('MILD','MODERATE','SEVERE'))
relgrn1n.fmt<-setNames(c('NOT RELATED','RELATED'),c(0,1))

adae.pre$AERELN <- aereln.fmt[adae.pre$AEREL]
adae.pre$AESEVN <- aesevn.fmt[adae.pre$AESEV]
adae.pre$RELGR1N <- ifelse(adae.pre$AERELN>0,1,0)
adae.pre$RELGR1  <- relgrn1n.fmt[adae.pre$RELGR1N]
adae.pre$TRTEMFL <-ifelse( adae.pre$ASTDT>=adae.pre$TRTSDT
                      | adae.pre$ASTDT<=0,'Y','N')

adae<-final_ds( input_data=adae.pre
                ,domain_label='ADAE') 

save(adae, file = paste(install_head,"\\Data\\output\\adae.Rdata",sep=''))
load(file = paste(install_head,"\\Data\\output\\adae.Rdata",sep=''))
contents(adae)