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

# Load up XP data
load(file = paste(install_head,"\\Data\\output\\xp.Rdata",sep=''))
# Load up ADSL as well
load(file = paste(install_head,"\\Data\\output\\adsl.Rdata",sep=''))


# Baseline is defined as the last non-missing value prior to study day 1 first dose;
# (note, values on Day 1 are assumed to occur before the first dose);
base<-xp[ xp$XPDY<=1 
          & !is.na(xp$XPSTRESN) 
          & is_last(c('USUBJID','VISITNUM'),xp)
         ,c('USUBJID','VISITNUM','XPSTRESN')]
base$ABLFL<-'Y'
names(base)[names(base) == 'XPSTRESN'] <- "BASE"

# Merge base value back onto All XP Records
adef.pre<-merge( base[,c('USUBJID','BASE')]
            ,xp[,c('USUBJID','VISITNUM','XPSTRESN','XPDTC','XPORRES','XPTESTCD','XPTEST','XPSEQ','VISIT')]
            ,by.x='USUBJID'
            ,by.y='USUBJID'
            ,all=TRUE)

# Merge flag onto XP Baseline Records
adef.pre<-merge( base[!is.na(base$USUBJID),c('USUBJID','VISITNUM','ABLFL')]
                   ,adef.pre[!is.na(adef.pre$USUBJID),]
                   ,by.x=c('USUBJID','VISITNUM')
                   ,by.y=c('USUBJID','VISITNUM')
                   ,all=TRUE)

adef.pre$CHG <- adef.pre$XPSTRESN - adef.pre$BASE
adef.pre$PCHG <- adef.pre$CHG/adef.pre$BASE*100

# Check for missing key values on adsl and adef.pre
if (   nrow( adsl[ !adsl$USUBJID %in% unique(adef.pre$USUBJID) , ] )>0
    | nrow(adef.pre [ !adef.pre$USUBJID %in% unique(adsl$USUBJID),])>0 ) 
  stop("Key Values missing on merged datasets")

adef.pre<-merge( adef.pre
                ,adsl[,c( 'USUBJID'
                         ,'STUDYID'
                         ,'SITEID' 
                         ,'COUNTRY' 
                         ,'AGE'
                         ,'AGEGR1'
                         ,'AGEGR1N' 
                         ,'SEX'
                         ,'RACE'
                         ,'RANDDT'
                         ,'TRT01P'
                         ,'TRT01PN'
                         ,'ITTFL')]
                ,by.x='USUBJID'
                ,by.y='USUBJID')

#ADT
adef.pre$ADT<-as.Date(adef.pre$XPDTC)
adef.pre$ADY<-as.Date(adef.pre$XPDTC)-as.Date(adef.pre$RANDDT)
adef.pre$CRIT1FL<-ifelse(adef.pre$CHG>=-2,'N','Y')
adef.pre$CRIT1<-"Pain improvement from baseline of at least 2 points"

# xptestcd = paramcd visit = avisit visitnum = avisitn xporres = avalc


adef.pre$TRTPN    <- adef.pre$TRT01PN
adef.pre$TRTP     <- adef.pre$TRT01P
adef.pre$PARAMCD  <- adef.pre$XPTESTCD
adef.pre$PARAM    <- adef.pre$XPTEST
adef.pre$AVISIT   <- adef.pre$VISIT
adef.pre$AVISITN  <- adef.pre$VISITNUM
adef.pre$AVAL     <- adef.pre$XPSTRESN
adef.pre$AVALC    <- adef.pre$XPORRES

adef<-final_ds( input_data=adef.pre
                ,domain_label='ADEF') 

save(adef, file = paste(install_head,"\\Data\\output\\adef.Rdata",sep=''))
load(file = paste(install_head,"\\Data\\output\\adef.Rdata",sep=''))
contents(adef)