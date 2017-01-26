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

# Load up DM and SUPPDM
# These form the basis for ADSL
load(file = paste(install_head,"\\Data\\output\\dm.Rdata",sep=''))
load(file = paste(install_head,"\\Data\\output\\suppdm.Rdata",sep=''))

# This needs to be functionalized!!!
# Merge supp data back onto Domain Data
supp_var<-setNames( unique(suppdm$QLABEL) ,unique(suppdm$QNAM))
names(supp_var)

subset1<-suppdm[ suppdm$QNAM=="RACEOTH" & suppdm$QVAL!='',]
# Rename QVAL to QNAM 
names(subset1)[names(subset1) == 'QVAL'] <- "RACEOTH"
# and add QLABEL
label(subset1$RACEOTH)<-supp_var['RACEOTH']
# Select final cols
subset1<-subset1[c('STUDYID','RDOMAIN','USUBJID','RACEOTH')]

subset2<-suppdm[ suppdm$QNAM=="RANDDT",]
# Rename QVAL to QNAM 
names(subset2)[names(subset2) == 'QVAL'] <- "RANDDT"
# and add QLABEL
label(subset2$RANDDT)<-supp_var['RANDDT']
# Convert to date class
subset2$RANDDT<-as.Date(subset2$RANDDT)

# Select final cols
subset2<-subset2[c('STUDYID','RDOMAIN','USUBJID','RANDDT')]

supp_data<-merge( subset1
                 ,subset2
                 ,by.x=c('STUDYID','RDOMAIN','USUBJID')
                 ,by.y=c('STUDYID','RDOMAIN','USUBJID')
                 ,all=TRUE)

final<-merge( supp_data
             ,dm
             ,by.x=c('STUDYID','RDOMAIN','USUBJID')
             ,by.y=c('STUDYID','DOMAIN','USUBJID'))

# - Catch some NAs in RACEOTH
final$RACEOTH<-ifelse(is.na(final$RACEOTH),'',final$RACEOTH)

# Create Responders from XP
load(file = paste(install_head,"\\Data\\output\\xp.Rdata",sep=''))
# Baseline is defined as the last non-missing value prior to study day 1 first dose;
# (note, values on Day 1 are assumed to occur before the first dose);
base1<-xp[ xp$XPDY<=1 
          & !is.na(xp$XPSTRESN) 
          & is_last(c('USUBJID','VISITNUM'),xp)
          ,c('USUBJID','VISITNUM','XPSTRESN')]
base1$ABLFL<-'Y'
names(base1)[names(base1) == 'XPSTRESN'] <- "BASE"

# Merge base value back onto XP
temp<-merge( base1[,c('USUBJID','BASE')]
            ,xp[xp$VISITNUM=='2',c('USUBJID','VISITNUM','XPSTRESN')]
            ,by.x='USUBJID'
            ,by.y='USUBJID'
            ,all=TRUE)

# Merge flag onto XP
responders<-merge( base1[,c('USUBJID','ABLFL')]
                  ,temp
                  ,by.x='USUBJID'
                  ,by.y='USUBJID'
                  ,all=TRUE)

responders$chg <- responders$XPSTRESN - responders$BASE
responders$pchg <- responders$chg/responders$BASE*100

# Merge responders to their DM records
adsl.pre<-merge( final,responders[!is.na(responders$USUBJID),],by.x='USUBJID',by.y='USUBJID')
adsl.pre$TRTSDT<-as.Date(adsl.pre$RFSTDTC)
adsl.pre$BRTHDT<-as.Date(adsl.pre$BRTHDTC)
adsl.pre$TRTEDT<-as.Date(adsl.pre$RFENDTC)
# Created flags for ITT and safety-evaluable;
adsl.pre$ITTFL <- ifelse(adsl.pre$RANDDT>0,'Y','N')
adsl.pre$SAFFL <- ifelse(adsl.pre$TRTSDT>0,'Y','N')
adsl.pre$TRT01P <- adsl.pre$ARM
adsl.pre$TRT01A <- adsl.pre$TRT01P

# Value Xlate for Arm Names -> numbers
arm_nm.fmt<-setNames( c('1','0'),c('Analgezia HCL 30 mg','Placebo'))

adsl.pre$TRT01PN<-as.numeric(arm_nm.fmt[adsl.pre$TRT01P])

adsl.pre$TRT01AN <- adsl.pre$TRT01PN

adsl.pre$AGEGR1N <- ifelse( adsl.pre$AGE>=0 & adsl.pre$AGE<=54
                           ,1
                           , ifelse( adsl.pre$AGE>54
                                    ,2
                                    ,-1))

age_grp.fmt <- setNames(c('<55 YEARS','>=55 YEARS'),c('1','2'))

adsl.pre$AGEGR1 <- age_grp.fmt[adsl.pre$AGEGR1N]

# 2-point improvement in pain at 6 months
adsl.pre$RESPFL <- ifelse( adsl.pre$chg <=-2 ,'Y','N') 
adsl.pre$RESPFL <- ifelse(is.na(adsl.pre$chg),'N',adsl.pre$RESPFL)

adsl<-final_ds( input_data=adsl.pre
               ,domain_label='ADSL') 

save(adsl, file = paste(install_head,"\\Data\\output\\adsl.Rdata",sep=''))
load(file = paste(install_head,"\\Data\\output\\adsl.Rdata",sep=''))
contents(adsl)