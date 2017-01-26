###############
###############
###############

install_head<-'C:\\Users\\camendola\\Google Drive\\CDISCr'

# Libraries
source(paste(install_head,'\\code\\main\\library_setup.R',sep=''))

# Local Functions
source(paste(install_head,'\\code\\func_lib\\base_lib.R',sep=''))

# Create Metadata objects from excel file
source(paste(install_head,'\\code\\main\\metadata_setup.R',sep=''))

# SDTM DM
# Build Lookup tables from code_lists
race.fmt<-var_format('demographic','RACE',code_lists)
gender.fmt<-var_format('demographic','SEX',code_lists)
armcd.fmt<-var_format('demographic','ARMCD',code_lists)
arm.fmt<-var_format('demographic','ARM',code_lists)

#### Secondary dosing data needs importing
dose_data<-read_sas(paste(install_head,"\\data\\input\\dosing.sas7bdat",sep=''))

# Start and end date defenseive programing
# Set start date to end date or end date to start date when one or the other is missing
# This will keep missing values from breaking up coming min max computations
dose_data$startdt<-ifelse(is.na(dose_data$startdt),dose_data$enddt,dose_data$startdt)
dose_data$enddt<-ifelse(is.na(dose_data$enddt),dose_data$startdt,dose_data$enddt)

### Find earliest and latest dose dates
min_max_doses<-dose_data %>%
  group_by(subject) %>%
  summarize( min_stdt=min(startdt)
            ,max_stdt=max(startdt)
            ,min_endt=min(enddt)
            ,max_endt=max(enddt))

min_max_doses$firstdose<-ifelse( min_max_doses$min_stdt>min_max_doses$min_endt
                                ,min_max_doses$min_endt
                                ,min_max_doses$min_stdt )
min_max_doses$lastdose<-ifelse( min_max_doses$max_endt<min_max_doses$max_stdt
                               ,min_max_doses$max_stdt
                               ,min_max_doses$max_endt)
# Final decision
min_max_doses$firstdose<-as.Date( min_max_doses$firstdose
                                 ,origin = "1970-01-01")
min_max_doses$lastdose<-as.Date( min_max_doses$lastdose
                                ,origin = "1970-01-01")

#Read the source DM data in
raw_data<-read_sas(paste(install_head,"\\data\\input\\demographic.sas7bdat",sep=''))

### Merge the dosing data
raw_dose <- merge( raw_data
                  ,min_max_doses[,c('subject','firstdose','lastdose')]
                  ,by.x='subject'
                  ,by.y='subject')

#Transform and add vars
# -Create xformed variables on raw ds
raw_dose$STUDYID  <- 'XYZ123'
raw_dose$DOMAIN   <- 'DM'
raw_dose$USUBJID  <- raw_dose$uniqueid
raw_dose$COUNTRY  <- 'USA'
raw_dose$SUBJID   <- as.character(raw_dose$subject)
raw_dose$RFSTDTC  <- as.character(raw_dose$firstdose)
raw_dose$RFENDTC  <- as.character(raw_dose$lastdose)
raw_dose$SITEID   <- paste(substr(raw_dose$SUBJID,1,1),"00",sep='')
raw_dose$BRTHDTC  <- as.character(raw_dose$dob)
raw_dose$raw_race <- raw_dose$race
raw_dose$RACE     <- unname(race.fmt[as.character(raw_dose$raw_race)])
raw_dose$SEX      <- unname(gender.fmt[as.character(raw_dose$gender)])
raw_dose$ARMCD    <- unname(armcd.fmt[as.character(raw_dose$trt)])
raw_dose$ARM      <- unname(arm.fmt[as.character(raw_dose$trt)])
raw_dose$AGE      <- age_calc( raw_dose$dob
                              ,enddate=raw_dose$firstdose
                              , units='years')
raw_dose$AGEU     <-ifelse(!is.na(raw_dose$AGE),'YEARS','')


dm<-final_ds( input_data=raw_dose
             ,domain_label='DM')

save(dm, file = paste(install_head,"\\Data\\output\\dm.Rdata",sep=''))
load(file = paste(install_head,"\\Data\\output\\dm.Rdata",sep=''))
contents(dm)

#SUPPDM
# Select Rows from raw_dose dataframe
supp_data_orace <- raw_dose[ raw_dose$orace!='' ,c( 'orace'
                                                      ,'randdt'
                                                      ,'STUDYID'
                                                      ,'USUBJID')]

supp_data_orace$QNAM     <- ifelse(!is.na(supp_data_orace$orace),'RACEOTH','')
supp_data_orace$QLABEL   <- ifelse(!is.na(supp_data_orace$orace),'Race, Other','')
supp_data_orace$QVAL     <- ifelse(!is.na(supp_data_orace$orace),supp_data_orace$orace,'')

supp_data_randdt <- raw_dose[ !is.na(raw_dose$randdt), c( 'orace'
                                                         ,'randdt'
                                                         ,'STUDYID'
                                                         ,'USUBJID')]

supp_data_randdt$QNAM     <- ifelse(!is.na(supp_data_randdt$randdt),'RANDDT','')
supp_data_randdt$QLABEL   <- ifelse(!is.na(supp_data_randdt$randdt),'Randomization Date','')
supp_data_randdt$QVAL     <- ifelse(!is.na(supp_data_randdt$randdt),as.character(supp_data_randdt$randdt),'')

supp_data <- rbind(supp_data_orace,supp_data_randdt)

supp_data$RDOMAIN  <- 'DM'
supp_data$QORIG    <- 'CRF'
supp_data$IDVAR    <- ''
supp_data$IDVARVAL <- ''
supp_data$QEVAL    <- ''

suppdm<-final_ds( input_data=supp_data
                 ,domain_label='SUPPDM')


save(suppdm, file = paste(install_head,"\\Data\\output\\suppdm.Rdata",sep=''))
load(file = paste(install_head,"\\Data\\output\\suppdm.Rdata",sep=''))
contents(suppdm)
