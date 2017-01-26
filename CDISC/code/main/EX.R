###############
###############
###############

install_head<-'C:\\Users\\camendol\\Documents\\R\\CDISCr'

# Libraries
source(paste(install_head,'\\code\\main\\library_setup.R',sep=''))

# Local Functions
source(paste(install_head,'\\code\\func_lib\\base_lib.R',sep=''))

# Create Metadata objects from excel file
source(paste(install_head,'\\code\\main\\metadata_setup.R',sep=''))

# Study Day calculation
load(file = paste(install_head,"\\Data\\output\\dm.Rdata",sep=''))
start_date_list<-dm[,c('USUBJID','RFSTDTC')]
start_date.fmt<-setNames(start_date_list$RFSTDTC,start_date_list$USUBJID)

# Very simple,clean case 
trtarm_list <- dm[,c('USUBJID','ARM')]
arm.fmt<-setNames(trtarm_list$ARM,trtarm_list$USUBJID)


# Read raw data
raw_data<-read_sas(paste(install_head,"\\data\\input\\dosing.sas7bdat",sep=''))

raw_data$STUDYID <- 'XYZ123'
raw_data$DOMAIN <- 'EX'
raw_data$USUBJID <- raw_data$uniqueid
raw_data$EXDOSE <- raw_data$dailydose
raw_data$EXDOSTOT <- raw_data$dailydose
raw_data$EXDOSU <- 'mg'
raw_data$EXDOSFRM <- 'TABLET, COATED'
raw_data$EXTRT   <- unname(arm.fmt[as.character(raw_data$USUBJID)])

raw_data$EXSTDTC <- as.Date(paste(raw_data$startyy,raw_data$startmm,raw_data$startdd,sep='-'))
raw_data$EXENDTC <- as.Date(paste(raw_data$endyy,raw_data$endmm,raw_data$enddd,sep='-'))

raw_data$EXSTDY <- raw_data$EXSTDTC-as.Date( (start_date.fmt[as.character(raw_data$USUBJID)])
                                             ,origin = "1970-01-01")
raw_data$EXENDY <- raw_data$EXENDTC-as.Date( (start_date.fmt[as.character(raw_data$USUBJID)])
                                             ,origin = "1970-01-01")


# Sort the raw_data
raw_data<-raw_data[order( raw_data$STUDYID
                          ,raw_data$USUBJID
                          ,raw_data$EXTRT
                          ,raw_data$EXSTDTC
                          ),]

# Use these to generate non-unique key flag-warning
raw_data$first_flag<- is_first(c('STUDYID','USUBJID','EXTRT','EXSTDTC'),raw_data)
raw_data$last_flag <- is_last(c('STUDYID','USUBJID','EXTRT','EXSTDTC'),raw_data)

# Add SEQ number
raw_data<-raw_data %>%
  group_by(STUDYID,USUBJID) %>%
  mutate(EXSEQ = 1:n())

# The Group by above requires that raw_data be specified as a data.frame in the final_ds function                        
ex<-final_ds( input_data=as.data.frame(raw_data)
              ,domain_label='EX')                        

save(ex, file = paste(install_head,"\\Data\\output\\ex.Rdata",sep=''))
load(file = paste(install_head,"\\Data\\output\\ex.Rdata",sep=''))
contents(ex)

