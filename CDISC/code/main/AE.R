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

# Generate XLATES
aesev.fmt<-var_format('adverse','AESEV',code_lists)
acn.fmt<-var_format('adverse','ACN',code_lists)
aerel.fmt<-var_format('adverse','AEREL',code_lists)
# Metadata seems 'broken' for this one
#   ny.fmt<-var_format('adverse','NY',code_lists)
ny.fmt <- c('No','Yes')
vals <- c('N','Y')
names(ny.fmt) <- vals
ny.fmt

#Read the source AE data in
raw_data.pre<-read_sas(paste(install_head,"\\data\\input\\adverse.sas7bdat",sep=''))
raw_data <- raw_data.pre %>% 
  rename(aerel_=aerel , aesev_=aesev)

raw_data$STUDYID   <- 'XYZ123'
raw_data$DOMAIN    <- 'AE'
raw_data$USUBJID   <- raw_data$uniqueid
raw_data$AETERM    <- raw_data$aetext
raw_data$AEDECOD   <- raw_data$prefterm
raw_data$AEBODSYS  <- raw_data$bodysys
raw_data$AESEV <- unname(aesev.fmt[as.character(raw_data$aesev_)])
raw_data$AEACN <- unname(acn.fmt[as.character(raw_data$aeaction)])
raw_data$AEREL <- unname(aerel.fmt[as.character(raw_data$aerel_)])
raw_data$AESER <- unname(ny.fmt[as.character(raw_data$serious)])

raw_data$AESTDTC <- as.Date(raw_data$aestart,origin = "1960-01-01")
raw_data$AEENDTC <- as.Date(raw_data$aeend,origin = "1960-01-01")

raw_data$AESTDY <- raw_data$AESTDTC-as.Date( (start_date.fmt[as.character(raw_data$USUBJID)])
                                            ,origin = "1960-01-01")
raw_data$AEENDY <- raw_data$AEENDTC-as.Date( (start_date.fmt[as.character(raw_data$USUBJID)])
                                            ,origin = "1960-01-01")

# Sort the raw_data
raw_data<-raw_data[order( raw_data$STUDYID
                          ,raw_data$USUBJID
                          ,raw_data$AEDECOD
                          ,raw_data$AESTDTC
                          ,raw_data$AEENDTC),]

# Use these to generate non-unique key flag-warning
raw_data$first_flag<- is_first(c('STUDYID','USUBJID','AEDECOD','AESTDTC','AEENDTC'),raw_data)
raw_data$last_flag <- is_last(c('STUDYID','USUBJID','AEDECOD','AESTDTC','AEENDTC'),raw_data)

# Add SEQ number
raw_data<-raw_data %>%
  group_by(STUDYID,USUBJID) %>%
  mutate(AESEQ = 1:n())

# The Group by above requires that raw_data be specified as a data.frame in the final_ds function                        
ae<-final_ds( input_data=as.data.frame(raw_data)
              ,domain_label='AE')                        

save(ae, file = paste(install_head,"\\Data\\output\\ae.Rdata",sep=''))
load(file = paste(install_head,"\\Data\\output\\ae.Rdata",sep=''))
contents(ae)
