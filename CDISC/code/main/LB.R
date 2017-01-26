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

#Study Day calculation
load(file = paste(install_head,"\\Data\\output\\dm.Rdata",sep=''))
start_date_list<-dm[,c('USUBJID','RFSTDTC')]
start_date.fmt<-setNames(start_date_list$RFSTDTC,start_date_list$USUBJID)


#Read the source LB data in
raw_data<-read_sas(paste(install_head,"\\data\\input\\labs.sas7bdat",sep=''))
# Build Lookup tables from code_lists
lbcat.fmt<-var_format('labs','LBCAT',code_lists)
lbtest.fmt<-var_format('labs','LBTEST',code_lists)
lbtestcd.fmt<-var_format('labs','LBTESTCD',code_lists)
visit_month.fmt<-var_format('labs','VISIT',code_lists)

raw_data$STUDYID <- 'XYZ123'
raw_data$DOMAIN  <- 'LB'
raw_data$USUBJID <- raw_data$uniqueid

raw_data$LBCAT     <- unname(lbcat.fmt[as.character(raw_data$labcat)])
raw_data$LBTEST    <- unname(lbtest.fmt[as.character(raw_data$labtest)]) 
raw_data$LBTESTCD  <- unname(lbtestcd.fmt[as.character(raw_data$labtest)])

raw_data$LBORRES  <- raw_data$nresult
raw_data$LBORRESU <- raw_data$colunits
raw_data$LBORNRLO <- raw_data$lownorm
raw_data$LBORNRHI <- raw_data$highnorm

#**** create standardized results;
raw_data$LBSTRESC <- raw_data$LBORRES
raw_data$LBSTRESN <- raw_data$nresult
raw_data$LBSTRESU <- raw_data$LBORRESU
raw_data$LBSTNRLO <- raw_data$lownorm
raw_data$LBSTNRHI <- raw_data$highnorm
raw_data$VISITNUM <- raw_data$month
raw_data$VISIT    <- unname(visit_month.fmt[as.character(raw_data$month)])

raw_data$LBBLFL  <- ifelse( raw_data$VISIT=='Baseline','Y',' ')

raw_data$LBNRIND <- ifelse( !is.na(raw_data$LBSTRESN),'NORMAL','')

raw_data$LBNRIND <- ifelse(  !is.na(raw_data$LBSTNRLO) 
                           & !is.na(raw_data$LBSTRESN) 
                           & raw_data$LBSTRESN<raw_data$LBSTNRLO,'LOW',raw_data$LBNRIND)

raw_data$LBNRIND <- ifelse(  !is.na(raw_data$LBSTNRHI) 
                            & !is.na(raw_data$LBSTRESN) 
                            & raw_data$LBSTRESN>raw_data$LBSTNRHI,'HIGH',raw_data$LBNRIND)

raw_data$LBDTC <- as.Date( raw_data$labdate,origin = "1970-01-01")
  
raw_data$LBDY <- raw_data$LBDTC-as.Date( (start_date.fmt[as.character(raw_data$USUBJID)])
                                         ,origin = "1970-01-01")

# Sort the raw_data(makes a function that checks the sortkey, use block below)
raw_data<-raw_data[order( raw_data$STUDYID
                         ,raw_data$USUBJID
                         ,raw_data$LBCAT
                         ,raw_data$LBTESTCD
                         ,raw_data$VISITNUM),]

# Use these to generate non-unique key flag-warning(is this from the meta data?)
raw_data$first_flag<- is_first(c('STUDYID','USUBJID','LBCAT','LBTESTCD','VISITNUM'),raw_data)
raw_data$last_flag <- is_last(c('STUDYID','USUBJID','LBCAT','LBTESTCD','VISITNUM'),raw_data)

# Add SEQ number(create seq func)
raw_data<-raw_data %>%
          group_by(STUDYID,USUBJID) %>%
          mutate(LBSEQ = 1:n())

# The Group by above requires that raw_data be specified as a data.frame in the final_ds function                        
lb<-final_ds( input_data=as.data.frame(raw_data)
             ,domain_label='LB')                        
                      
save(lb, file = paste(install_head,"\\Data\\output\\lb.Rdata",sep=''))
load(file = paste(install_head,"\\Data\\output\\lb.Rdata",sep=''))
contents(lb)
