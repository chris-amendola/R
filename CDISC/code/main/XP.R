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

#Study Day calculation
load(file = paste(install_head,"\\Data\\output\\dm.Rdata",sep=''))
start_date_list<-dm[,c('USUBJID','RFSTDTC')]
start_date.fmt<-setNames(start_date_list$RFSTDTC,start_date_list$USUBJID)

#Read the source LB data in
raw_data<-read_sas(paste(install_head,"\\data\\input\\pain.sas7bdat",sep=''))
# Build Pain Score labels list
pain_labels.fmt <- c('None','Mild','Moderate','Severe')
scores <- c(0,1,2,3)
#names(scores) <- pain_labels.fmt

names(pain_labels.fmt) <- scores

# Lab visit format
visit_month.fmt<-var_format('labs','VISIT',code_lists)

# Read Raw Data
raw_data <- read_sas("C:\\Users\\camendola\\Documents\\CDISCr\\data\\input\\pain.sas7bdat")
# "Stack" the repeated measures
# First separate each measure and add visitnum
base_data<-raw_data[!is.na(raw_data$painbase),c('uniqueid','randomizedt','painbase')] %>% 
  rename(XPDTC=randomizedt , xporres.pre=painbase) 
base_data$VISITNUM<-0

mon3data<-raw_data[!is.na(raw_data$pain3mo),c('uniqueid','month3dt','pain3mo')]%>% 
  rename(XPDTC=month3dt , xporres.pre=pain3mo) 
mon3data$VISITNUM<-1

mon6data<-raw_data[!is.na(raw_data$pain6mo),c('uniqueid','month6dt','pain6mo')]%>%
  rename(XPDTC=month6dt , xporres.pre=pain6mo) 
mon6data$VISITNUM<-2

# Stack measures togther
data.xpos<-rbind(base_data,mon3data,mon6data)
data.xpos$STUDYID <- 'XYZ123'
data.xpos$DOMAIN <- 'XP'
data.xpos$USUBJID <- data.xpos$uniqueid
data.xpos$XPTEST <- 'Pain Score'
data.xpos$XPTESTCD <- 'XPPAIN'
data.xpos$VISIT    <- unname(visit_month.fmt[as.character(data.xpos$VISITNUM)])
data.xpos$XPORRES <- unname(pain_labels.fmt[as.character(data.xpos$xporres.pre)])

data.xpos$XPSTRESC <- data.xpos$XPORRES
data.xpos$XPSTRESN <- data.xpos$xporres.pre

data.xpos$XPDY <- data.xpos$XPDTC-as.Date( (start_date.fmt[as.character(data.xpos$USUBJID)])
                                          ,origin = "1970-01-01")

# Sort the raw_data
data.xpos<-data.xpos[order( data.xpos$STUDYID
                           ,data.xpos$USUBJID
                           ,data.xpos$XPTESTCD
                           ,data.xpos$VISITNUM),]

# Use these to generate non-unique key flag-warning
data.xpos$first_flag<- is_first(c('STUDYID','USUBJID','XPTESTCD','VISITNUM'),data.xpos)
data.xpos$last_flag <- is_last(c('STUDYID','USUBJID','XPTESTCD','VISITNUM'),data.xpos)

# Add SEQ number
data.xpos<-data.xpos %>%
  group_by(STUDYID,USUBJID) %>%
  mutate(XPSEQ = 1:n())

# The Group by above requires that raw_data be specified as a data.frame in the final_ds function                        
xp<-final_ds( input_data=as.data.frame(data.xpos)
              ,domain_label='XP')                        

save(xp, file = paste(install_head,"\\Data\\output\\XP.Rdata",sep=''))
load(file = paste(install_head,"\\Data\\output\\XP.Rdata",sep=''))
contents(xp)

