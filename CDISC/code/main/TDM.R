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

# Creating a set of datasets
# These DS's "raw" data are stored in a workbook
# Load sheets 'raw' data (TA TE TI TS TV)

raw_ta<- access_excel_metadata( path=paste(install_head,'\\metadata\\',sep='')
                                ,file="trialdesign.xlsx"
                                ,sheet_name='TA')

raw_te<- access_excel_metadata( path=paste(install_head,'\\metadata\\',sep='')
                                ,file="trialdesign.xlsx"
                                ,sheet_name='TE')

raw_ti<- access_excel_metadata( path=paste(install_head,'\\metadata\\',sep='')
                                ,file="trialdesign.xlsx"
                                ,sheet_name='TI')

raw_ts<- access_excel_metadata( path=paste(install_head,'\\metadata\\',sep='')
                                ,file="trialdesign.xlsx"
                                ,sheet_name='TS')

raw_tv<- access_excel_metadata( path=paste(install_head,'\\metadata\\',sep='')
                                ,file="trialdesign.xlsx"
                                ,sheet_name='TV')

# Finalize each dataframe
ta<-final_ds( input_data=as.data.frame(raw_ta)
             ,domain_label='TA')

te<-final_ds( input_data=as.data.frame(raw_te)
             ,domain_label='TE')

ti<-final_ds( input_data=as.data.frame(raw_ti)
             ,domain_label='TI')

ts<-final_ds( input_data=as.data.frame(raw_ts)
             ,domain_label='TS')

tv<-final_ds( input_data=as.data.frame(raw_tv)
             ,domain_label='TV')

# Save dataframes

save(ta, file = paste(install_head,"\\Data\\output\\ta.Rdata",sep=''))
save(te, file = paste(install_head,"\\Data\\output\\te.Rdata",sep=''))
save(ti, file = paste(install_head,"\\Data\\output\\ti.Rdata",sep=''))
save(ts, file = paste(install_head,"\\Data\\output\\ts.Rdata",sep=''))
save(tv, file = paste(install_head,"\\Data\\output\\tv.Rdata",sep=''))



