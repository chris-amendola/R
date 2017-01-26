###############
###############
###############

install_head<-'C:\\Users\\camendola\\Documents\\CDISCr'

# Libraries
source(paste(install_head,'\\code\\main\\library_setup.R',sep=''))

# Local Functions
source(paste(install_head,'\\code\\func_lib\\base_lib.R',sep=''))

# Create Metadata objects from excel file
source(paste(install_head,'\\code\\main\\metadata_setup.R',sep=''))

dm<-read.xport(paste(install_head,"\\Data\\output\\dm.xpt",sep='' ))
dm_def<-dataset_definition( "DM"
                            ,variable_metadata
                            ,dataset_metadata)

varLabels<-label(dm_def)
print(paste('RECTIFY VAR LABELS: ',varLabels,sep=''))
label(dm_def)<-as.list(varLabels)


lb_def<-dataset_definition( "LB"
                            ,variable_metadata
                            ,dataset_metadata)

varLabels2<-label(lb_def)
print(paste('RECTIFY VAR LABELS: ',varLabels2,sep=''))
label(lb_def)<-as.list(varLabels2)