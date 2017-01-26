dataset_definition<-function( domain
                              ,var_source_data
                              ,dataset_source_data){
  
  print(paste('DOMAIN:',domain,sep=' '))
  structure<-var_source_data[var_source_data$DOMAIN==domain & !is.na(var_source_data$DOMAIN) ,]
  #"Translate SAS types to r
  structure$rtype<-structure$TYPE
  structure$rtype[structure$rtype=='text']<-'character'
  structure$rtype[structure$rtype=='float']<-'numeric'
  structure$rtype[structure$rtype=='integer']<-'numeric'
  structure$rtype[structure$rtype=='date']<-'character'
  structure$rtype[structure$rtype=='time']<-'character'
  
  #Arrange Columns by order_number
  structure<-structure[order(structure$VARNUM),]
  print('CREATING SHELL TABLE:')
  print(paste(structure$VARIABLE,'->',structure$rtype))
  print('---------------------------')
  # Create Shell lb dataframe
  shell <- read.table( text = "",colClasses= structure$rtype, col.names = structure$VARIABLE)
  print('CREATED SHELL TABLE')
  print(contents(shell))
  #Add dataframe label
  ds_label<-dataset_source_data[dataset_source_data$NAME==domain,'LABEL']
  print(paste('STRUCTURE DATSET LABEL: ',ds_label,sep=''))
  label(shell)<-as.character(ds_label)
  #Add Varlabels
  varLabels<-setNames(as.list(structure$LABEL),structure$VARNUM)
  label(shell)<-varLabels
  #SAS FORMATS???
  print('STRUCTURE DETERMINED')
  #print(paste('VARS:',structure$VARIABLE,sep=''))
  #print(paste('VAR LABELS:',varLabels,sep=''))
  return(shell)}

final_ds<-function( input_data=''
                    ,domain_label=''
                    ,ds_metadata=dataset_metadata
                    ,var_metadata=variable_metadata
                    ){
  
  print('ACCESS DOMAIN DATA DEFINITION')
  domain_def<-dataset_definition( domain_label
                                  ,var_metadata
                                  ,ds_metadata)
  print('RAW DATA VARIABLE ATTRIBUTES')
  print(contents(input_data))
  print('VARIABLE CHECK:')
  print(setdiff(names(domain_def),names(input_data)))
  # dataframe from raw data, defined by dm_structure
  data.pre<-rbind( domain_def
                   ,input_data[,names(domain_def)])
  
  # Dataset label
  ds_label<-ds_metadata[ds_metadata$NAME==domain_label & !is.na(ds_metadata$NAME),'LABEL']
  print(paste('RECTIFY DATASET LABEL: ',ds_label,sep=''))
  label(data.pre)<-as.character(ds_label)
  # Variable labels
  varLabels<-label(domain_def)
  print(paste('RECTIFY VAR LABELS: ',varLabels,sep=''))
  label(data.pre)<-as.list(varLabels)
  print('VAR LABELS ASSIGNED')
  # Sort
  ds_sortkey<-as.vector(ds_metadata[ds_metadata$NAME==domain_label & !is.na(ds_metadata$NAME),'DOMAINKEYS'])
  print(paste('DOMAINKEYS',ds_sortkey),sep=' ')
  # Remove spaces in key-list - they impeed using for sort key
  ds_sortkey<-gsub(" ", "", ds_sortkey)
  sk<-unlist(strsplit(as.character(unname(ds_sortkey)),','))
  return(data.pre[do.call("order", data.pre[sk]), ])}

is_first<-function(key,frm_data){
  ret_value<-ifelse(!duplicated( frm_data[key]),TRUE,FALSE)
  return(ret_value)}

is_last<-function(key,frm_data){
  ret_value<-ifelse(!duplicated( frm_data[key],fromLast = TRUE),TRUE,FALSE)
  return(ret_value)}

age_calc <- function( dob
                      ,enddate=Sys.Date()
                      ,units='months'){
  if (!inherits(dob, "Date") | !inherits(enddate, "Date"))
    stop("Both dob and enddate must be Date class objects")
  start <- as.POSIXlt(dob)
  end <- as.POSIXlt(enddate)
  
  years <- end$year - start$year
  if(units=='years'){
    result <- ifelse((end$mon < start$mon) | 
                       ((end$mon == start$mon) & (end$mday < start$mday)),
                     years - 1, years)    
  }else if(units=='months'){
    months <- (years-1) * 12
    result <- months + start$mon
  }else if(units=='days'){
    result <- difftime(end, start, units='days')
  }else{
    stop("Unrecognized units. Please choose years, months, or days.")
  }
  return(result)}

access_excel_metadata<-function( path="C:\\Users\\camendola\\Documents\\CDISCr\\Metadata\\"
                                 ,file="SDTM_METADATA.xlsx"
                                 ,sheet_name=''){
  #?? Can you get list of sheet names to make this list driven?
  sheet_data<-read_excel( paste(path,file,sep='')
                          ,sheet = sheet_name)
  
  return(sheet_data)}
var_format<-function(source,list,source_data){
  rxlate<-source_data[  source_data$sourcedataset==source & !is.na(source_data$sourcedataset)
                        & source_data$CODELISTNAME==list & !is.na(source_data$CODELISTNAME)
                        , c('sourcevalue','CODEDVALUE')]
  r.fmt<-setNames(rxlate$CODEDVALUE,rxlate$sourcevalue)
  return(r.fmt)
}