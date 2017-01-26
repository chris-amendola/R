# Required Packages ####
#-install.packages("RMySQL")
#-install.packages('SASxport',repos='http://cran.us.r-project.org')
#-install.packages('knitr')
#-install.packages("haven",repos='http://cran.us.r-project.org')
#-install.packages('readxl')
# Functions
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
  return(result)
}
var_format<-function(source,list){
  rxlate<-code_lists[  code_lists$sourcedataset==source & !is.na(code_lists$sourcedataset)
                     & code_lists$CODELISTNAME==list & !is.na(code_lists$CODELISTNAME)
                     , c('sourcevalue','CODEDVALUE')]
  r.fmt<-setNames(rxlate$CODEDVALUE,rxlate$sourcevalue)
  return(r.fmt)
}
dataset_definition<-function(domain){
  
  structure<-dataset_metadata[dataset_metadata$DOMAIN==domain,]
  #"Translate SAS types to r
  structure$rtype<-structure$TYPE
  structure$rtype[structure$rtype=='text']<-'character'
  structure$rtype[structure$rtype=='float']<-'numeric'
  structure$rtype[structure$rtype=='integer']<-'numeric'
  structure$rtype[structure$rtype=='date']<-'character'
  structure$rtype[structure$rtype=='time']<-'character'
  
  #Arrange Columns by order_number
  structure<-structure[order(structure$VARNUM),]
  # Create Shell lb dataframe
  shell <- read.table( text = "",colClasses= structure$rtype, col.names = structure$VARIABLE)
  #Add dataframe label
  ds_label<-toc_metadata[toc_metadata$NAME==domain,'LABEL']
  label(shell)<-as.character(ds_label)
  #Add Varlabels
  varLabels<-setNames(as.list(structure$LABEL),structure$VARNUM)
  label(shell)<-varLabels
  #SAS FORMATS???
  return(shell)
}

#Libraries
library('readxl')
library('haven')
library("Hmisc")
library("dplyr")

#Read an excel file
code_lists<-read_excel( "C:\\Users\\camendola\\Documents\\CDISCr\\Metadata\\SDTM_METADATA.xlsx"
                       ,sheet = "CODELISTS")

dataset_metadata<-read_excel( "C:\\Users\\camendola\\Documents\\CDISCr\\Metadata\\SDTM_METADATA.xlsx"
                              ,sheet="VARIABLE_METADATA")

toc_metadata<-read_excel( "C:\\Users\\camendola\\Documents\\CDISCr\\Metadata\\SDTM_METADATA.xlsx"
                          ,sheet="TOC_METADATA")

# SDTM DM
# Build Lookup tables from codelists
race.fmt<-var_format('demographic','RACE')
gender.fmt<-var_format('demographic','SEX')
armcd.fmt<-var_format('demographic','ARMCD')
arm.fmt<-var_format('demographic','ARM')

#### Secondary dosing data needs importing
dose_data<-read_sas("C:\\Users\\camendola\\Documents\\CDISCr\\data\\input\\dosing.sas7bdat")

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
min_max_doses$firstdose<-as.Date(min_max_doses$firstdose,origin = "1970-01-01")
min_max_doses$lastdose<-as.Date(min_max_doses$lastdose,origin = "1970-01-01")

## DM Structure
dm_structure<-dataset_definition('DM')
#Read the source DM data in
raw_data<-read_sas("C:\\Users\\camendola\\Documents\\CDISCr\\data\\input\\demographic.sas7bdat")
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
raw_dose$SUBJID   <- raw_dose$subject
raw_dose$RFSTDTC  <- as.character(raw_dose$firstdose)
raw_dose$RFENDTC  <- as.character(raw_dose$lastdose)
raw_dose$SITEID   <- paste(substr(raw_dose$subjid,1,1),"00",sep='')
raw_dose$BRTHDTC  <- as.character(raw_dose$dob)
raw_dose$raw_race <- raw_dose$race
raw_dose$RACE     <-unname(race.fmt[as.character(raw_dose$raw_race)])
raw_dose$SEX      <-unname(gender.fmt[as.character(raw_dose$gender)])
raw_dose$ARMCD    <-unname(armcd.fmt[as.character(raw_dose$trt)])
raw_dose$ARM      <-unname(arm.fmt[as.character(raw_dose$trt)])
raw_dose$AGE      <-age_calc( raw_dose$dob
                             ,enddate=raw_dose$firstdose
                             , units='years')
raw_dose$AGEU     <-ifelse(!is.na(raw_dose$AGE),'YEARS','')

# Create DM dataframe from raw data, defined by dm_structure
dm<-rbind(dm_structure,raw_dose[,names(dm_structure)])


# Dataset label
ds_label<-toc_metadata[toc_metadata$NAME=="DM",'LABEL']
label(dm)<-as.character(ds_label)
# Variable labels
varLabels<-label(dm_structure)
label(dm)<-as.list(varLabels)

#Sort by subject?

#SUPPDM
suppdm_structure<-dataset_definition('SUPPDM')
#Select rows from main dataframe
supp_data<-raw_dose[,c('subject','orace')]

