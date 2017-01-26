###############
###############
###############

install_head<-'C:\\Users\\camendola\\Google Drive\\CDISCr'

# Libraries
source(paste(install_head,'\\code\\main\\library_setup.R',sep=''))

# Local Functions
source(paste(install_head,'\\code\\func_lib\\base_lib.R',sep=''))

load(file = paste(install_head,"\\Data\\output\\adsl.Rdata",sep=''))

output_file_rtf = "C:\\Users\\camendola\\Documents\\CDISCr\\data\\output\\Demographic_Listing.rtf"

table_title = "Listing x.xxx Demographic Listing"

adsl$YOB<-substr(as.character(adsl$BRTHDT),1,4)
selected_cols<-adsl[,c('TRT01P','SUBJID','SEX','RACE','RACEOTH','AGE','YOB')]

# Generate rtf file ####
rtf<-RTF( output_file_rtf
          ,width=11
          ,height=8.5
          ,font.size=10
          ,omi=c(.5,.5,.5,.5))

# Header Arguments
setFontSize(rtf,13)
addParagraph(rtf,table_title)
setFontSize(rtf,10)

addParagraph(rtf,"HEADER")

addNewLine(rtf) 
addTable( rtf
         ,selected_cols
         ,font.size=8
         ,row.names=FALSE
         ,NA.string="-"
         ,header.col.justify=c("L","C","C","C","C","C","C")
         ,col.justify=c("L","C","C","C","C","C","C")
         ,col.widths=c(2,1,1,1,1,1.5,1))	

done(rtf)