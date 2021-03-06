###############
###############
###############

install_head<-'C:\\Users\\camendola\\Google Drive\\CDISCr'

# Libraries
source(paste(install_head,'\\code\\main\\library_setup.R',sep=''))

# Local Functions
source(paste(install_head,'\\code\\func_lib\\base_lib.R',sep=''))

# Bring in adsl data
load(file = paste(install_head,"\\Data\\output\\adsl.Rdata",sep=''))

output_file_rtf = "C:\\Users\\camendola\\Documents\\CDISCr\\data\\output\\DemographicSummary.rtf"

table_title = "Table x.xxx Demographic Summary"

# Define Global Header and Footer info 
header <<- array(c("<Enrolled / Randomized> Population","Study Name"))
header_num <<- 2
footer <<- array()
footer[1] = "Abbrevations: N= number of subjects in population;"
footer[2] = "		 n= number of subjects"
footer[3] = ""
footer[4] = "*a = number of subjects with non-missing data, used as denominator"
footer[5] = "*b = For categorical measure; p-value is calculated from Fisher's exact test. For continuous measure: t-test when comparing means."
footer_num <<- 5

# Create global variables for routine - This is the logical row structure
Col1 <<- array()
Col2 <<- array()
TotCol <<- array()
Treat <<- matrix(nrow=200,ncol=3)
pvalue <<- array()

row_count <<- 1

# Function output_rows ####
outputRows <- function(){
  #	Combine individual vectors into data frame
  row_frame <-data.frame( cbind( Col1
                                ,Col2
                                ,Treat[1:row_count-1,1]
                                ,Treat[1:row_count-1,2]
                                ,TotCol
                                ,pvalue))
  print(head(row_frame))
  #	Remove column names
  sTreata <- array()
  sTreata = paste(rownames(treatments)," (N=",treatments,")",sep = "")
  sTotal = paste("Total "," (N=",nrow(data),")",sep = "")
  colnames(row_frame)<-c("Demographic Parameter"," ",sTreata[1],sTreata[2],"Total","p-value*b")
  #	Convert to RTF table
  
  addTable( rtf
           ,row_frame
           ,font.size=8
           ,row.names=FALSE
           ,NA.string="-"
           ,header.col.justify=c("L","C","C","C","C","C")
           ,col.justify=c("L","C","C","C","C","C")
           ,col.widths=c(2.5,1,1,1,1,1.5))	
}
# Function empty_row ####
emptyRow <- function() {
  Col1[row_count] <<- ""
  Col2[row_count] <<- ""
  # total
  # for all treatments
  for (t in 1:nTreat) {
    Treat[row_count,t] <<- ""
  }
  TotalDenominator <<- ""
  TotCol[row_count] <<- ""
  pvalue[row_count] <<- ""
  row_count <<- row_count + 1 
}
# Function get_studyname ####
getStudyName <- function() {
  studies <<- xtabs(~STUDYID, data=data,drop.unused.levels=TRUE)
  # for now - first one
  return (rownames(studies)[1])
}
# Function pvalue_categorical ####
pvalue_categorical <- function(aggregated) {
  # Fischers exact test
  aresult = fisher.test(aggregated)
  return (aresult$p.value)
}
# Function pvalue_numerical ####
pvalue_numerical <- function(dep,indep) {
  # Fischers exact test
  aresult = t.test(dep~indep,data=data)
  return (aresult$p.value)
}
# Function output_categorical ####
outputCategorical <- function(data_type){
  #CAA: I very much dislike magic numbers had to change that from original code
  # change Type in function to dataType - passed parm drives conditional.
  if (data_type=="Sex") {
    aggreg <<- xtabs(~SEX+TRT01P, data=data,drop.unused.levels=TRUE)
    totals <<- xtabs(~SEX, data=data,drop.unused.levels=TRUE)
    denominators <<- xtabs(~TRT01P,data=data,drop.unused.levels=TRUE)
    Col1[row_count] <<- "Sex n(%)"
  }
  else if (data_type=="Race") {
    aggreg <<- xtabs(~RACE+TRT01P, data=data,drop.unused.levels=TRUE)
    totals <<- xtabs(~RACE, data=data,drop.unused.levels=TRUE)
    denominators <<- xtabs(~TRT01P,data=data,drop.unused.levels=TRUE)
    Col1[row_count] <<- "Race n(%)"
  }
  else if (data_type=="Ethnicity") {
    aggreg <<- xtabs(~ETHNIC+TRT01P, data=data,drop.unused.levels=TRUE)
    totals <<- xtabs(~ETHNIC, data=data,drop.unused.levels=TRUE)
    denominators <<- xtabs(~TRT01P,data=data,drop.unused.levels=TRUE)
    Col1[row_count] <<- "Ethnicity n(%)"
  }
  else if (data_type=="Age") {
    aggreg <<- xtabs(~AGEGR1+TRT01P, data=data,drop.unused.levels=TRUE)
    totals <<- xtabs(~AGEGR1, data=data,drop.unused.levels=TRUE)
    denominators <<- xtabs(~TRT01P,data=data,drop.unused.levels=TRUE)
    Col1[row_count] <<- "Age categories n(%)"
  }
  sumtotals = summary(totals)
  Col2[row_count] <<- "n*a"
  # total
  # for all treatments
  for (t in 1:nTreat) {
    Treat[row_count,t] <<- denominators[t]
  }
  TotalDenominator <- sumtotals$n.cases
  TotCol[row_count] <<- TotalDenominator
  pvalue[row_count] <<- ""
  print(paste("Created row",row_count,"with",data_type,"data"))
  row_count <<- row_count + 1 
  # aggreg data
  # loop for all in category
  clist <<- rownames(totals)
  first = TRUE
  for (i in clist) {
    Col1[row_count] <<- ""
    Col2[row_count] <<- i
    # for all treatments, get incidence for this parameter
    for (t in 1:nTreat) {
      value = aggreg[i,rownames(treatments)[t]]
      percent = 100.*value/denominators[t]
      Treat[row_count,t]<<-paste(value," (",round(percent,1),")",sep = "")
    }
    # total
    value = totals[i]
    percent = 100.*value/TotalDenominator
    TotCol[row_count] <<-paste(value," (",round(percent,1),")",sep = "")
    # obtain p-value on first look only
    if (first) {
      pvalue[row_count] <<- round(pvalue_categorical(aggreg),3)
    } else { pvalue[row_count] <<- "" }
    print(paste("Created row",row_count,"with",data_type,"data"))
    
    first = FALSE
    row_count <<- row_count + 1			
  }
}
# function for outputing special age categories ####
outputAgeGreater <- function(age){
  #
  # 
  dataAge <<- data[data$AGE>=age,]
  aggreg <<- xtabs(~TRT01P,data=dataAge,drop.unused.levels=TRUE)
  denominators <<- xtabs(~TRT01P,data=data,drop.unused.levels=TRUE)	
  totals <<- xtabs(~AGE, data=data,drop.unused.levels=TRUE)
  Col1[row_count] <<- ""
  Col2[row_count] <<- paste(">=",age,sep = "")
  # aggreg data
  sumtotals = summary(totals)
  TotalDenominator <- sumtotals$n.cases
  TotCol[row_count] <<- TotalDenominator
  
  # for all treatments, get incidence for this parameter
  for (t in 1:nTreat) {
    value = aggreg[rownames(treatments)[t]]
    percent = 100.*value/denominators[t]
    Treat[row_count,t]<<-paste(value," (",round(percent,1),")",sep = "")
  }
  # total
  value = nrow(dataAge)
  percent = 100.*value/TotalDenominator
  TotCol[row_count] <<-paste(value," (",round(percent,1),")",sep = "")
  # FIXME - obtain p-value? not possible without more dimensions
  # pvalue[row_count] <<- round(pvalue_categorical(aggreg),3)
  pvalue[row_count] <<- ""
  print(paste("Created row",row_count,"with age category data"))
  row_count <<- row_count + 1			
}
# function for general numerical calculations ####
outputNumerical <- function(data_type){
  #
  # "Age"
  indep = data$TRT01P
  if (data_type=="Age") {
    dep = data$AGE
    totals <<- xtabs(~AGE, data=data,drop.unused.levels=TRUE)
    denominators <<- xtabs(~TRT01P,data=data,drop.unused.levels=TRUE)
    Col1[row_count] <<- "Age (yrs)"
  }
  else if (data_type=="Weight") {
    dep = data$WEIGHTBL
    totals <<- xtabs(~WEIGHTBL, data=data,drop.unused.levels=TRUE)
    denominators <<- xtabs(~TRT01P,data=data[data$WEIGHTBL,],drop.unused.levels=TRUE)
    Col1[row_count] <<- "Weight (kg)"
  }
  sumtotals = summary(totals)
  Col2[row_count] <<- "n*a"
  # total
  # for all treatments
  for (t in 1:nTreat) {
    Treat[row_count,t] <<- denominators[t]
  }
  TotalDenominator <- sumtotals$n.cases
  TotCol[row_count] <<- TotalDenominator
  pvalue[row_count] <<- ""
  print(paste("Created row",row_count,"with",data_type,"data"))
  row_count <<- row_count + 1 
  # mean data
  # Mean, Std. Dev., Median, Q1 Q3, Min, Max
  # loop for all in category
  clist <<- c("mean","sd","median","quantile","min")
  clists <<- c("Mean","Std. Dev.","Median","Q1, Q3","Min, Max")
  first = TRUE
  ii = 1
  for (i in clist) {
    Col1[row_count] <<- ""
    Col2[row_count] <<- clists[ii]
    values = tapply(dep,data$TRT01P,i,na.rm=TRUE)
    valuesMax = tapply(dep,data$TRT01P,max,na.rm=TRUE)
    # for all treatments, get descriptive statistic
    for (t in 1:nTreat) {
      value = values[rownames(treatments)[t]]
      if (i=="min") {
        value2 = valuesMax[rownames(treatments)[t]]
        Treat[row_count,t]<<- paste(round(value,0),", ",round(value2,0),sep = "")
      }
      else if (i=="quantile") {
        # get 1st and 3rd quantile
        names(value)="name"
        value1  = value$name[2]	
        value2 = value$name[4]
        Treat[row_count,t]<<- paste(round(value1,0),", ",round(value2,0),sep = "")
      } else {
        Treat[row_count,t]<<-round(value,1)
        
      }
    }
    # total column
    if (i=="mean") {
      value = mean(dep,na.rm=TRUE)
      sValue = round(value,1)
    } else 	if (i=="sd") {
      value = sd(dep,na.rm=TRUE)
      sValue = round(value,1)
    } else 	if (i=="median") {
      value = median(dep,na.rm=TRUE)
      sValue = round(value,1)
    } else 	if (i=="quantile") {
      value = quantile(dep,na.rm=TRUE)
      value1  = value[2]	
      value2 = value[4]
      sValue = paste(round(value1,0),", ",round(value2,0),sep = "")
    } else 	if (i=="min") {
      value1 = min(dep,na.rm=TRUE)
      value2 = max(dep,na.rm=TRUE)
      sValue = paste(round(value1,0),", ",round(value2,0),sep = "")
    }
    TotCol[row_count] <<- sValue
    # obtain p-value on first row only
    if (first) {
      pvalue[row_count] <<- round(pvalue_numerical(dep,indep),3)
    } else {
      pvalue[row_count] <<- ""	
    }
    first= FALSE
    print(paste("Created row",row_count,"with",data_type,"data"))
    ii = ii + 1
    row_count <<- row_count + 1			
  }
}
# Main section ####

data <- adsl

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

# Pack Header into RTF
header[2]=getStudyName()

for (t in 1:header_num) {
  addParagraph(rtf,header[t])
} 

addNewLine(rtf) 
# Determine the treatment names	
treatments <<- xtabs(~TRT01P, data=data,drop.unused.levels=TRUE)
nTreat <<- 2

# Initialize row counter
row_count <<- 1

# Handle Data for each section
outputCategorical("Sex")	
emptyRow()
outputNumerical("Age")	
emptyRow()
outputCategorical("Age")	
emptyRow()
outputAgeGreater(65)	
outputAgeGreater(75)	
emptyRow()
outputCategorical("Race")	
emptyRow()
outputCategorical("Ethnicity")	
# output all the rows
outputRows()
setFontSize(rtf,10)
# write out footers
for (t in 1:footer_num) {
  addParagraph(rtf,footer[t])
}
#closed file
done(rtf)