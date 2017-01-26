# Library Includes ####
library('haven')
library('dplyr')
library('tidyr')
library('DT')
library('rtf')

# Root Dir ####
install_head<-'C:\\Users\\camendola\\Google Drive\\'
#"J:\\macros\\utildev\\tools\\sas_log_notes\\data\\"

# Load Log Scan Data ####
log_notes_lines<-read_sas(paste(install_head,'SAS LOG Analysis\\raw_sasnotes.sas7bdat',sep=''))
'Number of Log "NOTE:" lines found:'
nrow(log_notes_lines)
head(log_notes_lines,n=10)

# Get Frequency of distinct note messages ####
# "Compresses" raw data
note_summary<-log_notes_lines %>%
              group_by(note_line)%>%
              summarise(Freq=n())
nrow(note_summary)

# No Notes are categorized at first ####
note_summary$category<-'Un-Categorized'

# Read in category metadata ####
category_metadata<-read.csv(paste(install_head,'SAS LOG Analysis\\category_metadata_1.dlm',sep=''),sep='|')
# Lookup object ####
category_lookup<-setNames(category_metadata$marker,category_metadata$category)
overwrite_key<-setNames(category_metadata$overwrite,category_metadata$category)

# Categorize Note Summary ####
# Stuck with one line per category
for (cat in unique(names(category_lookup)))
{ note_summary$category<-ifelse( grepl( category_lookup[cat]
                                        ,note_summary$note_line)  
                                & ( note_summary$category=='Un-Categorized'
                                    | overwrite_key[note_summary$category]=='1')  
                                 ,cat
                                 ,note_summary$category)}
table(note_summary$category)
# Isolate un-categorized Notes ####
un_categorized<-note_summary[note_summary$category=='Un-Categorized',]

# Calculate Categorized Note Type Score ####
categorized_score<-round(1-nrow(un_categorized)/nrow(note_summary),digits=3)

#Create Review-able Datatables for each category
for (cat in unique(note_summary$category)){
  q<-datatable(note_summary[note_summary$category==cat,],filter='top')
  saveWidget(q,paste(install_head,'SAS LOG Analysis\\',cat,'.html',sep=''))}

nrow(note_summary[note_summary$category=='Dataset Read', ])

matched_notes<-merge( log_notes_lines[,c('note_line','log_file')]
                     ,note_summary[,c('note_line','category')]
                     ,by.x='note_line'
                     ,by.y='note_line')

category_proportions<-table(matched_notes$category)
category_percents<-as.data.frame(round(prop.table(category_proportions),digits=5)*100)
names(category_percents)<-c('Category','Percent')
category_percents$Pattern<-'???'

for (cat in unique(names(category_lookup))){
  print(paste('CATEGORY:',cat,sep=' '))
  print(paste('VALUE:',category_lookup[cat],sep=' '))
  category_percents$Pattern<-ifelse( category_percents$Category==cat
                                    ,as.character(category_lookup[cat])
                                    ,category_percents$Pattern)
}
raw_categorization_rate<-sum(category_percents[category_percents$Category!='Un-Categorized',c('Percent')])

rtf_doc<-RTF( paste(install_head,'SAS LOG Analysis\\','Category_Report.doc',sep='')
              ,width=8.5
              ,height=11
              ,font.size=11
              ,omi=c(.5,.5,.5,.5))
addText(rtf_doc,'Category Names and Regex Match Patterns\n',bold=TRUE,italic=TRUE)
addNewLine(rtf_doc)
addTable(rtf_doc,category_metadata[ ,c('category','marker')])
addNewLine(rtf_doc)
addPageBreak(rtf_doc)
addText(rtf_doc,'Percent NOTE:-Lines Categorized and Regex Patterns\n',bold=TRUE,italic=TRUE)
addNewLine(rtf_doc)
addTable(rtf_doc,category_percents[order(-category_percents$Percent), c('Category','Percent')])
addPageBreak(rtf_doc)
addText(rtf_doc,'Un-Categorized Note Lines\n',bold=TRUE,italic=TRUE)
addNewLine(rtf_doc)
addTable(rtf_doc,note_summary[note_summary$category=='Un-Categorized', ])
done(rtf_doc)

# Summarise NOTE: messages by log file
category_summary<-matched_notes %>%
                  group_by(log_file,category) %>%
                  summarise(Count=n())                  
# Flag the Concern/Not-Concern categories
concern_categories<-category_metadata[category_metadata$concern==1,'category']
concerns<-category_summary[category_summary$category %in% concern_categories, ]
q<-datatable(concerns,filter='top')
saveWidget(q,paste(install_head,'SAS LOG Analysis\\concerns.html',sep=''))

concern_map <- spread(concerns, category, Count,fill=0)
concern_map$log_file<-as.factor(concern_map$log_file)

