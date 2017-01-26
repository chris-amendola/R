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
# Load category table ####
load(file=paste(install_head,'SAS LOG Analysis\\note_summary.Rdata',sep=''))

# Merge to apply categories to note-lines ####
matched_notes<-merge( log_notes_lines[,c('note_line','log_file','line_num')]
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

save(matched_notes,file=paste(install_head,'SAS LOG Analysis\\matched_notes.Rdata',sep=''))

# RTF Table of Categories Percents in Note Lines Sample ####
rtf_doc<-RTF( paste(install_head,'SAS LOG Analysis\\','Category_Assignments.doc',sep='')
              ,width=8.5
              ,height=11
              ,font.size=11
              ,omi=c(.5,.5,.5,.5))
addText(rtf_doc,'Percent NOTE:-Lines Categorized and Regex Patterns\n',bold=TRUE,italic=TRUE)
addNewLine(rtf_doc)
addTable(rtf_doc,category_percents[order(-category_percents$Percent), c('Category','Percent')])
addPageBreak(rtf_doc)
addText(rtf_doc,'Un-Categorized Note Lines\n',bold=TRUE,italic=TRUE)
addNewLine(rtf_doc)
addTable(rtf_doc,note_summary[note_summary$category=='Un-Categorized', ])
done(rtf_doc)


