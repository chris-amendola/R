# Library Includes ####
library('haven')
library('dplyr')
library('tidyr')
library('DT')
library('rtf')
library('ggplot2')
library('rpivotTable')

# Root Dir ####
install_head<-'C:\\Users\\camendola\\Google Drive\\'
#"J:\\macros\\utildev\\tools\\sas_log_notes\\data\\"

# Load Matched Data ####
load(file=paste(install_head,'SAS LOG Analysis\\matched_notes.Rdata',sep=''))

# Read in category metadata ####
category_metadata<-read.csv(paste(install_head,'SAS LOG Analysis\\category_metadata_1.dlm',sep=''),sep='|')


# Summarise NOTE: messages by log file
category_summary<-matched_notes %>%
  group_by(log_file,category) %>%
  summarise(Count=n())                  

# Isolate the Notes of concern from summary
concern_categories<-category_metadata[category_metadata$concern==1,'category']
concerns<-category_summary[category_summary$category %in% concern_categories, ]

# Drop 'qc' and 'dev' dirs from the concerns summary
match<-'\\/qc\\/|\\/dev\\/'
concerns.nonqc<-concerns[!grepl(match,concerns$log_file), ]

# Drop 'qc' and 'dev' lines from the raw lines
matched.nonqc<-matched_notes[!grepl(match,matched_notes$log_file) , ]
# Isolate notes of concern from raw lines
raw_concerns<-matched.nonqc[ matched.nonqc$category %in% concern_categories, ]

#Summary of Error Types
p<-ggplot( data=as.data.frame(table(raw_concerns$category))
          , aes(x=Var1, y=Freq)) +
   geom_bar(stat="identity")+
   theme(text = element_text(size=15)) +
   ggtitle("Frequency of Potential Problem Notes - 'prog' directory logs") +
   labs(x="'NOTE:' Category")
p

as.data.frame(table(raw_concerns$category))

# Datatable of raw concerns ####
q<-datatable(raw_concerns,filter='top')
saveWidget(q,paste(install_head,'SAS LOG Analysis\\raw_concerns.html',sep=''))

ggplot(concerns.nonqc, aes(x=factor(category), y=Count)) + 
  geom_boxplot() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))

z<-rpivotTable( concerns.nonqc
               ,rows="log_file"
               ,col="category"
               ,aggregatorName="Total_Count"
               ,vals="Count")
saveWidget(z,paste(install_head,'SAS LOG Analysis\\pivot_notes.html',sep=''))
