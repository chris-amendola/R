###############
###############
###############

install_head<-'C:\\Users\\camendola\\Google Drive\\CDISCr'

# Libraries
source(paste(install_head,'\\code\\main\\library_setup.R',sep=''))

# Local Functions
source(paste(install_head,'\\code\\func_lib\\base_lib.R',sep=''))

load(file = paste(install_head,"\\Data\\output\\adef.Rdata",sep=''))
contents(adef)

table_data<-adef[,c('AVISIT','AVALC','TRTP','VISITNUM')]%>%
            group_by(AVISIT,VISITNUM,TRTP,AVALC)%>%
            summarize(Count=n())

# The factors will set the display order in the graphs
# -AVISIT will be ordered by VISITNUM
table_data$AVISIT <- factor( table_data$AVISIT
                            ,levels = table_data$AVISIT[order(table_data$VISITNUM)])
# -AVALC has an semantic order
table_data$AVALC<-factor(table_data$AVALC,levels=c('Severe','Moderate','Mild','None'))

# These plots Don't make a case for drug working.
ggplot( data=table_data
        , aes(x=TRTP, y=Count, fill=AVALC)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  ggtitle('Frequency of Pain Scores at Each Visit') + 
  ylim(0,31) + 
  facet_grid(.~AVISIT)

# These plots Don't make a case for drug working.
ggplot( data=table_data
        , aes(x=TRTP, y=Count, fill=AVALC)) + 
  geom_bar(stat="identity") + 
  ggtitle('Frequency of Pain Scores at Each Visit') + 
  ylim(0,31) + 
  facet_grid(.~AVISIT)

# Change instead of raw values
chg_table_data<-adef[adef$AVISIT!='Baseline',c('AVISIT','CHG','TRTP','VISITNUM')]%>%
  group_by(AVISIT,VISITNUM,TRTP)%>%
  summarize(Avg=mean(CHG,na.rm=TRUE),SEM=sd(CHG,na.rm=TRUE))

# The factors will set the display order in the graphs
# -AVISIT will be ordered by VISITNUM
chg_table_data$AVISIT <- factor( chg_table_data$AVISIT
                             ,levels = chg_table_data$AVISIT[order(chg_table_data$VISITNUM)])

# These plots Don't make a case for drug working.
# These plots Don't make a case for drug working.
ggplot( data=chg_table_data
        , aes(x=TRTP, y=Avg,fill=TRTP)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Avg-SEM, ymax=Avg+SEM),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ggtitle('Average Change of Pain Scores after Baseline') + 
  ylim(1,-5) + 
  facet_grid(.~AVISIT)