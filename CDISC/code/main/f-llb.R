###############
###############
###############

install_head<-'C:\\Users\\camendola\\Google Drive\\CDISCr'

# Libraries
source(paste(install_head,'\\code\\main\\library_setup.R',sep=''))

# Libraries
library(dplyr)
library(RColorBrewer)

# Read Data
load(file = paste(install_head,"\\Data\\output\\adsl.Rdata",sep=''))


z$TRTPC<-as.character(z$TRTPN)
analyte<-'Alanine Aminotransferase (IU/L)'

plot_ready<-z[z$PARAM==analyte , c('TRTPC','TRTP','AVISITN','AVAL') ] %>%
  group_by(TRTPC,AVISITN) %>%
  summarize( mean=mean(AVAL)
             ,n=n()
             ,uem=mean+(sd(AVAL)/sqrt(n()))
             ,lem=mean-(sd(AVAL)/sqrt(n())))

# Start a plot
pd <- position_dodge(0.1) 
ggplot(plot_ready, aes(x=AVISITN, y=mean, colour=TRTPC, group=TRTPC)) + 
  scale_color_brewer(palette = "Set1",type='qual') +
  geom_errorbar(aes(ymin=lem, ymax=uem),  width=3, position=pd) +
  geom_line(position=pd,size=1) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Treatment Number") +
  ylab("Mean (IU/L)") +
  ggtitle(analyte) +
  #expand_limits(y=0) +                        # Expand y range
  scale_y_continuous(breaks=8:20*4) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))               # Position legend in bottom right