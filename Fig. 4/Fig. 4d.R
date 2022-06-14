library(ggplot2)
library(reshape2)
data <- read.table('Lachnospiraceae.host_vir.abun_dynamic',header = T)
dataf <- melt(data[,1:3],id.vars = c("Dates"))

ggplot(dataf) + geom_area(aes(Dates,value,fill = variable),alpha = 0.5,position = "identity")+
  scale_fill_manual(values=c(rgb(129,184,223,max=255),rgb(255,59,59,max=255)))+
  labs(x="Dates",y="Abundance")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        axis.text.x = element_blank(),
        axis.text.y=element_text(size=12,colour = 'black'),
        axis.line = element_line(size=0.5,colour="black"),
        legend.text = element_text(size=18),legend.position = "bottom")+
  #scale_x_continuous(limits = c(0,54),breaks=seq(0,54,1))+
  scale_x_continuous(limits = c(0,50),breaks=seq(0,50,1))+
  scale_y_continuous(limits = c(0,6000),breaks=seq(0,6000,1000))+
  geom_vline(xintercept=c(1,3,5,7,12,17,20,23,26,29,31,35,38,43,47),linetype=1,color="grey",size=1)
