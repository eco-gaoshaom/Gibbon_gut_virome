data <- read.table("89sample.VLPs_Bulk.vOTUs_num",header = T)
library(reshape2)
library(ggplot2)
data <- data[,1:4]
new <- melt(data,id.vars = c("sample"))

new$variable <- factor(new$variable,levels=c("vlp.specific","share.votu","bulk.specific"))
ggplot(new)+
  stat_boxplot(geom = "errorbar",width=0.3,aes(variable,value,color=variable),position = position_dodge(1))+
  geom_boxplot(aes(variable,value,color=variable,fill=variable), width=0.5,position = position_dodge(1),outlier.shape = NA)+
  scale_fill_manual(values=c(rgb(225,135,39,max=255),rgb(220,220,220,max=255),rgb(32,133,76,max=255)))+
  scale_color_manual(values=c(rgb(0,0,0,max=255),rgb(0,0,0,max=255),rgb(0,0,0,max=255)))+
  labs(y="Number of vOTUs")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=20,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+expand_limits(y=250)+
  scale_y_continuous(limits = c(250,850),breaks=seq(250,850,200))+theme(legend.position = 'none')+
  coord_cartesian(ylim = c(250,850))
