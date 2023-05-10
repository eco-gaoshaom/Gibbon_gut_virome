data <- read.table("genome.family_checkv_lifestyle",header = T)
data <- na.omit(data)

data$method <- factor(data$method,levels=c("VLPs","Bulk"))
ggplot(data)+
  stat_boxplot(geom = "errorbar",width=0.3,aes(method,completeness,color=method),position = position_dodge(1))+
  geom_boxplot(aes(method,completeness,color=method,fill=method), width=0.5,position = position_dodge(1),outlier.shape = NA)+
  scale_fill_manual(values=c(rgb(225,135,39,max=255),rgb(32,133,76,max=255)))+
  scale_color_manual(values=c(rgb(0,0,0,max=255),rgb(0,0,0,max=255)))+
  labs(y="Genome completeness")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=20,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  scale_y_continuous(limits = c(0,100),breaks=seq(0,100,20))+theme(legend.position = 'none')+
  coord_cartesian(ylim = c(0,100))
