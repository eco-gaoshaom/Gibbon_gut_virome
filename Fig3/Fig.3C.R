data <- read.table("a2b2.vir_tem.votu_num",header = T)
library(reshape2)
library(ggplot2)

data$lifestyle <- factor(data$lifestyle,levels=c("vir","tem","A2-vir","A2-tem","B2-vir","B2-tem"))
ggplot(data)+
  stat_boxplot(geom = "errorbar",width=0.3,aes(lifestyle,votu,color=lifestyle),position = position_dodge(1))+
  geom_boxplot(aes(lifestyle,votu,color=lifestyle,fill=lifestyle), width=0.5,position = position_dodge(1),outlier.shape = NA)+
  scale_fill_manual(values=c(rgb(250,192,15,max=255),rgb(129,184,223,max=255),
                             rgb(250,192,15,max=255),rgb(129,184,223,max=255),
                             rgb(250,192,15,max=255),rgb(129,184,223,max=255)))+
  scale_color_manual(values=c(rgb(0,0,0,max=255),rgb(0,0,0,max=255),rgb(0,0,0,max=255),
                              rgb(0,0,0,max=255),rgb(0,0,0,max=255),rgb(0,0,0,max=255)))+
  labs(y="Number of vOTUs")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=20,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+expand_limits(y=300)+
  scale_y_continuous(limits = c(300,1500),breaks=seq(300,1500,300))+theme(legend.position = 'none')+
  coord_cartesian(ylim = c(300,1500))
