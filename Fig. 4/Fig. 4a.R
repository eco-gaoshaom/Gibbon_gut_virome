data<-read.table('a2b2.vvotu_tvotu.abun',header = T)
library(reshape2)
library(ggsignif)
library(ggplot2)
data$group <- factor(data$group,levels=c("vvOTU-A2-F","vvOTU-A2-L",
                                         "vvOTU-B2-F","vvOTU-B2-L",
                                         "tvOTU-A2-F","tvOTU-A2-L",
                                         "tvOTU-B2-F","tvOTU-B2-L"))
ggplot(data)+#stat_boxplot(geom = "errorbar",width=0.25,aes(group,abun,color=group),position = position_dodge(1))+
  geom_boxplot(aes(x=group,y=abun,color=group),width=0.5,position = position_dodge(1),outlier.shape = NA)+
  geom_jitter(aes(x=group,y=abun,color=group),shape = 16,width = 0.2,alpha = 0.5)+
  scale_color_manual(values=c(rgb(92,35,102,max=255),rgb(6,223,6,max=255),
                              rgb(255,59,59,max=255),rgb(14,96,107,max=255),
                              rgb(92,35,102,max=255),rgb(6,223,6,max=255),
                              rgb(255,59,59,max=255),rgb(14,96,107,max=255)))+
  labs(x="Group",y="Abundance")+
  scale_y_continuous(limits = c(0,1200000),breaks=seq(0,1200000,300000))+
  theme_bw()+
  theme(
    legend.position = "right",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.margin=margin(0,0,0,0,"mm"),
    axis.text.x=element_text(size=rel(2),face="bold",angle = 45,hjust = 1),
    axis.text.y = element_text(size=rel(2),face="bold"),
    axis.title.x = element_text(size=rel(1.4),face="bold"),
    axis.title.y = element_text(size=rel(1.4),face="bold"),
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    legend.text=element_text(size=rel(1.1)),
    panel.grid = element_blank()
  )
