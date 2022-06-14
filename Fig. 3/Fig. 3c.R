data<-read.table('a2b2.fl.shared_specific.votu_vvotu_tvotu.num',header = T)
library(reshape2)
library(ggsignif)
library(ggplot2)

data$group <- factor(data$group,levels=c("A2-F-F&L-vvOTU","A2-L-F&L-vvOTU","A2-F-specific-vvOTU","A2-L-specific-vvOTU",
                                         "A2-F-F&L-tvOTU","A2-L-F&L-tvOTU","A2-F-specific-tvOTU","A2-L-specific-tvOTU"))
ggplot(data)+#stat_boxplot(geom = "errorbar",width=0.3,aes(group,votu,color=group),position = position_dodge(1))+
  geom_boxplot(aes(x=group,y=votu,color=group),width=0.5,position = position_dodge(1),outlier.shape = NA)+
  #geom_point(aes(x=group,y=votu,color=group),size = 1)+
  geom_jitter(aes(x=group,y=votu,color=group),shape = 16,height = 0.2,alpha = 0.5)+
  scale_color_manual(values=c(rgb(92,35,102,max=255),rgb(6,223,6,max=255),
                              rgb(255,59,59,max=255),rgb(14,96,107,max=255),
                              rgb(92,35,102,max=255),rgb(6,223,6,max=255),
                              rgb(255,59,59,max=255),rgb(14,96,107,max=255)))+
  #geom_point(data=c4,aes(x=group,y=value,color=group),shape=15,size=1)+
  #geom_line(data=c4,aes(x=group,y=value,color=group),size=1,linetype = "dotted")+
  labs(x="Individual",y="Number of vOTUs")+
  scale_y_continuous(limits = c(0,4000),breaks=seq(0,4000,500))+
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
