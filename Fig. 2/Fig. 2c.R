data <- read.table('a2b2.shared_specific.votu_vvotu_tvotu.num',header=T)
data$groups <- factor(data$groups,levels=c("vvOTU-A2-shared","vvOTU-B2-shared","vvOTU-A2-specific","vvOTU-B2-specific",
                                           "tvOTU-A2-shared","tvOTU-B2-shared","tvOTU-A2-specific","tvOTU-B2-specific"))

ggplot(data)+
  #stat_boxplot(geom = "errorbar",width=0.4,aes(groups,votu,color=groups),position = position_dodge(1))+
  geom_boxplot(aes(x=groups,y=votu,color=groups),width=0.7,position = position_dodge(1),outlier.shape= NA)+
  geom_jitter(aes(x=groups,y=votu,color=groups),shape = 16,width = 0.2,alpha = 0.5)+
  scale_color_manual(values=c(rgb(255,59,59,max=255),rgb(14,96,107,max=255),
                              rgb(250,192,15,max=255),rgb(15,153,183,max=255),
                              rgb(6,223,6,max=255),rgb(129,184,223,max=255),
                              rgb(92,35,102,max=255),rgb(144,201,230,max=255)))+
  labs(x="Group",y="Number of vvOTUs/tvOTUs")+
  scale_y_continuous(limits = c(0,2500),breaks=seq(0,2500,500))+
  theme_bw()+
  theme(
    legend.position = "right",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.margin=margin(0,0,0,0,"mm"),
    axis.text.x=element_text(size=rel(1),face="bold",angle = 45,hjust = 1),
    axis.text.y = element_text(size=rel(1),face="bold"),
    axis.title.x = element_text(size=rel(1),face="bold"),
    axis.title.y = element_text(size=rel(1),face="bold"),
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    legend.text=element_text(size=rel(1.1)),
    panel.grid = element_blank()
  )
