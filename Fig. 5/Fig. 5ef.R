data<-read.table('a2b2.LEfSe.biomarker_vvotu.rela',header = T)

data$group <- factor(data$group,levels = c("a2b2","a2-fl","b2-fl"))
ggplot(data)+
  #stat_boxplot(geom = "errorbar",width=0.25,aes(group,rela,color=group),position = position_dodge(1))+
  geom_boxplot(aes(x=group,y=rela,color=group),width=0.5,position = position_dodge(1),outlier.shape = NA)+
  geom_jitter(aes(x=group,y=rela,color=group),shape = 16,width = 0.2,alpha = 0.5)+
  scale_color_manual(values=c(rgb(15,153,183,max=255),rgb(255,59,59,max=255),rgb(250,192,15,max=255)))+
  #geom_point(data=c4,aes(x=group,y=value,color=group),shape=15,size=1)+
  #geom_line(data=c4,aes(x=group,y=value,color=group),size=1,linetype = "dotted")+
  labs(x="group",y="rela")+
  scale_y_continuous(limits = c(0,0.03),breaks=seq(0,0.03,0.005))+
  theme_bw()+
  theme(
    legend.position = "right",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.margin=margin(0,0,0,0,"mm"),
    axis.text.x=element_text(size=rel(2),face="bold",angle = 0,hjust = 0.5),
    axis.text.y = element_text(size=rel(2),face="bold"),
    axis.title.x = element_text(size=rel(1.4),face="bold"),
    axis.title.y = element_text(size=rel(1.4),face="bold"),
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    legend.text=element_text(size=rel(1.1)),
    panel.grid = element_blank()
  )


data<-read.table('a2b2.LEfSe.biomarker_tvotu.rela',header = T)

data$group <- factor(data$group,levels = c("a2b2","a2-fl","b2-fl"))
ggplot(data)+
  #stat_boxplot(geom = "errorbar",width=0.25,aes(group,rela,color=group),position = position_dodge(1))+
  geom_boxplot(aes(x=group,y=rela,color=group),width=0.5,position = position_dodge(1),outlier.shape = NA)+
  geom_jitter(aes(x=group,y=rela,color=group),shape = 16,width = 0.2,alpha = 0.5)+
  scale_color_manual(values=c(rgb(15,153,183,max=255),rgb(255,59,59,max=255),rgb(250,192,15,max=255)))+
  #geom_point(data=c4,aes(x=group,y=value,color=group),shape=15,size=1)+
  #geom_line(data=c4,aes(x=group,y=value,color=group),size=1,linetype = "dotted")+
  labs(x="group",y="rela")+
  scale_y_continuous(limits = c(0,0.03),breaks=seq(0,0.03,0.005))+
  theme_bw()+
  theme(
    legend.position = "right",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.margin=margin(0,0,0,0,"mm"),
    axis.text.x=element_text(size=rel(2),face="bold",angle = 0,hjust = 0.5),
    axis.text.y = element_text(size=rel(2),face="bold"),
    axis.title.x = element_text(size=rel(1.4),face="bold"),
    axis.title.y = element_text(size=rel(1.4),face="bold"),
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    legend.text=element_text(size=rel(1.1)),
    panel.grid = element_blank()
  )
