data<-read.table('89sample.VLPs_Bulk.vOTUs_num',header = T)

data_a2<-data[which(data$ind == "A2"),]
data_b2<-data[which(data$ind == "B2"),]

lmall<-lm(vlp.votu~bulk.votu,data=data)
summary(lmall)
lma2<-lm(vlp.votu~bulk.votu,data=data_a2)
summary(lma2)
lmb2<-lm(vlp.votu~bulk.votu,data=data_b2)
summary(lmb2)
ggplot(data)+
  geom_point(aes(vlp.votu,bulk.votu,color=ind),size=2)+
  geom_smooth(aes(vlp.votu,bulk.votu,color=ind),method='lm',fill=NA)+
  geom_smooth(aes(vlp.votu,bulk.votu),method='lm',fill=NA,color = rgb(250,192,15,max=255))+
  labs(x="Number of VLPs-vOTUs",y="Number of Bulk-vOTUs")+
  scale_shape_manual(values=c(16,16))+
  scale_color_manual(values=c(rgb(210,15,38,max=255),rgb(26,115,232,max=255)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'bottom')+
  expand_limits(x=500,y=500)+
  scale_x_continuous(limits = c(500,1300),breaks = seq(500,1300,200))+
  scale_y_continuous(limits = c(500,1300),breaks = seq(500,1300,200))  

lmall<-lm(vlp.abun~bulk.abun,data=data)
summary(lmall)
lma2<-lm(vlp.abun~bulk.abun,data=data_a2)
summary(lma2)
lmb2<-lm(vlp.abun~bulk.abun,data=data_b2)
summary(lmb2)
ggplot(data)+
  geom_point(aes(vlp.abun,bulk.abun,color=ind),size=2)+
  geom_smooth(aes(vlp.abun,bulk.abun,color=ind),method='lm',fill=NA)+
  geom_smooth(aes(vlp.abun,bulk.abun),method='lm',fill=NA,color = rgb(250,192,15,max=255))+
  labs(x="Normalized abundance of VLPs-vOTUs (x104)",y="Normalized abundance of Bulk-vOTUs  (x104)")+
  scale_shape_manual(values=c(16,16))+
  scale_color_manual(values=c(rgb(210,15,38,max=255),rgb(26,115,232,max=255)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'bottom')+
  expand_limits(x=6,y=3)+
  scale_x_continuous(limits = c(6,46),breaks = seq(6,46,8))+
  scale_y_continuous(limits = c(3,27),breaks = seq(3,27,6))  
