data<-read.table('a2b2.vir_tem_host.pop_num',header = T)

data_vir <- data[which(data$group == "vir2host"),]
data_tem <- data[which(data$group == "tem2host"),]

data_a2<-data_vir[which(data_vir$ind == "A2"),]
data_b2<-data_vir[which(data_vir$ind == "B2"),]

lmall<-lm(votu~host_num,data=data_vir)
summary(lmall)
lma2<-lm(votu~host_num,data=data_a2)
summary(lma2)
lmb2<-lm(votu~host_num,data=data_b2)
summary(lmb2)
ggplot(data_vir)+
  geom_point(aes(votu,host_num,color=ind),size=2)+
  geom_smooth(aes(votu,host_num,color=ind),method='lm',fill=NA)+
  geom_smooth(aes(votu,host_num,color=ind),method='lm',fill=NA,color = rgb(250,192,15,max=255))+
  labs(x="Number of Vir-vOTUs",y="Number of prokaryotic populations")+
  scale_shape_manual(values=c(16,16))+
  scale_color_manual(values=c(rgb(210,15,38,max=255),rgb(26,115,232,max=255)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'bottom')+
  expand_limits(x=300,y=50)+
  scale_x_continuous(limits = c(300,700),breaks = seq(300,700,100))+
  scale_y_continuous(limits = c(50,300),breaks = seq(50,300,50))  



data_a2<-data_tem[which(data_tem$ind == "A2"),]
data_b2<-data_tem[which(data_tem$ind == "B2"),]

lmall<-lm(votu~host_num,data=data_tem)
summary(lmall)
lma2<-lm(votu~host_num,data=data_a2)
summary(lma2)
lmb2<-lm(votu~host_num,data=data_b2)
summary(lmb2)
ggplot(data_tem)+
  geom_point(aes(votu,host_num,color=ind),size=2)+
  geom_smooth(aes(votu,host_num,color=ind),method='lm',fill=NA)+
  geom_smooth(aes(votu,host_num,color=ind),method='lm',fill=NA,color = rgb(250,192,15,max=255))+
  labs(x="Number of Tem-vOTUs",y="Number of prokaryotic populations")+
  scale_shape_manual(values=c(16,16))+
  scale_color_manual(values=c(rgb(210,15,38,max=255),rgb(26,115,232,max=255)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'bottom')+
  expand_limits(x=750,y=50)+
  scale_x_continuous(limits = c(750,1350),breaks = seq(750,1350,200))+
  scale_y_continuous(limits = c(50,300),breaks = seq(50,300,50)) 

