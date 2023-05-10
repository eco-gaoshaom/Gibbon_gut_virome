data = read.table('90VLPsmean_100bulk.viromeQC',header = T)
library(ggplot2)
ssu_lsu_mean <- mean(data$SSU.LSU)
marker_mean <- mean(data$Marker_Rate)
data$type <- factor(data$type,levels = c("VLPs","Bulk"))
ggplot(data,aes(SSU.LSU,Marker_Rate))+
  geom_point(size=2,aes(color=type,shape=type))+
  scale_color_manual(values=c(rgb(225,135,39,max=255),rgb(32,133,76,max=255)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=20,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  expand_limits(x=0,y=0)+theme(legend.position = 'right')+
  geom_hline(yintercept=marker_mean,linetype=4,color="grey",size=1)+ 
  geom_vline(xintercept=ssu_lsu_mean,linetype=4,color="grey",size=1)
