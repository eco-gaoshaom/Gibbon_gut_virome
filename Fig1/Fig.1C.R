library(ggdist)
library(ggplot2)
data <- read.table("110VLPs_100bulk.reads_num",header = T)
data$group <- factor(data$group,levels=c("VLPs-A2","VLPs-B2","Bulk-A2","Bulk-B2"))
ggplot(data, aes(group,total)) + 
  ggdist::stat_halfeye(aes(color=type,fill=type),adjust = 1, width = .3, .width = 0, justification = -.9, point_colour = NA) + 
  geom_jitter(aes(color=type),width = .2) +
  scale_fill_manual(values=c(rgb(210,15,38,max=255),rgb(25,115,232,max=255),
                             rgb(255,150,150,max=255),rgb(129,184,223,max=255)))+
  scale_color_manual(values=c(rgb(210,15,38,max=255),rgb(25,115,232,max=255),
                              rgb(255,150,150,max=255),rgb(129,184,223,max=255)))+
  labs(y="Sequencing depth (paired reads, x10^6)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=20,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  scale_y_continuous(limits = c(0,250),breaks=seq(0,250,50))+theme(legend.position = 'none')+
  coord_cartesian(ylim = c(0,250))
