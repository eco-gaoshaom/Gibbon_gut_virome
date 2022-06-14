library(vegan)
library(ggplot2)

data<-read.table('a2b2.site_vvotu.abun',header = T)
group <- data$group
data <- data[,1:(ncol(data)-2)]
data<-decostand(data,method='hellinger')
data<-data.frame(data,group)
data_a2<-data[which(data$group == "A2"),]
data_b2<-data[which(data$group == "B2"),]
diet <- read.table('a2b2.vvotu.site_diet.com',header = T)
diet_a2<-diet[which(diet$individual == "A2"),]
diet_b2<-diet[which(diet$individual == "B2"),]

dist_data_a2 <- vegdist(data_a2[,1:(ncol(data_a2)-1)],method='bray')
dist_data_b2 <- vegdist(data_b2[,1:(ncol(data_b2)-1)],method='bray')

dist_diet_a2<- vegdist(diet_a2[,1:(ncol(diet_a2)-1)],method='bray')
dist_diet_b2 <- vegdist(diet_b2[,1:(ncol(diet_b2)-1)],method='bray')

mantel(dist_data_a2,dist_diet_a2,method='pearson')
mantel(dist_data_b2,dist_diet_b2,method='pearson')

dist_a2 <- data.frame(as.numeric(dist_data_a2),as.numeric(dist_diet_a2))
dist_b2 <- data.frame(as.numeric(dist_data_b2),as.numeric(dist_diet_b2))
colnames(dist_a2)<- c("com_dist","diet_dist")
colnames(dist_b2)<- c("com_dist","diet_dist")

group <- factor(c(rep('A2',780),rep('B2',1225)))
data <- rbind(dist_a2[1:780,],dist_b2[1:1225,])
data$group <- group

ggplot(data)+
  #geom_smooth(aes(diet_dist,com_dist),method = 'lm',color = rgb(250,192,15,max=255))+
  geom_point(aes(diet_dist,com_dist,color=group),size=0.5)+
  geom_smooth(aes(diet_dist,com_dist,color=group),method='lm',fill=NA)+
  #geom_smooth(aes(diet_dist,com_dist),method='lm',fill=NA,color = rgb(250,192,15,max=255))+
  labs(x="Dietary turnover",y="Virulent viral community turnover")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),
        axis.line = element_line(size=1,colour="black"),
        legend.text = element_text(size=15))+expand_limits(x=0,y=0)+
  scale_shape_manual(values=c(16,16))+
  scale_color_manual(values=c(rgb(255,59,59,max=255),rgb(14,96,107,max=255)))+
  scale_x_continuous(limits = c(0,1),breaks = seq(0,1,0.2))+
  scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.2)) 

data<-read.table('a2b2.site_tvotu.abun',header = T)
group <- data$group
data <- data[,1:(ncol(data)-2)]
data<-decostand(data,method='hellinger')
data<-data.frame(data,group)
data_a2<-data[which(data$group == "A2"),]
data_b2<-data[which(data$group == "B2"),]
diet <- read.table('a2b2.tvotu.site_diet.com',header = T)
diet_a2<-diet[which(diet$individual == "A2"),]
diet_b2<-diet[which(diet$individual == "B2"),]


dist_data_a2 <- vegdist(data_a2[,1:(ncol(data_a2)-1)],method='bray')
dist_data_b2 <- vegdist(data_b2[,1:(ncol(data_b2)-1)],method='bray')

dist_diet_a2<- vegdist(diet_a2[,1:(ncol(diet_a2)-1)],method='bray')
dist_diet_b2 <- vegdist(diet_b2[,1:(ncol(diet_b2)-1)],method='bray')

mantel(dist_data_a2,dist_diet_a2,method='pearson')
mantel(dist_data_b2,dist_diet_b2,method='pearson')

dist_a2 <- data.frame(as.numeric(dist_data_a2),as.numeric(dist_diet_a2))
dist_b2 <- data.frame(as.numeric(dist_data_b2),as.numeric(dist_diet_b2))
colnames(dist_a2)<- c("com_dist","diet_dist")
colnames(dist_b2)<- c("com_dist","diet_dist")

group <- factor(c(rep('A2',1035),rep('B2',1431)))
data <- rbind(dist_a2[1:1035,],dist_b2[1:1431,])
data$group <- group

ggplot(data)+
  geom_point(aes(diet_dist,com_dist,color=group),size=0.5)+
  geom_smooth(aes(diet_dist,com_dist,color=group),method='lm',fill=NA)+
  #geom_smooth(aes(diet_dist,com_dist),method='lm',fill=NA,color = rgb(250,192,15,max=255))+
  labs(x="Dietary turnover",y="Temperate turnover")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),
        axis.line = element_line(size=1,colour="black"),
        legend.text = element_text(size=15))+expand_limits(x=0,y=0)+
  scale_shape_manual(values=c(16,16))+
  scale_color_manual(values=c(rgb(255,59,59,max=255),rgb(15,153,183,max=255)))+
  scale_x_continuous(limits = c(0,1),breaks = seq(0,1,0.2))+
  scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.2))  
