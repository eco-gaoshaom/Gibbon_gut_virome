vir <- read.table("vir_89sample.abun",header = T)
hostv <- read.table("mag_89sample.abun",header = T)

tem <- read.table("tem_100sample.abun",header = T)
hostt <- read.table("mag_100sample.abun",header = T)

vira2 <- vir[,1:39]
hosta2 <- hostv[,1:39]
vira2 <- t(vira2)
hosta2 <- t(hosta2)
vira2 <- decostand(vira2,method = "hellinger")
hosta2 <- decostand(hosta2,method = 'hellinger')
vira2_dist <- vegdist(vira2,method = 'bray')
hosta2_dist <- vegdist(hosta2,method = 'bray')
mantel(vira2_dist,hosta2_dist)
vira2_dist_num <- as.numeric(vira2_dist)
hosta2_dist_num <- as.numeric(hosta2_dist)



virb2 <- vir[,40:89]
hostb2 <- hostv[,40:89]
virb2 <- t(virb2)
hostb2 <- t(hostb2)
virb2 <- decostand(virb2,method = "hellinger")
hostb2 <- decostand(hostb2,method = 'hellinger')
virb2_dist <- vegdist(virb2,method = 'bray')
hostb2_dist <- vegdist(hostb2,method = 'bray')
mantel(virb2_dist,hostb2_dist)
virb2_dist_num <- as.numeric(virb2_dist)
hostb2_dist_num <- as.numeric(hostb2_dist)


tema2 <- tem[,1:46]
hosta2 <- hostt[,1:46]
tema2 <- t(tema2)
hosta2 <- t(hosta2)
tema2 <- decostand(tema2,method = "hellinger")
hosta2 <- decostand(hosta2,method = 'hellinger')
tema2_dist <- vegdist(tema2,method = 'bray')
hosta2_dist <- vegdist(hosta2,method = 'bray')
mantel(tema2_dist,hosta2_dist)
tema2_dist_num <- as.numeric(tema2_dist)
temhosta2_dist_num <- as.numeric(hosta2_dist)



temb2 <- tem[,47:100]
hostb2 <- hostt[,47:100]
temb2 <- t(temb2)
hostb2 <- t(hostb2)
temb2 <- decostand(temb2,method = "hellinger")
hostb2 <- decostand(hostb2,method = 'hellinger')
temb2_dist <- vegdist(temb2,method = 'bray')
hostb2_dist <- vegdist(hostb2,method = 'bray')
mantel(temb2_dist,hostb2_dist)
temb2_dist_num <- as.numeric(temb2_dist)
temhostb2_dist_num <- as.numeric(hostb2_dist)


phage_dis <- c(vira2_dist_num,virb2_dist_num,tema2_dist_num,temb2_dist_num)
host_dis <- c(hosta2_dist_num,hostb2_dist_num,temhosta2_dist_num,temhostb2_dist_num)
group <- c(replicate(length(vira2_dist_num),"vir2host_a2"),replicate(length(virb2_dist_num),"vir2host_b2"),
           replicate(length(tema2_dist_num),"tem2host_a2"),replicate(length(temb2_dist_num),"tem2host_b2"))

dists <- data.frame(phage_dis,host_dis,group)
dists$group <- factor(dists$group,levels = c("vir2host_a2","vir2host_b2","tem2host_a2","tem2host_b2"))
ggplot(dists)+
  geom_point(aes(phage_dis,host_dis),size=2,color=rgb(220,220,220,max=255),alpha=0.5)+
  geom_smooth(aes(phage_dis,host_dis,color=group),method='lm',fill=NA)+
  labs(x="Viral community dissimilarity",y="Prokaryotic community dissimilarity")+
  scale_shape_manual(values=c(16,16,16,16))+
  scale_color_manual(values=c(rgb(210,15,38,max=255),rgb(26,115,232,max=255),
                              rgb(255,150,150,max=255),rgb(129,184,223,max=255)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'bottom')+
  expand_limits(x=0,y=0)+
  scale_x_continuous(limits = c(0,1),breaks = seq(0,1,0.2))+
  scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.2))  
