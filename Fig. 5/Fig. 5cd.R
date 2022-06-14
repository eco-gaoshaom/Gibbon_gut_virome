#virulent between a2b2, a2fl, b2fl

data<-read.table('a2b2.site_vvotu.abun',header = T)
data<-decostand(data[,1:(ncol(data)-2)],method='hellinger')
dists <- vegdist(data,method = 'bray')
dists <- as.matrix(dists)

dist_a2b2 <- dists[1:40,41:90]
dist_a2b2 <- dist_a2b2[upper.tri(dist_a2b2)]

dist_a2_fl <- dists[1:17,18:40]
dist_a2_fl <- dist_a2_fl[upper.tri(dist_a2_fl)]

dist_a2_f <- dists[1:17,1:17]
dist_a2_f <- dist_a2_f[upper.tri(dist_a2_f)]

dist_a2_l <- dists[18:40,18:40]
dist_a2_l <- dist_a2_l[upper.tri(dist_a2_l)]

dist_b2_fl <- dists[41:65,66:90]
dist_b2_fl <- dist_b2_fl[upper.tri(dist_b2_fl)]

dist_b2_f <- dists[41:65,41:65]
dist_b2_f <- dist_b2_f[upper.tri(dist_b2_f)]

dist_b2_l <- dists[66:90,66:90]
dist_b2_l <- dist_b2_l[upper.tri(dist_b2_l)]

dist_value <- c(dist_a2b2, dist_a2_fl, dist_a2_f, dist_a2_l, dist_b2_fl, dist_b2_f, dist_b2_l)
dist_type <- c(replicate(length(dist_a2b2),"dist_a2b2"),replicate(length(dist_a2_fl),"dist_a2_f2l"),
               replicate(length(dist_a2_f),"dist_a2_fl"),replicate(length(dist_a2_l),"dist_a2_fl"),
               replicate(length(dist_b2_fl),"dist_b2_f2l"),replicate(length(dist_b2_f),"dist_b2_fl"),
               replicate(length(dist_b2_l),"dist_b2_fl"))
new <- data.frame(dist_value,dist_type)
new$dist_type <- factor(new$dist_type,levels=c("dist_a2b2", "dist_a2_f2l", "dist_a2_fl",
                                               "dist_b2_f2l", "dist_b2_fl"))
ggplot(new)+
  #stat_boxplot(geom = "errorbar",width=0.25,aes(dist_type,dist_value,color=dist_type),position = position_dodge(1))+
  geom_boxplot(aes(x=dist_type,y=dist_value),color="black",width=0.15,position = position_dodge(1),outlier.shape = NA)+
  geom_violin(aes(x=dist_type,y=dist_value,color="black",fill=dist_type),alpha=0.5,width=0.9)+
  scale_fill_manual(values=c(rgb(15,153,183,max=255),
                             rgb(255,59,59,max=255),rgb(14,96,107,max=255),
                             rgb(250,192,15,max=255),rgb(129,184,223,max=255)))+
  #geom_point(data=c4,aes(x=group,y=value,color=group),shape=15,size=1)+
  #geom_line(data=c4,aes(x=group,y=value,color=group),size=1,linetype = "dotted")+
  labs(x="Group",y="Community dissimilarity")+
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+
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
    panel.grid = element_blank(),panel.border = element_blank()
  )


#temperate between a2b2, a2fl, b2fl

data<-read.table('a2b2.site_tvotu.abun',header = T)
data<-decostand(data[,1:(ncol(data)-2)],method='hellinger')
dists <- vegdist(data,method = 'bray')
dists <- as.matrix(dists)

dist_a2b2 <- dists[1:46,47:100]
dist_a2b2 <- dist_a2b2[upper.tri(dist_a2b2)]

dist_a2_fl <- dists[1:20,21:46]
dist_a2_fl <- dist_a2_fl[upper.tri(dist_a2_fl)]

dist_a2_f <- dists[1:20,1:20]
dist_a2_f <- dist_a2_f[upper.tri(dist_a2_f)]

dist_a2_l <- dists[21:46,21:46]
dist_a2_l <- dist_a2_l[upper.tri(dist_a2_l)]

dist_b2_fl <- dists[47:74,75:100]
dist_b2_fl <- dist_b2_fl[upper.tri(dist_b2_fl)]

dist_b2_f <- dists[47:74,47:74]
dist_b2_f <- dist_b2_f[upper.tri(dist_b2_f)]

dist_b2_l <- dists[75:100,75:100]
dist_b2_l <- dist_b2_l[upper.tri(dist_b2_l)]

dist_value <- c(dist_a2b2, dist_a2_fl, dist_a2_f, dist_a2_l, dist_b2_fl, dist_b2_f, dist_b2_l)
dist_type <- c(replicate(length(dist_a2b2),"dist_a2b2"),replicate(length(dist_a2_fl),"dist_a2_f2l"),
               replicate(length(dist_a2_f),"dist_a2_fl"),replicate(length(dist_a2_l),"dist_a2_fl"),
               replicate(length(dist_b2_fl),"dist_b2_f2l"),replicate(length(dist_b2_f),"dist_b2_fl"),
               replicate(length(dist_b2_l),"dist_b2_fl"))
new <- data.frame(dist_value,dist_type)
new$dist_type <- factor(new$dist_type,levels=c("dist_a2b2", "dist_a2_f2l", "dist_a2_fl",
                                               "dist_b2_f2l", "dist_b2_fl"))
ggplot(new)+
  #stat_boxplot(geom = "errorbar",width=0.25,aes(dist_type,dist_value,color=dist_type),position = position_dodge(1))+
  geom_boxplot(aes(x=dist_type,y=dist_value),color="black",width=0.15,position = position_dodge(1),outlier.shape = NA)+
  geom_violin(aes(x=dist_type,y=dist_value,color="black",fill=dist_type),alpha=0.5,width=1)+
  scale_fill_manual(values=c(rgb(15,153,183,max=255),
                             rgb(255,59,59,max=255),rgb(14,96,107,max=255),
                             rgb(250,192,15,max=255),rgb(129,184,223,max=255)))+
  #geom_point(data=c4,aes(x=group,y=value,color=group),shape=15,size=1)+
  #geom_line(data=c4,aes(x=group,y=value,color=group),size=1,linetype = "dotted")+
  labs(x="Group",y="Community dissimilarity")+
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+
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
    panel.grid = element_blank(),panel.border = element_blank()
  )
