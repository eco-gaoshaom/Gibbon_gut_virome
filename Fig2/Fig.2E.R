vlp_com <- read.table('VLPs_89sample.abun',header = T)
bulk_com <- read.table('Bulk_89sample.abun',header = T)

library(vegan)
vlp_com <- decostand(t(vlp_com),method = 'hellinger')
bulk_com <- decostand(t(bulk_com),method = 'hellinger')

dist_vlp <- vegdist(vlp_com,method='bray')
dist_bulk <- vegdist(bulk_com,method='bray')

mantel(dist_vlp,dist_bulk,method='pearson')
dist_vlp_num <- as.numeric(as.matrix(dist_vlp))
dist_bulk_num <- as.numeric(as.matrix(dist_bulk))
all <- data.frame(dist_vlp_num,dist_bulk_num)

ggplot(all)+
  geom_point(aes(dist_vlp_num,dist_bulk_num),size=2,color = rgb(220,220,220,max=255))+
  geom_smooth(aes(dist_vlp_num,dist_bulk_num),method='lm',fill=NA)+
  labs(x="Dissimilarity of viral community (VLPs-vOTUs)",y="Dissimilarity of viral community (Bulk-vOTUs)")+
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


dist_vlp_bet <- as.numeric(as.matrix(dist_vlp)[1:39,40:89])
dist_bulk_bet <- as.numeric(as.matrix(dist_bulk)[1:39,40:89])

dist_vlpa2 <- as.matrix(dist_vlp)[1:39,1:39]
dist_vlpb2 <- as.matrix(dist_vlp)[40:89,40:89]

dist_bulka2 <- as.matrix(dist_bulk)[1:39,1:39]
dist_bulkb2 <- as.matrix(dist_bulk)[40:89,40:89]

mantel(dist_vlpa2,dist_bulka2,method='pearson')
mantel(dist_vlpb2,dist_bulkb2,method='pearson')

vlp_dist <- c(dist_vlp_bet,
              as.numeric(dist_vlpa2),as.numeric(dist_vlpb2))
bulk_dist <- c(dist_bulk_bet,
               as.numeric(dist_bulka2),as.numeric(dist_bulkb2))
group <- c(replicate(length(dist_vlp_num),"Between A2 and B2"),
           replicate(length(as.numeric(dist_vlpa2)),"Within A2"),
           replicate(length(as.numeric(dist_vlpb2)),"Within B2"))

data <- data.frame(vlp_dist,bulk_dist,group)
data$group <- factor(data$group,levels = c("Between A2 and B2",
                                           "Within A2","Within B2"))
ggplot(data)+
  geom_point(aes(vlp_dist,bulk_dist,color=group),size=2)+
  geom_smooth(aes(vlp_dist,bulk_dist,color=group),method='lm',fill=NA)+
  scale_shape_manual(values=c(16,16))+
  scale_color_manual(values=c(rgb(250,192,15,max=255),
                              rgb(210,15,38,max=255),rgb(26,115,232,max=255)))+
  labs(x="Dissimilarity of viral community (VLPs-vOTUs)",y="Dissimilarity of viral community (Bulk-vOTUs)")+
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
