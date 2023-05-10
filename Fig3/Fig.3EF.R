data<-read.table('Vir_90sample.abun',header = T)
group <- data$group
data <- data[,1:(ncol(data)-1)]
data<-decostand(data,method='hellinger')
data<-data.frame(data,group)
adonis(data[,1:854]~data[,855],data=data,permutations = 999,method='bray')

data <- data[,1:(ncol(data)-1)]
pcoa.data<-capscale(data~1,distance = 'bray')

sumd<-summary(pcoa.data)
sites<-as.data.frame(sumd$sites[,1:2])

texts<-rownames(data)
sites$group<-group
pc1<-round(sumd$cont$importance[2,1]*100,2)
pc2<-round(sumd$cont$importance[2,2]*100,2)
label1<-paste("PCo1 (",pc1,"% of total variation)",sep = "")
label2<-paste("PCo2 (",pc2,"% of total variation)",sep = "")
ggplot(sites,aes(MDS1,MDS2))+
  geom_point(size=2,aes(color=group,shape=group))+labs(x=label1,y=label2)+
  scale_color_manual(values=c(rgb(210,15,38,max=255),rgb(25,115,232,max=255)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=20,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  expand_limits(x=0,y=0)+theme(legend.position = 'none')+
  geom_hline(yintercept=0,linetype=4,color="grey",size=1)+ 
  geom_vline(xintercept=0,linetype=4,color="grey",size=1)



data<-read.table('Tem_100sample.abun',header = T)
group <- data$group
data <- data[,1:(ncol(data)-1)]
data<-decostand(data,method='hellinger')
data<-data.frame(data,group)
adonis(data[,1:1785]~data[,1786],data=data,permutations = 999,method='bray')

data <- data[,1:(ncol(data)-1)]
pcoa.data<-capscale(data~1,distance = 'bray')

sumd<-summary(pcoa.data)
sites<-as.data.frame(sumd$sites[,1:2])

texts<-rownames(data)
sites$group<-group
pc1<-round(sumd$cont$importance[2,1]*100,2)
pc2<-round(sumd$cont$importance[2,2]*100,2)
label1<-paste("PCo1 (",pc1,"% of total variation)",sep = "")
label2<-paste("PCo2 (",pc2,"% of total variation)",sep = "")
#par(oma=c(2,2,2,2))
ggplot(sites,aes(MDS1,MDS2))+
  geom_point(size=2,aes(color=group,shape=group))+labs(x=label1,y=label2)+
  #geom_text_repel(aes(MDS1,MDS2,label=texts,color=factor(group)),size=5)+
  scale_color_manual(values=c(rgb(210,15,38,max=255),rgb(25,115,232,max=255)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=20,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  expand_limits(x=0,y=0)+theme(legend.position = 'none')+
  geom_hline(yintercept=0,linetype=4,color="grey",size=1)+ 
  geom_vline(xintercept=0,linetype=4,color="grey",size=1)


vir_com <-read.table('Vir_90sample.abun',header = T)
vir_dec <- decostand(vir_com[,1:(ncol(vir_com)-1)],method='hellinger')
vir_dist <- vegdist(vir_dec,method = 'bray')
vir_dist <- as.matrix(vir_dist)
vir_dist_a2b2 <- as.numeric(vir_dist[1:40,41:90])


tem_com <-read.table('Tem_100sample.abun',header = T)
tem_dec <- decostand(tem_com[,1:(ncol(tem_com)-1)],method='hellinger')
tem_dist <- vegdist(tem_dec,method = 'bray')
tem_dist <- as.matrix(tem_dist)
tem_dist_a2b2 <- as.numeric(tem_dist[1:46,47:100])

group <- c(replicate(2000,"vir_dist_a2b2"),replicate(2484,"tem_dist_a2b2"))
dists <- c(vir_dist_a2b2,tem_dist_a2b2)

new <- data.frame(group,dists)
new$group <- factor(new$group,levels=c("vir_dist_a2b2","tem_dist_a2b2"))
ggplot(new,aes(group,dists))+
  ggdist::stat_halfeye(aes(color=group,fill=group),adjust = 5, width = .3, .width = 0, justification = -.9, point_colour = NA) + 
  geom_jitter(aes(group,dists,color=group),shape = 16,width = 0.15,alpha = 1,size=0.8)+
  scale_fill_manual(values=c(rgb(250,192,15,max=255),rgb(129,184,223,max=255)))+
  scale_color_manual(values=c(rgb(250,192,15,max=255),rgb(129,184,223,max=255)))+
  labs(y="Dissimilarity")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=20,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+theme(legend.position = 'none')+
  coord_cartesian(ylim = c(0,1))
